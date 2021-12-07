#!/usr/bin/env node
'use strict'

// NPM imports
const path = require('path')
const util = require('util')
const fs = require('fs')
const stat = util.promisify(fs.stat)
const readFile = util.promisify(fs.readFile)
const writeFile = util.promisify(fs.writeFile)
const readdir = util.promisify(fs.readdir)
const commander = require('commander')
const { table } = require('table');

// Elm imports
const workerClass =
    require('./Morphir.Elm.IncrementalFrontendWorker')
        .Elm.Morphir.Elm.IncrementalFrontendWorker

// logging
require('log-timestamp')

// Set up Commander
const program = new commander.Command()
program
    .name('morphir-elm make')
    .description('Translate Elm sources to Morphir IR')
    .option('-p, --project-dir <path>', 'Root directory of the project where morphir.json is located.', '.')
    .option('-o, --output <path>', 'Target file location where the Morphir IR will be saved.', 'morphir-ir.json')
    .option('-t, --types-only', 'Only include type information in the IR, no values.', false)
    .parse(process.argv)

make(program.projectDir, program.opts())
    .then((result) => {
        if (result) {
            console.log(`Writing file ${program.output}.`)
            writeFile(program.output, JSON.stringify(result, null, 4))
                .then(() => {
                    console.log('Done.')
                })
        }
    })
    .catch((err) => {
        if (err instanceof Error) {
            console.error(err)
        } else if (typeof err === "object" && !Array.isArray(err) && err !== null) {
            console.error(JSON.stringify(err, null, 4))
        } else {
            console.error(err)
        }
    })


async function make(projectDir, options) {
    const morphirJsonPath = path.join(projectDir, 'morphir.json')
    const morphirJsonContent = await readFile(morphirJsonPath)
    const morphirJson = JSON.parse(morphirJsonContent.toString())
    const morphirIRPath = path.join(projectDir, 'morphir-ir.json')
    const flags = {
        packageName: morphirJson.name,
        exposedModules: morphirJson.exposedModules,
        existingDistribution: null
    }
    let lastMakeTime = 0
    if (await fileExist(morphirIRPath)) {
        lastMakeTime = (await stat(morphirIRPath)).mtimeMs
        console.log(`Existing morphir-ir.json detected. Only incremental changes since ${lastMakeTime} will be applied.`)
        flags.existingDistribution = JSON.parse((await readFile(morphirIRPath)).toString())
    } else {
        console.log("No existing morphir-ir.json detected. Building all from scratch.")
    }
    const worker = workerClass.init({ flags })

    const elmSources = await readElmSources(path.join(projectDir, morphirJson.sourceDirectory))
    console.log("Read the following source files:\n" + sourceFilesToTable(elmSources, flags.packageName))
    if (elmSources.filter(f => f.modifiedAtMillis > lastMakeTime).length == 0) {
        console.log("Nothing changed since IR was last generated.")

        return null
    } else {
        console.log(`${elmSources.filter(f => f.changed).length} out of ${elmSources.length} Elm files have changed.`)

        return await insertModuleSources(worker, elmSources)
    }
}


async function insertModuleSources(worker, moduleSources) {
    return new Promise((resolve, reject) => {
        // reject
        worker.ports.reportError.subscribe(err => {
            reject(err)
        })
        worker.ports.reportParseError.subscribe(err => {
            reject(err)
        })
        worker.ports.reportPackageError.subscribe(err => {
            reject(err)
        })
        // resolve
        worker.ports.insertModuleSourcesComplete.subscribe(result => {
            resolve(result)
        })
        worker.ports.insertModuleSources.send(moduleSources)
    })
}

async function readElmSources(baseDir) {
    const readElmSource = async function (filePath) {
        const fullPath = path.join(baseDir, filePath)
        const stats = await stat(fullPath)
        const content = await readFile(fullPath)
        return {
            path: filePath,
            modifiedAtMillis: stats.mtimeMs,
            content: content.toString()
        }
    }
    const readDir = async function (currentDir) {
        const entries = await readdir(path.join(baseDir, currentDir), {
            withFileTypes: true
        })
        const elmSources =
            entries
                .filter(entry => entry.isFile() && entry.name.endsWith('.elm'))
                .map(entry => readElmSource(path.join(currentDir, entry.name)))
        const subDirSources =
            entries
                .filter(entry => entry.isDirectory())
                .map(entry => readDir(path.join(currentDir, entry.name)))
                .reduce(async (soFarPromise, nextPromise) => {
                    const soFar = await soFarPromise
                    const next = await nextPromise
                    return soFar.concat(next)
                }, Promise.resolve([]))
        return elmSources.concat(await subDirSources)
    }

    return Promise.all(await readDir("."))
}

// UTILS

async function fileExist(filePath) {
    return new Promise((resolve, reject) => {
        fs.access(filePath, fs.F_OK, (err) => {
            if (err) {
                resolve(false)
            } else {
                resolve(true)
            }
        })
    });
}

function sourceFilesToTable(sourceFiles, packageName) {
    const data = sourceFiles.map((file) => {
        return [
            pathToModuleName(file.path, packageName),
            file.modifiedAtMillis
        ]
    })

    const config = {
        singleLine: true
    }

    return table(data, config)
}

function pathToModuleName(filePath, packageName) {
    const process = function (packageParts, pathParts) {
        if (packageParts.length == 0) {
            return pathParts
        } else if (packageParts[0] == pathParts[0]) {
            return process(packageParts.slice(1), pathParts.slice(1))
        } else {
            return null
        }
    }
    const modulePath = process(packageName.split("."), filePath.split("\\"))
    return modulePath.join(".")
}