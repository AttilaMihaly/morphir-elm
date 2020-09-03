#!/usr/bin/env node
'use strict'

// NPM imports
const path = require('path')
const util = require('util')
const fs = require('fs')
const readdir = util.promisify(fs.readdir)
const lstat = util.promisify(fs.lstat)
const readFile = util.promisify(fs.readFile)
const writeFile = util.promisify(fs.writeFile)
const commander = require('commander')
const cli = require('./cli')

// Set up Commander
const program = new commander.Command()
program
    .name('morphir-elm make')
    .description('Translate Elm sources to Morphir IR')
    .option('-p, --project-dir <path>', 'Root directory of the project where morphir.json is located.', '.')
    .option('-o, --output <path>', 'Target location where the Morphir IR will be sent. Defaults to STDOUT.')
    .parse(process.argv)


cli.make(program.projectDir)
    .then((packageDef) => {
        if (output) {
            console.log(`Writing file ${output}.`)
            writeFile(output, JSON.stringify(packageDef, null, 4))
                .then(() => {
                    console.log('Done.')
                })
                .catch((err) => {
                    console.error(`Could not write file: ${err}`)
                })
        } else {
            console.log(JSON.stringify(packageDef))
        }
    })
    .catch((err) => {
        if (err.code == 'ENOENT') {
            console.error(`Could not find file at '${err.path}'`)
        } else {
            console.error(JSON.stringify(err))
        }
        process.exit(1)
    })
