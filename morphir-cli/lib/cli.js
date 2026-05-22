#!/usr/bin/env node
"use strict";
var __createBinding = (this && this.__createBinding) || (Object.create ? (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    var desc = Object.getOwnPropertyDescriptor(m, k);
    if (!desc || ("get" in desc ? !m.__esModule : desc.writable || desc.configurable)) {
      desc = { enumerable: true, get: function() { return m[k]; } };
    }
    Object.defineProperty(o, k2, desc);
}) : (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    o[k2] = m[k];
}));
var __setModuleDefault = (this && this.__setModuleDefault) || (Object.create ? (function(o, v) {
    Object.defineProperty(o, "default", { enumerable: true, value: v });
}) : function(o, v) {
    o["default"] = v;
});
var __importStar = (this && this.__importStar) || function (mod) {
    if (mod && mod.__esModule) return mod;
    var result = {};
    if (mod != null) for (var k in mod) if (k !== "default" && Object.prototype.hasOwnProperty.call(mod, k)) __createBinding(result, mod, k);
    __setModuleDefault(result, mod);
    return result;
};
const fs = __importStar(require("fs"));
const util = __importStar(require("util"));
const path = __importStar(require("path"));
const FileChanges = __importStar(require("./FileChanges"));
const Dependencies = __importStar(require("./dependencies"));
const dependencies_1 = require("./dependencies");
const zod_1 = require("zod");
const fsExists = util.promisify(fs.exists);
const fsWriteFile = util.promisify(fs.writeFile);
const fsMakeDir = util.promisify(fs.mkdir);
const fsReadFile = util.promisify(fs.readFile);
const readdir = util.promisify(fs.readdir);
const worker = require("./../Morphir.Elm.CLI").Elm.Morphir.Elm.CLI.init();
const Includes = zod_1.z.array(zod_1.z.string()).optional();
async function make(projectDir, options) {
    // Morphir specific files expected to be in the project directory
    const morphirJsonPath = path.join(projectDir, "morphir.json");
    const hashFilePath = path.join(projectDir, "morphir-hashes.json");
    const morphirIrPath = path.join(projectDir, "morphir-ir.json");
    // Load the `morphir.json` file that describes the project
    const morphirJson = JSON.parse((await fsReadFile(morphirJsonPath)).toString());
    const includes = Includes.parse(options.include);
    const dependencyConfig = dependencies_1.DependencyConfig.parse({
        dependencies: morphirJson.dependencies,
        localDependencies: morphirJson.localDependencies,
        includes: includes,
        projectDir: projectDir
    });
    //load List Of Dependency IR
    const dependencies = await Dependencies.loadAllDependencies(dependencyConfig);
    // check the status of the build incremental flag
    if (options.buildIncrementally == false) {
        // We invoke file change detection but pass in no hashes which will generate inserts only
        const fileChanges = await FileChanges.detectChanges(new Map(), path.join(projectDir, morphirJson.sourceDirectory));
        const fileSnapshot = FileChanges.toFileSnapshotJson(fileChanges);
        const newIR = await buildFromScratch(morphirJson, fileSnapshot, dependencies, options);
        await writeContentHashes(hashFilePath, FileChanges.toContentHashes(fileChanges));
        return newIR;
    }
    else {
        if ((await fsExists(morphirIrPath)) && (await fsExists(hashFilePath))) {
            const oldContentHashes = await readContentHashes(hashFilePath);
            const fileChanges = await FileChanges.detectChanges(oldContentHashes, path.join(projectDir, morphirJson.sourceDirectory));
            if (reportFileChangeStats(fileChanges)) {
                console.log("There were file changes and there is an existing IR. Building incrementally.");
                const previousIR = (await fsReadFile(morphirIrPath)).toString();
                const updatedIR = await buildIncrementally(morphirJson, fileChanges, options, dependencies, previousIR);
                await writeContentHashes(hashFilePath, FileChanges.toContentHashes(fileChanges));
                return updatedIR;
            }
            else {
                console.log("There were no file changes and there is an existing IR. No actions needed.");
            }
        }
        else {
            console.log("Building from scratch.");
            // We invoke file change detection but pass in no hashes which will generate inserts only
            const fileChanges = await FileChanges.detectChanges(new Map(), path.join(projectDir, morphirJson.sourceDirectory));
            const fileSnapshot = FileChanges.toFileSnapshotJson(fileChanges);
            const newIR = await buildFromScratch(morphirJson, fileSnapshot, dependencies, options);
            await writeContentHashes(hashFilePath, FileChanges.toContentHashes(fileChanges));
            return newIR;
        }
    }
}
async function buildFromScratch(morphirJson, fileSnapshot, dependencies, options) {
    return new Promise((resolve, reject) => {
        worker.ports.decodeFailed.subscribe((err) => {
            reject(err);
        });
        worker.ports.buildFailed.subscribe((err) => {
            reject(err);
        });
        worker.ports.reportProgress.subscribe((message) => {
            console.log(message);
        });
        worker.ports.buildCompleted.subscribe(([err, ok]) => {
            if (err) {
                reject(err);
            }
            else {
                resolve(JSON.stringify(ok, null, options.indentJson ? 4 : 0));
            }
        });
        const opts = {
            typesOnly: options.typesOnly,
        };
        worker.ports.buildFromScratch.send({
            options: opts,
            packageInfo: morphirJson,
            dependencies: dependencies,
            fileSnapshot: fileSnapshot,
        });
    });
}
async function buildIncrementally(morphirJson, fileChanges, options, dependencies, previousIR) {
    return new Promise((resolve, reject) => {
        worker.ports.decodeFailed.subscribe((err) => {
            reject(err);
        });
        worker.ports.buildFailed.subscribe((err) => {
            reject(err);
        });
        worker.ports.reportProgress.subscribe((message) => {
            console.log(message);
        });
        worker.ports.buildCompleted.subscribe(([err, ok]) => {
            if (err) {
                reject(err);
            }
            else {
                resolve(JSON.stringify(ok, null, options.indentJson ? 4 : 0));
            }
        });
        const opts = {
            typesOnly: options.typesOnly,
        };
        let maybeDistribution = null;
        if (previousIR) {
            maybeDistribution = JSON.parse(previousIR);
        }
        worker.ports.buildIncrementally.send({
            options: opts,
            packageInfo: morphirJson,
            fileChanges: FileChanges.toFileChangesJson(fileChanges),
            dependencies: dependencies,
            distribution: maybeDistribution,
        });
    });
}
/**
 * Read content hashes from a file.
 *
 * @param filePath file path to read hashes from
 * @returns map of hashes
 */
async function readContentHashes(filePath) {
    // Check if the file exists
    if (await fsExists(filePath)) {
        const contentHashesJson = JSON.parse((await fsReadFile(filePath)).toString());
        const contentHashesMap = new Map();
        for (let path in contentHashesJson) {
            contentHashesMap.set(path, contentHashesJson[path]);
        }
        return contentHashesMap;
    }
    else {
        return new Map();
    }
}
/**
 * Write content hashes into a file.
 *
 * @param filePath file path to read hashes from
 * @returns map of hashes
 */
async function writeContentHashes(filePath, hashes) {
    const jsonObject = {};
    for (let [path, hash] of hashes) {
        jsonObject[path] = hash;
    }
    await writeFile(filePath, JSON.stringify(jsonObject, null, 4));
}
function reportFileChangeStats(fileChanges) {
    const stats = FileChanges.toStats(fileChanges);
    if (FileChanges.hasChanges(stats)) {
        const message = [
            `- inserted:  ${stats.inserted}`,
            `- updated:   ${stats.updated}`,
            `- deleted:   ${stats.deleted}`,
            `- unchanged: ${stats.unchanged}`,
        ].join("\n  ");
        console.log(`The following file changes were detected:\n  ${message}`);
        return true;
    }
    else {
        console.log(`No file changes were detected.`);
        return false;
    }
}
const stats = async (input, outputPath, options) => {
    await fsMakeDir(outputPath, {
        recursive: true,
    });
    const collectStats = async (ir) => {
        return new Promise((resolve, reject) => {
            worker.ports.jsonDecodeError.subscribe((err) => {
                reject(err);
            });
            worker.ports.statsResult.subscribe(([err, ok]) => {
                if (err) {
                    reject(err);
                }
                else {
                    resolve(ok);
                }
            });
            worker.ports.stats.send(ir);
        });
    };
    const morphirIrJson = await fsReadFile(path.resolve(input));
    const stats = await collectStats(JSON.parse(morphirIrJson.toString()));
    const writePromises = stats.map(async ([[dirPath, fileName], content]) => {
        const fileDir = dirPath.reduce((accum, next) => path.join(accum, next), outputPath);
        const filePath = path.join(fileDir, fileName);
        if (await fileExist(filePath)) {
            const existingContent = await fsReadFile(filePath);
            if (existingContent.toString() !== content) {
                await fsWriteFile(filePath, content);
                console.log(`UPDATE - ${filePath}`);
            }
        }
        else {
            await fsMakeDir(fileDir, {
                recursive: true,
            });
            await fsWriteFile(filePath, content);
            console.log(`INSERT - ${filePath}`);
        }
    });
    return Promise.all(writePromises);
};
const fileExist = async (filePath) => {
    return new Promise((resolve, reject) => {
        fs.access(filePath, fs.constants.F_OK, (err) => {
            if (err) {
                resolve(false);
            }
            else {
                resolve(true);
            }
        });
    });
};
const findFilesToDelete = async (outputPath, fileMap) => {
    const readDir = async function (currentDir, generatedFiles) {
        const entries = await readdir(currentDir, {
            withFileTypes: true,
        });
        const filesToDelete = entries
            .filter((entry) => {
            const entryPath = path.join(currentDir, entry.name);
            return entry.isFile() && !generatedFiles.includes(entryPath);
        })
            .map((entry) => path.join(currentDir, entry.name));
        const subDirFilesToDelete = entries
            .filter((entry) => entry.isDirectory())
            .map((entry) => readDir(path.join(currentDir, entry.name), generatedFiles))
            .reduce(async (soFarPromise, nextPromise) => {
            const soFar = await soFarPromise;
            const next = await nextPromise;
            return soFar.concat(next);
        }, Promise.resolve([]));
        return filesToDelete.concat(await subDirFilesToDelete);
    };
    const files = fileMap.map(([[dirPath, fileName], content]) => {
        const fileDir = dirPath.reduce((accum, next) => path.join(accum, next), outputPath);
        return path.resolve(fileDir, fileName);
    });
    return Promise.all(await readDir(outputPath, files));
};
function copyRedistributables(options, outputPath) {
    const copyFiles = (src, dest) => {
        const sourceDirectory = path.join(path.dirname(__dirname), "redistributable", src);
        copyRecursiveSync(sourceDirectory, outputPath);
    };
    copyFiles("Scala/sdk/src", outputPath);
    copyFiles(`Scala/sdk/src-${options.targetVersion}`, outputPath);
}
function copyRecursiveSync(src, dest) {
    const exists = fs.existsSync(src);
    if (exists) {
        const stats = exists && fs.statSync(src);
        const isDirectory = exists && stats.isDirectory();
        if (isDirectory) {
            if (!fs.existsSync(dest))
                fs.mkdirSync(dest);
            fs.readdirSync(src).forEach(function (childItemName) {
                copyRecursiveSync(path.join(src, childItemName), path.join(dest, childItemName));
            });
        }
        else {
            fs.copyFileSync(src, dest);
            console.log(`COPY - ${dest}`);
        }
    }
}
async function writeFile(filePath, content) {
    await fsMakeDir(path.dirname(filePath), {
        recursive: true,
    });
    return await fsWriteFile(filePath, content);
}
const generate = async (options, ir) => {
    return new Promise((resolve, reject) => {
        worker.ports.jsonDecodeError.subscribe((err) => {
            reject(err);
        });
        worker.ports.generateResult.subscribe(([err, ok]) => {
            if (err) {
                reject(err);
            }
            else {
                resolve(ok);
            }
        });
        worker.ports.generate.send([options, ir, []]);
    });
};
const gen = async (input, outputPath, options) => {
    await fsMakeDir(outputPath, {
        recursive: true,
    });
    const morphirIrJson = await fsReadFile(path.resolve(input));
    const generatedFiles = await generate(options, JSON.parse(morphirIrJson.toString()));
    const writePromises = generatedFiles.map(async ([[dirPath, fileName], content]) => {
        const fileDir = dirPath.reduce((accum, next) => path.join(accum, next), outputPath);
        const filePath = path.join(fileDir, fileName);
        if (await fileExist(filePath)) {
            const existingContent = await fsReadFile(filePath);
            if (existingContent.toString() !== content) {
                await fsWriteFile(filePath, content);
                console.log(`UPDATE - ${filePath}`);
            }
        }
        else {
            await fsMakeDir(fileDir, {
                recursive: true,
            });
            await fsWriteFile(filePath, content);
            console.log(`INSERT - ${filePath}`);
        }
    });
    const filesToDelete = await findFilesToDelete(outputPath, generatedFiles);
    const deletePromises = filesToDelete.map(async (fileToDelete) => {
        console.log(`DELETE - ${fileToDelete}`);
        return fs.unlinkSync(fileToDelete);
    });
    return Promise.all(writePromises.concat(deletePromises));
};
async function writeDockerfile(projectDir, programOpts) {
    // read docker template file
    let filePath = "./cli2/DockerTemplateFile";
    let fileContent = await fsReadFile(filePath, "utf-8");
    // replace specific characteres with the required
    let newContent = fileContent.replace("PROJECT_MODEL_DIR", projectDir.replace(/\\/g, "/"));
    // controlling ending slash in path
    let removeTrailingSlash = (str) => {
        return str.endsWith("/") ? str.slice(0, -1).trim() : str.trim();
    };
    let dockerfilePath = removeTrailingSlash(projectDir + "/Dockerfile");
    // check if there is an existing Dockerfile in projectDir
    if ((await fsExists(dockerfilePath)) && programOpts.force == false) {
        throw new Error("Dockerfile already exist. To overwrite please use the `-f` flag");
    }
    else {
        await fsWriteFile(dockerfilePath, newContent);
    }
}
async function testCoverage(irPath, testsPath, outputPath, options) {
    // Morphir IR
    const morphirIR = await fsReadFile(path.resolve(irPath));
    const morphirIRJson = JSON.parse(morphirIR.toString());
    // read Morphir Test
    const morphirTest = await fsReadFile(path.resolve(testsPath));
    const morphirTestJson = JSON.parse(morphirTest.toString());
    // output path 
    const output = path.join(path.resolve(outputPath), "morphir-test-coverage.json");
    return new Promise((resolve, reject) => {
        worker.ports.testCoverageResult.subscribe(([err, data]) => {
            if (err) {
                reject(err);
            }
            else {
                resolve(data);
            }
        });
        // send files through port
        worker.ports.testCoverage.send([morphirIRJson, morphirTestJson]);
    });
}
module.exports = {
    gen,
    make,
    writeFile,
    fileExist,
    stats,
    writeDockerfile,
    findFilesToDelete,
    copyRedistributables,
    testCoverage,
    worker,
    copyRecursiveSync,
};
