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
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
//NPM imports
const fs = __importStar(require("fs"));
const path_1 = __importDefault(require("path"));
const commander_1 = require("commander");
const cli = require("./cli");
const util = __importStar(require("util"));
const fsWriteFile = util.promisify(fs.writeFile);
const fsMakeDir = util.promisify(fs.mkdir);
const fsReadFile = util.promisify(fs.readFile);
const worker = require("./../Morphir.Elm.CLI").Elm.Morphir.Elm.CLI.init();
require('log-timestamp');
const generate = async (options, ir, testSuite) => {
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
        worker.ports.generate.send([options, ir, testSuite]);
    });
};
const gen = async (input, outputPath, options) => {
    await fsMakeDir(outputPath, {
        recursive: true,
    });
    const morphirIrJson = await fsReadFile(path_1.default.resolve(input));
    let morphirTestsJSONContent = [];
    try {
        const bufferContent = await fsReadFile(path_1.default.resolve('./morphir-tests.json'));
        morphirTestsJSONContent = JSON.parse(bufferContent.toString());
    }
    catch (_) {
        console.log("could not read morphir-tests.json, defaulting to an empty test!");
    }
    const generatedFiles = await generate(options, JSON.parse(morphirIrJson.toString()), morphirTestsJSONContent);
    const writePromises = generatedFiles.map(async ([[dirPath, fileName], content]) => {
        const fileDir = dirPath.reduce((accum, next) => path_1.default.join(accum, next), outputPath);
        const filePath = path_1.default.join(fileDir, fileName);
        if (await cli.fileExist(filePath)) {
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
    const filesToDelete = await cli.findFilesToDelete(outputPath, generatedFiles);
    const deletePromises = filesToDelete.map(async (fileToDelete) => {
        console.log(`DELETE - ${fileToDelete}`);
        return fs.unlinkSync(fileToDelete);
    });
    cli.copyRedistributables(options, outputPath);
    return Promise.all(writePromises.concat(deletePromises));
};
const program = new commander_1.Command();
program
    .name('morphir scala-gen')
    .description('Generate scala code from Morphir IR')
    .option('-i, --input <path>', 'Source location where the Morphir IR will be loaded from.', 'morphir-ir.json')
    .option('-o, --output <path>', 'Target location where the generated code will be saved.', './dist')
    .option('-t, --target <type>', 'Language to Generate.', 'Scala')
    .option('-e, --target-version <version>', 'Language version to Generate.', '2.11')
    .option('-c, --copy-deps', 'Copy the dependencies used by the generated code to the output path.', false)
    .option('-m, --limitToModules <comma.separated,list.of,module.names>', 'Limit the set of modules that will be included.', '')
    .option('-s, --include-codecs <type>', 'Generate the scala codecs as well', false)
    .option('--generate-test-generic', 'Generate generic test cases from morphir tests that can be used for testing', false)
    .option('--generate-test-scalatest', 'Generate runnable scalatest test cases', false)
    .parse(process.argv);
gen(program.opts().input, path_1.default.resolve(program.opts().output), program.opts())
    .then(() => {
    console.log("Done");
})
    .catch((err) => {
    console.log(err);
    process.exit(1);
});
