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
const prettier = require("prettier");
const fsAccess = util.promisify(fs.access);
const fsWriteFile = util.promisify(fs.writeFile);
const fsMakeDir = util.promisify(fs.mkdir);
const fsReadFile = util.promisify(fs.readFile);
const fsUnlink = util.promisify(fs.unlink);
const worker = require("./../Morphir.Elm.CLI").Elm.Morphir.Elm.CLI.init();
require('log-timestamp');
const program = new commander_1.Command();
program
    .name('morphir typescript-gen')
    .description('Generate typescript code from Morphir IR')
    .option('-i, --input <path>', 'Source location where the Morphir IR will be loaded from.', 'morphir-ir.json')
    .option('-o, --output <path>', 'Target location where the generated code will be saved.', './dist')
    .option('-c, --copy-deps', 'Copy the dependencies used by the generated code to the output path.', false)
    .parse(process.argv);
const mapCommandOptions = (programOpts) => {
    return {
        target: "TypeScript",
        input: programOpts.input,
        output: programOpts.output,
        copyDeps: programOpts.copyDeps,
    };
};
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
    await fsMakeDir(outputPath, { recursive: true });
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
        await fsMakeDir(fileDir, { recursive: true });
        const filePath = path_1.default.join(fileDir, fileName);
        const incomingContent = prettier.format(content, { parser: "typescript" });
        try {
            await fsAccess(filePath, fs.constants.F_OK);
            const existingContent = await fsReadFile(filePath);
            if (existingContent.toString() == incomingContent) {
                //console.log(`No Changes Detected - ${filePath}`);
            }
            else {
                console.log(`UPDATE - ${filePath}`);
            }
        }
        catch (_) {
            console.log(`INSERT - ${filePath}`);
        }
        await fsWriteFile(filePath, incomingContent);
    });
    const filesToDelete = await cli.findFilesToDelete(outputPath, generatedFiles);
    const deletePromises = filesToDelete.map(async (fileToDelete) => {
        console.log(`DELETE - ${fileToDelete}`);
        return fsUnlink(fileToDelete);
    });
    // Always copy redistributables for TypeScript (matching old CLI behavior)
    const copyFiles = (src, dest) => {
        const sourceDirectory = path_1.default.join(path_1.default.dirname(__dirname), "..", "redistributable", src);
        cli.copyRecursiveSync(sourceDirectory, outputPath);
    };
    copyFiles("TypeScript/", outputPath);
    return Promise.all(writePromises.concat(deletePromises));
};
gen(program.opts().input, path_1.default.resolve(program.opts().output), mapCommandOptions(program.opts()))
    .then(() => {
    console.log("Done");
})
    .catch((err) => {
    console.log(err);
    process.exit(1);
});
