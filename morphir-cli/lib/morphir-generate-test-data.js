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
Object.defineProperty(exports, "__esModule", { value: true });
// NPM imports
const commander_1 = require("commander");
const fs = __importStar(require("fs"));
const util = __importStar(require("util"));
const path = __importStar(require("path"));
const fsExists = util.promisify(fs.exists);
const fsWriteFile = util.promisify(fs.writeFile);
const fsMakeDir = util.promisify(fs.mkdir);
const fsReadFile = util.promisify(fs.readFile);
const readdir = util.promisify(fs.readdir);
const worker = require('./../Morphir.Elm.Generator').Elm.Morphir.Elm.Generator.init();
// logging
require('log-timestamp');
// Set up Commander
const program = new commander_1.Command();
program
    .name('morphir generate-test-data')
    .description('Generate test data for Models (types) in a Morphir IR')
    .option('-p, --project-dir <path>', 'Root directory of the project where morphir.json is located.', '.')
    .option('-o, --output <path>', 'Target file location where the test data will be saved.', 'test-data.json')
    .option('--seed <seed>', 'seed to use for randomness', Date.now().toString())
    .option('--size <size>', 'size of the data to be generated for each target', '1')
    .option('--targets <typefqns...>', 'Fully qualified names of types you want to generate test data for.')
    .option('--config <path-to-config>', 'specify a json file where configuration can be read from. Overrides other command options.')
    .parse(process.argv);
// run data generation
async function generateData() {
    const programOptions = program.opts();
    // CREATE CONFIG OPTIONS
    const morphirJsonPath = path.join(programOptions.projectDir, 'morphir-ir.json');
    if (!(await fsExists(morphirJsonPath)))
        throw Error('Not a morphir directory');
    const distroData = (await fsReadFile(morphirJsonPath)).toString();
    const distroJson = JSON.parse(distroData);
    if (!programOptions.targets || programOptions.targets.length <= 0)
        throw 'targets not provided';
    const opts = {
        morphirIrJson: distroJson,
        targets: programOptions.targets,
        seed: parseInt(programOptions.seed),
        size: parseInt(programOptions.size)
    };
    // SEND OFF TO ELM
    worker.ports.decodeFailed.subscribe((err) => {
        console.log('Decode Failed');
        console.log(err);
    });
    worker.ports.generationFailed.subscribe((err) => {
        console.log('Generation Failed', err);
        console.log(err);
    });
    worker.ports.generated.subscribe((data) => {
        const dataString = JSON.stringify(data, null, 4);
        const outputPath = path.join(programOptions.projectDir, programOptions.output);
        console.log('Writing test data to ' + outputPath);
        fsWriteFile(outputPath, dataString)
            .then(() => console.log('Done.'))
            .catch(err => {
            console.log(err);
        });
    });
    console.log('starting test data generation with options', opts);
    worker.ports.generate.send(opts);
}
generateData();
