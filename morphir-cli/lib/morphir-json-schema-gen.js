#!/usr/bin/env node
"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
// NPM imports
const commander_1 = require("commander");
const path_1 = __importDefault(require("path"));
const cli_1 = __importDefault(require("./cli"));
const config_processing_1 = __importDefault(require("./config-processing"));
require('log-timestamp');
const program = new commander_1.Command;
program
    .name('morphir json-schema-gen')
    .description('Generate Json Schema from Morphir IR')
    .option('-i, --input <path>', 'Source location where the Morphir IR will be loaded from.', 'morphir-ir.json')
    .option('-o, --output <path>', 'Target location where the generated code will be saved.', './dist')
    .option('-t, --target <type>', 'Language to Generate.', 'JsonSchema')
    .option('-e, --target-version <version>', 'Language version to Generate.', '2020-12')
    .option('-f, --filename <filename>', 'Filename of the generated JSON Schema.', '')
    .option('-m, --limit-to-modules <comma.separated,list.of,module.names>', 'Limit the set of modules that will be included.', '')
    .option('-g, --group-schema-by <string>', 'Group generate schema by package, module or type.', 'package')
    .option('-c, --use-config', 'Use configuration specified in the config file.', false)
    .option('-ls, --include <comma.separated,list.of,strings>', 'Limit what will be included.', '')
    .option('-d, --use-decorators', 'Read configuration based on decorators in decorator dictionary.', false)
    .parse(process.argv);
config_processing_1.default.inferBackendConfig(program.opts()).then((options) => {
    cli_1.default.gen(program.opts().input, path_1.default.resolve(program.opts().output), options)
        .then(() => {
        console.log("Done");
    })
        .catch((err) => {
        console.log(err);
        process.exit(1);
    });
});
