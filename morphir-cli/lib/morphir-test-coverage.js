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
// NPM Imports
const commander_1 = require("commander");
const fs = __importStar(require("fs"));
const path = __importStar(require("path"));
const util = __importStar(require("util"));
const cli_1 = __importDefault(require("./cli"));
// logging
require("log-timestamp");
const fsWriteFile = util.promisify(fs.writeFile);
const program = new commander_1.Command();
program
    .name("morphir test-coverage")
    .description("Generates report on number of branches in a Morphir value and TestCases covered")
    .option("-i, --ir <path>", "Source location where the Morphir IR will be loaded from.", "morphir-ir.json")
    .option("-t, --tests <path>", "Source location where the Morphir Test Json will be loaded from.", "morphir-tests.json")
    .option("-o, --output <path>", "Source location where the Morphir Test Coverage result will be ouput to.", ".")
    .parse(process.argv);
const { ir: irPath, tests: irTestPath, output: output } = program.opts();
cli_1.default.testCoverage(irPath, irTestPath, output, program.opts())
    .then((data) => {
    fsWriteFile(path.join(output, "morphir-test-coverage.json"), JSON.stringify(data));
})
    .catch((err) => {
    console.log("err --", err);
    process.exit(1);
});
