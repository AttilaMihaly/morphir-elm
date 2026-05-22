#!/usr/bin/env node
"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
const path_1 = __importDefault(require("path"));
const commander_1 = require("commander");
const execa_1 = __importDefault(require("execa"));
const legacyAPI_1 = require("./legacyAPI");
require("log-timestamp");
const program = new commander_1.Command();
program
    .name("morphir-elm gen")
    .description("Generate code from Morphir IR")
    .option("-i, --input <path>", "Source location where the Morphir IR will be loaded from.", "morphir-ir.json")
    .option("-o, --output <path>", "Target location where the generated code will be saved.", "./dist")
    .option("-t, --target <type>", "Language to Generate (Scala | SpringBoot | cypher | triples | TypeScript).", "Scala")
    .option("-e, --target-version <version>", "Language version to Generate.", "2.11")
    .option("-c, --copy-deps", "Copy the dependencies used by the generated code to the output path.", false)
    .option("-m, --modules-to-include <comma.separated,list.of,module.names>", "Limit the set of modules that will be included.")
    .option("-s, --include-codecs", "Generate JSON codecs", false)
    .option("-f, --filename <filename>", "Filename of the generated JSON Schema.", "")
    .option("-ls, --include <comma.separated,list.of,strings>", "Limit what will be included.", "")
    .option("-dec, --decorations <filename>", "JSON file with decorations")
    .parse(process.argv);
const options = program.opts();
const backendTarget = options.target;
const showDeprecationMessage = (cmd, opts) => {
    console.warn(`This Command is Deprecated. Switching to morphir ${cmd}-gen`);
    console.info(`Running => morphir ${cmd}-gen ${opts}`);
};
if (backendTarget === "TypeScript") {
    const args = [`--input=${options.input}`, `--output=${options.output}`];
    if (options.copyDeps) {
        args.push("--copy-deps");
    }
    showDeprecationMessage("typescript", args.join(" "));
    const morphirPath = path_1.default.join(__dirname, "morphir-typescript-gen.js");
    (0, execa_1.default)("node", [morphirPath, ...args], { stdio: "inherit" })
        .then(() => {
        console.log("Done.");
    })
        .catch((err) => {
        console.error(err);
        process.exit(1);
    });
}
else {
    (0, legacyAPI_1.gen)(options.input, path_1.default.resolve(options.output), options)
        .then(() => {
        console.log("Done.");
    })
        .catch((err) => {
        console.error(err);
        process.exit(1);
    });
}
