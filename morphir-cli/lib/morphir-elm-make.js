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
const commander_1 = require("commander");
const cliAPI_1 = require("./cliAPI");
const legacy = __importStar(require("./legacyAPI"));
require("log-timestamp");
const program = new commander_1.Command();
program
    .name("morphir-elm make")
    .description("Translate Elm sources to Morphir IR")
    .option("-p, --project-dir <path>", "Root directory of the project where morphir.json is located.", ".")
    .option("-o, --output <path>", "Target file location where the Morphir IR will be saved.", "morphir-ir.json")
    .option("-t, --types-only", "Only include type information in the IR, no values.", false)
    .option("-f, --fallback-cli", "Use the legacy CLI worker. Needed when compiling the morphir-elm Elm package itself.", false)
    .option("-i, --indent-json", "Use indentation in the generated JSON file.", false)
    .option("-I, --include [pathOrUrl...]", "Include additional Morphir distributions as a dependency. Can be specified multiple times. Can be a path, url, or data-url.")
    .parse(process.argv);
const opts = program.opts();
if (opts.fallbackCli) {
    legacy
        .make(opts.projectDir, opts)
        .then((packageDef) => {
        console.log(`Writing file ${opts.output}.`);
        return legacy.writeFile(opts.output, JSON.stringify(packageDef, null, opts.indentJson ? 4 : 0));
    })
        .then(() => {
        console.log("Done.");
    })
        .catch((err) => {
        if (err && err.code === "ENOENT") {
            console.error(`Could not find file at '${err.path}'`);
        }
        else if (err instanceof Error) {
            console.error(err);
        }
        else {
            console.error(`Error: ${JSON.stringify(err, null, 2)}`);
        }
        process.exit(1);
    });
}
else {
    (0, cliAPI_1.make)(opts.projectDir, opts);
}
