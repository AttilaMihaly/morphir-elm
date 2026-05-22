#!/usr/bin/env node
"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
const path_1 = __importDefault(require("path"));
const commander_1 = require("commander");
const packageJson = require(path_1.default.join(__dirname, "../../package.json"));
const program = new commander_1.Command();
program
    .version(packageJson.version, "-v, --version")
    .command("make", "Translate Elm sources to Morphir IR")
    .command("gen", "Generate code from Morphir IR")
    .command("develop", "Start up a web server and expose developer tools through a web UI")
    .command("test", "Start Testing all the test cases present in morphir-ir.json")
    .command("treeview", "Start up a web server and expose treeview through a web UI")
    .parse(process.argv);
