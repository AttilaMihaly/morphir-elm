#!/usr/bin/env node
import path from "path";
import { Command } from "commander";

const packageJson = require(path.join(__dirname, "../../package.json"));

const program = new Command();
program
  .version(packageJson.version, "-v, --version")
  .command("make", "Translate Elm sources to Morphir IR")
  .command("gen", "Generate code from Morphir IR")
  .command("develop", "Start up a web server and expose developer tools through a web UI")
  .command("test", "Start Testing all the test cases present in morphir-ir.json")
  .command("treeview", "Start up a web server and expose treeview through a web UI")
  .parse(process.argv);
