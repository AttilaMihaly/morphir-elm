#!/usr/bin/env node

import path from "path";
import { Command } from "commander";
import execa from "execa";
import { gen } from "./legacyAPI";

require("log-timestamp");

const program = new Command();
program
  .name("morphir-elm gen")
  .description("Generate code from Morphir IR")
  .option("-i, --input <path>", "Source location where the Morphir IR will be loaded from.", "morphir-ir.json")
  .option("-o, --output <path>", "Target location where the generated code will be saved.", "./dist")
  .option(
    "-t, --target <type>",
    "Language to Generate (Scala | SpringBoot | cypher | triples | TypeScript).",
    "Scala"
  )
  .option("-e, --target-version <version>", "Language version to Generate.", "2.11")
  .option("-c, --copy-deps", "Copy the dependencies used by the generated code to the output path.", false)
  .option(
    "-m, --modules-to-include <comma.separated,list.of,module.names>",
    "Limit the set of modules that will be included."
  )
  .option("-s, --include-codecs", "Generate JSON codecs", false)
  .option("-f, --filename <filename>", "Filename of the generated JSON Schema.", "")
  .option("-ls, --include <comma.separated,list.of,strings>", "Limit what will be included.", "")
  .option("-dec, --decorations <filename>", "JSON file with decorations")
  .parse(process.argv);

const options = program.opts();
const backendTarget = options.target;

const showDeprecationMessage = (cmd: string, opts: string) => {
  console.warn(`This Command is Deprecated. Switching to morphir ${cmd}-gen`);
  console.info(`Running => morphir ${cmd}-gen ${opts}`);
};

if (backendTarget === "TypeScript") {
  const args = [`--input=${options.input}`, `--output=${options.output}`];
  if (options.copyDeps) {
    args.push("--copy-deps");
  }
  showDeprecationMessage("typescript", args.join(" "));
  const morphirPath = path.join(__dirname, "morphir-typescript-gen.js");
  execa("node", [morphirPath, ...args], { stdio: "inherit" })
    .then(() => {
      console.log("Done.");
    })
    .catch((err) => {
      console.error(err);
      process.exit(1);
    });
} else {
  gen(options.input, path.resolve(options.output), options as any)
    .then(() => {
      console.log("Done.");
    })
    .catch((err) => {
      console.error(err);
      process.exit(1);
    });
}
