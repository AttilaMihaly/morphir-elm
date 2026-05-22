#!/usr/bin/env node

import { Command } from "commander";
import { make } from "./cliAPI";
import * as legacy from "./legacyAPI";

require("log-timestamp");

const program = new Command();
program
  .name("morphir-elm make")
  .description("Translate Elm sources to Morphir IR")
  .option("-p, --project-dir <path>", "Root directory of the project where morphir.json is located.", ".")
  .option("-o, --output <path>", "Target file location where the Morphir IR will be saved.", "morphir-ir.json")
  .option("-t, --types-only", "Only include type information in the IR, no values.", false)
  .option("-f, --fallback-cli", "Use the legacy CLI worker. Needed when compiling the morphir-elm Elm package itself.", false)
  .option("-i, --indent-json", "Use indentation in the generated JSON file.", false)
  .option(
    "-I, --include [pathOrUrl...]",
    "Include additional Morphir distributions as a dependency. Can be specified multiple times. Can be a path, url, or data-url."
  )
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
    .catch((err: NodeJS.ErrnoException) => {
      if (err && err.code === "ENOENT") {
        console.error(`Could not find file at '${err.path}'`);
      } else if (err instanceof Error) {
        console.error(err);
      } else {
        console.error(`Error: ${JSON.stringify(err, null, 2)}`);
      }
      process.exit(1);
    });
} else {
  make(opts.projectDir, opts);
}
