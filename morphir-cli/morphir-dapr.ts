#!/usr/bin/env node

import path from "path";
import fs from "fs";
import util from "util";
import { Command } from "commander";
import childProc from "child_process";
// eslint-disable-next-line @typescript-eslint/no-var-requires
const fsExtra = require("fs-extra");

const readdir = util.promisify(fs.readdir);
const readFile = util.promisify(fs.readFile);
const writeFile = util.promisify(fs.writeFile);

// Compiled Elm worker, produced into morphir-cli/Morphir.Elm.DaprCLI.js
// (one level up from the lib/ output directory).
const worker = require("./../Morphir.Elm.DaprCLI").Elm.Morphir.Elm.DaprCLI.init();

type SourceFile = { path: string; content: string };
type MorphirDaprJson = { sourceDirectories: string[] };

const program = new Command();
program
  .name("morphir-dapr ")
  .description("Generate Dapr Application from Morphir Model")
  .option("-p, --project-dir <path>", "Root directory of the project where morphir-dapr.json is located.", ".")
  .option(
    "-o, --output <path>",
    "Target location where the Dapr sources will be sent. Will create it if it does not exist",
    "dapr-output"
  )
  .option("-i, --info", "Print dapr intermediate output (elm) to STDOUT.")
  .option("-d, --delete", "Delete build directory")
  .parse(process.argv);

const opts = program.opts();

async function readElmSources(dirs: string[]): Promise<SourceFile[]> {
  const readElmSource = async (filePath: string): Promise<SourceFile> => {
    const content = await readFile(filePath);
    return { path: filePath, content: content.toString() };
  };
  const readDir = async (currentDir: string): Promise<SourceFile[]> => {
    const entries = await readdir(currentDir, { withFileTypes: true });
    const elmSources = entries
      .filter((entry) => entry.isFile() && entry.name.endsWith(".elm"))
      .map((entry) => readElmSource(path.join(currentDir, entry.name)));
    const subDirSources = await entries
      .filter((entry) => entry.isDirectory())
      .map((entry) => readDir(path.join(currentDir, entry.name)))
      .reduce(async (soFarPromise, nextPromise) => {
        const soFar = await soFarPromise;
        const next = await nextPromise;
        return soFar.concat(next);
      }, Promise.resolve([] as SourceFile[]));
    return (await Promise.all(elmSources)).concat(subDirSources);
  };
  const sources = await Promise.all(dirs.map((dir) => readDir(dir)));
  return sources.flat();
}

async function packageDefAndDaprCodeFromSrc(
  morphirJson: MorphirDaprJson,
  sourceFiles: SourceFile[]
): Promise<{ elmBackendResult: string }> {
  return new Promise((resolve, reject) => {
    worker.ports.decodeError.subscribe((err: unknown) => {
      reject(err);
    });
    worker.ports.packageDefAndDaprCodeFromSrcResult.subscribe(
      ([err, ok]: [unknown, { elmBackendResult: string }]) => {
        if (err) {
          reject(err);
        } else {
          resolve(ok);
        }
      }
    );
    worker.ports.packageDefinitionFromSource.send([morphirJson, sourceFiles]);
  });
}

async function gen(
  projectDir: string,
  output: string,
  debug: boolean,
  deleteBuildDir: boolean
): Promise<{ elmBackendResult: string }> {
  const morphirJsonPath = path.join(projectDir, "morphir-dapr.json");
  const morphirJson = JSON.parse((await readFile(morphirJsonPath)).toString()) as MorphirDaprJson;
  const sourceFiles = await readElmSources(morphirJson.sourceDirectories);
  const result = await packageDefAndDaprCodeFromSrc(morphirJson, sourceFiles);

  if (debug) {
    console.log(JSON.stringify(result.elmBackendResult));
  }

  const buildDir = "dapr-stuff";

  console.log(`Clearing ${buildDir} ...`);
  fs.rmSync(buildDir, { recursive: true, force: true });

  console.log(`Creating build directory: ${buildDir}`);
  fs.mkdirSync(buildDir);

  console.log(`Writing dapr files to ${buildDir} ...`);
  await writeFile(`${buildDir}/Main.elm`, result.elmBackendResult);

  // __dirname here is morphir-cli/lib; assets live one level up.
  const assetsDir = path.join(path.dirname(__dirname), "assets");
  const repoSrcDir = path.join(path.dirname(path.dirname(__dirname)), "src");

  console.log(`Copying compilation assets to ${buildDir} ...`);
  fsExtra.copySync(assetsDir, buildDir);
  fsExtra.copySync(repoSrcDir, buildDir);

  console.log(`Copying original sources to ${buildDir}`);
  morphirJson.sourceDirectories.forEach((dir) => {
    fsExtra.copySync(String(dir), buildDir);
  });

  console.log(`Using local elm-platform to perform final compilation`);
  childProc.execSync(`cd ${buildDir} && elm make Main.elm --output=Main.js`);

  console.log(`Copying files to output directory...`);
  if (!fs.existsSync(output)) {
    fs.mkdirSync(output);
  }
  fsExtra.copySync(`${buildDir}/DaprAppShell.js`, `${output}/DaprAppShell.js`);
  fsExtra.copySync(`${buildDir}/Main.js`, `${output}/Main.js`);
  fsExtra.copySync(`${buildDir}/package.json`, `${output}/package.json`);

  if (deleteBuildDir) {
    fs.rmSync(buildDir, { recursive: true, force: true });
  }

  return result;
}

gen(opts.projectDir, opts.output, opts.info, opts.delete)
  .then(() => {
    console.log("Done!");
  })
  .catch((err: NodeJS.ErrnoException) => {
    if (err && err.code === "ENOENT") {
      console.error(`Could not find file at '${err.path}'`);
    } else {
      console.error(err);
    }
    process.exit(1);
  });
