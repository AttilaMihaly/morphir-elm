#!/usr/bin/env node
"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
const path_1 = __importDefault(require("path"));
const fs_1 = __importDefault(require("fs"));
const util_1 = __importDefault(require("util"));
const commander_1 = require("commander");
const child_process_1 = __importDefault(require("child_process"));
// eslint-disable-next-line @typescript-eslint/no-var-requires
const fsExtra = require("fs-extra");
const readdir = util_1.default.promisify(fs_1.default.readdir);
const readFile = util_1.default.promisify(fs_1.default.readFile);
const writeFile = util_1.default.promisify(fs_1.default.writeFile);
// Compiled Elm worker, produced into morphir-cli/Morphir.Elm.DaprCLI.js
// (one level up from the lib/ output directory).
const worker = require("./../Morphir.Elm.DaprCLI").Elm.Morphir.Elm.DaprCLI.init();
const program = new commander_1.Command();
program
    .name("morphir-dapr ")
    .description("Generate Dapr Application from Morphir Model")
    .option("-p, --project-dir <path>", "Root directory of the project where morphir-dapr.json is located.", ".")
    .option("-o, --output <path>", "Target location where the Dapr sources will be sent. Will create it if it does not exist", "dapr-output")
    .option("-i, --info", "Print dapr intermediate output (elm) to STDOUT.")
    .option("-d, --delete", "Delete build directory")
    .parse(process.argv);
const opts = program.opts();
async function readElmSources(dirs) {
    const readElmSource = async (filePath) => {
        const content = await readFile(filePath);
        return { path: filePath, content: content.toString() };
    };
    const readDir = async (currentDir) => {
        const entries = await readdir(currentDir, { withFileTypes: true });
        const elmSources = entries
            .filter((entry) => entry.isFile() && entry.name.endsWith(".elm"))
            .map((entry) => readElmSource(path_1.default.join(currentDir, entry.name)));
        const subDirSources = await entries
            .filter((entry) => entry.isDirectory())
            .map((entry) => readDir(path_1.default.join(currentDir, entry.name)))
            .reduce(async (soFarPromise, nextPromise) => {
            const soFar = await soFarPromise;
            const next = await nextPromise;
            return soFar.concat(next);
        }, Promise.resolve([]));
        return (await Promise.all(elmSources)).concat(subDirSources);
    };
    const sources = await Promise.all(dirs.map((dir) => readDir(dir)));
    return sources.flat();
}
async function packageDefAndDaprCodeFromSrc(morphirJson, sourceFiles) {
    return new Promise((resolve, reject) => {
        worker.ports.decodeError.subscribe((err) => {
            reject(err);
        });
        worker.ports.packageDefAndDaprCodeFromSrcResult.subscribe(([err, ok]) => {
            if (err) {
                reject(err);
            }
            else {
                resolve(ok);
            }
        });
        worker.ports.packageDefinitionFromSource.send([morphirJson, sourceFiles]);
    });
}
async function gen(projectDir, output, debug, deleteBuildDir) {
    const morphirJsonPath = path_1.default.join(projectDir, "morphir-dapr.json");
    const morphirJson = JSON.parse((await readFile(morphirJsonPath)).toString());
    const sourceFiles = await readElmSources(morphirJson.sourceDirectories);
    const result = await packageDefAndDaprCodeFromSrc(morphirJson, sourceFiles);
    if (debug) {
        console.log(JSON.stringify(result.elmBackendResult));
    }
    const buildDir = "dapr-stuff";
    console.log(`Clearing ${buildDir} ...`);
    fs_1.default.rmSync(buildDir, { recursive: true, force: true });
    console.log(`Creating build directory: ${buildDir}`);
    fs_1.default.mkdirSync(buildDir);
    console.log(`Writing dapr files to ${buildDir} ...`);
    await writeFile(`${buildDir}/Main.elm`, result.elmBackendResult);
    // __dirname here is morphir-cli/lib; assets live one level up.
    const assetsDir = path_1.default.join(path_1.default.dirname(__dirname), "assets");
    const repoSrcDir = path_1.default.join(path_1.default.dirname(path_1.default.dirname(__dirname)), "src");
    console.log(`Copying compilation assets to ${buildDir} ...`);
    fsExtra.copySync(assetsDir, buildDir);
    fsExtra.copySync(repoSrcDir, buildDir);
    console.log(`Copying original sources to ${buildDir}`);
    morphirJson.sourceDirectories.forEach((dir) => {
        fsExtra.copySync(String(dir), buildDir);
    });
    console.log(`Using local elm-platform to perform final compilation`);
    child_process_1.default.execSync(`cd ${buildDir} && elm make Main.elm --output=Main.js`);
    console.log(`Copying files to output directory...`);
    if (!fs_1.default.existsSync(output)) {
        fs_1.default.mkdirSync(output);
    }
    fsExtra.copySync(`${buildDir}/DaprAppShell.js`, `${output}/DaprAppShell.js`);
    fsExtra.copySync(`${buildDir}/Main.js`, `${output}/Main.js`);
    fsExtra.copySync(`${buildDir}/package.json`, `${output}/package.json`);
    if (deleteBuildDir) {
        fs_1.default.rmSync(buildDir, { recursive: true, force: true });
    }
    return result;
}
gen(opts.projectDir, opts.output, opts.info, opts.delete)
    .then(() => {
    console.log("Done!");
})
    .catch((err) => {
    if (err && err.code === "ENOENT") {
        console.error(`Could not find file at '${err.path}'`);
    }
    else {
        console.error(err);
    }
    process.exit(1);
});
