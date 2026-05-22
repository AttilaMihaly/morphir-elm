"use strict";
// TypeScript port of legacy cli/cli.js helpers that drive the legacy Elm worker
// (Morphir.Elm.LegacyCLI). Used by `morphir-elm gen` (and later `morphir-elm test`)
// to keep behaviour identical to the legacy JS CLI.
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.test = exports.gen = void 0;
const path_1 = __importDefault(require("path"));
const fs_1 = __importDefault(require("fs"));
const util_1 = __importDefault(require("util"));
// eslint-disable-next-line @typescript-eslint/no-var-requires
const prettier = require("prettier");
const readdir = util_1.default.promisify(fs_1.default.readdir);
const mkdir = util_1.default.promisify(fs_1.default.mkdir);
const readFile = util_1.default.promisify(fs_1.default.readFile);
const fsWriteFile = util_1.default.promisify(fs_1.default.writeFile);
// The compiled Elm worker is produced into ../Morphir.Elm.LegacyCLI.js
// relative to the lib/ output directory.
const worker = require("./../Morphir.Elm.LegacyCLI").Elm.Morphir.Elm.CLI.init();
async function fileExist(filePath) {
    return new Promise((resolve) => {
        fs_1.default.access(filePath, fs_1.default.constants.F_OK, (err) => {
            resolve(!err);
        });
    });
}
function copyRecursiveSync(src, dest) {
    if (!fs_1.default.existsSync(src))
        return;
    const stats = fs_1.default.statSync(src);
    if (stats.isDirectory()) {
        if (!fs_1.default.existsSync(dest))
            fs_1.default.mkdirSync(dest);
        for (const childItemName of fs_1.default.readdirSync(src)) {
            copyRecursiveSync(path_1.default.join(src, childItemName), path_1.default.join(dest, childItemName));
        }
    }
    else {
        fs_1.default.copyFileSync(src, dest);
        console.log(`COPY - ${dest}`);
    }
}
function copyRedistributables(options, outputPath) {
    const copyFiles = (src) => {
        const sourceDirectory = path_1.default.join(path_1.default.dirname(path_1.default.dirname(__dirname)), "redistributable", src);
        copyRecursiveSync(sourceDirectory, outputPath);
    };
    if (options.target === "SpringBoot") {
        copyFiles("SpringBoot");
    }
    else if (options.target === "Scala" && options.copyDeps) {
        const copyScalaFeature = (feature) => {
            copyFiles(`Scala/sdk/${feature}/src`);
            copyFiles(`Scala/sdk/${feature}/src-${options.targetVersion}`);
        };
        if (options.includeCodecs) {
            copyScalaFeature("json");
        }
        copyScalaFeature("core");
    }
    else if (options.target === "TypeScript") {
        copyFiles("TypeScript/");
    }
    else if (options.target === "Snowpark") {
        copyFiles("Snowpark/");
    }
}
async function generate(options, ir) {
    return new Promise((resolve, reject) => {
        worker.ports.jsonDecodeError.subscribe((err) => {
            reject(err);
        });
        worker.ports.generateResult.subscribe(([err, ok]) => {
            if (err) {
                reject(err);
            }
            else {
                resolve(ok);
            }
        });
        worker.ports.generate.send([options, ir]);
    });
}
async function findFilesToDelete(outputPath, fileMap) {
    const readDir = async (currentDir, generatedFiles) => {
        const entries = await readdir(currentDir, { withFileTypes: true });
        const filesToDelete = entries
            .map((entry) => [entry, path_1.default.resolve(path_1.default.join(currentDir, entry.name))])
            .filter(([entry, absolutePath]) => entry.isFile() && !generatedFiles.includes(absolutePath))
            .map(([, absolutePath]) => absolutePath);
        const subDirFilesToDelete = await entries
            .filter((entry) => entry.isDirectory())
            .map((entry) => readDir(path_1.default.join(currentDir, entry.name), generatedFiles))
            .reduce(async (soFarPromise, nextPromise) => {
            const soFar = await soFarPromise;
            const next = await nextPromise;
            return soFar.concat(next);
        }, Promise.resolve([]));
        return filesToDelete.concat(subDirFilesToDelete);
    };
    const files = fileMap.map(([[dirPath, fileName]]) => {
        const fileDir = dirPath.reduce((accum, next) => path_1.default.join(accum, next), outputPath);
        return path_1.default.resolve(fileDir, fileName);
    });
    return readDir(outputPath, files);
}
async function gen(input, outputPath, options) {
    await mkdir(outputPath, { recursive: true });
    const morphirIrJson = await readFile(path_1.default.resolve(input));
    const opts = { ...options };
    opts.limitToModules = options.modulesToInclude ? options.modulesToInclude.split(",") : null;
    opts.includeCodecs = !!options.includeCodecs;
    opts.filename = options.filename === "" ? "" : options.filename;
    if (options.decorations) {
        if (await fileExist(path_1.default.resolve(options.decorations))) {
            opts.decorationsObj = JSON.parse((await readFile(path_1.default.resolve(options.decorations))).toString());
        }
    }
    const fileMap = await generate(opts, JSON.parse(morphirIrJson.toString()));
    const writePromises = fileMap.map(async ([[dirPath, fileName], content]) => {
        const fileDir = dirPath.reduce((accum, next) => path_1.default.join(accum, next), outputPath);
        const filePath = path_1.default.join(fileDir, fileName);
        if (await fileExist(filePath)) {
            console.log(`UPDATE - ${filePath}`);
        }
        else {
            await mkdir(fileDir, { recursive: true });
            console.log(`INSERT - ${filePath}`);
        }
        if (options.target === "TypeScript") {
            return fsWriteFile(filePath, prettier.format(content, { parser: "typescript" }));
        }
        return fsWriteFile(filePath, content);
    });
    const filesToDelete = await findFilesToDelete(outputPath, fileMap);
    const deletePromises = filesToDelete.map(async (fileToDelete) => {
        console.log(`DELETE - ${fileToDelete}`);
        return fs_1.default.unlinkSync(fileToDelete);
    });
    copyRedistributables(opts, outputPath);
    return Promise.all([...writePromises, ...deletePromises]);
}
exports.gen = gen;
async function test(projectDir) {
    const morphirIRJsonPath = path_1.default.join(projectDir, "morphir-ir.json");
    const morphirIRJson = JSON.parse((await readFile(morphirIRJsonPath)).toString());
    const morphirTestsJsonPath = path_1.default.join(projectDir, "morphir-tests.json");
    const morphirTestsJson = JSON.parse((await readFile(morphirTestsJsonPath)).toString());
    return new Promise((resolve, reject) => {
        worker.ports.jsonDecodeError.subscribe((err) => {
            reject(err);
        });
        worker.ports.runTestCasesResultError.subscribe((err) => {
            reject(err);
        });
        worker.ports.runTestCasesResult.subscribe((ok) => {
            resolve(ok);
        });
        worker.ports.runTestCases.send([morphirIRJson, morphirTestsJson]);
    });
}
exports.test = test;
