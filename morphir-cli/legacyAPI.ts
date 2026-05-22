// TypeScript port of legacy cli/cli.js helpers that drive the legacy Elm worker
// (Morphir.Elm.LegacyCLI). Used by `morphir-elm gen` (and later `morphir-elm test`)
// to keep behaviour identical to the legacy JS CLI.

import path from "path";
import fs from "fs";
import util from "util";
// eslint-disable-next-line @typescript-eslint/no-var-requires
const prettier = require("prettier");

const readdir = util.promisify(fs.readdir);
const mkdir = util.promisify(fs.mkdir);
const readFile = util.promisify(fs.readFile);
const fsWriteFile = util.promisify(fs.writeFile);

// The compiled Elm worker is produced into ../Morphir.Elm.LegacyCLI.js
// relative to the lib/ output directory.
const worker = require("./../Morphir.Elm.LegacyCLI").Elm.Morphir.Elm.CLI.init();

type GenOptions = {
    target: string;
    targetVersion?: string;
    copyDeps?: boolean;
    modulesToInclude?: string;
    includeCodecs?: boolean;
    filename?: string;
    decorations?: string;
    decorationsObj?: unknown;
    limitToModules?: string[] | null;
    [key: string]: unknown;
};

type FileMapEntry = [[string[], string], string];

async function fileExist(filePath: string): Promise<boolean> {
    return new Promise((resolve) => {
        fs.access(filePath, fs.constants.F_OK, (err) => {
            resolve(!err);
        });
    });
}

function copyRecursiveSync(src: string, dest: string): void {
    if (!fs.existsSync(src)) return;
    const stats = fs.statSync(src);
    if (stats.isDirectory()) {
        if (!fs.existsSync(dest)) fs.mkdirSync(dest);
        for (const childItemName of fs.readdirSync(src)) {
            copyRecursiveSync(path.join(src, childItemName), path.join(dest, childItemName));
        }
    } else {
        fs.copyFileSync(src, dest);
        console.log(`COPY - ${dest}`);
    }
}

function copyRedistributables(options: GenOptions, outputPath: string): void {
    const copyFiles = (src: string) => {
        const sourceDirectory = path.join(path.dirname(path.dirname(__dirname)), "redistributable", src);
        copyRecursiveSync(sourceDirectory, outputPath);
    };
    if (options.target === "SpringBoot") {
        copyFiles("SpringBoot");
    } else if (options.target === "Scala" && options.copyDeps) {
        const copyScalaFeature = (feature: string) => {
            copyFiles(`Scala/sdk/${feature}/src`);
            copyFiles(`Scala/sdk/${feature}/src-${options.targetVersion}`);
        };
        if (options.includeCodecs) {
            copyScalaFeature("json");
        }
        copyScalaFeature("core");
    } else if (options.target === "TypeScript") {
        copyFiles("TypeScript/");
    } else if (options.target === "Snowpark") {
        copyFiles("Snowpark/");
    }
}

async function generate(options: GenOptions, ir: unknown): Promise<FileMapEntry[]> {
    return new Promise((resolve, reject) => {
        worker.ports.jsonDecodeError.subscribe((err: unknown) => {
            reject(err);
        });

        worker.ports.generateResult.subscribe(([err, ok]: [unknown, FileMapEntry[]]) => {
            if (err) {
                reject(err);
            } else {
                resolve(ok);
            }
        });

        worker.ports.generate.send([options, ir]);
    });
}

async function findFilesToDelete(outputPath: string, fileMap: FileMapEntry[]): Promise<string[]> {
    const readDir = async (currentDir: string, generatedFiles: string[]): Promise<string[]> => {
        const entries = await readdir(currentDir, { withFileTypes: true });
        const filesToDelete = entries
            .map((entry) => [entry, path.resolve(path.join(currentDir, entry.name))] as const)
            .filter(([entry, absolutePath]) => entry.isFile() && !generatedFiles.includes(absolutePath))
            .map(([, absolutePath]) => absolutePath);
        const subDirFilesToDelete = await entries
            .filter((entry) => entry.isDirectory())
            .map((entry) => readDir(path.join(currentDir, entry.name), generatedFiles))
            .reduce(async (soFarPromise, nextPromise) => {
                const soFar = await soFarPromise;
                const next = await nextPromise;
                return soFar.concat(next);
            }, Promise.resolve([] as string[]));
        return filesToDelete.concat(subDirFilesToDelete);
    };

    const files = fileMap.map(([[dirPath, fileName]]) => {
        const fileDir = dirPath.reduce((accum, next) => path.join(accum, next), outputPath);
        return path.resolve(fileDir, fileName);
    });
    return readDir(outputPath, files);
}

export async function gen(input: string, outputPath: string, options: GenOptions): Promise<unknown[]> {
    await mkdir(outputPath, { recursive: true });
    const morphirIrJson = await readFile(path.resolve(input));
    const opts: GenOptions = { ...options };
    opts.limitToModules = options.modulesToInclude ? options.modulesToInclude.split(",") : null;
    opts.includeCodecs = !!options.includeCodecs;
    opts.filename = options.filename === "" ? "" : options.filename;

    if (options.decorations) {
        if (await fileExist(path.resolve(options.decorations))) {
            opts.decorationsObj = JSON.parse((await readFile(path.resolve(options.decorations))).toString());
        }
    }

    const fileMap = await generate(opts, JSON.parse(morphirIrJson.toString()));
    const writePromises = fileMap.map(async ([[dirPath, fileName], content]) => {
        const fileDir = dirPath.reduce((accum, next) => path.join(accum, next), outputPath);
        const filePath = path.join(fileDir, fileName);
        if (await fileExist(filePath)) {
            console.log(`UPDATE - ${filePath}`);
        } else {
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
        return fs.unlinkSync(fileToDelete);
    });
    copyRedistributables(opts, outputPath);
    return Promise.all([...writePromises, ...deletePromises]);
}

export async function test(projectDir: string): Promise<unknown> {
    const morphirIRJsonPath = path.join(projectDir, "morphir-ir.json");
    const morphirIRJson = JSON.parse((await readFile(morphirIRJsonPath)).toString());
    const morphirTestsJsonPath = path.join(projectDir, "morphir-tests.json");
    const morphirTestsJson = JSON.parse((await readFile(morphirTestsJsonPath)).toString());
    return new Promise((resolve, reject) => {
        worker.ports.jsonDecodeError.subscribe((err: unknown) => {
            reject(err);
        });
        worker.ports.runTestCasesResultError.subscribe((err: unknown) => {
            reject(err);
        });
        worker.ports.runTestCasesResult.subscribe((ok: unknown) => {
            resolve(ok);
        });
        worker.ports.runTestCases.send([morphirIRJson, morphirTestsJson]);
    });
}
