#!/usr/bin/env node

// NPM imports
import { Command } from 'commander';
import path from 'path';
import * as fs from 'fs';
import * as util from 'util';
// Type-only import for compile-time typing
import * as IR from 'morphir-elm/ir';

// logging
require('log-timestamp');

const fsReadFile = util.promisify(fs.readFile);
const fsWriteFile = util.promisify(fs.writeFile);
const fsMkdir = util.promisify(fs.mkdir);

// Utility to ensure directory exists
async function ensureDir(dir: string) {
    await fsMkdir(dir, { recursive: true });
}

// Convert a Morphir module path ([["foo"],["bar"]]) to a filesystem path
function formatNameSegment(seg: string[]): string {
    const human = nameToHumanWords(seg);
    if (human.length === 0) return 'module';
    return human.join('-');
}

function modulePathToDir(base: string, modulePath: string[][]): string {
    const parts = modulePath.map(formatNameSegment);
    return path.join(base, ...parts);
}

// Pick a reasonable file name for a module file
function moduleFileName(modulePath: string[][]): string {
    // Last segment as file name; apply human words formatting
    const last = modulePath[modulePath.length - 1] || ["module"];
    const formatted = formatNameSegment(last);
    return `${formatted}.json`;
}

function nameToFieldName(words: string[]): string {
    return nameToHumanWords(words).join(" ");
}

/**
 * Equivalent to Morphir.IR.Name.toHumanWords (see Name.elm).
 * Given a list of words (the internal representation of a Morphir Name) it
 * collapses consecutive single-character words (abbreviations) into a single
 * upper-case word. A single-element name that is itself a single character is
 * returned unchanged (not upper-cased) to mirror the Elm semantics.
 *
 * Examples:
 *  nameToHumanWords(["value","in","u","s","d"]) => ["value","in","USD"]
 *  nameToHumanWords(["a","b"]) => ["AB"]
 *  nameToHumanWords(["x"]) => ["x"] (NOT ["X"])    
 */
function nameToHumanWords(words: string[]): string[] {
    // Preserve single-letter single-word case exactly as Elm implementation
    if (words.length === 1 && words[0].length === 1) {
        return [...words];
    }

    const joinAbbrev = (abbrev: string[]) => abbrev.join("").toUpperCase();

    const process = (prefix: string[], abbrev: string[], suffix: string[]): string[] => {
        if (suffix.length === 0) {
            return abbrev.length === 0 ? prefix : prefix.concat(joinAbbrev(abbrev));
        }
        const [first, ...rest] = suffix;
        if (first.length === 1) {
            // Continue accumulating abbreviation
            return process(prefix, [...abbrev, first], rest);
        } else {
            if (abbrev.length === 0) {
                return process([...prefix, first], [], rest);
            } else {
                return process([...prefix, joinAbbrev(abbrev), first], [], rest);
            }
        }
    };

    return process([], [], words);
}

async function run(inputPath: string, outputDir: string) {
    const irBuffer = await fsReadFile(path.resolve(inputPath));
    const irJson = JSON.parse(irBuffer.toString());

    if (irJson?.distribution != null) {
        const distro = IR.Distribution.decodeDistribution(irJson.distribution);
        distro.packageDef.modules.forEach(async (accessControlledModuleDef, moduleName) => {

            const moduleDef = accessControlledModuleDef.value;

            const modPathJson = IR.Path.encodePath(moduleName) as unknown as string[][];

            const dir = modulePathToDir(outputDir, modPathJson);
            const moduleFilePath = path.join(dir, moduleFileName(modPathJson));
            await ensureDir(dir);
            let newTypes: { [key: string]: IR.Type.Definition<any> } = {};
            moduleDef.types.forEach((typeDef, typeName) => {
                newTypes[nameToFieldName(typeName)] = typeDef.value.value;
            });
            const newModuleDef = {
                types: newTypes
            }
            const content = JSON.stringify(newModuleDef, null, 2);

            await fsWriteFile(moduleFilePath, content, 'utf8');
            console.log(`WRITE - ${moduleFilePath}`);

            if (moduleDef.doc != null) {
                const moduleFileDocPath = path.join(dir, moduleFileName(modPathJson) + '.md');
                await fsWriteFile(moduleFileDocPath, moduleDef.doc, 'utf8');
                console.log(`WRITE - ${moduleFileDocPath}`);
            }
        });
    }

}

// Set up Commander
const program = new Command();
program
    .name('morphir decompile')
    .description('Decompile a Morphir IR into per-module files')
    .option('-i, --input <path>', 'Path to morphir-ir.json', 'morphir-ir.json')
    .option('-o, --output <path>', 'Output directory for modules', './decompiled')
    .parse(process.argv);

const opts = program.opts();
run(opts.input, path.resolve(opts.output))
    .then(() => {
        console.log('Done');
    })
    .catch((err) => {
        console.error(err);
        process.exit(1);
    });
