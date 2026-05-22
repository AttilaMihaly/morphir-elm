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
// eslint-disable-next-line @typescript-eslint/no-var-requires
const express = require("express");
const readFile = util_1.default.promisify(fs_1.default.readFile);
const program = new commander_1.Command();
program
    .name("morphir-elm treeview")
    .description("Start up a web server and expose treeview through a web UI")
    .option("-p, --port <port>", "Port to bind the web server to.", "3000")
    .option("-o, --host <host>", "Host to bind the web server to.", "localhost")
    .option("-i, --project-dir <path>", "Root directory of the project where morphir.json is located.", ".")
    .parse(process.argv);
const app = express();
const port = program.opts().port;
// __dirname is morphir-cli/lib; treeview assets live one level up.
const morphirCliDir = path_1.default.dirname(__dirname);
const webDir = path_1.default.join(morphirCliDir, "treeview", "dist");
app.use(express.static(webDir, { index: false }));
app.use(express.json({ limit: "100mb" }));
function wrap(fn) {
    return (...args) => fn(...args).catch(args[2]);
}
async function indexHtmlWithVersion() {
    const packageJson = require(path_1.default.join(path_1.default.dirname(morphirCliDir), "package.json"));
    const indexHtml = await readFile(path_1.default.join(webDir, "index.html"), "utf8");
    return indexHtml.replace("__VERSION_NUMBER__", packageJson.version.toString());
}
function createSimpleGetJsonApi(app, filePath, defaultContent) {
    app.get("/server/" + filePath, wrap(async (_req, res) => {
        const jsonPath = path_1.default.join(program.opts().projectDir, filePath);
        try {
            const jsonContent = await readFile(jsonPath);
            res.send(JSON.parse(jsonContent.toString()));
        }
        catch (err) {
            const e = err;
            if (defaultContent && e.code === "ENOENT") {
                res.send(defaultContent);
            }
            else {
                throw err;
            }
        }
    }));
}
app.get("/", wrap(async (_req, res) => {
    res.setHeader("Content-type", "text/html");
    res.send(await indexHtmlWithVersion());
}));
createSimpleGetJsonApi(app, "morphir.json");
createSimpleGetJsonApi(app, "morphir-ir.json");
app.get("/assets/2020_Morphir_Logo_Icon_WHT.svg", (_req, res) => {
    const options = { root: morphirCliDir };
    const fileName = path_1.default.join(program.opts().projectDir, "treeview/assets/2020_Morphir_Logo_Icon_WHT.svg");
    res.sendFile(fileName, options, (err) => {
        if (err) {
            console.error(err);
        }
    });
});
app.get("*", wrap(async (_req, res) => {
    res.setHeader("Content-type", "text/html");
    res.send(await indexHtmlWithVersion());
}));
app.listen(port, program.opts().host, () => {
    console.log(`Developer server listening at http://${program.opts().host}:${port}`);
});
