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
const fsExists = util_1.default.promisify(fs_1.default.exists);
const writeFile = util_1.default.promisify(fs_1.default.writeFile);
const program = new commander_1.Command();
program
    .name("morphir-elm develop")
    .description("Start up a web server and expose developer tools through a web UI")
    .option("-p, --port <port>", "Port to bind the web server to.", "3000")
    .option("-o, --host <host>", "Host to bind the web server to.", "localhost")
    .option("-i, --project-dir <path>", "Root directory of the project where morphir.json is located.", ".")
    .parse(process.argv);
const app = express();
const port = program.opts().port;
// __dirname is morphir-cli/lib; web assets live one level up.
const webDir = path_1.default.join(path_1.default.dirname(__dirname), "web");
app.use(express.static(webDir, { index: false }));
app.use(express.json({ limit: "100mb" }));
function wrap(fn) {
    return (...args) => fn(...args).catch(args[2]);
}
async function indexHtmlWithVersion() {
    const packageJson = require(path_1.default.join(path_1.default.dirname(path_1.default.dirname(__dirname)), "package.json"));
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
async function getMorphirConfig() {
    const filePath = path_1.default.join(program.opts().projectDir, "morphir.json");
    const fileContent = await readFile(filePath);
    return JSON.parse(fileContent.toString());
}
async function getDecorationConfig() {
    var _a;
    const morphirConfig = await getMorphirConfig();
    return (_a = morphirConfig.decorations) !== null && _a !== void 0 ? _a : [];
}
async function getDecorationFilePath(decorationID) {
    var _a;
    const decorationConfig = (await getDecorationConfig())[decorationID];
    const storageLocation = (_a = decorationConfig.storageLocation) !== null && _a !== void 0 ? _a : `${decorationID}.json`;
    return path_1.default.join(program.opts().projectDir, storageLocation);
}
app.get("/", wrap(async (_req, res) => {
    res.setHeader("Content-type", "text/html");
    res.send(await indexHtmlWithVersion());
}));
createSimpleGetJsonApi(app, "morphir.json");
createSimpleGetJsonApi(app, "morphir-ir.json");
createSimpleGetJsonApi(app, "morphir-tests.json", "[]");
app.get("/server/decorations", wrap(async (_req, res) => {
    const configJsonContent = await getDecorationConfig();
    const decorationIDs = Object.keys(configJsonContent);
    const responseJson = {};
    for (const decorationID of decorationIDs) {
        const decorationFilePath = await getDecorationFilePath(decorationID);
        const irFilePath = path_1.default.join(program.opts().projectDir, configJsonContent[decorationID].ir);
        if (!(await fsExists(decorationFilePath))) {
            await writeFile(decorationFilePath, "{}");
        }
        const attrFileContent = await readFile(decorationFilePath);
        const irFileContent = await readFile(irFilePath);
        responseJson[decorationID] = {
            data: JSON.parse(attrFileContent.toString()),
            displayName: configJsonContent[decorationID].displayName,
            entryPoint: configJsonContent[decorationID].entryPoint,
            iR: JSON.parse(irFileContent.toString()),
        };
    }
    res.send(responseJson);
}));
app.post("/server/update-decoration/:decorationID", wrap(async (req, res) => {
    const decorationID = req.params.decorationID;
    await writeFile(await getDecorationFilePath(decorationID), JSON.stringify(req.body, null, 4));
    res.send(req.body);
}));
app.post("/server/morphir-tests.json", wrap(async (req, res) => {
    const morphirTestsJsonPath = path_1.default.join(program.opts().projectDir, "morphir-tests.json");
    const jsonContent = JSON.stringify(req.body, null, 4);
    await writeFile(morphirTestsJsonPath, jsonContent);
    const morphirTestsJsonContent = await readFile(morphirTestsJsonPath);
    res.send(JSON.parse(morphirTestsJsonContent.toString()));
}));
app.post("/server/morphir-ir.json", wrap(async (req, res) => {
    const morphirIRJsonPath = path_1.default.join(program.opts().projectDir, "morphir-ir.json");
    const jsonContent = JSON.stringify(req.body, null, 4);
    await writeFile(morphirIRJsonPath, jsonContent);
    const morphirIRJsonContent = await readFile(morphirIRJsonPath);
    res.send(JSON.parse(morphirIRJsonContent.toString()));
}));
app.get("*", wrap(async (_req, res) => {
    res.setHeader("Content-type", "text/html");
    res.send(await indexHtmlWithVersion());
}));
app.listen(port, program.opts().host, () => {
    console.log(`Developer server listening at http://${program.opts().host}:${port}`);
});
