#!/usr/bin/env node

import path from "path";
import fs from "fs";
import util from "util";
import { Command } from "commander";
// eslint-disable-next-line @typescript-eslint/no-var-requires
const express = require("express");

const readFile = util.promisify(fs.readFile);
const fsExists = util.promisify(fs.exists);
const writeFile = util.promisify(fs.writeFile);

const program = new Command();
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
const webDir = path.join(path.dirname(__dirname), "web");

app.use(express.static(webDir, { index: false }));
app.use(express.json({ limit: "100mb" }));

type Req = any;
type Res = any;
type Next = (err?: unknown) => void;
type Handler = (req: Req, res: Res, next: Next) => Promise<unknown>;

function wrap(fn: Handler) {
  return (...args: [Req, Res, Next]) => fn(...args).catch(args[2]);
}

async function indexHtmlWithVersion(): Promise<string> {
  const packageJson = require(path.join(path.dirname(path.dirname(__dirname)), "package.json"));
  const indexHtml = await readFile(path.join(webDir, "index.html"), "utf8");
  return indexHtml.replace("__VERSION_NUMBER__", packageJson.version.toString());
}

function createSimpleGetJsonApi(app: any, filePath: string, defaultContent?: string) {
  app.get(
    "/server/" + filePath,
    wrap(async (_req: Req, res: Res) => {
      const jsonPath = path.join(program.opts().projectDir, filePath);
      try {
        const jsonContent = await readFile(jsonPath);
        res.send(JSON.parse(jsonContent.toString()));
      } catch (err) {
        const e = err as NodeJS.ErrnoException;
        if (defaultContent && e.code === "ENOENT") {
          res.send(defaultContent);
        } else {
          throw err;
        }
      }
    })
  );
}

async function getMorphirConfig() {
  const filePath = path.join(program.opts().projectDir, "morphir.json");
  const fileContent = await readFile(filePath);
  return JSON.parse(fileContent.toString());
}

async function getDecorationConfig(): Promise<Record<string, any>> {
  const morphirConfig = await getMorphirConfig();
  return morphirConfig.decorations ?? [];
}

async function getDecorationFilePath(decorationID: string): Promise<string> {
  const decorationConfig = (await getDecorationConfig())[decorationID];
  const storageLocation = decorationConfig.storageLocation ?? `${decorationID}.json`;
  return path.join(program.opts().projectDir, storageLocation);
}

app.get(
  "/",
  wrap(async (_req: Req, res: Res) => {
    res.setHeader("Content-type", "text/html");
    res.send(await indexHtmlWithVersion());
  })
);

createSimpleGetJsonApi(app, "morphir.json");
createSimpleGetJsonApi(app, "morphir-ir.json");
createSimpleGetJsonApi(app, "morphir-tests.json", "[]");

app.get(
  "/server/decorations",
  wrap(async (_req: Req, res: Res) => {
    const configJsonContent = await getDecorationConfig();
    const decorationIDs = Object.keys(configJsonContent);
    const responseJson: Record<string, any> = {};
    for (const decorationID of decorationIDs) {
      const decorationFilePath = await getDecorationFilePath(decorationID);
      const irFilePath = path.join(program.opts().projectDir, configJsonContent[decorationID].ir);
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
  })
);

app.post(
  "/server/update-decoration/:decorationID",
  wrap(async (req: Req, res: Res) => {
    const decorationID = req.params.decorationID;
    await writeFile(await getDecorationFilePath(decorationID), JSON.stringify(req.body, null, 4));
    res.send(req.body);
  })
);

app.post(
  "/server/morphir-tests.json",
  wrap(async (req: Req, res: Res) => {
    const morphirTestsJsonPath = path.join(program.opts().projectDir, "morphir-tests.json");
    const jsonContent = JSON.stringify(req.body, null, 4);
    await writeFile(morphirTestsJsonPath, jsonContent);
    const morphirTestsJsonContent = await readFile(morphirTestsJsonPath);
    res.send(JSON.parse(morphirTestsJsonContent.toString()));
  })
);

app.post(
  "/server/morphir-ir.json",
  wrap(async (req: Req, res: Res) => {
    const morphirIRJsonPath = path.join(program.opts().projectDir, "morphir-ir.json");
    const jsonContent = JSON.stringify(req.body, null, 4);
    await writeFile(morphirIRJsonPath, jsonContent);
    const morphirIRJsonContent = await readFile(morphirIRJsonPath);
    res.send(JSON.parse(morphirIRJsonContent.toString()));
  })
);

app.get(
  "*",
  wrap(async (_req: Req, res: Res) => {
    res.setHeader("Content-type", "text/html");
    res.send(await indexHtmlWithVersion());
  })
);

app.listen(port, program.opts().host, () => {
  console.log(`Developer server listening at http://${program.opts().host}:${port}`);
});
