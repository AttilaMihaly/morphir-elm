#!/usr/bin/env node

import path from "path";
import fs from "fs";
import util from "util";
import { Command } from "commander";
// eslint-disable-next-line @typescript-eslint/no-var-requires
const express = require("express");

const readFile = util.promisify(fs.readFile);

const program = new Command();
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
const morphirCliDir = path.dirname(__dirname);
const webDir = path.join(morphirCliDir, "treeview", "dist");

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
  const packageJson = require(path.join(path.dirname(morphirCliDir), "package.json"));
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

app.get(
  "/",
  wrap(async (_req: Req, res: Res) => {
    res.setHeader("Content-type", "text/html");
    res.send(await indexHtmlWithVersion());
  })
);

createSimpleGetJsonApi(app, "morphir.json");
createSimpleGetJsonApi(app, "morphir-ir.json");

app.get("/assets/2020_Morphir_Logo_Icon_WHT.svg", (_req: Req, res: Res) => {
  const options = { root: morphirCliDir };
  const fileName = path.join(program.opts().projectDir, "treeview/assets/2020_Morphir_Logo_Icon_WHT.svg");
  res.sendFile(fileName, options, (err: unknown) => {
    if (err) {
      console.error(err);
    }
  });
});

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
