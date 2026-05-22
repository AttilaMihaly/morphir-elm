#!/usr/bin/env node
"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
const commander_1 = require("commander");
const chalk_1 = __importDefault(require("chalk"));
const legacyAPI_1 = require("./legacyAPI");
require("log-timestamp");
const program = new commander_1.Command();
program
    .name("morphir-elm test")
    .description("Start Testing the Models")
    .option("-p, --project-dir <path>", "Root directory of the project where morphir.json is located.", ".")
    .parse(process.argv);
(0, legacyAPI_1.test)(program.opts().projectDir)
    .then((testResult) => {
    const results = testResult;
    if (results.length === 0) {
        console.log(chalk_1.default.magenta("No TestCases found in morphir-tests.json file."));
    }
    else {
        for (const testObject of results) {
            console.log(chalk_1.default.cyan(`Function Name - ${testObject["Function Name"]}`));
            console.log(chalk_1.default.yellow(`Total TestCases - ${testObject["Total TestCases"]}`));
            console.log(chalk_1.default.green(`Pass TestCases - ${testObject["Pass TestCases"]}\n`));
        }
    }
})
    .catch((err) => {
    var _a;
    if (Array.isArray(err)) {
        for (const testObject of err) {
            console.log(chalk_1.default.cyan(`Function Name - ${testObject["Function Name"]}`));
            console.log(chalk_1.default.yellow(`Total TestCases - ${testObject["Total TestCases"]}`));
            console.log(chalk_1.default.yellow(`Fail TestCases - ${testObject["Fail TestCases"]}\n`));
            const failTestCaseJson = (_a = testObject["Fail TestCases List"]) !== null && _a !== void 0 ? _a : [];
            for (const failTestOutputs of failTestCaseJson) {
                console.log(chalk_1.default.red(`Expected Output - ${failTestOutputs["Expected Output"]}`));
                console.log(chalk_1.default.red(`Actual Output - ${failTestOutputs["Actual Output"]}\n`));
            }
        }
    }
    else {
        console.error(chalk_1.default.red(String(err)));
    }
    process.exit(1);
});
