#!/usr/bin/env node

import { Command } from "commander";
import chalk from "chalk";
import { test } from "./legacyAPI";

require("log-timestamp");

const program = new Command();
program
  .name("morphir-elm test")
  .description("Start Testing the Models")
  .option("-p, --project-dir <path>", "Root directory of the project where morphir.json is located.", ".")
  .parse(process.argv);

type TestObject = {
  "Function Name": string;
  "Total TestCases": number;
  "Pass TestCases"?: number;
  "Fail TestCases"?: number;
  "Fail TestCases List"?: Array<{ "Expected Output": unknown; "Actual Output": unknown }>;
};

test(program.opts().projectDir)
  .then((testResult) => {
    const results = testResult as TestObject[];
    if (results.length === 0) {
      console.log(chalk.magenta("No TestCases found in morphir-tests.json file."));
    } else {
      for (const testObject of results) {
        console.log(chalk.cyan(`Function Name - ${testObject["Function Name"]}`));
        console.log(chalk.yellow(`Total TestCases - ${testObject["Total TestCases"]}`));
        console.log(chalk.green(`Pass TestCases - ${testObject["Pass TestCases"]}\n`));
      }
    }
  })
  .catch((err: unknown) => {
    if (Array.isArray(err)) {
      for (const testObject of err as TestObject[]) {
        console.log(chalk.cyan(`Function Name - ${testObject["Function Name"]}`));
        console.log(chalk.yellow(`Total TestCases - ${testObject["Total TestCases"]}`));
        console.log(chalk.yellow(`Fail TestCases - ${testObject["Fail TestCases"]}\n`));
        const failTestCaseJson = testObject["Fail TestCases List"] ?? [];
        for (const failTestOutputs of failTestCaseJson) {
          console.log(chalk.red(`Expected Output - ${failTestOutputs["Expected Output"]}`));
          console.log(chalk.red(`Actual Output - ${failTestOutputs["Actual Output"]}\n`));
        }
      }
    } else {
      console.error(chalk.red(String(err)));
    }
    process.exit(1);
  });
