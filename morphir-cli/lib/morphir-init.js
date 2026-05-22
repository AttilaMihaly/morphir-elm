"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
const fs_1 = __importDefault(require("fs"));
const chalk_1 = __importDefault(require("chalk"));
const path_1 = __importDefault(require("path"));
const util_1 = require("util");
const commander_1 = require("commander");
const inquirer_1 = __importDefault(require("inquirer"));
const exists = (0, util_1.promisify)(fs_1.default.exists);
const writeFile = (0, util_1.promisify)(fs_1.default.writeFile);
const readFile = (0, util_1.promisify)(fs_1.default.readFile);
const mkdir = (0, util_1.promisify)(fs_1.default.mkdir);
const program = new commander_1.Command();
program
    .name('morphir init')
    .description('Launches an interactive session to initialize a new morphir project.')
    .parse(process.argv);
console.log(chalk_1.default.blue('-- NEW MORPHIR PROJECT ---------------------------------------\n'));
console.log("I'll create a new morphir project for you, you just need to provide some details.\n");
inquirer_1.default
    .prompt([
    {
        type: 'list',
        name: 'type',
        message: 'Select project type',
        choices: ['Business Model', 'Decoration Schema'],
        default: 0
    },
    {
        type: 'input',
        name: 'name',
        message: 'Project Name (no spaces)',
        default: 'Demo',
        validate(input) {
            // prevent spaces, leading digits and a leading lowercase.
            if (input.includes(' '))
                return `You can't have spaces in there. You could try: ${getSuggestions(input)}`;
            if (input.match(/^\d/))
                return `Project name can't start with a number`;
            if (input[0] === input[0].toLowerCase())
                return `It can't start with a lowercase character`;
            return true;
        }
    },
    {
        type: 'input',
        name: 'dir',
        message: 'Where do you want to create it? (use `.` for current directory)',
        default(answer) {
            return `./${answer.name}`;
        }
    }
])
    .then(async (answers) => {
    const { name, dir } = answers;
    console.log();
    console.log(chalk_1.default.blue(`Creating your new morphir project\n`));
    const projectPath = path_1.default.resolve(dir);
    // check if there's a morphir.json at the directory
    if (await exists(path_1.default.join(projectPath, 'morphir.json'))) {
        console.log(chalk_1.default.blue(`The path you specified already contains a morphir project. Exiting.`));
        process.exit(0);
    }
    // ask to create a new dir if it doesn't exist
    if (!(await exists(projectPath))) {
        try {
            const { shouldCreate } = await inquirer_1.default.prompt([
                {
                    type: 'confirm',
                    message: `The directory you provided doesn't exist. Do you want me to create a new directory at\n"${projectPath}"?`,
                    name: 'shouldCreate',
                    default: false
                }
            ]);
            if (shouldCreate)
                await mkdir(projectPath);
            else {
                console.log(chalk_1.default.blue('Cannot create new project because the specified directory does not exists. Exiting.'));
                process.exit(0);
            }
        }
        catch (e) {
            console.log(chalk_1.default.red(`Failed to create directory: ${projectPath}`));
            console.error(e);
            process.exit(1);
        }
    }
    // create the the morphir.json
    const morphirJson = JSON.stringify({
        name: name,
        sourceDirectory: 'src'
    }, null, 4);
    const morphirJsonPath = path_1.default.join(projectPath, 'morphir.json');
    try {
        await writeFile(morphirJsonPath, morphirJson);
    }
    catch (error) {
        console.log(chalk_1.default.red(`Could not create morphir.json`));
        console.error(error);
        process.exit(1);
    }
    // create the src directory
    const srcDir = path_1.default.join(projectPath, 'src');
    try {
        mkdir(srcDir);
    }
    catch (error) {
        console.log(chalk_1.default.red(`Failed to create directory: ${srcDir}`));
        console.error(error);
        process.exit(1);
    }
    console.log();
    console.log(chalk_1.default.blue('-- ALL DONE --------------------------------------------------\n'));
});
/**
 * Creates suggestions for use based on their inputs.
 * Performs some basic string cleanup before creating combinations.
 *
 * @param input original user input
 * @returns comma separated string of suggestions.
 */
function getSuggestions(input) {
    const noLeadingDigits = input.replace(/^(\d*\s*\d*)/, '').trim();
    const capitalized = noLeadingDigits[0].toUpperCase() + noLeadingDigits.substring(1);
    const suggestions = new Set([capitalized.replace(' ', ''), capitalized.replace(' ', '.')]);
    return [...suggestions].join(', ');
}
