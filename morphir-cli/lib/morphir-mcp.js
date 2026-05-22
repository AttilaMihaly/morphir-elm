#!/usr/bin/env node
"use strict";
var __createBinding = (this && this.__createBinding) || (Object.create ? (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    var desc = Object.getOwnPropertyDescriptor(m, k);
    if (!desc || ("get" in desc ? !m.__esModule : desc.writable || desc.configurable)) {
      desc = { enumerable: true, get: function() { return m[k]; } };
    }
    Object.defineProperty(o, k2, desc);
}) : (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    o[k2] = m[k];
}));
var __setModuleDefault = (this && this.__setModuleDefault) || (Object.create ? (function(o, v) {
    Object.defineProperty(o, "default", { enumerable: true, value: v });
}) : function(o, v) {
    o["default"] = v;
});
var __importStar = (this && this.__importStar) || function (mod) {
    if (mod && mod.__esModule) return mod;
    var result = {};
    if (mod != null) for (var k in mod) if (k !== "default" && Object.prototype.hasOwnProperty.call(mod, k)) __createBinding(result, mod, k);
    __setModuleDefault(result, mod);
    return result;
};
Object.defineProperty(exports, "__esModule", { value: true });
const mcp_js_1 = require("@modelcontextprotocol/sdk/server/mcp.js");
const stdio_js_1 = require("@modelcontextprotocol/sdk/server/stdio.js");
const zod_1 = require("zod");
const commander_1 = require("commander"); // Import commander
const path = __importStar(require("path"));
// Read the package.json of this package
const packageJson = require(path.join(__dirname, "../../package.json"));
// Process arguments using commander
const program = new commander_1.Command();
program
    .option("--elm-command <command>", "Specify the Elm command")
    .option("--root-dir <directory>", "Specify the root directory of the Morphir project")
    .parse(process.argv);
const options = program.opts();
const elmCommand = options.elmCommand;
const rootDir = options.rootDir; // Capture the new option
if (!rootDir) {
    throw new Error("Root directory is not specified. Use the --root-dir option.");
}
// Create an MCP server
const server = new mcp_js_1.McpServer({
    name: "Morphir MCP Server",
    version: packageJson.version // Use the version from package.json
}, {
    // Define the server's resources
    instructions: `
        This is a Morphir MCP server. It provides tools to interact with Morphir projects.
        The server supports the following tools:
        - addModule: Adds a module to the Morphir project. A module can contain types and
        functions defined using the syntax of the Elm programming language.
        `
});
// Utility function to ensure morphir.json exists, creates it if missing
async function ensureMorphirJson(rootDir) {
    const fs = await Promise.resolve().then(() => __importStar(require("fs/promises")));
    const path = await Promise.resolve().then(() => __importStar(require("path")));
    const morphirJsonPath = path.join(rootDir, "morphir.json");
    let existed = true;
    try {
        await fs.access(morphirJsonPath);
    }
    catch {
        existed = false;
        const morphirJsonContent = JSON.stringify({
            name: "MorphirMCP",
            sourceDirectory: "src"
        }, null, 4);
        await fs.writeFile(morphirJsonPath, morphirJsonContent, "utf8");
    }
    return { existed };
}
// Utility function to ensure elm.json exists, creates it if missing
async function ensureElmJson(rootDir, sourceDirectory) {
    const fs = await Promise.resolve().then(() => __importStar(require("fs/promises")));
    const path = await Promise.resolve().then(() => __importStar(require("path")));
    const elmJsonPath = path.join(rootDir, "elm.json");
    let existed = true;
    try {
        await fs.access(elmJsonPath);
    }
    catch {
        existed = false;
        const elmJsonContent = JSON.stringify({
            type: "application",
            "source-directories": [sourceDirectory],
            "elm-version": "0.19.1",
            dependencies: {
                direct: {
                    "elm/browser": "1.0.2",
                    "elm/core": "1.0.5",
                    "elm/html": "1.0.0"
                },
                indirect: {
                    "elm/json": "1.1.3",
                    "elm/time": "1.0.0",
                    "elm/url": "1.0.0",
                    "elm/virtual-dom": "1.0.3"
                }
            },
            "test-dependencies": {
                direct: {},
                indirect: {}
            }
        }, null, 4);
        await fs.writeFile(elmJsonPath, elmJsonContent, "utf8");
    }
    return { existed };
}
// Add a new tool to add a module to the Morphir project
server.tool("addModule", `This tool adds a module to the Morphir project. A module can contain types and 
    functions defined using the syntax of the Elm programming language. 

    The tool takes two arguments: the name of the module and the content of the module.
    The name of the module should be a valid Elm module name, preferably single word, 
    and the content should be a valid Elm code without the module declaration line. 
    Imports are supported, but should be limited to the elm/core library.

    Every module name will implicitly be prefixed with "MorphirMCP.".

    Follow these rules when implementing the Elm logic:
    - Exclude the module declaration.
    - Only use imports from elm/core library.
    - When defining custom types prefer to use positional argument over record structures as arguments.
    - Implement only what the user asked for, do not add extra utility or testing functions.
    
    `, {
    moduleName: zod_1.z.string(),
    content: zod_1.z.string()
}, async ({ moduleName, content }) => {
    if (!rootDir) {
        throw new Error("Root directory is not specified. Use the --root-dir option.");
    }
    const fs = await Promise.resolve().then(() => __importStar(require("fs/promises")));
    const path = await Promise.resolve().then(() => __importStar(require("path")));
    const { exec } = await Promise.resolve().then(() => __importStar(require("child_process")));
    const { promisify } = await Promise.resolve().then(() => __importStar(require("util")));
    const execAsync = promisify(exec);
    // Ensure morphir.json exists in the root directory, create if missing
    await ensureMorphirJson(rootDir);
    // Read morphir.json to get the sourceDirectory and name
    const morphirConfigPath = path.join(rootDir, "morphir.json");
    const morphirConfig = JSON.parse(await fs.readFile(morphirConfigPath, "utf8"));
    const sourceDirectory = morphirConfig.sourceDirectory;
    const projectName = morphirConfig.name;
    if (!sourceDirectory) {
        throw new Error("sourceDirectory is not defined in morphir.json.");
    }
    if (!projectName) {
        throw new Error("name is not defined in morphir.json.");
    }
    // Prepend the module declaration to the content
    const moduleDeclaration = `module ${projectName}.${moduleName} exposing (..)\n\n`;
    const fullContent = moduleDeclaration + content;
    // Construct the full path for the module
    const modulePath = path.join(rootDir, sourceDirectory, projectName, `${moduleName}.elm`);
    // Ensure the directory exists
    await fs.mkdir(path.dirname(modulePath), { recursive: true });
    // Write the module file
    await fs.writeFile(modulePath, fullContent, "utf8");
    // Ensure elm.json exists in the root directory, create if missing
    const { existed: elmJsonExisted } = await ensureElmJson(rootDir, sourceDirectory);
    // Run "elm make" on the saved module file
    try {
        await execAsync(`${elmCommand} make ${modulePath}`, { cwd: rootDir });
    }
    catch (error) {
        await fs.unlink(modulePath).catch(() => { });
        if (!elmJsonExisted) {
            const elmJsonPath = path.join(rootDir, "elm.json");
            await fs.unlink(elmJsonPath).catch(() => { });
        }
        return {
            content: [{ type: "text", text: `Elm compile error:\n${error.stderr || error.message}` }]
        };
    }
    // Run "morphir make" in the root directory
    try {
        const { stdout } = await execAsync("morphir make", { cwd: rootDir });
        return {
            content: [{ type: "text", text: `Module ${moduleName} added successfully.\n${stdout}` }]
        };
    }
    catch (error) {
        await fs.unlink(modulePath).catch(() => { });
        if (!elmJsonExisted) {
            const elmJsonPath = path.join(rootDir, "elm.json");
            await fs.unlink(elmJsonPath).catch(() => { });
        }
        return {
            content: [{ type: "text", text: `Failed to run "morphir make":\n${error.stderr || error.message}` }]
        };
    }
});
// Add a new tool to set test cases for a module and function
server.tool("setTestCases", `This tool sets test cases for a given module and function in the Morphir project.
    It takes a module name, a function name, and a JSON object representing the test cases.
    
    Each test case should have the following structure:
    {
      "inputs": [value1, value2, ...], // Array of input values (can be null for optional inputs)
      "expectedOutput": value,         // The expected output value
      "description": "string"          // Description of the test case
    }
    
    Where value can be any valid Elm value, including null for optional inputs. Use the following rules to turn values into JSON:
    - Constructors are represented as arrays where the first item is the name of the constructor as a string and the remaining items are the arguments.
    - Zero argument constructors are represented as a single element array with the name of the constructor. (e.g., ["Just"] for Maybe Just constructor)
    `, {
    moduleName: zod_1.z.string(),
    functionName: zod_1.z.string(),
    testCases: zod_1.z.array(zod_1.z.object({
        inputs: zod_1.z.array(zod_1.z.any()),
        expectedOutput: zod_1.z.any(),
        description: zod_1.z.string()
    }))
}, async ({ moduleName, functionName, testCases }) => {
    const fs = await Promise.resolve().then(() => __importStar(require("fs/promises")));
    const path = await Promise.resolve().then(() => __importStar(require("path")));
    const testsPath = path.join(rootDir, "morphir-tests.json");
    let testSuite = [];
    // Check if morphir-tests.json exists, if not create it as an empty object
    try {
        const content = await fs.readFile(testsPath, "utf8");
        testSuite = JSON.parse(content);
    }
    catch {
        testSuite = [];
    }
    const fqName = [[["morphir", "m", "c", "p"]], [stringToName(moduleName)], stringToName(functionName)];
    // Update or add the test cases for this function
    testSuite.push([fqName, testCases]);
    // Write the updated test suite back to the file
    await fs.writeFile(testsPath, JSON.stringify(testSuite, null, 4), "utf8");
    return {
        content: [{
                type: "text",
                text: `Test cases for ${fqName} have been added successfully.`
            }]
    };
});
// Start receiving messages on stdin and sending messages on stdout
const transport = new stdio_js_1.StdioServerTransport();
// Try to start the server
server.connect(transport).then(() => {
    // nothing to log here since stdout is used for MCP transport
}).catch((error) => {
    console.error("Failed to connect server:", error);
});
// Convert a camel-case string to an array of lowercase words (name)
function stringToName(str) {
    return str
        .replace(/([a-z])([A-Z])/g, '$1 $2') // Add space before uppercase letters
        .split(' ') // Split by spaces
        .map(word => word.toLowerCase()); // Convert to lowercase
}
