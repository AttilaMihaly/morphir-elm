# morphir-elm

![morphir-elm](docs/assets/2020_Morphir_Logo_Horizontal.svg)

[Morphir](https://github.com/finos/morphir) is a multi-language system built on a data format that captures an
application's domain model and business logic in a technology agnostic manner. This repo contains tools that
allow you to write your business logic in [Elm](https://elm-lang.org/), parse it into Morphir IR and transpile
it to other languages like [Scala](https://www.scala-lang.org/) or visualize it to your business users using Elm.

We publish it both as an NPM and an Elm package:

![Package Overview](docs/assets/package-overview.png)

- The [NPM package](#npm-package) contains the CLI for running the tools as part of your build.
- The [Elm package](#elm-package) supports multiple use-cases:
  - It includes SDK functions that you can use while writing your business logic beyond the default `elm/core` support.
  - It provides a type-safe API to work with the Morphir IR directly. You can use this to add your own logic builder,
    visualization or language transpiler.
  - It also provides access to the frontend that parses the Elm source code and returns Morphir IR. You could use this
    to embed a business logic editor in your web UI.

# NPM package

[![npm version](https://badge.fury.io/js/morphir-elm.svg)](https://badge.fury.io/js/morphir-elm)

The **morphir-elm** NPM package provides a CLI to run the tooling.

## Installation

```
npm install -g morphir-elm
```

## Usage

The package installs three entry points:

- `morphir` — newer CLI surface, used for codegen targets, MCP, project init, dockerization, etc.
- `morphir-elm` — original CLI surface, retained for backwards compatibility. Covers the IR build, generic codegen, and the web tools.
- `morphir-dapr` — standalone command that generates a Dapr application from a Morphir model.

Both umbrellas share implementation behind the scenes (the `make` operation, for example, is the same code whether invoked as `morphir make` or `morphir-elm make`). Pick the umbrella that matches the subcommand you want.

```
Usage: morphir-elm [options] [command]

Commands:
  make           Translate Elm sources to Morphir IR
  gen            Generate code from Morphir IR
  develop        Start up a web server and expose developer tools through a web UI
  test           Run the test cases recorded in morphir-tests.json
  treeview       Start up a web server that exposes the model as a tree view
```

```
Usage: morphir [options] [command]

Commands:
  make             Translate Elm sources to Morphir IR
  json-schema-gen  Generate JSON Schema from the Morphir IR
  stats            Collect Morphir features used in a model into a document
  dockerize        Create a Docker image of a Morphir IR and Morphir Develop
  test-coverage    Report branch / test-case coverage for a Morphir model
  init             Interactive session that scaffolds a new Morphir project
  mcp              Start a Model Context Protocol server for Morphir projects
  scala-gen        Generate Scala code from Morphir IR
  snowpark-gen     Generate Scala-with-Snowpark code from Morphir IR
  typescript-gen   Generate TypeScript code from Morphir IR
```

Most subcommands take `-h, --help` to print their flags.

### `morphir-elm make` / `morphir make`

Reads Elm sources, translates to Morphir IR, and writes the IR as JSON.

```
Options:
  -p, --project-dir <path>      Root directory of the project where morphir.json is located. (default: ".")
  -o, --output <path>           Target file location where the Morphir IR will be saved. (default: "morphir-ir.json")
  -t, --types-only              Only include type information in the IR, no values. (default: false)
  -i, --indent-json             Use indentation in the generated JSON file. (default: false)
  -I, --include [pathOrUrl...]  Include additional Morphir distributions as a dependency.
                                Can be specified multiple times. Path, URL, or data-URL.
```

**Important**: the command requires a `morphir.json` configuration file in the project root:

```
{
    "name": "My.Package",
    "sourceDirectory": "src",
    "dependencies": ["a", "b"],
    "localDependencies": ["a", "b"],
    "exposedModules": [
        "Foo",
        "Bar"
    ]
}
```

- **name** — Name of the package. Must be a valid Elm module name; used as the prefix for all Elm modules in the package.
- **sourceDirectory** — Directory containing your Elm sources.
- **dependencies** — URI references to other IR files. Supports `file://`, `http://`, `https://`, `data://`.
- **localDependencies** — Relative paths to depending IRs (e.g. `"../sibling-folder/morphir-ir.json"`); kept for backwards compatibility.
- **exposedModules** — Modules in the public interface of the package. Names exclude the common prefix; `Foo` refers to `My.Package.Foo`.

A working example is available under `tests-integration/reference-model` — `cd` in and run the command.

### `morphir-elm gen`

Reads the IR produced by `make` and generates code into the output folder. For finer-grained control over the JSON Schema, Scala, Snowpark, or TypeScript backends, prefer the dedicated `morphir json-schema-gen` / `scala-gen` / `snowpark-gen` / `typescript-gen` commands documented below.

```
Options:
  -i, --input <path>                                Source IR. (default: "morphir-ir.json")
  -o, --output <path>                               Target output directory. (default: "./dist")
  -t, --target <type>                               Language to generate
                                                    (Scala | SpringBoot | cypher | triples | TypeScript | Snowpark).
                                                    (default: "Scala")
  -e, --target-version <version>                    Language version to generate. (default: "2.11")
  -c, --copy-deps                                   Copy backend dependencies into the output path. (default: false)
  -m, --modules-to-include <module.names>           Comma-separated allow-list of modules to include.
  -s, --include-codecs                              Generate JSON codecs. (default: false)
  -f, --filename <filename>                         Filename of the generated JSON Schema. (default: "")
  -ls, --include <strings>                          Comma-separated allow-list of names to include. (default: "")
  -dec, --decorations <filename>                    JSON file with decorations.
```

When `--target TypeScript` is used the command delegates to `morphir typescript-gen`.

### `morphir-elm develop`

Brings up a web server that browses the IR produced by `morphir-elm make`.

```
Options:
  -p, --port <port>         Port to bind to. (default: "3000")
  -o, --host <host>         Host to bind to. (default: "localhost")
  -i, --project-dir <path>  Root directory of the project where morphir.json is located. (default: ".")
```

### `morphir-elm test`

Runs the test cases stored in `morphir-tests.json` against the IR.

```
Options:
  -p, --project-dir <path>  Root directory of the project where morphir.json is located. (default: ".")
```

### `morphir-elm treeview`

Brings up a web server with a tree-view of the IR.

```
Options:
  -p, --port <port>         Port to bind to. (default: "3000")
  -o, --host <host>         Host to bind to. (default: "localhost")
  -i, --project-dir <path>  Root directory of the project where morphir.json is located. (default: ".")
```

### `morphir json-schema-gen`

Generates JSON Schema from the Morphir IR. More flexible than `morphir-elm gen -t JsonSchema`.

```
Options:
  -i, --input <path>                       Source IR. (default: "morphir-ir.json")
  -o, --output <path>                      Output directory. (default: "./dist")
  -t, --target <type>                      Schema flavour. (default: "JsonSchema")
  -e, --target-version <version>           Schema version to generate. (default: "2020-12")
  -f, --filename <filename>                Filename of the generated schema. (default: "")
  -m, --limit-to-modules <module.names>    Comma-separated allow-list of modules.
  -g, --group-schema-by <string>           Group output by `package`, `module`, or `type`. (default: "package")
  -c, --use-config                         Read configuration from a config file. (default: false)
  -ls, --include <strings>                 Comma-separated allow-list of names. (default: "")
  -d, --use-decorators                     Read configuration from decorator dictionary. (default: false)
```

### `morphir scala-gen`

Generates Scala code from the Morphir IR.

```
Options:
  -i, --input <path>                                Source IR. (default: "morphir-ir.json")
  -o, --output <path>                               Output directory. (default: "./dist")
  -t, --target <type>                               Backend variant. (default: "Scala")
  -e, --target-version <version>                    Scala version. (default: "2.11")
  -c, --copy-deps                                   Copy backend dependencies into the output. (default: false)
  -m, --limitToModules <module.names>               Comma-separated allow-list of modules.
  -s, --include-codecs <type>                       Generate Scala codecs. (default: false)
  --generate-test-generic                           Generate generic test cases from morphir tests. (default: false)
  --generate-test-scalatest                         Generate runnable scalatest cases. (default: false)
```

### `morphir snowpark-gen`

Generates Scala-with-Snowpark code from the Morphir IR.

```
Options:
  -i, --input <path>              Source IR. (default: "morphir-ir.json")
  -o, --output <path>             Output directory. (default: "./dist")
  -dec, --decorations <filename>  JSON file with decorations.
```

### `morphir typescript-gen`

Generates TypeScript code from the Morphir IR.

```
Options:
  -i, --input <path>   Source IR. (default: "morphir-ir.json")
  -o, --output <path>  Output directory. (default: "./dist")
  -c, --copy-deps      Copy backend dependencies into the output path. (default: false)
```

### `morphir stats`

Writes a report of the Morphir features used by the model.

```
Options:
  -i, --input <path>   Source IR. (default: "morphir-ir.json")
  -o, --output <path>  Output directory. (default: "./stats")
```

### `morphir test-coverage`

Generates a coverage report comparing branch counts in the IR against the recorded test cases.

```
Options:
  -i, --ir <path>      Source IR. (default: "morphir-ir.json")
  -t, --tests <path>   Test JSON. (default: "morphir-tests.json")
  -o, --output <path>  Output directory. (default: ".")
```

### `morphir dockerize`

Builds a Docker image bundling a Morphir IR with Morphir Develop.

```
Options:
  -p, --project-dir <path>  Root directory of the project where morphir.json is located. (default: ".")
  -f, --force               Overwrite any Dockerfile in the target location. (default: false)
```

### `morphir init`

Launches an interactive session that scaffolds a new Morphir project (`morphir.json`, `elm.json`, source folder).

### `morphir mcp`

Starts a Model Context Protocol (MCP) server that exposes Morphir-project tooling to MCP-compatible clients (AI assistants, IDEs, etc.).

```
Options:
  --elm-command <command>  Elm command used for compilation. (default: "elm")
  --root-dir <directory>   Root directory of the Morphir project. (required)
```

The server exposes:

- **addModule** — adds a new module to the project with Elm code.
- **setTestCases** — sets test cases for functions in the project.

It creates `morphir.json` and `elm.json` if missing, making it usable on a brand-new project.

```bash
morphir mcp --root-dir .
morphir mcp --root-dir ./my-project --elm-command /path/to/elm
```

The server communicates over stdin/stdout per the MCP protocol.

### `morphir-dapr`

Standalone command that generates a Dapr application from a Morphir model. Requires a `morphir-dapr.json` config in the project root.

```
Options:
  -p, --project-dir <path>  Root directory of the project where morphir-dapr.json is located. (default: ".")
  -o, --output <path>       Target location for the Dapr sources. (default: "dapr-output")
  -i, --info                Print the intermediate Elm output to stdout.
  -d, --delete              Delete the build directory after generation.
```

# Elm package

[![Latest version of the Elm package](https://reiner-dolp.github.io/elm-badges/finos/morphir-elm/version.svg)](https://package.elm-lang.org/packages/finos/morphir-elm/latest)

The [finos/morphir-elm](https://package.elm-lang.org/packages/finos/morphir-elm/latest) package
provides various tools to work with Morphir. It contains the following main components:

- The [Morphir SDK](#morphir-sdk) which provides the base set of types and functions that Morphir tools support
  out-of-the-box. (the SDK is a superset [elm/core](https://package.elm-lang.org/packages/elm/core/latest) with a few
  exceptions documented below)
- A type-safe API for the [Morphir IR](#morphir-ir) that allows you to create or inspect it.

## Installation

```
elm install finos/morphir-elm
```

## Morphir SDK

The goal of the `Morphir.SDK` module is to provide you the basic building blocks to build your domain model and
business logic. It also serves as a specification for backend developers that describes the minimum set of functionality
each backend implementation should support.

It is generally based on [elm/core/1.0.5](https://package.elm-lang.org/packages/elm/core/1.0.5/) and provides most of
the functionality provided there except for some modules that fall outside the scope of business knowledge modeling:
`Debug`, `Platform`, `Process` and `Task`.

Apart from the modules mentioned above you can use everything that's available in `elm/core/1.0.5` without importing
the `Morphir SDK`. The Elm frontend will simply map those to the corresponding type/function names in the Morphir SDK.

The `Morphir SDK` also provides some features beyond `elm/core/1.0.5`. To use those features you have to import the
specific `Morphir SDK` module.

## Morphir IR

The `Morphir.IR` module defines a type-safe API to work with Morphir's intermediate representation. The module
structure follows the structure of the IR. Here's a list of concepts in a top-down approach:

- [Distribution](https://package.elm-lang.org/packages/finos/morphir-elm/latest/Morphir-IR-Distribution) is the output
  of `morphir-elm make`. It represents a whole package with all of its dependencies.
- [Package](https://package.elm-lang.org/packages/finos/morphir-elm/latest/Morphir-IR-Package) represents a set of
  modules that are versioned together.
- [Module](https://package.elm-lang.org/packages/finos/morphir-elm/latest/Morphir-IR-Module) is a container
  to group types and values.
- [Types](https://package.elm-lang.org/packages/finos/morphir-elm/latest/Morphir-IR-Type) allow you to describe
  your domain model.
- [Values](https://package.elm-lang.org/packages/finos/morphir-elm/latest/Morphir-IR-Value) allows you to
  describe your business logic.
- [Names](https://package.elm-lang.org/packages/finos/morphir-elm/latest/Morphir-IR-Name) provide a naming
  convention agnostic representation for all nodes that can be named: types, values, modules and packages. Names can be
  composed into hierarchies:
  - [path](https://package.elm-lang.org/packages/finos/morphir-elm/latest/Morphir-IR-Path) is a list of names
  - [qualifield name](https://package.elm-lang.org/packages/finos/morphir-elm/latest/Morphir-IR-QName) is a module path with a local name
  - [fully-qualifield name](https://package.elm-lang.org/packages/finos/morphir-elm/latest/Morphir-IR-FQName) is a package path with a qualified name
- [AccessControlled](https://package.elm-lang.org/packages/finos/morphir-elm/latest/Morphir-IR-AccessControlled)
  is a utility to define visibility constraints for modules, types and values

## Contributing

[Contribution Guide](CONTRIBUTING.md)

1. Fork it (<https://github.com/finos/morphir-elm/fork>)
2. Create your feature branch (`git checkout -b feature/fooBar`)
3. Read our [contribution guidelines](CONTRIBUTING.md) and [Community Code of Conduct](https://www.finos.org/code-of-conduct)
4. Commit your changes (`git commit -am 'Add some fooBar'`)
5. Push to the branch (`git push origin feature/fooBar`)
6. Create a new Pull Request

_NOTE:_ Commits and pull requests to FINOS repositories will only be accepted from those contributors with an active, executed Individual Contributor License Agreement (ICLA) with FINOS OR who are covered under an existing and active Corporate Contribution License Agreement (CCLA) executed with FINOS. Commits from individuals not covered under an ICLA or CCLA will be flagged and blocked by the FINOS Clabot tool. Please note that some CCLAs require individuals/employees to be explicitly named on the CCLA.

_Need an ICLA? Unsure if you are covered under an existing CCLA? Email [help@finos.org](mailto:help@finos.org)_

### Publishing new releases

[Steps for publishing a new release](publishing.md)

## License

Copyright 2014 Morgan Stanley

Distributed under the [Apache License, Version 2.0](http://www.apache.org/licenses/LICENSE-2.0).

SPDX-License-Identifier: [Apache-2.0](https://spdx.org/licenses/Apache-2.0)
