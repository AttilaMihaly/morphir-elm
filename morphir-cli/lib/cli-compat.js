"use strict";
// Compatibility shim that combines `make` (from cliAPI) and `gen` (from legacyAPI)
// into a single object, mirroring the surface of the old cli/cli.js exports.
// Used by tests-integration suites that pre-date the cli/cli2 → morphir-cli merge.
Object.defineProperty(exports, "__esModule", { value: true });
exports.gen = exports.make = void 0;
var cliAPI_1 = require("./cliAPI");
Object.defineProperty(exports, "make", { enumerable: true, get: function () { return cliAPI_1.make; } });
var legacyAPI_1 = require("./legacyAPI");
Object.defineProperty(exports, "gen", { enumerable: true, get: function () { return legacyAPI_1.gen; } });
