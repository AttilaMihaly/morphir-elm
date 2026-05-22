"use strict";
// Compatibility shim that mirrors the surface of the old cli/cli.js exports
// for tests-integration suites that pre-date the cli/cli2 → morphir-cli merge.
// Everything routes through the legacy worker because those tests assert on
// the legacy worker's output shape (e.g. `IR.distribution[3].modules`).
Object.defineProperty(exports, "__esModule", { value: true });
exports.test = exports.writeFile = exports.gen = exports.make = void 0;
var legacyAPI_1 = require("./legacyAPI");
Object.defineProperty(exports, "make", { enumerable: true, get: function () { return legacyAPI_1.make; } });
Object.defineProperty(exports, "gen", { enumerable: true, get: function () { return legacyAPI_1.gen; } });
Object.defineProperty(exports, "writeFile", { enumerable: true, get: function () { return legacyAPI_1.writeFile; } });
Object.defineProperty(exports, "test", { enumerable: true, get: function () { return legacyAPI_1.test; } });
