// Compatibility shim that combines `make` (from cliAPI) and `gen` (from legacyAPI)
// into a single object, mirroring the surface of the old cli/cli.js exports.
// Used by tests-integration suites that pre-date the cli/cli2 → morphir-cli merge.

export { make } from "./cliAPI";
export { gen } from "./legacyAPI";
