// Compatibility shim that mirrors the surface of the old cli/cli.js exports
// for tests-integration suites that pre-date the cli/cli2 → morphir-cli merge.
// Everything routes through the legacy worker because those tests assert on
// the legacy worker's output shape (e.g. `IR.distribution[3].modules`).

export { make, gen, writeFile, test } from "./legacyAPI";
