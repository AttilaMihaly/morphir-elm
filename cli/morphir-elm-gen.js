#!/usr/bin/env node
'use strict'

// NPM imports
const path = require('path')
const commander = require('commander')
const cli = require('./cli')

// Set up Commander
const program = new commander.Command()
program
    .name('morphir-elm gen')
    .description('Generate code from Morphir IR')
    .option('-i, --input <path>', 'Source location where the Morphir IR will be loaded from. Defaults to STDIN.')
    .option('-o, --output <path>', 'Target location where the generated code will be saved. Defaults to ./dist.', './dist')
    .parse(process.argv)


cli.gen(program.input, path.resolve(program.output), {})
    .then(() => {
        console.log("Done.")
    })
    .catch((err) => {
        console.error(err)
        process.exit(1)
    })

