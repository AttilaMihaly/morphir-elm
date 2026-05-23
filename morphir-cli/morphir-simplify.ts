#!/usr/bin/env node

// NPM imports
import { Command } from 'commander'
import cli from './cli'

// logging
require('log-timestamp')

// Set up Commander
const program = new Command()

program
    .name('morphir simplify')
    .description('Generate a simplified per-module JSON representation of a Morphir IR')
    .option('-i, --input <path>', 'Source location where the Morphir IR will be loaded from.', 'morphir-ir.json')
    .option('-o, --output <path>', 'Target directory where the per-module JSON files will be saved.', './simplified-ir')
    .parse(process.argv)

const { input, output } = program.opts()

cli.simplify(input, output, program.opts())
    .then(() => {
        console.log('Done')
    })
    .catch((err: any) => {
        console.error(err)
        process.exit(1)
    })

