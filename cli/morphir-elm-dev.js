#!/usr/bin/env node
'use strict'

// NPM imports
const commander = require('commander')
const express = require('express')
const cli = require('./cli')

// Set up Commander
const program = new commander.Command()
program
    .name('morphir-elm dev')
    .description('Start a web server with dev tooling')
    .option('-p, --project-dir <path>', 'Root directory of the project where morphir.json is located.', '.')
    .parse(process.argv)

const app = express()
const port = 3000

app.get('/', (req, res) => {
    res.send('Hello World!')
})


app.get('/make', (req, res, next) => {
    cli.make(program.projectDir)
        .then((packageDef) => {
            res.send(JSON.stringify(packageDef))
        })
        .catch((err) => {
            next(err)
        })
})


app.listen(port, () => {
    console.log(`Example app listening at http://localhost:${port}`)
})