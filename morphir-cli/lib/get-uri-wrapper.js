"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.fetchUriToJson = void 0;
const get_uri_1 = require("get-uri");
async function fetchUriToJson(uri) {
    const data = await (0, get_uri_1.getUri)(uri);
    const buffer = await toBuffer(data);
    const jsonString = buffer.toString();
    return JSON.parse(jsonString);
}
exports.fetchUriToJson = fetchUriToJson;
async function toBuffer(stream) {
    const chunks = [];
    for await (const chunk of stream) {
        chunks.push(chunk);
    }
    return Buffer.concat(chunks);
}
