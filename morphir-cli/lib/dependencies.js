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
exports.loadAllDependencies = exports.DependencyConfig = exports.Url = exports.LocalFile = exports.FileUrl = exports.DataUrl = void 0;
const fs = __importStar(require("fs"));
const neverthrow_1 = require("neverthrow");
const path = __importStar(require("path"));
const util = __importStar(require("util"));
const whatwg_encoding_1 = require("whatwg-encoding");
const zod_1 = require("zod");
const get_uri_wrapper_1 = require("./get-uri-wrapper");
const parseDataUrl = require("data-urls");
const fsReadFile = util.promisify(fs.readFile);
exports.DataUrl = zod_1.z.string().trim().transform((val, ctx) => {
    const parsed = parseDataUrl(val);
    if (parsed == null) {
        ctx.addIssue({
            code: zod_1.z.ZodIssueCode.custom,
            message: "Not a valid data url"
        });
        return zod_1.z.NEVER;
    }
    return parsed;
});
exports.FileUrl = zod_1.z.string().trim().url().transform((val, ctx) => {
    if (!val.startsWith("file:")) {
        ctx.addIssue({
            code: zod_1.z.ZodIssueCode.custom,
            message: "Not a valid file url"
        });
        return zod_1.z.NEVER;
    }
    return new URL(val);
});
exports.LocalFile = zod_1.z.object({
    baseDir: zod_1.z.string(),
    sanitized: zod_1.z.string(),
}).transform(val => ({ baseDir: val.baseDir, original: val.sanitized, fullPath: path.resolve(val.baseDir, val.sanitized) }))
    .transform(ref => ({ ...ref, url: new URL(`file://${ref.fullPath}`) }))
    .refine((ref) => fs.existsSync(ref.fullPath), (ref) => {
    console.error(`File not found ${ref.original}: ${ref.fullPath}`);
    return { message: `File not found ${ref.original}` };
})
    .transform(ref => ref.url);
const SUPPORTED_PROTOCOLS = new Set(["http:", "https:", "ftp:"]);
exports.Url = zod_1.z.string().url().transform((url) => new URL(url))
    .refine((url) => SUPPORTED_PROTOCOLS.has(url.protocol));
const PathOrUrl = zod_1.z.union([exports.FileUrl, zod_1.z.string().trim().min(1)]);
const GithubData = zod_1.z.object({
    owner: zod_1.z.string(),
    repo: zod_1.z.string(),
    baseUrl: zod_1.z.string().optional()
});
const GithubConfig = zod_1.z.union([GithubData, zod_1.z.string()]);
const DependencySettings = zod_1.z.string().trim();
const Dependencies = zod_1.z.array(DependencySettings).default([]);
exports.DependencyConfig = zod_1.z.object({
    dependencies: Dependencies,
    localDependencies: zod_1.z.array(zod_1.z.string()).default([]),
    includes: zod_1.z.array(zod_1.z.string()).default([]),
    projectDir: zod_1.z.string()
});
const IncludeProvided = zod_1.z.object({
    eventKind: zod_1.z.literal('IncludeProvided'),
    payload: zod_1.z.string()
});
const LocalDependencyProvided = zod_1.z.object({
    eventKind: zod_1.z.literal('LocalDependencyProvided'),
    payload: zod_1.z.string()
});
const DependencyProvided = zod_1.z.object({
    eventKind: zod_1.z.literal('DependencyProvided'),
    payload: DependencySettings
});
const DependencyEvent = zod_1.z.discriminatedUnion("eventKind", [
    IncludeProvided,
    LocalDependencyProvided,
    DependencyProvided
]);
const DependencyEvents = zod_1.z.array(DependencyEvent);
const DependencyConfigToDependencyEvents = exports.DependencyConfig.transform((config) => {
    let events = DependencyEvents.parse([]);
    const includes = config.includes.map((include) => IncludeProvided.parse({ eventKind: "IncludeProvided", payload: include }));
    events.push(...includes);
    const localDeps = config.localDependencies.map((localDependency) => LocalDependencyProvided.parse({ eventKind: "LocalDependencyProvided", payload: localDependency }));
    events.push(...localDeps);
    const deps = config.dependencies.map((dep) => DependencyProvided.parse({ eventKind: "DependencyProvided", payload: dep }));
    events.push(...deps);
    return events;
});
const MorphirDistribution = zod_1.z.tuple([zod_1.z.string()]).rest(zod_1.z.unknown());
const MorphirIRFile = zod_1.z.object({
    formatVersion: zod_1.z.number().int(),
    distribution: MorphirDistribution
}).passthrough();
async function loadAllDependencies(config) {
    const events = DependencyConfigToDependencyEvents.parse(config);
    const results = events.map(load(config));
    const finalResults = await Promise.all(results);
    return finalResults.flatMap((result) => {
        if (result.isOk()) {
            console.info("Successfully loaded dependency", result.value.dependency);
            return result.value.dependency;
        }
        else {
            console.error("Error loading dependency", result.error);
            return [];
        }
    });
}
exports.loadAllDependencies = loadAllDependencies;
const load = (config) => function (event) {
    //TODO: Clear this up
    let source;
    let payload = event.payload;
    switch (event.eventKind) {
        case 'IncludeProvided':
            source = "includes";
            return loadDependenciesFromString(config)(event.payload, source)
                .map((dependency) => ({ dependency: dependency, source: source, payload: payload }));
        case 'LocalDependencyProvided':
            source = "localDependencies";
            return loadDependenciesFromString(config)(event.payload, source)
                .map((dependency) => ({ dependency: dependency, source: source, payload: payload }));
        case 'DependencyProvided':
            source = "dependencies";
            if (typeof payload === "string") {
                return loadDependenciesFromString(config)(payload, source)
                    .map((dependency) => ({ dependency: dependency, source: source, payload: payload }));
            }
            else {
                return loadDependenciesFromURL(payload, source)
                    .map((dependency) => ({ dependency: dependency, source: source, payload: payload }));
            }
    }
};
const loadDependenciesFromString = (config) => function (input, source) {
    const doWork = async () => {
        let sanitized = input.trim();
        let { success, data } = exports.DataUrl.safeParse(sanitized);
        if (success) {
            console.info("Loading Data url", data);
            const encodingName = (0, whatwg_encoding_1.labelToName)(data.mimeType.parameters.get("charset") || "utf-8") || "UTF-8";
            const bodyDecoded = (0, whatwg_encoding_1.decode)(data.body, encodingName);
            console.info("Data from data url", bodyDecoded);
            return JSON.parse(bodyDecoded);
        }
        let { success: fileSuccess, data: fileData } = exports.FileUrl.safeParse(sanitized);
        if (fileSuccess && fileData !== undefined) {
            console.info("Loading file url", fileData);
            return (0, get_uri_wrapper_1.fetchUriToJson)(fileData);
        }
        let { success: urlSuccess, data: urlData } = exports.Url.safeParse(sanitized);
        if (urlSuccess && urlData !== undefined) {
            console.info("Loading url", urlData);
            return (0, get_uri_wrapper_1.fetchUriToJson)(urlData);
        }
        let { success: localFileCWDSuccess, data: localUrlCWDData } = exports.LocalFile.safeParse({ baseDir: process.cwd(), sanitized });
        if (localFileCWDSuccess && localUrlCWDData !== undefined) {
            console.info("Loading local file url from current working directory ", localUrlCWDData);
            return (0, get_uri_wrapper_1.fetchUriToJson)(localUrlCWDData);
        }
        let { success: localFileSuccess, data: localUrlData } = exports.LocalFile.safeParse({ baseDir: config.projectDir, sanitized });
        if (localFileSuccess && localUrlData !== undefined) {
            console.info("Loading local file url from morphir.json directory", localUrlData);
            return (0, get_uri_wrapper_1.fetchUriToJson)(localUrlData);
        }
        throw new DependencyError("Invalid dependency string", input);
    };
    return neverthrow_1.ResultAsync.fromPromise(doWork(), (err) => new DependencyError("Error loading dependency", source, input, err));
};
function loadDependenciesFromURL(url, source) {
    const doWork = async () => {
        return (0, get_uri_wrapper_1.fetchUriToJson)(url);
    };
    return neverthrow_1.ResultAsync.fromPromise(doWork(), (err) => new DependencyError("Error loading dependency", source, url, err));
}
class DependencyError extends Error {
    constructor(message, source, dependency, cause) {
        super(message);
        this.name = "DependencyError";
        if (cause) {
            this.cause = cause;
        }
        if (dependency) {
            this.dependency = dependency;
        }
        if (source) {
            this.source = source;
        }
    }
}
class LocalDependencyNotFound extends Error {
    constructor(message, source, pathOrUrl, cause) {
        super(message);
        this.name = "LocalDependencyNotFound";
        if (cause) {
            this.cause = cause;
        }
        if (pathOrUrl) {
            this.pathOrUrl = pathOrUrl;
        }
        if (source) {
            this.source = source;
        }
    }
}
async function toBuffer(stream) {
    const chunks = [];
    for await (const chunk of stream) {
        chunks.push(chunk);
    }
    return Buffer.concat(chunks);
}
