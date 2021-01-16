'use strict';

Object.defineProperty(exports, '__esModule', { value: true });

var fs = require('fs');

/**
 * @license
 * Copyright Google Inc. All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
function findArgument(argv, argName) {
    const index = argv.indexOf(argName);
    if (index < 0 || index === argv.length - 1) {
        return;
    }
    return argv[index + 1];
}
function parseStringArray(argv, argName) {
    const arg = findArgument(argv, argName);
    if (!arg) {
        return [];
    }
    return arg.split(',');
}
function hasArgument(argv, argName) {
    return argv.includes(argName);
}
function parseCommandLine(argv) {
    return {
        help: hasArgument(argv, '--help'),
        ivy: hasArgument(argv, '--experimental-ivy'),
        logFile: findArgument(argv, '--logFile'),
        logVerbosity: findArgument(argv, '--logVerbosity'),
        ngProbeLocations: parseStringArray(argv, '--ngProbeLocations'),
        tsProbeLocations: parseStringArray(argv, '--tsProbeLocations'),
    };
}

/**
 * @license
 * Copyright Google Inc. All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
const MIN_TS_VERSION = '3.9';
function resolve(packageName, location, rootPackage) {
    rootPackage = rootPackage || packageName;
    try {
        const packageJsonPath = require.resolve(`${rootPackage}/package.json`, {
            paths: [location],
        });
        // Do not use require() to read JSON files since it's a potential security
        // vulnerability.
        const packageJson = JSON.parse(fs.readFileSync(packageJsonPath, 'utf8'));
        const resolvedPath = require.resolve(packageName, {
            paths: [location],
        });
        return {
            name: packageName,
            resolvedPath,
            version: new Version(packageJson.version),
        };
    }
    catch (_a) {
    }
}
/**
 * Resolve the node module with the specified `packageName` that satisfies
 * the specified minimum version.
 * @param packageName name of package to be resolved
 * @param minVersionStr minimum version
 * @param probeLocations locations to initiate node module resolution
 * @param rootPackage location of package.json. For example, the root package of
 * `typescript/lib/tsserverlibrary` is `typescript`.
 */
function resolveWithMinVersion(packageName, minVersionStr, probeLocations, rootPackage) {
    if (!packageName.startsWith(rootPackage)) {
        throw new Error(`${packageName} must be in the root package`);
    }
    const minVersion = new Version(minVersionStr);
    for (const location of probeLocations) {
        const nodeModule = resolve(packageName, location, rootPackage);
        if (nodeModule && nodeModule.version.greaterThanOrEqual(minVersion)) {
            return nodeModule;
        }
    }
    throw new Error(`Failed to resolve '${packageName}' with minimum version '${minVersion}' from ` +
        JSON.stringify(probeLocations, null, 2));
}
/**
 * Resolve `typescript/lib/tsserverlibrary` from the given locations.
 * @param probeLocations
 */
function resolveTsServer(probeLocations) {
    const tsserver = 'typescript/lib/tsserverlibrary';
    return resolveWithMinVersion(tsserver, MIN_TS_VERSION, probeLocations, 'typescript');
}
/**
 * Converts the specified string `a` to non-negative integer.
 * Returns -1 if the result is NaN.
 * @param a
 */
function parseNonNegativeInt(a) {
    // parseInt() will try to convert as many as possible leading characters that
    // are digits. This means a string like "123abc" will be converted to 123.
    // For our use case, this is sufficient.
    const i = parseInt(a, 10 /* radix */);
    return isNaN(i) ? -1 : i;
}
class Version {
    constructor(versionStr) {
        this.versionStr = versionStr;
        const [major, minor, patch] = Version.parseVersionStr(versionStr);
        this.major = major;
        this.minor = minor;
        this.patch = patch;
    }
    greaterThanOrEqual(other) {
        if (this.major < other.major) {
            return false;
        }
        if (this.major > other.major) {
            return true;
        }
        if (this.minor < other.minor) {
            return false;
        }
        if (this.minor > other.minor) {
            return true;
        }
        return this.patch >= other.patch;
    }
    toString() {
        return this.versionStr;
    }
    /**
     * Converts the specified `versionStr` to its number constituents. Invalid
     * number value is represented as negative number.
     * @param versionStr
     */
    static parseVersionStr(versionStr) {
        const [major, minor, patch] = versionStr.split('.').map(parseNonNegativeInt);
        return [
            major === undefined ? 0 : major,
            minor === undefined ? 0 : minor,
            patch === undefined ? 0 : patch,
        ];
    }
}

/**
 * This method provides a custom implementation for the AMD loader to resolve
 * `typescript` module at runtime.
 * @param modules modules to resolve
 * @param cb function to invoke with resolved modules
 */
function define(modules, cb) {
    const TSSERVER = 'typescript/lib/tsserverlibrary';
    const resolvedModules = modules.map(m => {
        if (m === 'typescript') {
            throw new Error(`Import '${TSSERVER}' instead of 'typescript'`);
        }
        if (m === TSSERVER) {
            const { tsProbeLocations } = parseCommandLine(process.argv);
            m = resolveTsServer(tsProbeLocations).resolvedPath;
        }
        return require(m);
    });
    cb(...resolvedModules);
}

exports.define = define;

define(['fs', 'path', 'typescript/lib/tsserverlibrary', 'vscode-languageserver', 'vscode-uri'], function (fs, path, tsserverlibrary, vscodeLanguageserver, vscodeUri) { 'use strict';

	function _interopDefaultLegacy (e) { return e && typeof e === 'object' && 'default' in e ? e : { 'default': e }; }

	var fs__default = /*#__PURE__*/_interopDefaultLegacy(fs);
	var path__default = /*#__PURE__*/_interopDefaultLegacy(path);
	var tsserverlibrary__default = /*#__PURE__*/_interopDefaultLegacy(tsserverlibrary);
	var vscodeLanguageserver__default = /*#__PURE__*/_interopDefaultLegacy(vscodeLanguageserver);
	var vscodeUri__default = /*#__PURE__*/_interopDefaultLegacy(vscodeUri);

	function unwrapExports (x) {
		return x && x.__esModule && Object.prototype.hasOwnProperty.call(x, 'default') ? x['default'] : x;
	}

	function createCommonjsModule(fn, module) {
		return module = { exports: {} }, fn(module, module.exports), module.exports;
	}

	var cmdline_utils = createCommonjsModule(function (module, exports) {
	/**
	 * @license
	 * Copyright Google Inc. All Rights Reserved.
	 *
	 * Use of this source code is governed by an MIT-style license that can be
	 * found in the LICENSE file at https://angular.io/license
	 */
	Object.defineProperty(exports, "__esModule", { value: true });
	exports.generateHelpMessage = exports.parseCommandLine = void 0;
	function findArgument(argv, argName) {
	    const index = argv.indexOf(argName);
	    if (index < 0 || index === argv.length - 1) {
	        return;
	    }
	    return argv[index + 1];
	}
	function parseStringArray(argv, argName) {
	    const arg = findArgument(argv, argName);
	    if (!arg) {
	        return [];
	    }
	    return arg.split(',');
	}
	function hasArgument(argv, argName) {
	    return argv.includes(argName);
	}
	function parseCommandLine(argv) {
	    return {
	        help: hasArgument(argv, '--help'),
	        ivy: hasArgument(argv, '--experimental-ivy'),
	        logFile: findArgument(argv, '--logFile'),
	        logVerbosity: findArgument(argv, '--logVerbosity'),
	        ngProbeLocations: parseStringArray(argv, '--ngProbeLocations'),
	        tsProbeLocations: parseStringArray(argv, '--tsProbeLocations'),
	    };
	}
	exports.parseCommandLine = parseCommandLine;
	function generateHelpMessage(argv) {
	    // Do not expose --experimental-ivy flag since it's only used for development.
	    return `Angular Language Service that implements the Language Server Protocol (LSP).

  Usage: ${argv[0]} ${argv[1]} [options]

  Options:
    --help: Prints help message.
    --logFile: Location to log messages. Logging is disabled if not provided.
    --logVerbosity: terse|normal|verbose|requestTime. See ts.server.LogLevel.
    --ngProbeLocations: Path of @angular/language-service. Required.
    --tsProbeLocations: Path of typescript. Required.

  Additional options supported by vscode-languageserver:
    --clientProcessId=<number>: Automatically kills the server if the client process dies.
    --node-ipc: Communicate using Node's IPC. This is the default.
    --stdio: Communicate over stdin/stdout.
    --socket=<number>: Communicate using Unix socket.
  `;
	}
	exports.generateHelpMessage = generateHelpMessage;

	});

	unwrapExports(cmdline_utils);
	var cmdline_utils_1 = cmdline_utils.generateHelpMessage;
	var cmdline_utils_2 = cmdline_utils.parseCommandLine;

	var logger = createCommonjsModule(function (module, exports) {
	/**
	 * @license
	 * Copyright Google Inc. All Rights Reserved.
	 *
	 * Use of this source code is governed by an MIT-style license that can be
	 * found in the LICENSE file at https://angular.io/license
	 */
	Object.defineProperty(exports, "__esModule", { value: true });
	exports.Logger = exports.createLogger = void 0;



	/**
	 * Create a logger instance to write to file.
	 * @param options Logging options.
	 */
	function createLogger(options) {
	    let logLevel;
	    switch (options.logVerbosity) {
	        case 'requestTime':
	            logLevel = tsserverlibrary__default['default'].server.LogLevel.requestTime;
	            break;
	        case 'verbose':
	            logLevel = tsserverlibrary__default['default'].server.LogLevel.verbose;
	            break;
	        case 'normal':
	            logLevel = tsserverlibrary__default['default'].server.LogLevel.normal;
	            break;
	        case 'terse':
	        default:
	            logLevel = tsserverlibrary__default['default'].server.LogLevel.terse;
	            break;
	    }
	    // If logFile is not provided then just trace to console.
	    const traceToConsole = !options.logFile;
	    return new Logger(traceToConsole, logLevel, options.logFile);
	}
	exports.createLogger = createLogger;
	// TODO: Code below is from TypeScript's repository. Maybe create our own
	// implementation.
	// https://github.com/microsoft/TypeScript/blob/ec39d412876d0dcf704fc886d5036cb625220d2f/src/tsserver/server.ts#L120
	function noop(_) { } // tslint:disable-line no-empty
	function nowString() {
	    // E.g. "12:34:56.789"
	    const d = new Date();
	    return `${d.getHours()}:${d.getMinutes()}:${d.getSeconds()}.${d.getMilliseconds()}`;
	}
	class Logger {
	    constructor(traceToConsole, level, logFilename) {
	        this.traceToConsole = traceToConsole;
	        this.level = level;
	        this.logFilename = logFilename;
	        this.fd = -1;
	        this.seq = 0;
	        this.inGroup = false;
	        this.firstInGroup = true;
	        if (logFilename) {
	            try {
	                const dir = path__default['default'].dirname(logFilename);
	                if (!fs__default['default'].existsSync(dir)) {
	                    fs__default['default'].mkdirSync(dir);
	                }
	                this.fd = fs__default['default'].openSync(logFilename, 'w');
	            }
	            catch (_a) {
	                // swallow the error and keep logging disabled if file cannot be opened
	            }
	        }
	    }
	    static padStringRight(str, padding) {
	        return (str + padding).slice(0, padding.length);
	    }
	    close() {
	        if (this.fd >= 0) {
	            fs__default['default'].close(this.fd, noop);
	        }
	    }
	    getLogFileName() {
	        return this.logFilename;
	    }
	    perftrc(s) {
	        this.msg(s, tsserverlibrary__default['default'].server.Msg.Perf);
	    }
	    info(s) {
	        this.msg(s, tsserverlibrary__default['default'].server.Msg.Info);
	    }
	    err(s) {
	        this.msg(s, tsserverlibrary__default['default'].server.Msg.Err);
	    }
	    startGroup() {
	        this.inGroup = true;
	        this.firstInGroup = true;
	    }
	    endGroup() {
	        this.inGroup = false;
	    }
	    loggingEnabled() {
	        return !!this.logFilename || this.traceToConsole;
	    }
	    hasLevel(level) {
	        return this.loggingEnabled() && this.level >= level;
	    }
	    msg(s, type = tsserverlibrary__default['default'].server.Msg.Err) {
	        if (!this.canWrite)
	            return;
	        s = `[${nowString()}] ${s}\n`;
	        if (!this.inGroup || this.firstInGroup) {
	            const prefix = Logger.padStringRight(type + ' ' + this.seq.toString(), '          ');
	            s = prefix + s;
	        }
	        this.write(s);
	        if (!this.inGroup) {
	            this.seq++;
	        }
	    }
	    get canWrite() {
	        return this.fd >= 0 || this.traceToConsole;
	    }
	    write(s) {
	        if (this.fd >= 0) {
	            const buf = Buffer.from(s);
	            // tslint:disable-next-line no-null-keyword
	            fs__default['default'].writeSync(this.fd, buf, 0, buf.length, /*position*/ null); // TODO: GH#18217
	        }
	        if (this.traceToConsole) {
	            console.warn(s);
	        }
	    }
	}
	exports.Logger = Logger;

	});

	unwrapExports(logger);
	var logger_1 = logger.Logger;
	var logger_2 = logger.createLogger;

	var server_host = createCommonjsModule(function (module, exports) {
	/**
	 * @license
	 * Copyright Google Inc. All Rights Reserved.
	 *
	 * Use of this source code is governed by an MIT-style license that can be
	 * found in the LICENSE file at https://angular.io/license
	 */
	Object.defineProperty(exports, "__esModule", { value: true });
	exports.ServerHost = void 0;

	/**
	 * `ServerHost` is a wrapper around `ts.sys` for the Node system. In Node, all
	 * optional methods of `ts.System` are implemented.
	 * See
	 * https://github.com/microsoft/TypeScript/blob/ec39d412876d0dcf704fc886d5036cb625220d2f/src/compiler/sys.ts#L716
	 */
	class ServerHost {
	    constructor() {
	        this.args = tsserverlibrary__default['default'].sys.args;
	        this.newLine = tsserverlibrary__default['default'].sys.newLine;
	        this.useCaseSensitiveFileNames = tsserverlibrary__default['default'].sys.useCaseSensitiveFileNames;
	    }
	    write(s) {
	        tsserverlibrary__default['default'].sys.write(s);
	    }
	    writeOutputIsTTY() {
	        return tsserverlibrary__default['default'].sys.writeOutputIsTTY();
	    }
	    readFile(path, encoding) {
	        return tsserverlibrary__default['default'].sys.readFile(path, encoding);
	    }
	    getFileSize(path) {
	        return tsserverlibrary__default['default'].sys.getFileSize(path);
	    }
	    writeFile(path, data, writeByteOrderMark) {
	        return tsserverlibrary__default['default'].sys.writeFile(path, data, writeByteOrderMark);
	    }
	    /**
	     * @pollingInterval - this parameter is used in polling-based watchers and
	     * ignored in watchers that use native OS file watching
	     */
	    watchFile(path, callback, pollingInterval) {
	        return tsserverlibrary__default['default'].sys.watchFile(path, callback, pollingInterval);
	    }
	    watchDirectory(path, callback, recursive) {
	        return tsserverlibrary__default['default'].sys.watchDirectory(path, callback, recursive);
	    }
	    resolvePath(path) {
	        return tsserverlibrary__default['default'].sys.resolvePath(path);
	    }
	    fileExists(path) {
	        return tsserverlibrary__default['default'].sys.fileExists(path);
	    }
	    directoryExists(path) {
	        return tsserverlibrary__default['default'].sys.directoryExists(path);
	    }
	    createDirectory(path) {
	        return tsserverlibrary__default['default'].sys.createDirectory(path);
	    }
	    getExecutingFilePath() {
	        return tsserverlibrary__default['default'].sys.getExecutingFilePath();
	    }
	    getCurrentDirectory() {
	        return tsserverlibrary__default['default'].sys.getCurrentDirectory();
	    }
	    getDirectories(path) {
	        return tsserverlibrary__default['default'].sys.getDirectories(path);
	    }
	    readDirectory(path, extensions, exclude, include, depth) {
	        return tsserverlibrary__default['default'].sys.readDirectory(path, extensions, exclude, include, depth);
	    }
	    getModifiedTime(path) {
	        return tsserverlibrary__default['default'].sys.getModifiedTime(path);
	    }
	    setModifiedTime(path, time) {
	        return tsserverlibrary__default['default'].sys.setModifiedTime(path, time);
	    }
	    deleteFile(path) {
	        return tsserverlibrary__default['default'].sys.deleteFile(path);
	    }
	    /**
	     * A good implementation is node.js' `crypto.createHash`.
	     * (https://nodejs.org/api/crypto.html#crypto_crypto_createhash_algorithm)
	     */
	    createHash(data) {
	        return tsserverlibrary__default['default'].sys.createHash(data);
	    }
	    /**
	     * This must be cryptographically secure. Only implement this method using
	     * `crypto.createHash("sha256")`.
	     */
	    createSHA256Hash(data) {
	        return tsserverlibrary__default['default'].sys.createSHA256Hash(data);
	    }
	    getMemoryUsage() {
	        return tsserverlibrary__default['default'].sys.getMemoryUsage();
	    }
	    exit(exitCode) {
	        return tsserverlibrary__default['default'].sys.exit(exitCode);
	    }
	    realpath(path) {
	        return tsserverlibrary__default['default'].sys.realpath(path);
	    }
	    setTimeout(callback, ms, ...args) {
	        return tsserverlibrary__default['default'].sys.setTimeout(callback, ms, ...args);
	    }
	    clearTimeout(timeoutId) {
	        return tsserverlibrary__default['default'].sys.clearTimeout(timeoutId);
	    }
	    clearScreen() {
	        return tsserverlibrary__default['default'].sys.clearScreen();
	    }
	    base64decode(input) {
	        return tsserverlibrary__default['default'].sys.base64decode(input);
	    }
	    base64encode(input) {
	        return tsserverlibrary__default['default'].sys.base64encode(input);
	    }
	    setImmediate(callback, ...args) {
	        return setImmediate(callback, ...args);
	    }
	    clearImmediate(timeoutId) {
	        return clearImmediate(timeoutId);
	    }
	    require(initialPath, moduleName) {
	        try {
	            const modulePath = require.resolve(moduleName, {
	                paths: [initialPath],
	            });
	            return {
	                module: require(modulePath),
	                error: undefined,
	            };
	        }
	        catch (e) {
	            return {
	                module: undefined,
	                error: e,
	            };
	        }
	    }
	}
	exports.ServerHost = ServerHost;

	});

	unwrapExports(server_host);
	var server_host_1 = server_host.ServerHost;

	var utils = createCommonjsModule(function (module, exports) {
	/**
	 * @license
	 * Copyright Google Inc. All Rights Reserved.
	 *
	 * Use of this source code is governed by an MIT-style license that can be
	 * found in the LICENSE file at https://angular.io/license
	 */
	Object.defineProperty(exports, "__esModule", { value: true });
	exports.lspRangeToTsPositions = exports.lspPositionToTsPosition = exports.tsTextSpanToLspRange = exports.filePathToUri = exports.uriToFilePath = void 0;


	var Scheme;
	(function (Scheme) {
	    Scheme["File"] = "file";
	})(Scheme || (Scheme = {}));
	/**
	 * Extract the file path from the specified `uri`.
	 * @param uri
	 */
	function uriToFilePath(uri) {
	    // Note: uri.path is different from uri.fsPath
	    // See
	    // https://github.com/microsoft/vscode-uri/blob/413805221cc6ed167186ab3103d3248d6f7161f2/src/index.ts#L622-L645
	    const { scheme, fsPath } = vscodeUri__default['default'].URI.parse(uri);
	    if (scheme !== Scheme.File) {
	        return '';
	    }
	    return fsPath;
	}
	exports.uriToFilePath = uriToFilePath;
	/**
	 * Converts the specified `filePath` to a proper URI.
	 * @param filePath
	 */
	function filePathToUri(filePath) {
	    return vscodeUri__default['default'].URI.file(filePath).toString();
	}
	exports.filePathToUri = filePathToUri;
	/**
	 * Convert ts.TextSpan to lsp.TextSpan. TypeScript keeps track of offset using
	 * 1-based index whereas LSP uses 0-based index.
	 * @param scriptInfo Used to determine the offsets.
	 * @param textSpan
	 */
	function tsTextSpanToLspRange(scriptInfo, textSpan) {
	    const start = scriptInfo.positionToLineOffset(textSpan.start);
	    const end = scriptInfo.positionToLineOffset(textSpan.start + textSpan.length);
	    // ScriptInfo (TS) is 1-based, LSP is 0-based.
	    return vscodeLanguageserver__default['default'].Range.create(start.line - 1, start.offset - 1, end.line - 1, end.offset - 1);
	}
	exports.tsTextSpanToLspRange = tsTextSpanToLspRange;
	/**
	 * Convert lsp.Position to the absolute offset in the file. LSP keeps track of
	 * offset using 0-based index whereas TypeScript uses 1-based index.
	 * @param scriptInfo Used to determine the offsets.
	 * @param position
	 */
	function lspPositionToTsPosition(scriptInfo, position) {
	    const { line, character } = position;
	    // ScriptInfo (TS) is 1-based, LSP is 0-based.
	    return scriptInfo.lineOffsetToPosition(line + 1, character + 1);
	}
	exports.lspPositionToTsPosition = lspPositionToTsPosition;
	/**
	 * Convert lsp.Range which is made up of `start` and `end` positions to
	 * TypeScript's absolute offsets.
	 * @param scriptInfo Used to determine the offsets.
	 * @param range
	 */
	function lspRangeToTsPositions(scriptInfo, range) {
	    const start = lspPositionToTsPosition(scriptInfo, range.start);
	    const end = lspPositionToTsPosition(scriptInfo, range.end);
	    return [start, end];
	}
	exports.lspRangeToTsPositions = lspRangeToTsPositions;

	});

	unwrapExports(utils);
	var utils_1 = utils.lspRangeToTsPositions;
	var utils_2 = utils.lspPositionToTsPosition;
	var utils_3 = utils.tsTextSpanToLspRange;
	var utils_4 = utils.filePathToUri;
	var utils_5 = utils.uriToFilePath;

	var completion = createCommonjsModule(function (module, exports) {
	/**
	 * @license
	 * Copyright Google Inc. All Rights Reserved.
	 *
	 * Use of this source code is governed by an MIT-style license that can be
	 * found in the LICENSE file at https://angular.io/license
	 */
	Object.defineProperty(exports, "__esModule", { value: true });
	exports.tsCompletionEntryToLspCompletionItem = void 0;


	// TODO: Move this to `@angular/language-service`.
	var CompletionKind;
	(function (CompletionKind) {
	    CompletionKind["attribute"] = "attribute";
	    CompletionKind["htmlAttribute"] = "html attribute";
	    CompletionKind["property"] = "property";
	    CompletionKind["component"] = "component";
	    CompletionKind["element"] = "element";
	    CompletionKind["key"] = "key";
	    CompletionKind["method"] = "method";
	    CompletionKind["pipe"] = "pipe";
	    CompletionKind["type"] = "type";
	    CompletionKind["reference"] = "reference";
	    CompletionKind["variable"] = "variable";
	    CompletionKind["entity"] = "entity";
	})(CompletionKind || (CompletionKind = {}));
	/**
	 * Convert Angular's CompletionKind to LSP CompletionItemKind.
	 * @param kind Angular's CompletionKind
	 */
	function ngCompletionKindToLspCompletionItemKind(kind) {
	    switch (kind) {
	        case CompletionKind.attribute:
	        case CompletionKind.htmlAttribute:
	        case CompletionKind.property:
	            return vscodeLanguageserver__default['default'].CompletionItemKind.Property;
	        case CompletionKind.component:
	        case CompletionKind.element:
	        case CompletionKind.key:
	            return vscodeLanguageserver__default['default'].CompletionItemKind.Class;
	        case CompletionKind.method:
	            return vscodeLanguageserver__default['default'].CompletionItemKind.Method;
	        case CompletionKind.pipe:
	            return vscodeLanguageserver__default['default'].CompletionItemKind.Function;
	        case CompletionKind.type:
	            return vscodeLanguageserver__default['default'].CompletionItemKind.Interface;
	        case CompletionKind.reference:
	        case CompletionKind.variable:
	            return vscodeLanguageserver__default['default'].CompletionItemKind.Variable;
	        case CompletionKind.entity:
	        default:
	            return vscodeLanguageserver__default['default'].CompletionItemKind.Text;
	    }
	}
	/**
	 * Convert ts.CompletionEntry to LSP Completion Item.
	 * @param entry completion entry
	 * @param position position where completion is requested.
	 * @param scriptInfo
	 */
	function tsCompletionEntryToLspCompletionItem(entry, position, scriptInfo) {
	    const item = vscodeLanguageserver__default['default'].CompletionItem.create(entry.name);
	    // Even though `entry.kind` is typed as ts.ScriptElementKind, it's
	    // really Angular's CompletionKind. This is because ts.ScriptElementKind does
	    // not sufficiently capture the HTML entities.
	    // This is a limitation of being a tsserver plugin.
	    const kind = entry.kind;
	    item.kind = ngCompletionKindToLspCompletionItemKind(kind);
	    item.detail = entry.kind;
	    item.sortText = entry.sortText;
	    // Text that actually gets inserted to the document. It could be different
	    // from 'entry.name'. For example, a method name could be 'greet', but the
	    // insertText is 'greet()'.
	    const insertText = entry.insertText || entry.name;
	    item.textEdit = entry.replacementSpan ?
	        vscodeLanguageserver__default['default'].TextEdit.replace(utils.tsTextSpanToLspRange(scriptInfo, entry.replacementSpan), insertText) :
	        vscodeLanguageserver__default['default'].TextEdit.insert(position, insertText);
	    return item;
	}
	exports.tsCompletionEntryToLspCompletionItem = tsCompletionEntryToLspCompletionItem;

	});

	unwrapExports(completion);
	var completion_1 = completion.tsCompletionEntryToLspCompletionItem;

	var diagnostic = createCommonjsModule(function (module, exports) {
	/**
	 * @license
	 * Copyright Google Inc. All Rights Reserved.
	 *
	 * Use of this source code is governed by an MIT-style license that can be
	 * found in the LICENSE file at https://angular.io/license
	 */
	Object.defineProperty(exports, "__esModule", { value: true });
	exports.tsDiagnosticToLspDiagnostic = void 0;



	/**
	 * Convert ts.DiagnosticCategory to lsp.DiagnosticSeverity
	 * @param category diagnostic category
	 */
	function tsDiagnosticCategoryToLspDiagnosticSeverity(category) {
	    switch (category) {
	        case tsserverlibrary__default['default'].DiagnosticCategory.Warning:
	            return vscodeLanguageserver__default['default'].DiagnosticSeverity.Warning;
	        case tsserverlibrary__default['default'].DiagnosticCategory.Error:
	            return vscodeLanguageserver__default['default'].DiagnosticSeverity.Error;
	        case tsserverlibrary__default['default'].DiagnosticCategory.Suggestion:
	            return vscodeLanguageserver__default['default'].DiagnosticSeverity.Hint;
	        case tsserverlibrary__default['default'].DiagnosticCategory.Message:
	        default:
	            return vscodeLanguageserver__default['default'].DiagnosticSeverity.Information;
	    }
	}
	/**
	 * Convert ts.Diagnostic to lsp.Diagnostic
	 * @param tsDiag TS diagnostic
	 * @param scriptInfo Used to compute proper offset.
	 */
	function tsDiagnosticToLspDiagnostic(tsDiag, scriptInfo) {
	    const textSpan = {
	        start: tsDiag.start || 0,
	        length: tsDiag.length || 0,
	    };
	    return vscodeLanguageserver__default['default'].Diagnostic.create(utils.tsTextSpanToLspRange(scriptInfo, textSpan), tsserverlibrary__default['default'].flattenDiagnosticMessageText(tsDiag.messageText, '\n'), tsDiagnosticCategoryToLspDiagnosticSeverity(tsDiag.category), tsDiag.code, tsDiag.source);
	}
	exports.tsDiagnosticToLspDiagnostic = tsDiagnosticToLspDiagnostic;

	});

	unwrapExports(diagnostic);
	var diagnostic_1 = diagnostic.tsDiagnosticToLspDiagnostic;

	var protocol = createCommonjsModule(function (module, exports) {
	/**
	 * @license
	 * Copyright Google Inc. All Rights Reserved.
	 *
	 * Use of this source code is governed by an MIT-style license that can be
	 * found in the LICENSE file at https://angular.io/license
	 */
	Object.defineProperty(exports, "__esModule", { value: true });
	exports.projectLoadingNotification = void 0;

	exports.projectLoadingNotification = {
	    start: new vscodeLanguageserver__default['default'].NotificationType0('angular-language-service/projectLoadingStart'),
	    finish: new vscodeLanguageserver__default['default'].NotificationType0('angular-language-service/projectLoadingFinish')
	};

	});

	unwrapExports(protocol);
	var protocol_1 = protocol.projectLoadingNotification;

	var session = createCommonjsModule(function (module, exports) {
	/**
	 * @license
	 * Copyright Google Inc. All Rights Reserved.
	 *
	 * Use of this source code is governed by an MIT-style license that can be
	 * found in the LICENSE file at https://angular.io/license
	 */
	Object.defineProperty(exports, "__esModule", { value: true });
	exports.Session = void 0;






	var LanguageId;
	(function (LanguageId) {
	    LanguageId["TS"] = "typescript";
	    LanguageId["HTML"] = "html";
	})(LanguageId || (LanguageId = {}));
	// Empty definition range for files without `scriptInfo`
	const EMPTY_RANGE = vscodeLanguageserver__default['default'].Range.create(0, 0, 0, 0);
	/**
	 * Session is a wrapper around lsp.IConnection, with all the necessary protocol
	 * handlers installed for Angular language service.
	 */
	class Session {
	    constructor(options) {
	        this.diagnosticsTimeout = null;
	        this.isProjectLoading = false;
	        // Create a connection for the server. The connection uses Node's IPC as a transport.
	        this.connection = vscodeLanguageserver__default['default'].createConnection();
	        this.addProtocolHandlers(this.connection);
	        this.projectService = this.createProjectService(options);
	    }
	    createProjectService(options) {
	        const projSvc = new tsserverlibrary__default['default'].server.ProjectService({
	            host: options.host,
	            logger: options.logger,
	            cancellationToken: tsserverlibrary__default['default'].server.nullCancellationToken,
	            useSingleInferredProject: true,
	            useInferredProjectPerProjectRoot: true,
	            typingsInstaller: tsserverlibrary__default['default'].server.nullTypingsInstaller,
	            // Not supressing diagnostic events can cause a type error to be thrown when the
	            // language server session gets an event for a file that is outside the project
	            // managed by the project service, and for which a program does not exist in the
	            // corresponding project's language service.
	            // See https://github.com/angular/vscode-ng-language-service/issues/693
	            suppressDiagnosticEvents: true,
	            eventHandler: (e) => this.handleProjectServiceEvent(e),
	            globalPlugins: [options.ngPlugin],
	            pluginProbeLocations: [options.ngProbeLocation],
	            allowLocalPluginLoads: false,
	        });
	        projSvc.setHostConfiguration({
	            formatOptions: projSvc.getHostFormatCodeOptions(),
	            extraFileExtensions: [
	                {
	                    extension: '.html',
	                    isMixedContent: false,
	                    scriptKind: tsserverlibrary__default['default'].ScriptKind.External,
	                },
	            ],
	        });
	        projSvc.configurePlugin({
	            pluginName: options.ngPlugin,
	            configuration: {
	                angularOnly: true,
	            },
	        });
	        return projSvc;
	    }
	    addProtocolHandlers(conn) {
	        conn.onInitialize(p => this.onInitialize(p));
	        conn.onDidOpenTextDocument(p => this.onDidOpenTextDocument(p));
	        conn.onDidCloseTextDocument(p => this.onDidCloseTextDocument(p));
	        conn.onDidChangeTextDocument(p => this.onDidChangeTextDocument(p));
	        conn.onDidSaveTextDocument(p => this.onDidSaveTextDocument(p));
	        conn.onDefinition(p => this.onDefinition(p));
	        conn.onHover(p => this.onHover(p));
	        conn.onCompletion(p => this.onCompletion(p));
	    }
	    /**
	     * An event handler that gets invoked whenever the program changes and
	     * TS ProjectService sends `ProjectUpdatedInBackgroundEvent`. This particular
	     * event is used to trigger diagnostic checks.
	     * @param event
	     */
	    handleProjectServiceEvent(event) {
	        switch (event.eventName) {
	            case tsserverlibrary__default['default'].server.ProjectLoadingStartEvent:
	                this.isProjectLoading = true;
	                this.connection.sendNotification(protocol.projectLoadingNotification.start);
	                break;
	            case tsserverlibrary__default['default'].server.ProjectLoadingFinishEvent: {
	                const { project } = event.data;
	                try {
	                    // Disable language service if project is not Angular
	                    this.checkIsAngularProject(project);
	                }
	                finally {
	                    if (this.isProjectLoading) {
	                        this.isProjectLoading = false;
	                        this.connection.sendNotification(protocol.projectLoadingNotification.finish);
	                    }
	                }
	                break;
	            }
	            case tsserverlibrary__default['default'].server.ProjectsUpdatedInBackgroundEvent:
	                // ProjectsUpdatedInBackgroundEvent is sent whenever diagnostics are
	                // requested via project.refreshDiagnostics()
	                this.triggerDiagnostics(event.data.openFiles);
	                break;
	        }
	    }
	    /**
	     * Retrieve Angular diagnostics for the specified `openFiles` after a specific
	     * `delay`, or renew the request if there's already a pending one.
	     * @param openFiles
	     * @param delay time to wait before sending request (milliseconds)
	     */
	    triggerDiagnostics(openFiles, delay = 200) {
	        // Do not immediately send a diagnostics request. Send only after user has
	        // stopped typing after the specified delay.
	        if (this.diagnosticsTimeout) {
	            // If there's an existing timeout, cancel it
	            clearTimeout(this.diagnosticsTimeout);
	        }
	        // Set a new timeout
	        this.diagnosticsTimeout = setTimeout(() => {
	            this.diagnosticsTimeout = null; // clear the timeout
	            this.sendPendingDiagnostics(openFiles);
	            // Default delay is 200ms, consistent with TypeScript. See
	            // https://github.com/microsoft/vscode/blob/7b944a16f52843b44cede123dd43ae36c0405dfd/extensions/typescript-language-features/src/features/bufferSyncSupport.ts#L493)
	        }, delay);
	    }
	    /**
	     * Execute diagnostics request for each of the specified `openFiles`.
	     * @param openFiles
	     */
	    sendPendingDiagnostics(openFiles) {
	        for (const fileName of openFiles) {
	            const scriptInfo = this.projectService.getScriptInfo(fileName);
	            if (!scriptInfo) {
	                continue;
	            }
	            const ngLS = this.getDefaultLanguageService(scriptInfo);
	            if (!ngLS) {
	                continue;
	            }
	            const diagnostics = ngLS.getSemanticDiagnostics(fileName);
	            // Need to send diagnostics even if it's empty otherwise editor state will
	            // not be updated.
	            this.connection.sendDiagnostics({
	                uri: utils.filePathToUri(fileName),
	                diagnostics: diagnostics.map(d => diagnostic.tsDiagnosticToLspDiagnostic(d, scriptInfo)),
	            });
	        }
	    }
	    /**
	     * Return the default project for the specified `scriptInfo` if it is already
	     * a configured project. If not, attempt to find a relevant config file and
	     * make that project its default. This method is to ensure HTML files always
	     * belong to a configured project instead of the default behavior of being in
	     * an inferred project.
	     * @param scriptInfo
	     */
	    getDefaultProjectForScriptInfo(scriptInfo) {
	        let project = this.projectService.getDefaultProjectForFile(scriptInfo.fileName, 
	        // ensureProject tries to find a default project for the scriptInfo if
	        // it does not already have one. It is not needed here because we are
	        // going to assign it a project below if it does not have one.
	        false // ensureProject
	        );
	        // TODO: verify that HTML files are attached to Inferred project by default.
	        // If they are already part of a ConfiguredProject then the following is
	        // not needed.
	        if (!project || project.projectKind !== tsserverlibrary__default['default'].server.ProjectKind.Configured) {
	            const { configFileName } = this.projectService.openClientFile(scriptInfo.fileName);
	            if (!configFileName) {
	                // Failed to find a config file. There is nothing we could do.
	                return;
	            }
	            project = this.projectService.findProject(configFileName);
	            if (!project) {
	                return;
	            }
	            scriptInfo.detachAllProjects();
	            scriptInfo.attachToProject(project);
	        }
	        return project;
	    }
	    /**
	     * Returns a language service for a default project created for the specified `scriptInfo`. If the
	     * project does not support a language service, nothing is returned.
	     */
	    getDefaultLanguageService(scriptInfo) {
	        const project = this.getDefaultProjectForScriptInfo(scriptInfo);
	        if (!(project === null || project === void 0 ? void 0 : project.languageServiceEnabled))
	            return;
	        return project.getLanguageService();
	    }
	    onInitialize(params) {
	        return {
	            capabilities: {
	                textDocumentSync: vscodeLanguageserver__default['default'].TextDocumentSyncKind.Incremental,
	                completionProvider: {
	                    // The server does not provide support to resolve additional information
	                    // for a completion item.
	                    resolveProvider: false,
	                    triggerCharacters: ['<', '.', '*', '[', '(', '$', '|']
	                },
	                definitionProvider: true,
	                hoverProvider: true,
	            },
	        };
	    }
	    onDidOpenTextDocument(params) {
	        const { uri, languageId, text } = params.textDocument;
	        const filePath = utils.uriToFilePath(uri);
	        if (!filePath) {
	            return;
	        }
	        const scriptKind = languageId === LanguageId.TS ? tsserverlibrary__default['default'].ScriptKind.TS : tsserverlibrary__default['default'].ScriptKind.External;
	        try {
	            // The content could be newer than that on disk. This could be due to
	            // buffer in the user's editor which has not been saved to disk.
	            // See https://github.com/angular/vscode-ng-language-service/issues/632
	            const result = this.projectService.openClientFile(filePath, text, scriptKind);
	            const { configFileName, configFileErrors } = result;
	            if (configFileErrors && configFileErrors.length) {
	                // configFileErrors is an empty array even if there's no error, so check length.
	                this.connection.console.error(configFileErrors.map(e => e.messageText).join('\n'));
	            }
	            if (!configFileName) {
	                this.connection.console.error(`No config file for ${filePath}`);
	                return;
	            }
	            const project = this.projectService.findProject(configFileName);
	            if (!project) {
	                this.connection.console.error(`Failed to find project for ${filePath}`);
	                return;
	            }
	            if (project.languageServiceEnabled) {
	                project.refreshDiagnostics(); // Show initial diagnostics
	            }
	        }
	        catch (error) {
	            if (this.isProjectLoading) {
	                this.isProjectLoading = false;
	                this.connection.sendNotification(protocol.projectLoadingNotification.finish);
	            }
	            if (error.stack) {
	                this.error(error.stack);
	            }
	            throw error;
	        }
	    }
	    onDidCloseTextDocument(params) {
	        const { textDocument } = params;
	        const filePath = utils.uriToFilePath(textDocument.uri);
	        if (!filePath) {
	            return;
	        }
	        this.projectService.closeClientFile(filePath);
	    }
	    onDidChangeTextDocument(params) {
	        const { contentChanges, textDocument } = params;
	        const filePath = utils.uriToFilePath(textDocument.uri);
	        if (!filePath) {
	            return;
	        }
	        const scriptInfo = this.projectService.getScriptInfo(filePath);
	        if (!scriptInfo) {
	            this.connection.console.log(`Failed to get script info for ${filePath}`);
	            return;
	        }
	        for (const change of contentChanges) {
	            if ('range' in change) {
	                const [start, end] = utils.lspRangeToTsPositions(scriptInfo, change.range);
	                scriptInfo.editContent(start, end, change.text);
	            }
	            else {
	                // New text is considered to be the full content of the document.
	                scriptInfo.editContent(0, scriptInfo.getSnapshot().getLength(), change.text);
	            }
	        }
	        const project = this.getDefaultProjectForScriptInfo(scriptInfo);
	        if (!project || !project.languageServiceEnabled) {
	            return;
	        }
	        project.refreshDiagnostics();
	    }
	    onDidSaveTextDocument(params) {
	        const { text, textDocument } = params;
	        const filePath = utils.uriToFilePath(textDocument.uri);
	        const scriptInfo = this.projectService.getScriptInfo(filePath);
	        if (!scriptInfo) {
	            return;
	        }
	        if (text) {
	            scriptInfo.open(text);
	        }
	        else {
	            scriptInfo.reloadFromFile();
	        }
	    }
	    onDefinition(params) {
	        const { position, textDocument } = params;
	        const filePath = utils.uriToFilePath(textDocument.uri);
	        const scriptInfo = this.projectService.getScriptInfo(filePath);
	        if (!scriptInfo) {
	            this.connection.console.log(`Script info not found for ${filePath}`);
	            return;
	        }
	        const { fileName } = scriptInfo;
	        const langSvc = this.getDefaultLanguageService(scriptInfo);
	        if (!langSvc) {
	            return;
	        }
	        const offset = utils.lspPositionToTsPosition(scriptInfo, position);
	        const definition = langSvc.getDefinitionAndBoundSpan(fileName, offset);
	        if (!definition || !definition.definitions) {
	            return;
	        }
	        const originSelectionRange = utils.tsTextSpanToLspRange(scriptInfo, definition.textSpan);
	        const results = [];
	        for (const d of definition.definitions) {
	            const scriptInfo = this.projectService.getScriptInfo(d.fileName);
	            // Some definitions, like definitions of CSS files, may not be recorded files with a
	            // `scriptInfo` but are still valid definitions because they are files that exist. In this
	            // case, check to make sure that the text span of the definition is zero so that the file
	            // doesn't have to be read; if the span is non-zero, we can't do anything with this
	            // definition.
	            if (!scriptInfo && d.textSpan.length > 0) {
	                continue;
	            }
	            const range = scriptInfo ? utils.tsTextSpanToLspRange(scriptInfo, d.textSpan) : EMPTY_RANGE;
	            const targetUri = utils.filePathToUri(d.fileName);
	            results.push({
	                originSelectionRange,
	                targetUri,
	                targetRange: range,
	                targetSelectionRange: range,
	            });
	        }
	        return results;
	    }
	    onHover(params) {
	        const { position, textDocument } = params;
	        const filePath = utils.uriToFilePath(textDocument.uri);
	        if (!filePath) {
	            return;
	        }
	        const scriptInfo = this.projectService.getScriptInfo(filePath);
	        if (!scriptInfo) {
	            return;
	        }
	        const langSvc = this.getDefaultLanguageService(scriptInfo);
	        if (!langSvc) {
	            return;
	        }
	        const offset = utils.lspPositionToTsPosition(scriptInfo, position);
	        const info = langSvc.getQuickInfoAtPosition(scriptInfo.fileName, offset);
	        if (!info) {
	            return;
	        }
	        const { kind, kindModifiers, textSpan, displayParts, documentation } = info;
	        let desc = kindModifiers ? kindModifiers + ' ' : '';
	        if (displayParts) {
	            // displayParts does not contain info about kindModifiers
	            // but displayParts does contain info about kind
	            desc += displayParts.map(dp => dp.text).join('');
	        }
	        else {
	            desc += kind;
	        }
	        const contents = [{
	                language: 'typescript',
	                value: desc,
	            }];
	        if (documentation) {
	            for (const d of documentation) {
	                contents.push(d.text);
	            }
	        }
	        return {
	            contents,
	            range: utils.tsTextSpanToLspRange(scriptInfo, textSpan),
	        };
	    }
	    onCompletion(params) {
	        const { position, textDocument } = params;
	        const filePath = utils.uriToFilePath(textDocument.uri);
	        if (!filePath) {
	            return;
	        }
	        const scriptInfo = this.projectService.getScriptInfo(filePath);
	        if (!scriptInfo) {
	            return;
	        }
	        const { fileName } = scriptInfo;
	        const langSvc = this.getDefaultLanguageService(scriptInfo);
	        if (!langSvc) {
	            return;
	        }
	        const offset = utils.lspPositionToTsPosition(scriptInfo, position);
	        const completions = langSvc.getCompletionsAtPosition(fileName, offset, {
	        // options
	        });
	        if (!completions) {
	            return;
	        }
	        return completions.entries.map((e) => completion.tsCompletionEntryToLspCompletionItem(e, position, scriptInfo));
	    }
	    /**
	     * Show an error message.
	     *
	     * @param message The message to show.
	     */
	    error(message) {
	        this.connection.console.error(message);
	    }
	    /**
	     * Show a warning message.
	     *
	     * @param message The message to show.
	     */
	    warn(message) {
	        this.connection.console.warn(message);
	    }
	    /**
	     * Show an information message.
	     *
	     * @param message The message to show.
	     */
	    info(message) {
	        this.connection.console.info(message);
	    }
	    /**
	     * Log a message.
	     *
	     * @param message The message to log.
	     */
	    log(message) {
	        this.connection.console.log(message);
	    }
	    /**
	     * Start listening on the input stream for messages to process.
	     */
	    listen() {
	        this.connection.listen();
	    }
	    /**
	     * Determine if the specified `project` is Angular, and disable the language
	     * service if not.
	     * @param project
	     */
	    checkIsAngularProject(project) {
	        const NG_CORE = '@angular/core/core.d.ts';
	        const { projectName } = project;
	        if (!project.languageServiceEnabled) {
	            const msg = `Language service is already disabled for ${projectName}. ` +
	                `This could be due to non-TS files that exceeded the size limit (${tsserverlibrary__default['default'].server.maxProgramSizeForNonTsFiles} bytes).` +
	                `Please check log file for details.`;
	            this.connection.console.info(msg); // log to remote console to inform users
	            project.log(msg); // log to file, so that it's easier to correlate with ts entries
	            return;
	        }
	        if (!isAngularProject(project, NG_CORE)) {
	            project.disableLanguageService();
	            const msg = `Disabling language service for ${projectName} because it is not an Angular project ` +
	                `('${NG_CORE}' could not be found). ` +
	                `If you believe you are seeing this message in error, please reinstall the packages in your package.json.`;
	            this.connection.console.info(msg);
	            project.log(msg);
	            if (project.getExcludedFiles().some(f => f.endsWith(NG_CORE))) {
	                const msg = `Please check your tsconfig.json to make sure 'node_modules' directory is not excluded.`;
	                this.connection.console.info(msg);
	                project.log(msg);
	            }
	            return;
	        }
	        // The language service should be enabled at this point.
	        this.connection.console.info(`Enabling language service for ${projectName}.`);
	    }
	}
	exports.Session = Session;
	/**
	 * Return true if the specified `project` contains the Angular core declaration.
	 * @param project
	 * @param ngCore path that uniquely identifies `@angular/core`.
	 */
	function isAngularProject(project, ngCore) {
	    project.markAsDirty(); // Must mark project as dirty to rebuild the program.
	    if (project.isNonTsProject()) {
	        return false;
	    }
	    for (const fileName of project.getFileNames()) {
	        if (fileName.endsWith(ngCore)) {
	            return true;
	        }
	    }
	    return false;
	}

	});

	unwrapExports(session);
	var session_1 = session.Session;

	var version_provider = createCommonjsModule(function (module, exports) {
	/**
	 * @license
	 * Copyright Google Inc. All Rights Reserved.
	 *
	 * Use of this source code is governed by an MIT-style license that can be
	 * found in the LICENSE file at https://angular.io/license
	 */
	Object.defineProperty(exports, "__esModule", { value: true });
	exports.Version = exports.resolveNgLangSvc = exports.resolveTsServer = void 0;

	const MIN_TS_VERSION = '3.9';
	const MIN_NG_VERSION = '10.0';
	function resolve(packageName, location, rootPackage) {
	    rootPackage = rootPackage || packageName;
	    try {
	        const packageJsonPath = require.resolve(`${rootPackage}/package.json`, {
	            paths: [location],
	        });
	        // Do not use require() to read JSON files since it's a potential security
	        // vulnerability.
	        const packageJson = JSON.parse(fs__default['default'].readFileSync(packageJsonPath, 'utf8'));
	        const resolvedPath = require.resolve(packageName, {
	            paths: [location],
	        });
	        return {
	            name: packageName,
	            resolvedPath,
	            version: new Version(packageJson.version),
	        };
	    }
	    catch (_a) {
	    }
	}
	/**
	 * Resolve the node module with the specified `packageName` that satisfies
	 * the specified minimum version.
	 * @param packageName name of package to be resolved
	 * @param minVersionStr minimum version
	 * @param probeLocations locations to initiate node module resolution
	 * @param rootPackage location of package.json. For example, the root package of
	 * `typescript/lib/tsserverlibrary` is `typescript`.
	 */
	function resolveWithMinVersion(packageName, minVersionStr, probeLocations, rootPackage) {
	    if (!packageName.startsWith(rootPackage)) {
	        throw new Error(`${packageName} must be in the root package`);
	    }
	    const minVersion = new Version(minVersionStr);
	    for (const location of probeLocations) {
	        const nodeModule = resolve(packageName, location, rootPackage);
	        if (nodeModule && nodeModule.version.greaterThanOrEqual(minVersion)) {
	            return nodeModule;
	        }
	    }
	    throw new Error(`Failed to resolve '${packageName}' with minimum version '${minVersion}' from ` +
	        JSON.stringify(probeLocations, null, 2));
	}
	/**
	 * Resolve `typescript/lib/tsserverlibrary` from the given locations.
	 * @param probeLocations
	 */
	function resolveTsServer(probeLocations) {
	    const tsserver = 'typescript/lib/tsserverlibrary';
	    return resolveWithMinVersion(tsserver, MIN_TS_VERSION, probeLocations, 'typescript');
	}
	exports.resolveTsServer = resolveTsServer;
	/**
	 * Resolve `@angular/language-service` from the given locations.
	 * @param probeLocations locations from which resolution is attempted
	 * @param ivy true if Ivy language service is requested
	 */
	function resolveNgLangSvc(probeLocations, ivy) {
	    const nglangsvc = '@angular/language-service';
	    const packageName = ivy ? `${nglangsvc}/bundles/ivy` : nglangsvc;
	    return resolveWithMinVersion(packageName, MIN_NG_VERSION, probeLocations, nglangsvc);
	}
	exports.resolveNgLangSvc = resolveNgLangSvc;
	/**
	 * Converts the specified string `a` to non-negative integer.
	 * Returns -1 if the result is NaN.
	 * @param a
	 */
	function parseNonNegativeInt(a) {
	    // parseInt() will try to convert as many as possible leading characters that
	    // are digits. This means a string like "123abc" will be converted to 123.
	    // For our use case, this is sufficient.
	    const i = parseInt(a, 10 /* radix */);
	    return isNaN(i) ? -1 : i;
	}
	class Version {
	    constructor(versionStr) {
	        this.versionStr = versionStr;
	        const [major, minor, patch] = Version.parseVersionStr(versionStr);
	        this.major = major;
	        this.minor = minor;
	        this.patch = patch;
	    }
	    greaterThanOrEqual(other) {
	        if (this.major < other.major) {
	            return false;
	        }
	        if (this.major > other.major) {
	            return true;
	        }
	        if (this.minor < other.minor) {
	            return false;
	        }
	        if (this.minor > other.minor) {
	            return true;
	        }
	        return this.patch >= other.patch;
	    }
	    toString() {
	        return this.versionStr;
	    }
	    /**
	     * Converts the specified `versionStr` to its number constituents. Invalid
	     * number value is represented as negative number.
	     * @param versionStr
	     */
	    static parseVersionStr(versionStr) {
	        const [major, minor, patch] = versionStr.split('.').map(parseNonNegativeInt);
	        return [
	            major === undefined ? 0 : major,
	            minor === undefined ? 0 : minor,
	            patch === undefined ? 0 : patch,
	        ];
	    }
	}
	exports.Version = Version;

	});

	unwrapExports(version_provider);
	var version_provider_1 = version_provider.Version;
	var version_provider_2 = version_provider.resolveNgLangSvc;
	var version_provider_3 = version_provider.resolveTsServer;

	var server = createCommonjsModule(function (module, exports) {
	/**
	 * @license
	 * Copyright Google Inc. All Rights Reserved.
	 *
	 * Use of this source code is governed by an MIT-style license that can be
	 * found in the LICENSE file at https://angular.io/license
	 */
	Object.defineProperty(exports, "__esModule", { value: true });





	// Parse command line arguments
	const options = cmdline_utils.parseCommandLine(process.argv);
	if (options.help) {
	    console.error(cmdline_utils.generateHelpMessage(process.argv));
	    process.exit(0);
	}
	// Create a logger that logs to file. OK to emit verbose entries.
	const logger$1 = logger.createLogger({
	    logFile: options.logFile,
	    logVerbosity: options.logVerbosity,
	});
	const ts = version_provider.resolveTsServer(options.tsProbeLocations);
	const ng = version_provider.resolveNgLangSvc(options.ngProbeLocations, options.ivy);
	// ServerHost provides native OS functionality
	const host = new server_host.ServerHost();
	// Establish a new server session that encapsulates lsp connection.
	const session$1 = new session.Session({
	    host,
	    logger: logger$1,
	    ngPlugin: ng.name,
	    ngProbeLocation: ng.resolvedPath,
	});
	// Log initialization info
	session$1.info(`Angular language server process ID: ${process.pid}`);
	session$1.info(`Using ${ts.name} v${ts.version} from ${ts.resolvedPath}`);
	session$1.info(`Using ${ng.name} v${ng.version} from ${ng.resolvedPath}`);
	session$1.info(`Log file: ${logger$1.getLogFileName()}`);
	if (process.env.NG_DEBUG === 'true') {
	    session$1.info('Angular Language Service is running under DEBUG mode');
	}
	if (process.env.TSC_NONPOLLING_WATCHER !== 'true') {
	    session$1.warn(`Using less efficient polling watcher. Set TSC_NONPOLLING_WATCHER to true.`);
	}
	session$1.listen();

	});

	var server$1 = unwrapExports(server);

	return server$1;

});
