/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
(function (factory) {
    if (typeof module === "object" && typeof module.exports === "object") {
        var v = factory(require, exports);
        if (v !== undefined) module.exports = v;
    }
    else if (typeof define === "function" && define.amd) {
        define("@angular/language-service/src/typescript_host", ["require", "exports", "tslib", "@angular/compiler", "@angular/core", "typescript/lib/tsserverlibrary", "@angular/language-service/src/language_service", "@angular/language-service/src/reflector_host", "@angular/language-service/src/template", "@angular/language-service/src/ts_utils"], factory);
    }
})(function (require, exports) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    exports.TypeScriptServiceHost = exports.DummyResourceLoader = exports.DummyHtmlParser = exports.createLanguageServiceFromTypescript = void 0;
    var tslib_1 = require("tslib");
    var compiler_1 = require("@angular/compiler");
    var core_1 = require("@angular/core");
    var tss = require("typescript/lib/tsserverlibrary");
    var language_service_1 = require("@angular/language-service/src/language_service");
    var reflector_host_1 = require("@angular/language-service/src/reflector_host");
    var template_1 = require("@angular/language-service/src/template");
    var ts_utils_1 = require("@angular/language-service/src/ts_utils");
    /**
     * Create a `LanguageServiceHost`
     */
    function createLanguageServiceFromTypescript(host, service) {
        var ngHost = new TypeScriptServiceHost(host, service);
        var ngServer = language_service_1.createLanguageService(ngHost);
        return ngServer;
    }
    exports.createLanguageServiceFromTypescript = createLanguageServiceFromTypescript;
    /**
     * The language service never needs the normalized versions of the metadata. To avoid parsing
     * the content and resolving references, return an empty file. This also allows normalizing
     * template that are syntatically incorrect which is required to provide completions in
     * syntactically incorrect templates.
     */
    var DummyHtmlParser = /** @class */ (function (_super) {
        tslib_1.__extends(DummyHtmlParser, _super);
        function DummyHtmlParser() {
            return _super !== null && _super.apply(this, arguments) || this;
        }
        DummyHtmlParser.prototype.parse = function () {
            return new compiler_1.ParseTreeResult([], []);
        };
        return DummyHtmlParser;
    }(compiler_1.HtmlParser));
    exports.DummyHtmlParser = DummyHtmlParser;
    /**
     * Avoid loading resources in the language servcie by using a dummy loader.
     */
    var DummyResourceLoader = /** @class */ (function (_super) {
        tslib_1.__extends(DummyResourceLoader, _super);
        function DummyResourceLoader() {
            return _super !== null && _super.apply(this, arguments) || this;
        }
        DummyResourceLoader.prototype.get = function (_url) {
            return Promise.resolve('');
        };
        return DummyResourceLoader;
    }(compiler_1.ResourceLoader));
    exports.DummyResourceLoader = DummyResourceLoader;
    /**
     * An implementation of a `LanguageServiceHost` for a TypeScript project.
     *
     * The `TypeScriptServiceHost` implements the Angular `LanguageServiceHost` using
     * the TypeScript language services.
     *
     * @publicApi
     */
    var TypeScriptServiceHost = /** @class */ (function () {
        function TypeScriptServiceHost(tsLsHost, tsLS) {
            var _this = this;
            this.tsLsHost = tsLsHost;
            this.tsLS = tsLS;
            this.staticSymbolCache = new compiler_1.StaticSymbolCache();
            this.fileToComponent = new Map();
            this.collectedErrors = new Map();
            this.fileVersions = new Map();
            this.lastProgram = undefined;
            this.analyzedModules = {
                files: [],
                ngModuleByPipeOrDirective: new Map(),
                ngModules: [],
            };
            this.summaryResolver = new compiler_1.AotSummaryResolver({
                loadSummary: function (_filePath) {
                    return null;
                },
                isSourceFile: function (_sourceFilePath) {
                    return true;
                },
                toSummaryFileName: function (sourceFilePath) {
                    return sourceFilePath;
                },
                fromSummaryFileName: function (filePath) {
                    return filePath;
                },
            }, this.staticSymbolCache);
            this.reflectorHost = new reflector_host_1.ReflectorHost(function () { return _this.program; }, tsLsHost);
            this.staticSymbolResolver = new compiler_1.StaticSymbolResolver(this.reflectorHost, this.staticSymbolCache, this.summaryResolver, function (e, filePath) { return _this.collectError(e, filePath); });
        }
        Object.defineProperty(TypeScriptServiceHost.prototype, "resolver", {
            /**
             * Return the singleton instance of the MetadataResolver.
             */
            get: function () {
                var _this = this;
                if (this._resolver) {
                    return this._resolver;
                }
                // StaticReflector keeps its own private caches that are not clearable.
                // We have no choice but to create a new instance to invalidate the caches.
                // TODO: Revisit this when language service gets rewritten for Ivy.
                var staticReflector = new compiler_1.StaticReflector(this.summaryResolver, this.staticSymbolResolver, [], // knownMetadataClasses
                [], // knownMetadataFunctions
                function (e, filePath) { return _this.collectError(e, filePath); });
                // Because static reflector above is changed, we need to create a new
                // resolver.
                var moduleResolver = new compiler_1.NgModuleResolver(staticReflector);
                var directiveResolver = new compiler_1.DirectiveResolver(staticReflector);
                var pipeResolver = new compiler_1.PipeResolver(staticReflector);
                var elementSchemaRegistry = new compiler_1.DomElementSchemaRegistry();
                var resourceLoader = new DummyResourceLoader();
                var urlResolver = compiler_1.createOfflineCompileUrlResolver();
                var htmlParser = new DummyHtmlParser();
                // This tracks the CompileConfig in codegen.ts. Currently these options
                // are hard-coded.
                var config = new compiler_1.CompilerConfig({
                    defaultEncapsulation: core_1.ViewEncapsulation.Emulated,
                    useJit: false,
                });
                var directiveNormalizer = new compiler_1.DirectiveNormalizer(resourceLoader, urlResolver, htmlParser, config);
                this._resolver = new compiler_1.CompileMetadataResolver(config, htmlParser, moduleResolver, directiveResolver, pipeResolver, new compiler_1.JitSummaryResolver(), elementSchemaRegistry, directiveNormalizer, new core_1.ɵConsole(), this.staticSymbolCache, staticReflector, function (error, type) { return _this.collectError(error, type && type.filePath); });
                return this._resolver;
            },
            enumerable: false,
            configurable: true
        });
        Object.defineProperty(TypeScriptServiceHost.prototype, "reflector", {
            /**
             * Return the singleton instance of the StaticReflector hosted in the
             * MetadataResolver.
             */
            get: function () {
                return this.resolver.getReflector();
            },
            enumerable: false,
            configurable: true
        });
        /**
         * Return all known external templates.
         */
        TypeScriptServiceHost.prototype.getExternalTemplates = function () {
            return tslib_1.__spread(this.fileToComponent.keys());
        };
        /**
         * Checks whether the program has changed and returns all analyzed modules.
         * If program has changed, invalidate all caches and update fileToComponent
         * and templateReferences.
         * In addition to returning information about NgModules, this method plays the
         * same role as 'synchronizeHostData' in tsserver.
         */
        TypeScriptServiceHost.prototype.getAnalyzedModules = function () {
            var e_1, _a, e_2, _b;
            if (this.upToDate()) {
                return this.analyzedModules;
            }
            // Invalidate caches
            this.fileToComponent.clear();
            this.collectedErrors.clear();
            this.resolver.clearCache();
            var analyzeHost = {
                isSourceFile: function (_filePath) {
                    return true;
                }
            };
            var programFiles = this.program.getSourceFiles().map(function (sf) { return sf.fileName; });
            try {
                this.analyzedModules =
                    compiler_1.analyzeNgModules(programFiles, analyzeHost, this.staticSymbolResolver, this.resolver);
            }
            catch (e) {
                // Analyzing modules may throw; in that case, reuse the old modules.
                this.error("Analyzing NgModules failed. " + e);
                return this.analyzedModules;
            }
            // update template references and fileToComponent
            var urlResolver = compiler_1.createOfflineCompileUrlResolver();
            try {
                for (var _c = tslib_1.__values(this.analyzedModules.ngModules), _d = _c.next(); !_d.done; _d = _c.next()) {
                    var ngModule = _d.value;
                    try {
                        for (var _e = (e_2 = void 0, tslib_1.__values(ngModule.declaredDirectives)), _f = _e.next(); !_f.done; _f = _e.next()) {
                            var directive = _f.value;
                            var metadata = this.resolver.getNonNormalizedDirectiveMetadata(directive.reference).metadata;
                            if (metadata.isComponent && metadata.template && metadata.template.templateUrl) {
                                var templateName = urlResolver.resolve(this.reflector.componentModuleUrl(directive.reference), metadata.template.templateUrl);
                                this.fileToComponent.set(templateName, directive.reference);
                            }
                        }
                    }
                    catch (e_2_1) { e_2 = { error: e_2_1 }; }
                    finally {
                        try {
                            if (_f && !_f.done && (_b = _e.return)) _b.call(_e);
                        }
                        finally { if (e_2) throw e_2.error; }
                    }
                }
            }
            catch (e_1_1) { e_1 = { error: e_1_1 }; }
            finally {
                try {
                    if (_d && !_d.done && (_a = _c.return)) _a.call(_c);
                }
                finally { if (e_1) throw e_1.error; }
            }
            return this.analyzedModules;
        };
        /**
         * Checks whether the program has changed, and invalidate static symbols in
         * the source files that have changed.
         * Returns true if modules are up-to-date, false otherwise.
         * This should only be called by getAnalyzedModules().
         */
        TypeScriptServiceHost.prototype.upToDate = function () {
            var e_3, _a, e_4, _b, e_5, _c;
            var _d = this, lastProgram = _d.lastProgram, program = _d.program;
            if (lastProgram === program) {
                return true;
            }
            this.lastProgram = program;
            // Even though the program has changed, it could be the case that none of
            // the source files have changed. If all source files remain the same, then
            // program is still up-to-date, and we should not invalidate caches.
            var filesAdded = 0;
            var filesChangedOrRemoved = [];
            // Check if any source files have been added / changed since last computation.
            var seen = new Set();
            var ANGULAR_CORE = '@angular/core';
            var corePath = this.reflectorHost.moduleNameToFileName(ANGULAR_CORE);
            try {
                for (var _e = tslib_1.__values(program.getSourceFiles()), _f = _e.next(); !_f.done; _f = _e.next()) {
                    var fileName = _f.value.fileName;
                    // If `@angular/core` is edited, the language service would have to be
                    // restarted, so ignore changes to `@angular/core`.
                    // When the StaticReflector is initialized at startup, it loads core
                    // symbols from @angular/core by calling initializeConversionMap(). This
                    // is only done once. If the file is invalidated, some of the core symbols
                    // will be lost permanently.
                    if (fileName === corePath) {
                        continue;
                    }
                    seen.add(fileName);
                    var version = this.tsLsHost.getScriptVersion(fileName);
                    var lastVersion = this.fileVersions.get(fileName);
                    if (lastVersion === undefined) {
                        filesAdded++;
                        this.fileVersions.set(fileName, version);
                    }
                    else if (version !== lastVersion) {
                        filesChangedOrRemoved.push(fileName); // changed
                        this.fileVersions.set(fileName, version);
                    }
                }
            }
            catch (e_3_1) { e_3 = { error: e_3_1 }; }
            finally {
                try {
                    if (_f && !_f.done && (_a = _e.return)) _a.call(_e);
                }
                finally { if (e_3) throw e_3.error; }
            }
            try {
                // Check if any source files have been removed since last computation.
                for (var _g = tslib_1.__values(this.fileVersions), _h = _g.next(); !_h.done; _h = _g.next()) {
                    var _j = tslib_1.__read(_h.value, 1), fileName = _j[0];
                    if (!seen.has(fileName)) {
                        filesChangedOrRemoved.push(fileName); // removed
                        // Because Maps are iterated in insertion order, it is safe to delete
                        // entries from the same map while iterating.
                        // See https://stackoverflow.com/questions/35940216 and
                        // https://www.ecma-international.org/ecma-262/10.0/index.html#sec-map.prototype.foreach
                        this.fileVersions.delete(fileName);
                    }
                }
            }
            catch (e_4_1) { e_4 = { error: e_4_1 }; }
            finally {
                try {
                    if (_h && !_h.done && (_b = _g.return)) _b.call(_g);
                }
                finally { if (e_4) throw e_4.error; }
            }
            try {
                for (var filesChangedOrRemoved_1 = tslib_1.__values(filesChangedOrRemoved), filesChangedOrRemoved_1_1 = filesChangedOrRemoved_1.next(); !filesChangedOrRemoved_1_1.done; filesChangedOrRemoved_1_1 = filesChangedOrRemoved_1.next()) {
                    var fileName = filesChangedOrRemoved_1_1.value;
                    var symbols = this.staticSymbolResolver.invalidateFile(fileName);
                    this.reflector.invalidateSymbols(symbols);
                }
            }
            catch (e_5_1) { e_5 = { error: e_5_1 }; }
            finally {
                try {
                    if (filesChangedOrRemoved_1_1 && !filesChangedOrRemoved_1_1.done && (_c = filesChangedOrRemoved_1.return)) _c.call(filesChangedOrRemoved_1);
                }
                finally { if (e_5) throw e_5.error; }
            }
            // Program is up-to-date iff no files are added, changed, or removed.
            return filesAdded === 0 && filesChangedOrRemoved.length === 0;
        };
        /**
         * Find all templates in the specified `file`.
         * @param fileName TS or HTML file
         */
        TypeScriptServiceHost.prototype.getTemplates = function (fileName) {
            var _this = this;
            var results = [];
            if (fileName.endsWith('.ts')) {
                // Find every template string in the file
                var visit_1 = function (child) {
                    var template = _this.getInternalTemplate(child);
                    if (template) {
                        results.push(template);
                    }
                    else {
                        tss.forEachChild(child, visit_1);
                    }
                };
                var sourceFile = this.getSourceFile(fileName);
                if (sourceFile) {
                    tss.forEachChild(sourceFile, visit_1);
                }
            }
            else {
                var template = this.getExternalTemplate(fileName);
                if (template) {
                    results.push(template);
                }
            }
            return results;
        };
        /**
         * Return metadata about all class declarations in the file that are Angular
         * directives. Potential matches are `@NgModule`, `@Component`, `@Directive`,
         * `@Pipes`, etc. class declarations.
         *
         * @param fileName TS file
         */
        TypeScriptServiceHost.prototype.getDeclarations = function (fileName) {
            var _this = this;
            if (!fileName.endsWith('.ts')) {
                return [];
            }
            var sourceFile = this.getSourceFile(fileName);
            if (!sourceFile) {
                return [];
            }
            var results = [];
            var visit = function (child) {
                var candidate = ts_utils_1.getDirectiveClassLike(child);
                if (candidate) {
                    var classId = candidate.classId;
                    var declarationSpan = spanOf(classId);
                    var className = classId.getText();
                    var classSymbol = _this.reflector.getStaticSymbol(sourceFile.fileName, className);
                    // Ask the resolver to check if candidate is actually Angular directive
                    if (!_this.resolver.isDirective(classSymbol)) {
                        return;
                    }
                    var data = _this.resolver.getNonNormalizedDirectiveMetadata(classSymbol);
                    if (!data) {
                        return;
                    }
                    results.push({
                        type: classSymbol,
                        declarationSpan: declarationSpan,
                        metadata: data.metadata,
                        errors: _this.getCollectedErrors(declarationSpan, sourceFile),
                    });
                }
                else {
                    child.forEachChild(visit);
                }
            };
            tss.forEachChild(sourceFile, visit);
            return results;
        };
        TypeScriptServiceHost.prototype.getSourceFile = function (fileName) {
            if (!fileName.endsWith('.ts')) {
                throw new Error("Non-TS source file requested: " + fileName);
            }
            return this.program.getSourceFile(fileName);
        };
        Object.defineProperty(TypeScriptServiceHost.prototype, "program", {
            get: function () {
                var program = this.tsLS.getProgram();
                if (!program) {
                    // Program is very very unlikely to be undefined.
                    throw new Error('No program in language service!');
                }
                return program;
            },
            enumerable: false,
            configurable: true
        });
        /**
         * Return the TemplateSource if `node` is a template node.
         *
         * For example,
         *
         * @Component({
         *   template: '<div></div>' <-- template node
         * })
         * class AppComponent {}
         *           ^---- class declaration node
         *
         * @param node Potential template node
         */
        TypeScriptServiceHost.prototype.getInternalTemplate = function (node) {
            if (!tss.isStringLiteralLike(node)) {
                return;
            }
            var tmplAsgn = ts_utils_1.getPropertyAssignmentFromValue(node, 'template');
            if (!tmplAsgn) {
                return;
            }
            var classDecl = ts_utils_1.getClassDeclFromDecoratorProp(tmplAsgn);
            if (!classDecl || !classDecl.name) { // Does not handle anonymous class
                return;
            }
            var fileName = node.getSourceFile().fileName;
            var classSymbol = this.reflector.getStaticSymbol(fileName, classDecl.name.text);
            return new template_1.InlineTemplate(node, classDecl, classSymbol, this);
        };
        /**
         * Return the external template for `fileName`.
         * @param fileName HTML file
         */
        TypeScriptServiceHost.prototype.getExternalTemplate = function (fileName) {
            // First get the text for the template
            var snapshot = this.tsLsHost.getScriptSnapshot(fileName);
            if (!snapshot) {
                return;
            }
            var source = snapshot.getText(0, snapshot.getLength());
            // Next find the component class symbol
            var classSymbol = this.fileToComponent.get(fileName);
            if (!classSymbol) {
                return;
            }
            // Then use the class symbol to find the actual ts.ClassDeclaration node
            var sourceFile = this.getSourceFile(classSymbol.filePath);
            if (!sourceFile) {
                return;
            }
            // TODO: This only considers top-level class declarations in a source file.
            // This would not find a class declaration in a namespace, for example.
            var classDecl = sourceFile.forEachChild(function (child) {
                if (tss.isClassDeclaration(child) && child.name && child.name.text === classSymbol.name) {
                    return child;
                }
            });
            if (!classDecl) {
                return;
            }
            return new template_1.ExternalTemplate(source, fileName, classDecl, classSymbol, this);
        };
        TypeScriptServiceHost.prototype.collectError = function (error, filePath) {
            if (filePath) {
                var errors = this.collectedErrors.get(filePath);
                if (!errors) {
                    errors = [];
                    this.collectedErrors.set(filePath, errors);
                }
                errors.push(error);
            }
        };
        TypeScriptServiceHost.prototype.getCollectedErrors = function (defaultSpan, sourceFile) {
            var errors = this.collectedErrors.get(sourceFile.fileName);
            if (!errors) {
                return [];
            }
            // TODO: Add better typings for the errors
            return errors.map(function (e) {
                var line = e.line || (e.position && e.position.line);
                var column = e.column || (e.position && e.position.column);
                var span = spanAt(sourceFile, line, column) || defaultSpan;
                if (compiler_1.isFormattedError(e)) {
                    return errorToDiagnosticWithChain(e, span);
                }
                return { message: e.message, span: span };
            });
        };
        /**
         * Return the parsed template for the template at the specified `position`.
         * @param fileName TS or HTML file
         * @param position Position of the template in the TS file, otherwise ignored.
         */
        TypeScriptServiceHost.prototype.getTemplateAstAtPosition = function (fileName, position) {
            var template;
            if (fileName.endsWith('.ts')) {
                var sourceFile = this.getSourceFile(fileName);
                if (!sourceFile) {
                    return;
                }
                // Find the node that most closely matches the position
                var node = ts_utils_1.findTightestNode(sourceFile, position);
                if (!node) {
                    return;
                }
                template = this.getInternalTemplate(node);
            }
            else {
                template = this.getExternalTemplate(fileName);
            }
            if (!template) {
                return;
            }
            return this.getTemplateAst(template);
        };
        /**
         * Find the NgModule which the directive associated with the `classSymbol`
         * belongs to, then return its schema and transitive directives and pipes.
         * @param classSymbol Angular Symbol that defines a directive
         */
        TypeScriptServiceHost.prototype.getModuleMetadataForDirective = function (classSymbol) {
            var e_6, _a, e_7, _b, _c;
            var result = {
                directives: [],
                pipes: [],
                schemas: [],
            };
            // First find which NgModule the directive belongs to.
            var ngModule = this.analyzedModules.ngModuleByPipeOrDirective.get(classSymbol) ||
                findSuitableDefaultModule(this.analyzedModules);
            if (!ngModule) {
                return result;
            }
            // Then gather all transitive directives and pipes.
            var _d = ngModule.transitiveModule, directives = _d.directives, pipes = _d.pipes;
            try {
                for (var directives_1 = tslib_1.__values(directives), directives_1_1 = directives_1.next(); !directives_1_1.done; directives_1_1 = directives_1.next()) {
                    var directive = directives_1_1.value;
                    var data = this.resolver.getNonNormalizedDirectiveMetadata(directive.reference);
                    if (data) {
                        result.directives.push(data.metadata.toSummary());
                    }
                }
            }
            catch (e_6_1) { e_6 = { error: e_6_1 }; }
            finally {
                try {
                    if (directives_1_1 && !directives_1_1.done && (_a = directives_1.return)) _a.call(directives_1);
                }
                finally { if (e_6) throw e_6.error; }
            }
            try {
                for (var pipes_1 = tslib_1.__values(pipes), pipes_1_1 = pipes_1.next(); !pipes_1_1.done; pipes_1_1 = pipes_1.next()) {
                    var pipe = pipes_1_1.value;
                    var metadata = this.resolver.getOrLoadPipeMetadata(pipe.reference);
                    result.pipes.push(metadata.toSummary());
                }
            }
            catch (e_7_1) { e_7 = { error: e_7_1 }; }
            finally {
                try {
                    if (pipes_1_1 && !pipes_1_1.done && (_b = pipes_1.return)) _b.call(pipes_1);
                }
                finally { if (e_7) throw e_7.error; }
            }
            (_c = result.schemas).push.apply(_c, tslib_1.__spread(ngModule.schemas));
            return result;
        };
        /**
         * Parse the `template` and return its AST, if any.
         * @param template template to be parsed
         */
        TypeScriptServiceHost.prototype.getTemplateAst = function (template) {
            var classSymbol = template.type, fileName = template.fileName;
            var data = this.resolver.getNonNormalizedDirectiveMetadata(classSymbol);
            if (!data) {
                return;
            }
            var htmlParser = new compiler_1.HtmlParser();
            var expressionParser = new compiler_1.Parser(new compiler_1.Lexer());
            var parser = new compiler_1.TemplateParser(new compiler_1.CompilerConfig(), this.reflector, expressionParser, new compiler_1.DomElementSchemaRegistry(), htmlParser, null, // console
            [] // tranforms
            );
            var htmlResult = htmlParser.parse(template.source, fileName, {
                tokenizeExpansionForms: true,
                preserveLineEndings: true,
            });
            var _a = this.getModuleMetadataForDirective(classSymbol), directives = _a.directives, pipes = _a.pipes, schemas = _a.schemas;
            var parseResult = parser.tryParseHtml(htmlResult, data.metadata, directives, pipes, schemas);
            if (!parseResult.templateAst) {
                return;
            }
            return {
                htmlAst: htmlResult.rootNodes,
                templateAst: parseResult.templateAst,
                directive: data.metadata,
                directives: directives,
                pipes: pipes,
                parseErrors: parseResult.errors,
                expressionParser: expressionParser,
                template: template,
            };
        };
        /**
         * Log the specified `msg` to file at INFO level. If logging is not enabled
         * this method is a no-op.
         * @param msg Log message
         */
        TypeScriptServiceHost.prototype.log = function (msg) {
            if (this.tsLsHost.log) {
                this.tsLsHost.log(msg);
            }
        };
        /**
         * Log the specified `msg` to file at ERROR level. If logging is not enabled
         * this method is a no-op.
         * @param msg error message
         */
        TypeScriptServiceHost.prototype.error = function (msg) {
            if (this.tsLsHost.error) {
                this.tsLsHost.error(msg);
            }
        };
        /**
         * Log debugging info to file at INFO level, only if verbose setting is turned
         * on. Otherwise, this method is a no-op.
         * @param msg debugging message
         */
        TypeScriptServiceHost.prototype.debug = function (msg) {
            var project = this.tsLsHost;
            if (!project.projectService) {
                // tsLsHost is not a Project
                return;
            }
            var logger = project.projectService.logger;
            if (logger.hasLevel(tss.server.LogLevel.verbose)) {
                logger.info(msg);
            }
        };
        return TypeScriptServiceHost;
    }());
    exports.TypeScriptServiceHost = TypeScriptServiceHost;
    function findSuitableDefaultModule(modules) {
        var e_8, _a;
        var result = undefined;
        var resultSize = 0;
        try {
            for (var _b = tslib_1.__values(modules.ngModules), _c = _b.next(); !_c.done; _c = _b.next()) {
                var module_1 = _c.value;
                var moduleSize = module_1.transitiveModule.directives.length;
                if (moduleSize > resultSize) {
                    result = module_1;
                    resultSize = moduleSize;
                }
            }
        }
        catch (e_8_1) { e_8 = { error: e_8_1 }; }
        finally {
            try {
                if (_c && !_c.done && (_a = _b.return)) _a.call(_b);
            }
            finally { if (e_8) throw e_8.error; }
        }
        return result;
    }
    function spanOf(node) {
        return { start: node.getStart(), end: node.getEnd() };
    }
    function spanAt(sourceFile, line, column) {
        if (line != null && column != null) {
            var position_1 = tss.getPositionOfLineAndCharacter(sourceFile, line, column);
            var findChild = function findChild(node) {
                if (node.kind > tss.SyntaxKind.LastToken && node.pos <= position_1 && node.end > position_1) {
                    var betterNode = tss.forEachChild(node, findChild);
                    return betterNode || node;
                }
            };
            var node = tss.forEachChild(sourceFile, findChild);
            if (node) {
                return { start: node.getStart(), end: node.getEnd() };
            }
        }
    }
    function convertChain(chain) {
        return { message: chain.message, next: chain.next ? chain.next.map(convertChain) : undefined };
    }
    function errorToDiagnosticWithChain(error, span) {
        return { message: error.chain ? convertChain(error.chain) : error.message, span: span };
    }
});
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoidHlwZXNjcmlwdF9ob3N0LmpzIiwic291cmNlUm9vdCI6IiIsInNvdXJjZXMiOlsiLi4vLi4vLi4vLi4vLi4vLi4vcGFja2FnZXMvbGFuZ3VhZ2Utc2VydmljZS9zcmMvdHlwZXNjcmlwdF9ob3N0LnRzIl0sIm5hbWVzIjpbXSwibWFwcGluZ3MiOiJBQUFBOzs7Ozs7R0FNRzs7Ozs7Ozs7Ozs7Ozs7SUFFSCw4Q0FBNmlCO0lBQzdpQixzQ0FBcUY7SUFDckYsb0RBQXNEO0lBRXRELG1GQUF5RDtJQUN6RCwrRUFBK0M7SUFDL0MsbUVBQTREO0lBQzVELG1FQUFrSTtJQUdsSTs7T0FFRztJQUNILFNBQWdCLG1DQUFtQyxDQUMvQyxJQUE2QixFQUFFLE9BQTRCO1FBQzdELElBQU0sTUFBTSxHQUFHLElBQUkscUJBQXFCLENBQUMsSUFBSSxFQUFFLE9BQU8sQ0FBQyxDQUFDO1FBQ3hELElBQU0sUUFBUSxHQUFHLHdDQUFxQixDQUFDLE1BQU0sQ0FBQyxDQUFDO1FBQy9DLE9BQU8sUUFBUSxDQUFDO0lBQ2xCLENBQUM7SUFMRCxrRkFLQztJQUVEOzs7OztPQUtHO0lBQ0g7UUFBcUMsMkNBQVU7UUFBL0M7O1FBSUEsQ0FBQztRQUhDLCtCQUFLLEdBQUw7WUFDRSxPQUFPLElBQUksMEJBQWUsQ0FBQyxFQUFFLEVBQUUsRUFBRSxDQUFDLENBQUM7UUFDckMsQ0FBQztRQUNILHNCQUFDO0lBQUQsQ0FBQyxBQUpELENBQXFDLHFCQUFVLEdBSTlDO0lBSlksMENBQWU7SUFNNUI7O09BRUc7SUFDSDtRQUF5QywrQ0FBYztRQUF2RDs7UUFJQSxDQUFDO1FBSEMsaUNBQUcsR0FBSCxVQUFJLElBQVk7WUFDZCxPQUFPLE9BQU8sQ0FBQyxPQUFPLENBQUMsRUFBRSxDQUFDLENBQUM7UUFDN0IsQ0FBQztRQUNILDBCQUFDO0lBQUQsQ0FBQyxBQUpELENBQXlDLHlCQUFjLEdBSXREO0lBSlksa0RBQW1CO0lBTWhDOzs7Ozs7O09BT0c7SUFDSDtRQWlCRSwrQkFBcUIsUUFBaUMsRUFBVyxJ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aHQgR29vZ2xlIExMQyBBbGwgUmlnaHRzIFJlc2VydmVkLlxuICpcbiAqIFVzZSBvZiB0aGlzIHNvdXJjZSBjb2RlIGlzIGdvdmVybmVkIGJ5IGFuIE1JVC1zdHlsZSBsaWNlbnNlIHRoYXQgY2FuIGJlXG4gKiBmb3VuZCBpbiB0aGUgTElDRU5TRSBmaWxlIGF0IGh0dHBzOi8vYW5ndWxhci5pby9saWNlbnNlXG4gKi9cblxuaW1wb3J0IHthbmFseXplTmdNb2R1bGVzLCBBb3RTdW1tYXJ5UmVzb2x2ZXIsIENvbXBpbGVEaXJlY3RpdmVTdW1tYXJ5LCBDb21waWxlTWV0YWRhdGFSZXNvbHZlciwgQ29tcGlsZU5nTW9kdWxlTWV0YWRhdGEsIENvbXBpbGVQaXBlU3VtbWFyeSwgQ29tcGlsZXJDb25maWcsIGNyZWF0ZU9mZmxpbmVDb21waWxlVXJsUmVzb2x2ZXIsIERpcmVjdGl2ZU5vcm1hbGl6ZXIsIERpcmVjdGl2ZVJlc29sdmVyLCBEb21FbGVtZW50U2NoZW1hUmVnaXN0cnksIEZvcm1hdHRlZEVycm9yLCBGb3JtYXR0ZWRNZXNzYWdlQ2hhaW4sIEh0bWxQYXJzZXIsIGlzRm9ybWF0dGVkRXJyb3IsIEppdFN1bW1hcnlSZXNvbHZlciwgTGV4ZXIsIE5nQW5hbHl6ZWRNb2R1bGVzLCBOZ01vZHVsZVJlc29sdmVyLCBQYXJzZXIsIFBhcnNlVHJlZVJlc3VsdCwgUGlwZVJlc29sdmVyLCBSZXNvdXJjZUxvYWRlciwgU3RhdGljUmVmbGVjdG9yLCBTdGF0aWNTeW1ib2wsIFN0YXRpY1N5bWJvbENhY2hlLCBTdGF0aWNTeW1ib2xSZXNvbHZlciwgVGVtcGxhdGVQYXJzZXJ9IGZyb20gJ0Bhbmd1bGFyL2NvbXBpbGVyJztcbmltcG9ydCB7U2NoZW1hTWV0YWRhdGEsIFZpZXdFbmNhcHN1bGF0aW9uLCDJtUNvbnNvbGUgYXMgQ29uc29sZX0gZnJvbSAnQGFuZ3VsYXIvY29yZSc7XG5pbXBvcnQgKiBhcyB0c3MgZnJvbSAndHlwZXNjcmlwdC9saWIvdHNzZXJ2ZXJsaWJyYXJ5JztcblxuaW1wb3J0IHtjcmVhdGVMYW5ndWFnZVNlcnZpY2V9IGZyb20gJy4vbGFuZ3VhZ2Vfc2VydmljZSc7XG5pbXBvcnQge1JlZmxlY3Rvckhvc3R9IGZyb20gJy4vcmVmbGVjdG9yX2hvc3QnO1xuaW1wb3J0IHtFeHRlcm5hbFRlbXBsYXRlLCBJbmxpbmVUZW1wbGF0ZX0gZnJvbSAnLi90ZW1wbGF0ZSc7XG5pbXBvcnQge2ZpbmRUaWdodGVzdE5vZGUsIGdldENsYXNzRGVjbEZyb21EZWNvcmF0b3JQcm9wLCBnZXREaXJlY3RpdmVDbGFzc0xpa2UsIGdldFByb3BlcnR5QXNzaWdubWVudEZyb21WYWx1ZX0gZnJvbSAnLi90c191dGlscyc7XG5pbXBvcnQge0FzdFJlc3VsdCwgRGVjbGFyYXRpb24sIERlY2xhcmF0aW9uRXJyb3IsIERpYWdub3N0aWNNZXNzYWdlQ2hhaW4sIExhbmd1YWdlU2VydmljZSwgTGFuZ3VhZ2VTZXJ2aWNlSG9zdCwgU3BhbiwgVGVtcGxhdGVTb3VyY2V9IGZyb20gJy4vdHlwZXMnO1xuXG4vKipcbiAqIENyZWF0ZSBhIGBMYW5ndWFnZVNlcnZpY2VIb3N0YFxuICovXG5leHBvcnQgZnVuY3Rpb24gY3JlYXRlTGFuZ3VhZ2VTZXJ2aWNlRnJvbVR5cGVzY3JpcHQoXG4gICAgaG9zdDogdHNzLkxhbmd1YWdlU2VydmljZUhvc3QsIHNlcnZpY2U6IHRzcy5MYW5ndWFnZVNlcnZpY2UpOiBMYW5ndWFnZVNlcnZpY2Uge1xuICBjb25zdCBuZ0hvc3QgPSBuZXcgVHlwZVNjcmlwdFNlcnZpY2VIb3N0KGhvc3QsIHNlcnZpY2UpO1xuICBjb25zdCBuZ1NlcnZlciA9IGNyZWF0ZUxhbmd1YWdlU2VydmljZShuZ0hvc3QpO1xuICByZXR1cm4gbmdTZXJ2ZXI7XG59XG5cbi8qKlxuICogVGhlIGxhbmd1YWdlIHNlcnZpY2UgbmV2ZXIgbmVlZHMgdGhlIG5vcm1hbGl6ZWQgdmVyc2lvbnMgb2YgdGhlIG1ldGFkYXRhLiBUbyBhdm9pZCBwYXJzaW5nXG4gKiB0aGUgY29udGVudCBhbmQgcmVzb2x2aW5nIHJlZmVyZW5jZXMsIHJldHVybiBhbiBlbXB0eSBmaWxlLiBUaGlzIGFsc28gYWxsb3dzIG5vcm1hbGl6aW5nXG4gKiB0ZW1wbGF0ZSB0aGF0IGFyZSBzeW50YXRpY2FsbHkgaW5jb3JyZWN0IHdoaWNoIGlzIHJlcXVpcmVkIHRvIHByb3ZpZGUgY29tcGxldGlvbnMgaW5cbiAqIHN5bnRhY3RpY2FsbHkgaW5jb3JyZWN0IHRlbXBsYXRlcy5cbiAqL1xuZXhwb3J0IGNsYXNzIER1bW15SHRtbFBhcnNlciBleHRlbmRzIEh0bWxQYXJzZXIge1xuICBwYXJzZSgpOiBQYXJzZVRyZWVSZXN1bHQge1xuICAgIHJldHVybiBuZXcgUGFyc2VUcmVlUmVzdWx0KFtdLCBbXSk7XG4gIH1cbn1cblxuLyoqXG4gKiBBdm9pZCBsb2FkaW5nIHJlc291cmNlcyBpbiB0aGUgbGFuZ3VhZ2Ugc2VydmNpZSBieSB1c2luZyBhIGR1bW15IGxvYWRlci5cbiAqL1xuZXhwb3J0IGNsYXNzIER1bW15UmVzb3VyY2VMb2FkZXIgZXh0ZW5kcyBSZXNvdXJjZUxvYWRlciB7XG4gIGdldChfdXJsOiBzdHJpbmcpOiBQcm9taXNlPHN0cmluZz4ge1xuICAgIHJldHVybiBQcm9taXNlLnJlc29sdmUoJycpO1xuICB9XG59XG5cbi8qKlxuICogQW4gaW1wbGVtZW50YXRpb24gb2YgYSBgTGFuZ3VhZ2VTZXJ2aWNlSG9zdGAgZm9yIGEgVHlwZVNjcmlwdCBwcm9qZWN0LlxuICpcbiAqIFRoZSBgVHlwZVNjcmlwdFNlcnZpY2VIb3N0YCBpbXBsZW1lbnRzIHRoZSBBbmd1bGFyIGBMYW5ndWFnZVNlcnZpY2VIb3N0YCB1c2luZ1xuICogdGhlIFR5cGVTY3JpcHQgbGFuZ3VhZ2Ugc2VydmljZXMuXG4gKlxuICogQHB1YmxpY0FwaVxuICovXG5leHBvcnQgY2xhc3MgVHlwZVNjcmlwdFNlcnZpY2VIb3N0IGltcGxlbWVudHMgTGFuZ3VhZ2VTZXJ2aWNlSG9zdCB7XG4gIHByaXZhdGUgcmVhZG9ubHkgc3VtbWFyeVJlc29sdmVyOiBBb3RTdW1tYXJ5UmVzb2x2ZXI7XG4gIHByaXZhdGUgcmVhZG9ubHkgcmVmbGVjdG9ySG9zdDogUmVmbGVjdG9ySG9zdDtcbiAgcHJpdmF0ZSByZWFkb25seSBzdGF0aWNTeW1ib2xSZXNvbHZlcjogU3RhdGljU3ltYm9sUmVzb2x2ZXI7XG5cbiAgcHJpdmF0ZSByZWFkb25seSBzdGF0aWNTeW1ib2xDYWNoZSA9IG5ldyBTdGF0aWNTeW1ib2xDYWNoZSgpO1xuICBwcml2YXRlIHJlYWRvbmx5IGZpbGVUb0NvbXBvbmVudCA9IG5ldyBNYXA8c3RyaW5nLCBTdGF0aWNTeW1ib2w+KCk7XG4gIHByaXZhdGUgcmVhZG9ubHkgY29sbGVjdGVkRXJyb3JzID0gbmV3IE1hcDxzdHJpbmcsIGFueVtdPigpO1xuICBwcml2YXRlIHJlYWRvbmx5IGZpbGVWZXJzaW9ucyA9IG5ldyBNYXA8c3RyaW5nLCBzdHJpbmc+KCk7XG5cbiAgcHJpdmF0ZSBsYXN0UHJvZ3JhbTogdHNzLlByb2dyYW18dW5kZWZpbmVkID0gdW5kZWZpbmVkO1xuICBwcml2YXRlIGFuYWx5emVkTW9kdWxlczogTmdBbmFseXplZE1vZHVsZXMgPSB7XG4gICAgZmlsZXM6IFtdLFxuICAgIG5nTW9kdWxlQnlQaXBlT3JEaXJlY3RpdmU6IG5ldyBNYXAoKSxcbiAgICBuZ01vZHVsZXM6IFtdLFxuICB9O1xuXG4gIGNvbnN0cnVjdG9yKHJlYWRvbmx5IHRzTHNIb3N0OiB0c3MuTGFuZ3VhZ2VTZXJ2aWNlSG9zdCwgcmVhZG9ubHkgdHNMUzogdHNzLkxhbmd1YWdlU2VydmljZSkge1xuICAgIHRoaXMuc3VtbWFyeVJlc29sdmVyID0gbmV3IEFvdFN1bW1hcnlSZXNvbHZlcihcbiAgICAgICAge1xuICAgICAgICAgIGxvYWRTdW1tYXJ5KF9maWxlUGF0aDogc3RyaW5nKSB7XG4gICAgICAgICAgICByZXR1cm4gbnVsbDtcbiAgICAgICAgICB9LFxuICAgICAgICAgIGlzU291cmNlRmlsZShfc291cmNlRmlsZVBhdGg6IHN0cmluZykge1xuICAgICAgICAgICAgcmV0dXJuIHRydWU7XG4gICAgICAgICAgfSxcbiAgICAgICAgICB0b1N1bW1hcnlGaWxlTmFtZShzb3VyY2VGaWxlUGF0aDogc3RyaW5nKSB7XG4gICAgICAgICAgICByZXR1cm4gc291cmNlRmlsZVBhdGg7XG4gICAgICAgICAgfSxcbiAgICAgICAgICBmcm9tU3VtbWFyeUZpbGVOYW1lKGZpbGVQYXRoOiBzdHJpbmcpOiBzdHJpbmcge1xuICAgICAgICAgICAgcmV0dXJuIGZpbGVQYXRoO1xuICAgICAgICAgIH0sXG4gICAgICAgIH0sXG4gICAgICAgIHRoaXMuc3RhdGljU3ltYm9sQ2FjaGUpO1xuICAgIHRoaXMucmVmbGVjdG9ySG9zdCA9IG5ldyBSZWZsZWN0b3JIb3N0KCgpID0+IHRoaXMucHJvZ3JhbSwgdHNMc0hvc3QpO1xuICAgIHRoaXMuc3RhdGljU3ltYm9sUmVzb2x2ZXIgPSBuZXcgU3RhdGljU3ltYm9sUmVzb2x2ZXIoXG4gICAgICAgIHRoaXMucmVmbGVjdG9ySG9zdCwgdGhpcy5zdGF0aWNTeW1ib2xDYWNoZSwgdGhpcy5zdW1tYXJ5UmVzb2x2ZXIsXG4gICAgICAgIChlLCBmaWxlUGF0aCkgPT4gdGhpcy5jb2xsZWN0RXJyb3IoZSwgZmlsZVBhdGgpKTtcbiAgfVxuXG4gIC8vIFRoZSByZXNvbHZlciBpcyBpbnN0YW50aWF0ZWQgbGF6aWx5IGFuZCBzaG91bGQgbm90IGJlIGFjY2Vzc2VkIGRpcmVjdGx5LlxuICAvLyBJbnN0ZWFkLCBjYWxsIHRoZSByZXNvbHZlciBnZXR0ZXIuIFRoZSBpbnN0YW50aWF0aW9uIG9mIHRoZSByZXNvbHZlciBhbHNvXG4gIC8vIHJlcXVpcmVzIGluc3RhbnRpYXRpb24gb2YgdGhlIFN0YXRpY1JlZmxlY3RvciwgYW5kIHRoZSBsYXR0ZXIgcmVxdWlyZXNcbiAgLy8gcmVzb2x1dGlvbiBvZiBjb3JlIEFuZ3VsYXIgc3ltYm9scy4gTW9kdWxlIHJlc29sdXRpb24gc2hvdWxkIG5vdCBiZSBkb25lXG4gIC8vIGR1cmluZyBpbnN0YW50aWF0aW9uIHRvIGF2b2lkIGN5Y2xpYyBkZXBlbmRlbmN5IGJldHdlZW4gdGhlIHBsdWdpbiBhbmQgdGhlXG4gIC8vIGNvbnRhaW5pbmcgUHJvamVjdCwgc28gdGhlIFNpbmdsZXRvbiBwYXR0ZXJuIGlzIHVzZWQgaGVyZS5cbiAgcHJpdmF0ZSBfcmVzb2x2ZXI6IENvbXBpbGVNZXRhZGF0YVJlc29sdmVyfHVuZGVmaW5lZDtcblxuICAvKipcbiAgICogUmV0dXJuIHRoZSBzaW5nbGV0b24gaW5zdGFuY2Ugb2YgdGhlIE1ldGFkYXRhUmVzb2x2ZXIuXG4gICAqL1xuICBwcml2YXRlIGdldCByZXNvbHZlcigpOiBDb21waWxlTWV0YWRhdGFSZXNvbHZlciB7XG4gICAgaWYgKHRoaXMuX3Jlc29sdmVyKSB7XG4gICAgICByZXR1cm4gdGhpcy5fcmVzb2x2ZXI7XG4gICAgfVxuICAgIC8vIFN0YXRpY1JlZmxlY3RvciBrZWVwcyBpdHMgb3duIHByaXZhdGUgY2FjaGVzIHRoYXQgYXJlIG5vdCBjbGVhcmFibGUuXG4gICAgLy8gV2UgaGF2ZSBubyBjaG9pY2UgYnV0IHRvIGNyZWF0ZSBhIG5ldyBpbnN0YW5jZSB0byBpbnZhbGlkYXRlIHRoZSBjYWNoZXMuXG4gICAgLy8gVE9ETzogUmV2aXNpdCB0aGlzIHdoZW4gbGFuZ3VhZ2Ugc2VydmljZSBnZXRzIHJld3JpdHRlbiBmb3IgSXZ5LlxuICAgIGNvbnN0IHN0YXRpY1JlZmxlY3RvciA9IG5ldyBTdGF0aWNSZWZsZWN0b3IoXG4gICAgICAgIHRoaXMuc3VtbWFyeVJlc29sdmVyLCB0aGlzLnN0YXRpY1N5bWJvbFJlc29sdmVyLFxuICAgICAgICBbXSwgIC8vIGtub3duTWV0YWRhdGFDbGFzc2VzXG4gICAgICAgIFtdLCAgLy8ga25vd25NZXRhZGF0YUZ1bmN0aW9uc1xuICAgICAgICAoZSwgZmlsZVBhdGgpID0+IHRoaXMuY29sbGVjdEVycm9yKGUsIGZpbGVQYXRoKSk7XG4gICAgLy8gQmVjYXVzZSBzdGF0aWMgcmVmbGVjdG9yIGFib3ZlIGlzIGNoYW5nZWQsIHdlIG5lZWQgdG8gY3JlYXRlIGEgbmV3XG4gICAgLy8gcmVzb2x2ZXIuXG4gICAgY29uc3QgbW9kdWxlUmVzb2x2ZXIgPSBuZXcgTmdNb2R1bGVSZXNvbHZlcihzdGF0aWNSZWZsZWN0b3IpO1xuICAgIGNvbnN0IGRpcmVjdGl2ZVJlc29sdmVyID0gbmV3IERpcmVjdGl2ZVJlc29sdmVyKHN0YXRpY1JlZmxlY3Rvcik7XG4gICAgY29uc3QgcGlwZVJlc29sdmVyID0gbmV3IFBpcGVSZXNvbHZlcihzdGF0aWNSZWZsZWN0b3IpO1xuICAgIGNvbnN0IGVsZW1lbnRTY2hlbWFSZWdpc3RyeSA9IG5ldyBEb21FbGVtZW50U2NoZW1hUmVnaXN0cnkoKTtcbiAgICBjb25zdCByZXNvdXJjZUxvYWRlciA9IG5ldyBEdW1teVJlc291cmNlTG9hZGVyKCk7XG4gICAgY29uc3QgdXJsUmVzb2x2ZXIgPSBjcmVhdGVPZmZsaW5lQ29tcGlsZVVybFJlc29sdmVyKCk7XG4gICAgY29uc3QgaHRtbFBhcnNlciA9IG5ldyBEdW1teUh0bWxQYXJzZXIoKTtcbiAgICAvLyBUaGlzIHRyYWNrcyB0aGUgQ29tcGlsZUNvbmZpZyBpbiBjb2RlZ2VuLnRzLiBDdXJyZW50bHkgdGhlc2Ugb3B0aW9uc1xuICAgIC8vIGFyZSBoYXJkLWNvZGVkLlxuICAgIGNvbnN0IGNvbmZpZyA9IG5ldyBDb21waWxlckNvbmZpZyh7XG4gICAgICBkZWZhdWx0RW5jYXBzdWxhdGlvbjogVmlld0VuY2Fwc3VsYXRpb24uRW11bGF0ZWQsXG4gICAgICB1c2VKaXQ6IGZhbHNlLFxuICAgIH0pO1xuICAgIGNvbnN0IGRpcmVjdGl2ZU5vcm1hbGl6ZXIgPVxuICAgICAgICBuZXcgRGlyZWN0aXZlTm9ybWFsaXplcihyZXNvdXJjZUxvYWRlciwgdXJsUmVzb2x2ZXIsIGh0bWxQYXJzZXIsIGNvbmZpZyk7XG4gICAgdGhpcy5fcmVzb2x2ZXIgPSBuZXcgQ29tcGlsZU1ldGFkYXRhUmVzb2x2ZXIoXG4gICAgICAgIGNvbmZpZywgaHRtbFBhcnNlciwgbW9kdWxlUmVzb2x2ZXIsIGRpcmVjdGl2ZVJlc29sdmVyLCBwaXBlUmVzb2x2ZXIsXG4gICAgICAgIG5ldyBKaXRTdW1tYXJ5UmVzb2x2ZXIoKSwgZWxlbWVudFNjaGVtYVJlZ2lzdHJ5LCBkaXJlY3RpdmVOb3JtYWxpemVyLCBuZXcgQ29uc29sZSgpLFxuICAgICAgICB0aGlzLnN0YXRpY1N5bWJvbENhY2hlLCBzdGF0aWNSZWZsZWN0b3IsXG4gICAgICAgIChlcnJvciwgdHlwZSkgPT4gdGhpcy5jb2xsZWN0RXJyb3IoZXJyb3IsIHR5cGUgJiYgdHlwZS5maWxlUGF0aCkpO1xuICAgIHJldHVybiB0aGlzLl9yZXNvbHZlcjtcbiAgfVxuXG4gIC8qKlxuICAgKiBSZXR1cm4gdGhlIHNpbmdsZXRvbiBpbnN0YW5jZSBvZiB0aGUgU3RhdGljUmVmbGVjdG9yIGhvc3RlZCBpbiB0aGVcbiAgICogTWV0YWRhdGFSZXNvbHZlci5cbiAgICovXG4gIHByaXZhdGUgZ2V0IHJlZmxlY3RvcigpOiBTdGF0aWNSZWZsZWN0b3Ige1xuICAgIHJldHVybiB0aGlzLnJlc29sdmVyLmdldFJlZmxlY3RvcigpIGFzIFN0YXRpY1JlZmxlY3RvcjtcbiAgfVxuXG4gIC8qKlxuICAgKiBSZXR1cm4gYWxsIGtub3duIGV4dGVybmFsIHRlbXBsYXRlcy5cbiAgICovXG4gIGdldEV4dGVybmFsVGVtcGxhdGVzKCk6IHN0cmluZ1tdIHtcbiAgICByZXR1cm4gWy4uLnRoaXMuZmlsZVRvQ29tcG9uZW50LmtleXMoKV07XG4gIH1cblxuICAvKipcbiAgICogQ2hlY2tzIHdoZXRoZXIgdGhlIHByb2dyYW0gaGFzIGNoYW5nZWQgYW5kIHJldHVybnMgYWxsIGFuYWx5emVkIG1vZHVsZXMuXG4gICAqIElmIHByb2dyYW0gaGFzIGNoYW5nZWQsIGludmFsaWRhdGUgYWxsIGNhY2hlcyBhbmQgdXBkYXRlIGZpbGVUb0NvbXBvbmVudFxuICAgKiBhbmQgdGVtcGxhdGVSZWZlcmVuY2VzLlxuICAgKiBJbiBhZGRpdGlvbiB0byByZXR1cm5pbmcgaW5mb3JtYXRpb24gYWJvdXQgTmdNb2R1bGVzLCB0aGlzIG1ldGhvZCBwbGF5cyB0aGVcbiAgICogc2FtZSByb2xlIGFzICdzeW5jaHJvbml6ZUhvc3REYXRhJyBpbiB0c3NlcnZlci5cbiAgICovXG4gIGdldEFuYWx5emVkTW9kdWxlcygpOiBOZ0FuYWx5emVkTW9kdWxlcyB7XG4gICAgaWYgKHRoaXMudXBUb0RhdGUoKSkge1xuICAgICAgcmV0dXJuIHRoaXMuYW5hbHl6ZWRNb2R1bGVzO1xuICAgIH1cblxuICAgIC8vIEludmFsaWRhdGUgY2FjaGVzXG4gICAgdGhpcy5maWxlVG9Db21wb25lbnQuY2xlYXIoKTtcbiAgICB0aGlzLmNvbGxlY3RlZEVycm9ycy5jbGVhcigpO1xuICAgIHRoaXMucmVzb2x2ZXIuY2xlYXJDYWNoZSgpO1xuXG4gICAgY29uc3QgYW5hbHl6ZUhvc3QgPSB7XG4gICAgICBpc1NvdXJjZUZpbGUoX2ZpbGVQYXRoOiBzdHJpbmcpIHtcbiAgICAgICAgcmV0dXJuIHRydWU7XG4gICAgICB9XG4gICAgfTtcbiAgICBjb25zdCBwcm9ncmFtRmlsZXMgPSB0aGlzLnByb2dyYW0uZ2V0U291cmNlRmlsZXMoKS5tYXAoc2YgPT4gc2YuZmlsZU5hbWUpO1xuXG4gICAgdHJ5IHtcbiAgICAgIHRoaXMuYW5hbHl6ZWRNb2R1bGVzID1cbiAgICAgICAgICBhbmFseXplTmdNb2R1bGVzKHByb2dyYW1GaWxlcywgYW5hbHl6ZUhvc3QsIHRoaXMuc3RhdGljU3ltYm9sUmVzb2x2ZXIsIHRoaXMucmVzb2x2ZXIpO1xuICAgIH0gY2F0Y2ggKGUpIHtcbiAgICAgIC8vIEFuYWx5emluZyBtb2R1bGVzIG1heSB0aHJvdzsgaW4gdGhhdCBjYXNlLCByZXVzZSB0aGUgb2xkIG1vZHVsZXMuXG4gICAgICB0aGlzLmVycm9yKGBBbmFseXppbmcgTmdNb2R1bGVzIGZhaWxlZC4gJHtlfWApO1xuICAgICAgcmV0dXJuIHRoaXMuYW5hbHl6ZWRNb2R1bGVzO1xuICAgIH1cblxuICAgIC8vIHVwZGF0ZSB0ZW1wbGF0ZSByZWZlcmVuY2VzIGFuZCBmaWxlVG9Db21wb25lbnRcbiAgICBjb25zdCB1cmxSZXNvbHZlciA9IGNyZWF0ZU9mZmxpbmVDb21waWxlVXJsUmVzb2x2ZXIoKTtcbiAgICBmb3IgKGNvbnN0IG5nTW9kdWxlIG9mIHRoaXMuYW5hbHl6ZWRNb2R1bGVzLm5nTW9kdWxlcykge1xuICAgICAgZm9yIChjb25zdCBkaXJlY3RpdmUgb2YgbmdNb2R1bGUuZGVjbGFyZWREaXJlY3RpdmVzKSB7XG4gICAgICAgIGNvbnN0IHttZXRhZGF0YX0gPSB0aGlzLnJlc29sdmVyLmdldE5vbk5vcm1hbGl6ZWREaXJlY3RpdmVNZXRhZGF0YShkaXJlY3RpdmUucmVmZXJlbmNlKSE7XG4gICAgICAgIGlmIChtZXRhZGF0YS5pc0NvbXBvbmVudCAmJiBtZXRhZGF0YS50ZW1wbGF0ZSAmJiBtZXRhZGF0YS50ZW1wbGF0ZS50ZW1wbGF0ZVVybCkge1xuICAgICAgICAgIGNvbnN0IHRlbXBsYXRlTmFtZSA9IHVybFJlc29sdmVyLnJlc29sdmUoXG4gICAgICAgICAgICAgIHRoaXMucmVmbGVjdG9yLmNvbXBvbmVudE1vZHVsZVVybChkaXJlY3RpdmUucmVmZXJlbmNlKSxcbiAgICAgICAgICAgICAgbWV0YWRhdGEudGVtcGxhdGUudGVtcGxhdGVVcmwpO1xuICAgICAgICAgIHRoaXMuZmlsZVRvQ29tcG9uZW50LnNldCh0ZW1wbGF0ZU5hbWUsIGRpcmVjdGl2ZS5yZWZlcmVuY2UpO1xuICAgICAgICB9XG4gICAgICB9XG4gICAgfVxuXG4gICAgcmV0dXJuIHRoaXMuYW5hbHl6ZWRNb2R1bGVzO1xuICB9XG5cbiAgLyoqXG4gICAqIENoZWNrcyB3aGV0aGVyIHRoZSBwcm9ncmFtIGhhcyBjaGFuZ2VkLCBhbmQgaW52YWxpZGF0ZSBzdGF0aWMgc3ltYm9scyBpblxuICAgKiB0aGUgc291cmNlIGZpbGVzIHRoYXQgaGF2ZSBjaGFuZ2VkLlxuICAgKiBSZXR1cm5zIHRydWUgaWYgbW9kdWxlcyBhcmUgdXAtdG8tZGF0ZSwgZmFsc2Ugb3RoZXJ3aXNlLlxuICAgKiBUaGlzIHNob3VsZCBvbmx5IGJlIGNhbGxlZCBieSBnZXRBbmFseXplZE1vZHVsZXMoKS5cbiAgICovXG4gIHByaXZhdGUgdXBUb0RhdGUoKTogYm9vbGVhbiB7XG4gICAgY29uc3Qge2xhc3RQcm9ncmFtLCBwcm9ncmFtfSA9IHRoaXM7XG4gICAgaWYgKGxhc3RQcm9ncmFtID09PSBwcm9ncmFtKSB7XG4gICAgICByZXR1cm4gdHJ1ZTtcbiAgICB9XG4gICAgdGhpcy5sYXN0UHJvZ3JhbSA9IHByb2dyYW07XG5cbiAgICAvLyBFdmVuIHRob3VnaCB0aGUgcHJvZ3JhbSBoYXMgY2hhbmdlZCwgaXQgY291bGQgYmUgdGhlIGNhc2UgdGhhdCBub25lIG9mXG4gICAgLy8gdGhlIHNvdXJjZSBmaWxlcyBoYXZlIGNoYW5nZWQuIElmIGFsbCBzb3VyY2UgZmlsZXMgcmVtYWluIHRoZSBzYW1lLCB0aGVuXG4gICAgLy8gcHJvZ3JhbSBpcyBzdGlsbCB1cC10by1kYXRlLCBhbmQgd2Ugc2hvdWxkIG5vdCBpbnZhbGlkYXRlIGNhY2hlcy5cbiAgICBsZXQgZmlsZXNBZGRlZCA9IDA7XG4gICAgY29uc3QgZmlsZXNDaGFuZ2VkT3JSZW1vdmVkOiBzdHJpbmdbXSA9IFtdO1xuXG4gICAgLy8gQ2hlY2sgaWYgYW55IHNvdXJjZSBmaWxlcyBoYXZlIGJlZW4gYWRkZWQgLyBjaGFuZ2VkIHNpbmNlIGxhc3QgY29tcHV0YXRpb24uXG4gICAgY29uc3Qgc2VlbiA9IG5ldyBTZXQ8c3RyaW5nPigpO1xuICAgIGNvbnN0IEFOR1VMQVJfQ09SRSA9ICdAYW5ndWxhci9jb3JlJztcbiAgICBjb25zdCBjb3JlUGF0aCA9IHRoaXMucmVmbGVjdG9ySG9zdC5tb2R1bGVOYW1lVG9GaWxlTmFtZShBTkdVTEFSX0NPUkUpO1xuICAgIGZvciAoY29uc3Qge2ZpbGVOYW1lfSBvZiBwcm9ncmFtLmdldFNvdXJjZUZpbGVzKCkpIHtcbiAgICAgIC8vIElmIGBAYW5ndWxhci9jb3JlYCBpcyBlZGl0ZWQsIHRoZSBsYW5ndWFnZSBzZXJ2aWNlIHdvdWxkIGhhdmUgdG8gYmVcbiAgICAgIC8vIHJlc3RhcnRlZCwgc28gaWdub3JlIGNoYW5nZXMgdG8gYEBhbmd1bGFyL2NvcmVgLlxuICAgICAgLy8gV2hlbiB0aGUgU3RhdGljUmVmbGVjdG9yIGlzIGluaXRpYWxpemVkIGF0IHN0YXJ0dXAsIGl0IGxvYWRzIGNvcmVcbiAgICAgIC8vIHN5bWJvbHMgZnJvbSBAYW5ndWxhci9jb3JlIGJ5IGNhbGxpbmcgaW5pdGlhbGl6ZUNvbnZlcnNpb25NYXAoKS4gVGhpc1xuICAgICAgLy8gaXMgb25seSBkb25lIG9uY2UuIElmIHRoZSBmaWxlIGlzIGludmFsaWRhdGVkLCBzb21lIG9mIHRoZSBjb3JlIHN5bWJvbHNcbiAgICAgIC8vIHdpbGwgYmUgbG9zdCBwZXJtYW5lbnRseS5cbiAgICAgIGlmIChmaWxlTmFtZSA9PT0gY29yZVBhdGgpIHtcbiAgICAgICAgY29udGludWU7XG4gICAgICB9XG4gICAgICBzZWVuLmFkZChmaWxlTmFtZSk7XG4gICAgICBjb25zdCB2ZXJzaW9uID0gdGhpcy50c0xzSG9zdC5nZXRTY3JpcHRWZXJzaW9uKGZpbGVOYW1lKTtcbiAgICAgIGNvbnN0IGxhc3RWZXJzaW9uID0gdGhpcy5maWxlVmVyc2lvbnMuZ2V0KGZpbGVOYW1lKTtcbiAgICAgIGlmIChsYXN0VmVyc2lvbiA9PT0gdW5kZWZpbmVkKSB7XG4gICAgICAgIGZpbGVzQWRkZWQrKztcbiAgICAgICAgdGhpcy5maWxlVmVyc2lvbnMuc2V0KGZpbGVOYW1lLCB2ZXJzaW9uKTtcbiAgICAgIH0gZWxzZSBpZiAodmVyc2lvbiAhPT0gbGFzdFZlcnNpb24pIHtcbiAgICAgICAgZmlsZXNDaGFuZ2VkT3JSZW1vdmVkLnB1c2goZmlsZU5hbWUpOyAgLy8gY2hhbmdlZFxuICAgICAgICB0aGlzLmZpbGVWZXJzaW9ucy5zZXQoZmlsZU5hbWUsIHZlcnNpb24pO1xuICAgICAgfVxuICAgIH1cblxuICAgIC8vIENoZWNrIGlmIGFueSBzb3VyY2UgZmlsZXMgaGF2ZSBiZWVuIHJlbW92ZWQgc2luY2UgbGFzdCBjb21wdXRhdGlvbi5cbiAgICBmb3IgKGNvbnN0IFtmaWxlTmFtZV0gb2YgdGhpcy5maWxlVmVyc2lvbnMpIHtcbiAgICAgIGlmICghc2Vlbi5oYXMoZmlsZU5hbWUpKSB7XG4gICAgICAgIGZpbGVzQ2hhbmdlZE9yUmVtb3ZlZC5wdXNoKGZpbGVOYW1lKTsgIC8vIHJlbW92ZWRcbiAgICAgICAgLy8gQmVjYXVzZSBNYXBzIGFyZSBpdGVyYXRlZCBpbiBpbnNlcnRpb24gb3JkZXIsIGl0IGlzIHNhZmUgdG8gZGVsZXRlXG4gICAgICAgIC8vIGVudHJpZXMgZnJvbSB0aGUgc2FtZSBtYXAgd2hpbGUgaXRlcmF0aW5nLlxuICAgICAgICAvLyBTZWUgaHR0cHM6Ly9zdGFja292ZXJmbG93LmNvbS9xdWVzdGlvbnMvMzU5NDAyMTYgYW5kXG4gICAgICAgIC8vIGh0dHBzOi8vd3d3LmVjbWEtaW50ZXJuYXRpb25hbC5vcmcvZWNtYS0yNjIvMTAuMC9pbmRleC5odG1sI3NlYy1tYXAucHJvdG90eXBlLmZvcmVhY2hcbiAgICAgICAgdGhpcy5maWxlVmVyc2lvbnMuZGVsZXRlKGZpbGVOYW1lKTtcbiAgICAgIH1cbiAgICB9XG5cbiAgICBmb3IgKGNvbnN0IGZpbGVOYW1lIG9mIGZpbGVzQ2hhbmdlZE9yUmVtb3ZlZCkge1xuICAgICAgY29uc3Qgc3ltYm9scyA9IHRoaXMuc3RhdGljU3ltYm9sUmVzb2x2ZXIuaW52YWxpZGF0ZUZpbGUoZmlsZU5hbWUpO1xuICAgICAgdGhpcy5yZWZsZWN0b3IuaW52YWxpZGF0ZVN5bWJvbHMoc3ltYm9scyk7XG4gICAgfVxuXG4gICAgLy8gUHJvZ3JhbSBpcyB1cC10by1kYXRlIGlmZiBubyBmaWxlcyBhcmUgYWRkZWQsIGNoYW5nZWQsIG9yIHJlbW92ZWQuXG4gICAgcmV0dXJuIGZpbGVzQWRkZWQgPT09IDAgJiYgZmlsZXNDaGFuZ2VkT3JSZW1vdmVkLmxlbmd0aCA9PT0gMDtcbiAgfVxuXG4gIC8qKlxuICAgKiBGaW5kIGFsbCB0ZW1wbGF0ZXMgaW4gdGhlIHNwZWNpZmllZCBgZmlsZWAuXG4gICAqIEBwYXJhbSBmaWxlTmFtZSBUUyBvciBIVE1MIGZpbGVcbiAgICovXG4gIGdldFRlbXBsYXRlcyhmaWxlTmFtZTogc3RyaW5nKTogVGVtcGxhdGVTb3VyY2VbXSB7XG4gICAgY29uc3QgcmVzdWx0czogVGVtcGxhdGVTb3VyY2VbXSA9IFtdO1xuICAgIGlmIChmaWxlTmFtZS5lbmRzV2l0aCgnLnRzJykpIHtcbiAgICAgIC8vIEZpbmQgZXZlcnkgdGVtcGxhdGUgc3RyaW5nIGluIHRoZSBmaWxlXG4gICAgICBjb25zdCB2aXNpdCA9IChjaGlsZDogdHNzLk5vZGUpID0+IHtcbiAgICAgICAgY29uc3QgdGVtcGxhdGUgPSB0aGlzLmdldEludGVybmFsVGVtcGxhdGUoY2hpbGQpO1xuICAgICAgICBpZiAodGVtcGxhdGUpIHtcbiAgICAgICAgICByZXN1bHRzLnB1c2godGVtcGxhdGUpO1xuICAgICAgICB9IGVsc2Uge1xuICAgICAgICAgIHRzcy5mb3JFYWNoQ2hpbGQoY2hpbGQsIHZpc2l0KTtcbiAgICAgICAgfVxuICAgICAgfTtcbiAgICAgIGNvbnN0IHNvdXJjZUZpbGUgPSB0aGlzLmdldFNvdXJjZUZpbGUoZmlsZU5hbWUpO1xuICAgICAgaWYgKHNvdXJjZUZpbGUpIHtcbiAgICAgICAgdHNzLmZvckVhY2hDaGlsZChzb3VyY2VGaWxlLCB2aXNpdCk7XG4gICAgICB9XG4gICAgfSBlbHNlIHtcbiAgICAgIGNvbnN0IHRlbXBsYXRlID0gdGhpcy5nZXRFeHRlcm5hbFRlbXBsYXRlKGZpbGVOYW1lKTtcbiAgICAgIGlmICh0ZW1wbGF0ZSkge1xuICAgICAgICByZXN1bHRzLnB1c2godGVtcGxhdGUpO1xuICAgICAgfVxuICAgIH1cbiAgICByZXR1cm4gcmVzdWx0cztcbiAgfVxuXG4gIC8qKlxuICAgKiBSZXR1cm4gbWV0YWRhdGEgYWJvdXQgYWxsIGNsYXNzIGRlY2xhcmF0aW9ucyBpbiB0aGUgZmlsZSB0aGF0IGFyZSBBbmd1bGFyXG4gICAqIGRpcmVjdGl2ZXMuIFBvdGVudGlhbCBtYXRjaGVzIGFyZSBgQE5nTW9kdWxlYCwgYEBDb21wb25lbnRgLCBgQERpcmVjdGl2ZWAsXG4gICAqIGBAUGlwZXNgLCBldGMuIGNsYXNzIGRlY2xhcmF0aW9ucy5cbiAgICpcbiAgICogQHBhcmFtIGZpbGVOYW1lIFRTIGZpbGVcbiAgICovXG4gIGdldERlY2xhcmF0aW9ucyhmaWxlTmFtZTogc3RyaW5nKTogRGVjbGFyYXRpb25bXSB7XG4gICAgaWYgKCFmaWxlTmFtZS5lbmRzV2l0aCgnLnRzJykpIHtcbiAgICAgIHJldHVybiBbXTtcbiAgICB9XG4gICAgY29uc3Qgc291cmNlRmlsZSA9IHRoaXMuZ2V0U291cmNlRmlsZShmaWxlTmFtZSk7XG4gICAgaWYgKCFzb3VyY2VGaWxlKSB7XG4gICAgICByZXR1cm4gW107XG4gICAgfVxuICAgIGNvbnN0IHJlc3VsdHM6IERlY2xhcmF0aW9uW10gPSBbXTtcbiAgICBjb25zdCB2aXNpdCA9IChjaGlsZDogdHNzLk5vZGUpID0+IHtcbiAgICAgIGNvbnN0IGNhbmRpZGF0ZSA9IGdldERpcmVjdGl2ZUNsYXNzTGlrZShjaGlsZCk7XG4gICAgICBpZiAoY2FuZGlkYXRlKSB7XG4gICAgICAgIGNvbnN0IHtjbGFzc0lkfSA9IGNhbmRpZGF0ZTtcbiAgICAgICAgY29uc3QgZGVjbGFyYXRpb25TcGFuID0gc3Bhbk9mKGNsYXNzSWQpO1xuICAgICAgICBjb25zdCBjbGFzc05hbWUgPSBjbGFzc0lkLmdldFRleHQoKTtcbiAgICAgICAgY29uc3QgY2xhc3NTeW1ib2wgPSB0aGlzLnJlZmxlY3Rvci5nZXRTdGF0aWNTeW1ib2woc291cmNlRmlsZS5maWxlTmFtZSwgY2xhc3NOYW1lKTtcbiAgICAgICAgLy8gQXNrIHRoZSByZXNvbHZlciB0byBjaGVjayBpZiBjYW5kaWRhdGUgaXMgYWN0dWFsbHkgQW5ndWxhciBkaXJlY3RpdmVcbiAgICAgICAgaWYgKCF0aGlzLnJlc29sdmVyLmlzRGlyZWN0aXZlKGNsYXNzU3ltYm9sKSkge1xuICAgICAgICAgIHJldHVybjtcbiAgICAgICAgfVxuICAgICAgICBjb25zdCBkYXRhID0gdGhpcy5yZXNvbHZlci5nZXROb25Ob3JtYWxpemVkRGlyZWN0aXZlTWV0YWRhdGEoY2xhc3NTeW1ib2wpO1xuICAgICAgICBpZiAoIWRhdGEpIHtcbiAgICAgICAgICByZXR1cm47XG4gICAgICAgIH1cbiAgICAgICAgcmVzdWx0cy5wdXNoKHtcbiAgICAgICAgICB0eXBlOiBjbGFzc1N5bWJvbCxcbiAgICAgICAgICBkZWNsYXJhdGlvblNwYW4sXG4gICAgICAgICAgbWV0YWRhdGE6IGRhdGEubWV0YWRhdGEsXG4gICAgICAgICAgZXJyb3JzOiB0aGlzLmdldENvbGxlY3RlZEVycm9ycyhkZWNsYXJhdGlvblNwYW4sIHNvdXJjZUZpbGUpLFxuICAgICAgICB9KTtcbiAgICAgIH0gZWxzZSB7XG4gICAgICAgIGNoaWxkLmZvckVhY2hDaGlsZCh2aXNpdCk7XG4gICAgICB9XG4gICAgfTtcbiAgICB0c3MuZm9yRWFjaENoaWxkKHNvdXJjZUZpbGUsIHZpc2l0KTtcblxuICAgIHJldHVybiByZXN1bHRzO1xuICB9XG5cbiAgZ2V0U291cmNlRmlsZShmaWxlTmFtZTogc3RyaW5nKTogdHNzLlNvdXJjZUZpbGV8dW5kZWZpbmVkIHtcbiAgICBpZiAoIWZpbGVOYW1lLmVuZHNXaXRoKCcudHMnKSkge1xuICAgICAgdGhyb3cgbmV3IEVycm9yKGBOb24tVFMgc291cmNlIGZpbGUgcmVxdWVzdGVkOiAke2ZpbGVOYW1lfWApO1xuICAgIH1cbiAgICByZXR1cm4gdGhpcy5wcm9ncmFtLmdldFNvdXJjZUZpbGUoZmlsZU5hbWUpO1xuICB9XG5cbiAgZ2V0IHByb2dyYW0oKTogdHNzLlByb2dyYW0ge1xuICAgIGNvbnN0IHByb2dyYW0gPSB0aGlzLnRzTFMuZ2V0UHJvZ3JhbSgpO1xuICAgIGlmICghcHJvZ3JhbSkge1xuICAgICAgLy8gUHJvZ3JhbSBpcyB2ZXJ5IHZlcnkgdW5saWtlbHkgdG8gYmUgdW5kZWZpbmVkLlxuICAgICAgdGhyb3cgbmV3IEVycm9yKCdObyBwcm9ncmFtIGluIGxhbmd1YWdlIHNlcnZpY2UhJyk7XG4gICAgfVxuICAgIHJldHVybiBwcm9ncmFtO1xuICB9XG5cbiAgLyoqXG4gICAqIFJldHVybiB0aGUgVGVtcGxhdGVTb3VyY2UgaWYgYG5vZGVgIGlzIGEgdGVtcGxhdGUgbm9kZS5cbiAgICpcbiAgICogRm9yIGV4YW1wbGUsXG4gICAqXG4gICAqIEBDb21wb25lbnQoe1xuICAgKiAgIHRlbXBsYXRlOiAnPGRpdj48L2Rpdj4nIDwtLSB0ZW1wbGF0ZSBub2RlXG4gICAqIH0pXG4gICAqIGNsYXNzIEFwcENvbXBvbmVudCB7fVxuICAgKiAgICAgICAgICAgXi0tLS0gY2xhc3MgZGVjbGFyYXRpb24gbm9kZVxuICAgKlxuICAgKiBAcGFyYW0gbm9kZSBQb3RlbnRpYWwgdGVtcGxhdGUgbm9kZVxuICAgKi9cbiAgcHJpdmF0ZSBnZXRJbnRlcm5hbFRlbXBsYXRlKG5vZGU6IHRzcy5Ob2RlKTogVGVtcGxhdGVTb3VyY2V8dW5kZWZpbmVkIHtcbiAgICBpZiAoIXRzcy5pc1N0cmluZ0xpdGVyYWxMaWtlKG5vZGUpKSB7XG4gICAgICByZXR1cm47XG4gICAgfVxuICAgIGNvbnN0IHRtcGxBc2duID0gZ2V0UHJvcGVydHlBc3NpZ25tZW50RnJvbVZhbHVlKG5vZGUsICd0ZW1wbGF0ZScpO1xuICAgIGlmICghdG1wbEFzZ24pIHtcbiAgICAgIHJldHVybjtcbiAgICB9XG4gICAgY29uc3QgY2xhc3NEZWNsID0gZ2V0Q2xhc3NEZWNsRnJvbURlY29yYXRvclByb3AodG1wbEFzZ24pO1xuICAgIGlmICghY2xhc3NEZWNsIHx8ICFjbGFzc0RlY2wubmFtZSkgeyAgLy8gRG9lcyBub3QgaGFuZGxlIGFub255bW91cyBjbGFzc1xuICAgICAgcmV0dXJuO1xuICAgIH1cbiAgICBjb25zdCBmaWxlTmFtZSA9IG5vZGUuZ2V0U291cmNlRmlsZSgpLmZpbGVOYW1lO1xuICAgIGNvbnN0IGNsYXNzU3ltYm9sID0gdGhpcy5yZWZsZWN0b3IuZ2V0U3RhdGljU3ltYm9sKGZpbGVOYW1lLCBjbGFzc0RlY2wubmFtZS50ZXh0KTtcbiAgICByZXR1cm4gbmV3IElubGluZVRlbXBsYXRlKG5vZGUsIGNsYXNzRGVjbCwgY2xhc3NTeW1ib2wsIHRoaXMpO1xuICB9XG5cbiAgLyoqXG4gICAqIFJldHVybiB0aGUgZXh0ZXJuYWwgdGVtcGxhdGUgZm9yIGBmaWxlTmFtZWAuXG4gICAqIEBwYXJhbSBmaWxlTmFtZSBIVE1MIGZpbGVcbiAgICovXG4gIHByaXZhdGUgZ2V0RXh0ZXJuYWxUZW1wbGF0ZShmaWxlTmFtZTogc3RyaW5nKTogVGVtcGxhdGVTb3VyY2V8dW5kZWZpbmVkIHtcbiAgICAvLyBGaXJzdCBnZXQgdGhlIHRleHQgZm9yIHRoZSB0ZW1wbGF0ZVxuICAgIGNvbnN0IHNuYXBzaG90ID0gdGhpcy50c0xzSG9zdC5nZXRTY3JpcHRTbmFwc2hvdChmaWxlTmFtZSk7XG4gICAgaWYgKCFzbmFwc2hvdCkge1xuICAgICAgcmV0dXJuO1xuICAgIH1cbiAgICBjb25zdCBzb3VyY2UgPSBzbmFwc2hvdC5nZXRUZXh0KDAsIHNuYXBzaG90LmdldExlbmd0aCgpKTtcbiAgICAvLyBOZXh0IGZpbmQgdGhlIGNvbXBvbmVudCBjbGFzcyBzeW1ib2xcbiAgICBjb25zdCBjbGFzc1N5bWJvbCA9IHRoaXMuZmlsZVRvQ29tcG9uZW50LmdldChmaWxlTmFtZSk7XG4gICAgaWYgKCFjbGFzc1N5bWJvbCkge1xuICAgICAgcmV0dXJuO1xuICAgIH1cbiAgICAvLyBUaGVuIHVzZSB0aGUgY2xhc3Mgc3ltYm9sIHRvIGZpbmQgdGhlIGFjdHVhbCB0cy5DbGFzc0RlY2xhcmF0aW9uIG5vZGVcbiAgICBjb25zdCBzb3VyY2VGaWxlID0gdGhpcy5nZXRTb3VyY2VGaWxlKGNsYXNzU3ltYm9sLmZpbGVQYXRoKTtcbiAgICBpZiAoIXNvdXJjZUZpbGUpIHtcbiAgICAgIHJldHVybjtcbiAgICB9XG4gICAgLy8gVE9ETzogVGhpcyBvbmx5IGNvbnNpZGVycyB0b3AtbGV2ZWwgY2xhc3MgZGVjbGFyYXRpb25zIGluIGEgc291cmNlIGZpbGUuXG4gICAgLy8gVGhpcyB3b3VsZCBub3QgZmluZCBhIGNsYXNzIGRlY2xhcmF0aW9uIGluIGEgbmFtZXNwYWNlLCBmb3IgZXhhbXBsZS5cbiAgICBjb25zdCBjbGFzc0RlY2wgPSBzb3VyY2VGaWxlLmZvckVhY2hDaGlsZCgoY2hpbGQpID0+IHtcbiAgICAgIGlmICh0c3MuaXNDbGFzc0RlY2xhcmF0aW9uKGNoaWxkKSAmJiBjaGlsZC5uYW1lICYmIGNoaWxkLm5hbWUudGV4dCA9PT0gY2xhc3NTeW1ib2wubmFtZSkge1xuICAgICAgICByZXR1cm4gY2hpbGQ7XG4gICAgICB9XG4gICAgfSk7XG4gICAgaWYgKCFjbGFzc0RlY2wpIHtcbiAgICAgIHJldHVybjtcbiAgICB9XG4gICAgcmV0dXJuIG5ldyBFeHRlcm5hbFRlbXBsYXRlKHNvdXJjZSwgZmlsZU5hbWUsIGNsYXNzRGVjbCwgY2xhc3NTeW1ib2wsIHRoaXMpO1xuICB9XG5cbiAgcHJpdmF0ZSBjb2xsZWN0RXJyb3IoZXJyb3I6IGFueSwgZmlsZVBhdGg/OiBzdHJpbmcpIHtcbiAgICBpZiAoZmlsZVBhdGgpIHtcbiAgICAgIGxldCBlcnJvcnMgPSB0aGlzLmNvbGxlY3RlZEVycm9ycy5nZXQoZmlsZVBhdGgpO1xuICAgICAgaWYgKCFlcnJvcnMpIHtcbiAgICAgICAgZXJyb3JzID0gW107XG4gICAgICAgIHRoaXMuY29sbGVjdGVkRXJyb3JzLnNldChmaWxlUGF0aCwgZXJyb3JzKTtcbiAgICAgIH1cbiAgICAgIGVycm9ycy5wdXNoKGVycm9yKTtcbiAgICB9XG4gIH1cblxuICBwcml2YXRlIGdldENvbGxlY3RlZEVycm9ycyhkZWZhdWx0U3BhbjogU3Bhbiwgc291cmNlRmlsZTogdHNzLlNvdXJjZUZpbGUpOiBEZWNsYXJhdGlvbkVycm9yW10ge1xuICAgIGNvbnN0IGVycm9ycyA9IHRoaXMuY29sbGVjdGVkRXJyb3JzLmdldChzb3VyY2VGaWxlLmZpbGVOYW1lKTtcbiAgICBpZiAoIWVycm9ycykge1xuICAgICAgcmV0dXJuIFtdO1xuICAgIH1cbiAgICAvLyBUT0RPOiBBZGQgYmV0dGVyIHR5cGluZ3MgZm9yIHRoZSBlcnJvcnNcbiAgICByZXR1cm4gZXJyb3JzLm1hcCgoZTogYW55KSA9PiB7XG4gICAgICBjb25zdCBsaW5lID0gZS5saW5lIHx8IChlLnBvc2l0aW9uICYmIGUucG9zaXRpb24ubGluZSk7XG4gICAgICBjb25zdCBjb2x1bW4gPSBlLmNvbHVtbiB8fCAoZS5wb3NpdGlvbiAmJiBlLnBvc2l0aW9uLmNvbHVtbik7XG4gICAgICBjb25zdCBzcGFuID0gc3BhbkF0KHNvdXJjZUZpbGUsIGxpbmUsIGNvbHVtbikgfHwgZGVmYXVsdFNwYW47XG4gICAgICBpZiAoaXNGb3JtYXR0ZWRFcnJvcihlKSkge1xuICAgICAgICByZXR1cm4gZXJyb3JUb0RpYWdub3N0aWNXaXRoQ2hhaW4oZSwgc3Bhbik7XG4gICAgICB9XG4gICAgICByZXR1cm4ge21lc3NhZ2U6IGUubWVzc2FnZSwgc3Bhbn07XG4gICAgfSk7XG4gIH1cblxuICAvKipcbiAgICogUmV0dXJuIHRoZSBwYXJzZWQgdGVtcGxhdGUgZm9yIHRoZSB0ZW1wbGF0ZSBhdCB0aGUgc3BlY2lmaWVkIGBwb3NpdGlvbmAuXG4gICAqIEBwYXJhbSBmaWxlTmFtZSBUUyBvciBIVE1MIGZpbGVcbiAgICogQHBhcmFtIHBvc2l0aW9uIFBvc2l0aW9uIG9mIHRoZSB0ZW1wbGF0ZSBpbiB0aGUgVFMgZmlsZSwgb3RoZXJ3aXNlIGlnbm9yZWQuXG4gICAqL1xuICBnZXRUZW1wbGF0ZUFzdEF0UG9zaXRpb24oZmlsZU5hbWU6IHN0cmluZywgcG9zaXRpb246IG51bWJlcik6IEFzdFJlc3VsdHx1bmRlZmluZWQge1xuICAgIGxldCB0ZW1wbGF0ZTogVGVtcGxhdGVTb3VyY2V8dW5kZWZpbmVkO1xuICAgIGlmIChmaWxlTmFtZS5lbmRzV2l0aCgnLnRzJykpIHtcbiAgICAgIGNvbnN0IHNvdXJjZUZpbGUgPSB0aGlzLmdldFNvdXJjZUZpbGUoZmlsZU5hbWUpO1xuICAgICAgaWYgKCFzb3VyY2VGaWxlKSB7XG4gICAgICAgIHJldHVybjtcbiAgICAgIH1cbiAgICAgIC8vIEZpbmQgdGhlIG5vZGUgdGhhdCBtb3N0IGNsb3NlbHkgbWF0Y2hlcyB0aGUgcG9zaXRpb25cbiAgICAgIGNvbnN0IG5vZGUgPSBmaW5kVGlnaHRlc3ROb2RlKHNvdXJjZUZpbGUsIHBvc2l0aW9uKTtcbiAgICAgIGlmICghbm9kZSkge1xuICAgICAgICByZXR1cm47XG4gICAgICB9XG4gICAgICB0ZW1wbGF0ZSA9IHRoaXMuZ2V0SW50ZXJuYWxUZW1wbGF0ZShub2RlKTtcbiAgICB9IGVsc2Uge1xuICAgICAgdGVtcGxhdGUgPSB0aGlzLmdldEV4dGVybmFsVGVtcGxhdGUoZmlsZU5hbWUpO1xuICAgIH1cbiAgICBpZiAoIXRlbXBsYXRlKSB7XG4gICAgICByZXR1cm47XG4gICAgfVxuICAgIHJldHVybiB0aGlzLmdldFRlbXBsYXRlQXN0KHRlbXBsYXRlKTtcbiAgfVxuXG4gIC8qKlxuICAgKiBGaW5kIHRoZSBOZ01vZHVsZSB3aGljaCB0aGUgZGlyZWN0aXZlIGFzc29jaWF0ZWQgd2l0aCB0aGUgYGNsYXNzU3ltYm9sYFxuICAgKiBiZWxvbmdzIHRvLCB0aGVuIHJldHVybiBpdHMgc2NoZW1hIGFuZCB0cmFuc2l0aXZlIGRpcmVjdGl2ZXMgYW5kIHBpcGVzLlxuICAgKiBAcGFyYW0gY2xhc3NTeW1ib2wgQW5ndWxhciBTeW1ib2wgdGhhdCBkZWZpbmVzIGEgZGlyZWN0aXZlXG4gICAqL1xuICBwcml2YXRlIGdldE1vZHVsZU1ldGFkYXRhRm9yRGlyZWN0aXZlKGNsYXNzU3ltYm9sOiBTdGF0aWNTeW1ib2wpIHtcbiAgICBjb25zdCByZXN1bHQgPSB7XG4gICAgICBkaXJlY3RpdmVzOiBbXSBhcyBDb21waWxlRGlyZWN0aXZlU3VtbWFyeVtdLFxuICAgICAgcGlwZXM6IFtdIGFzIENvbXBpbGVQaXBlU3VtbWFyeVtdLFxuICAgICAgc2NoZW1hczogW10gYXMgU2NoZW1hTWV0YWRhdGFbXSxcbiAgICB9O1xuICAgIC8vIEZpcnN0IGZpbmQgd2hpY2ggTmdNb2R1bGUgdGhlIGRpcmVjdGl2ZSBiZWxvbmdzIHRvLlxuICAgIGNvbnN0IG5nTW9kdWxlID0gdGhpcy5hbmFseXplZE1vZHVsZXMubmdNb2R1bGVCeVBpcGVPckRpcmVjdGl2ZS5nZXQoY2xhc3NTeW1ib2wpIHx8XG4gICAgICAgIGZpbmRTdWl0YWJsZURlZmF1bHRNb2R1bGUodGhpcy5hbmFseXplZE1vZHVsZXMpO1xuICAgIGlmICghbmdNb2R1bGUpIHtcbiAgICAgIHJldHVybiByZXN1bHQ7XG4gICAgfVxuICAgIC8vIFRoZW4gZ2F0aGVyIGFsbCB0cmFuc2l0aXZlIGRpcmVjdGl2ZXMgYW5kIHBpcGVzLlxuICAgIGNvbnN0IHtkaXJlY3RpdmVzLCBwaXBlc30gPSBuZ01vZHVsZS50cmFuc2l0aXZlTW9kdWxlO1xuICAgIGZvciAoY29uc3QgZGlyZWN0aXZlIG9mIGRpcmVjdGl2ZXMpIHtcbiAgICAgIGNvbnN0IGRhdGEgPSB0aGlzLnJlc29sdmVyLmdldE5vbk5vcm1hbGl6ZWREaXJlY3RpdmVNZXRhZGF0YShkaXJlY3RpdmUucmVmZXJlbmNlKTtcbiAgICAgIGlmIChkYXRhKSB7XG4gICAgICAgIHJlc3VsdC5kaXJlY3RpdmVzLnB1c2goZGF0YS5tZXRhZGF0YS50b1N1bW1hcnkoKSk7XG4gICAgICB9XG4gICAgfVxuICAgIGZvciAoY29uc3QgcGlwZSBvZiBwaXBlcykge1xuICAgICAgY29uc3QgbWV0YWRhdGEgPSB0aGlzLnJlc29sdmVyLmdldE9yTG9hZFBpcGVNZXRhZGF0YShwaXBlLnJlZmVyZW5jZSk7XG4gICAgICByZXN1bHQucGlwZXMucHVzaChtZXRhZGF0YS50b1N1bW1hcnkoKSk7XG4gICAgfVxuICAgIHJlc3VsdC5zY2hlbWFzLnB1c2goLi4ubmdNb2R1bGUuc2NoZW1hcyk7XG4gICAgcmV0dXJuIHJlc3VsdDtcbiAgfVxuXG4gIC8qKlxuICAgKiBQYXJzZSB0aGUgYHRlbXBsYXRlYCBhbmQgcmV0dXJuIGl0cyBBU1QsIGlmIGFueS5cbiAgICogQHBhcmFtIHRlbXBsYXRlIHRlbXBsYXRlIHRvIGJlIHBhcnNlZFxuICAgKi9cbiAgZ2V0VGVtcGxhdGVBc3QodGVtcGxhdGU6IFRlbXBsYXRlU291cmNlKTogQXN0UmVzdWx0fHVuZGVmaW5lZCB7XG4gICAgY29uc3Qge3R5cGU6IGNsYXNzU3ltYm9sLCBmaWxlTmFtZX0gPSB0ZW1wbGF0ZTtcbiAgICBjb25zdCBkYXRhID0gdGhpcy5yZXNvbHZlci5nZXROb25Ob3JtYWxpemVkRGlyZWN0aXZlTWV0YWRhdGEoY2xhc3NTeW1ib2wpO1xuICAgIGlmICghZGF0YSkge1xuICAgICAgcmV0dXJuO1xuICAgIH1cbiAgICBjb25zdCBodG1sUGFyc2VyID0gbmV3IEh0bWxQYXJzZXIoKTtcbiAgICBjb25zdCBleHByZXNzaW9uUGFyc2VyID0gbmV3IFBhcnNlcihuZXcgTGV4ZXIoKSk7XG4gICAgY29uc3QgcGFyc2VyID0gbmV3IFRlbXBsYXRlUGFyc2VyKFxuICAgICAgICBuZXcgQ29tcGlsZXJDb25maWcoKSwgdGhpcy5yZWZsZWN0b3IsIGV4cHJlc3Npb25QYXJzZXIsIG5ldyBEb21FbGVtZW50U2NoZW1hUmVnaXN0cnkoKSxcbiAgICAgICAgaHRtbFBhcnNlcixcbiAgICAgICAgbnVsbCwgIC8vIGNvbnNvbGVcbiAgICAgICAgW10gICAgIC8vIHRyYW5mb3Jtc1xuICAgICk7XG4gICAgY29uc3QgaHRtbFJlc3VsdCA9IGh0bWxQYXJzZXIucGFyc2UodGVtcGxhdGUuc291cmNlLCBmaWxlTmFtZSwge1xuICAgICAgdG9rZW5pemVFeHBhbnNpb25Gb3JtczogdHJ1ZSxcbiAgICAgIHByZXNlcnZlTGluZUVuZGluZ3M6IHRydWUsICAvLyBkbyBub3QgY29udmVydCBDUkxGIHRvIExGXG4gICAgfSk7XG4gICAgY29uc3Qge2RpcmVjdGl2ZXMsIHBpcGVzLCBzY2hlbWFzfSA9IHRoaXMuZ2V0TW9kdWxlTWV0YWRhdGFGb3JEaXJlY3RpdmUoY2xhc3NTeW1ib2wpO1xuICAgIGNvbnN0IHBhcnNlUmVzdWx0ID0gcGFyc2VyLnRyeVBhcnNlSHRtbChodG1sUmVzdWx0LCBkYXRhLm1ldGFkYXRhLCBkaXJlY3RpdmVzLCBwaXBlcywgc2NoZW1hcyk7XG4gICAgaWYgKCFwYXJzZVJlc3VsdC50ZW1wbGF0ZUFzdCkge1xuICAgICAgcmV0dXJuO1xuICAgIH1cbiAgICByZXR1cm4ge1xuICAgICAgaHRtbEFzdDogaHRtbFJlc3VsdC5yb290Tm9kZXMsXG4gICAgICB0ZW1wbGF0ZUFzdDogcGFyc2VSZXN1bHQudGVtcGxhdGVBc3QsXG4gICAgICBkaXJlY3RpdmU6IGRhdGEubWV0YWRhdGEsXG4gICAgICBkaXJlY3RpdmVzLFxuICAgICAgcGlwZXMsXG4gICAgICBwYXJzZUVycm9yczogcGFyc2VSZXN1bHQuZXJyb3JzLFxuICAgICAgZXhwcmVzc2lvblBhcnNlcixcbiAgICAgIHRlbXBsYXRlLFxuICAgIH07XG4gIH1cblxuICAvKipcbiAgICogTG9nIHRoZSBzcGVjaWZpZWQgYG1zZ2AgdG8gZmlsZSBhdCBJTkZPIGxldmVsLiBJZiBsb2dnaW5nIGlzIG5vdCBlbmFibGVkXG4gICAqIHRoaXMgbWV0aG9kIGlzIGEgbm8tb3AuXG4gICAqIEBwYXJhbSBtc2cgTG9nIG1lc3NhZ2VcbiAgICovXG4gIGxvZyhtc2c6IHN0cmluZykge1xuICAgIGlmICh0aGlzLnRzTHNIb3N0LmxvZykge1xuICAgICAgdGhpcy50c0xzSG9zdC5sb2cobXNnKTtcbiAgICB9XG4gIH1cblxuICAvKipcbiAgICogTG9nIHRoZSBzcGVjaWZpZWQgYG1zZ2AgdG8gZmlsZSBhdCBFUlJPUiBsZXZlbC4gSWYgbG9nZ2luZyBpcyBub3QgZW5hYmxlZFxuICAgKiB0aGlzIG1ldGhvZCBpcyBhIG5vLW9wLlxuICAgKiBAcGFyYW0gbXNnIGVycm9yIG1lc3NhZ2VcbiAgICovXG4gIGVycm9yKG1zZzogc3RyaW5nKSB7XG4gICAgaWYgKHRoaXMudHNMc0hvc3QuZXJyb3IpIHtcbiAgICAgIHRoaXMudHNMc0hvc3QuZXJyb3IobXNnKTtcbiAgICB9XG4gIH1cblxuICAvKipcbiAgICogTG9nIGRlYnVnZ2luZyBpbmZvIHRvIGZpbGUgYXQgSU5GTyBsZXZlbCwgb25seSBpZiB2ZXJib3NlIHNldHRpbmcgaXMgdHVybmVkXG4gICAqIG9uLiBPdGhlcndpc2UsIHRoaXMgbWV0aG9kIGlzIGEgbm8tb3AuXG4gICAqIEBwYXJhbSBtc2cgZGVidWdnaW5nIG1lc3NhZ2VcbiAgICovXG4gIGRlYnVnKG1zZzogc3RyaW5nKSB7XG4gICAgY29uc3QgcHJvamVjdCA9IHRoaXMudHNMc0hvc3QgYXMgdHNzLnNlcnZlci5Qcm9qZWN0O1xuICAgIGlmICghcHJvamVjdC5wcm9qZWN0U2VydmljZSkge1xuICAgICAgLy8gdHNMc0hvc3QgaXMgbm90IGEgUHJvamVjdFxuICAgICAgcmV0dXJuO1xuICAgIH1cbiAgICBjb25zdCB7bG9nZ2VyfSA9IHByb2plY3QucHJvamVjdFNlcnZpY2U7XG4gICAgaWYgKGxvZ2dlci5oYXNMZXZlbCh0c3Muc2VydmVyLkxvZ0xldmVsLnZlcmJvc2UpKSB7XG4gICAgICBsb2dnZXIuaW5mbyhtc2cpO1xuICAgIH1cbiAgfVxufVxuXG5mdW5jdGlvbiBmaW5kU3VpdGFibGVEZWZhdWx0TW9kdWxlKG1vZHVsZXM6IE5nQW5hbHl6ZWRNb2R1bGVzKTogQ29tcGlsZU5nTW9kdWxlTWV0YWRhdGF8dW5kZWZpbmVkIHtcbiAgbGV0IHJlc3VsdDogQ29tcGlsZU5nTW9kdWxlTWV0YWRhdGF8dW5kZWZpbmVkID0gdW5kZWZpbmVkO1xuICBsZXQgcmVzdWx0U2l6ZSA9IDA7XG4gIGZvciAoY29uc3QgbW9kdWxlIG9mIG1vZHVsZXMubmdNb2R1bGVzKSB7XG4gICAgY29uc3QgbW9kdWxlU2l6ZSA9IG1vZHVsZS50cmFuc2l0aXZlTW9kdWxlLmRpcmVjdGl2ZXMubGVuZ3RoO1xuICAgIGlmIChtb2R1bGVTaXplID4gcmVzdWx0U2l6ZSkge1xuICAgICAgcmVzdWx0ID0gbW9kdWxlO1xuICAgICAgcmVzdWx0U2l6ZSA9IG1vZHVsZVNpemU7XG4gICAgfVxuICB9XG4gIHJldHVybiByZXN1bHQ7XG59XG5cbmZ1bmN0aW9uIHNwYW5PZihub2RlOiB0c3MuTm9kZSk6IFNwYW4ge1xuICByZXR1cm4ge3N0YXJ0OiBub2RlLmdldFN0YXJ0KCksIGVuZDogbm9kZS5nZXRFbmQoKX07XG59XG5cbmZ1bmN0aW9uIHNwYW5BdChzb3VyY2VGaWxlOiB0c3MuU291cmNlRmlsZSwgbGluZTogbnVtYmVyLCBjb2x1bW46IG51bWJlcik6IFNwYW58dW5kZWZpbmVkIHtcbiAgaWYgKGxpbmUgIT0gbnVsbCAmJiBjb2x1bW4gIT0gbnVsbCkge1xuICAgIGNvbnN0IHBvc2l0aW9uID0gdHNzLmdldFBvc2l0aW9uT2ZMaW5lQW5kQ2hhcmFjdGVyKHNvdXJjZUZpbGUsIGxpbmUsIGNvbHVtbik7XG4gICAgY29uc3QgZmluZENoaWxkID0gZnVuY3Rpb24gZmluZENoaWxkKG5vZGU6IHRzcy5Ob2RlKTogdHNzLk5vZGV8dW5kZWZpbmVkIHtcbiAgICAgIGlmIChub2RlLmtpbmQgPiB0c3MuU3ludGF4S2luZC5MYXN0VG9rZW4gJiYgbm9kZS5wb3MgPD0gcG9zaXRpb24gJiYgbm9kZS5lbmQgPiBwb3NpdGlvbikge1xuICAgICAgICBjb25zdCBiZXR0ZXJOb2RlID0gdHNzLmZvckVhY2hDaGlsZChub2RlLCBmaW5kQ2hpbGQpO1xuICAgICAgICByZXR1cm4gYmV0dGVyTm9kZSB8fCBub2RlO1xuICAgICAgfVxuICAgIH07XG5cbiAgICBjb25zdCBub2RlID0gdHNzLmZvckVhY2hDaGlsZChzb3VyY2VGaWxlLCBmaW5kQ2hpbGQpO1xuICAgIGlmIChub2RlKSB7XG4gICAgICByZXR1cm4ge3N0YXJ0OiBub2RlLmdldFN0YXJ0KCksIGVuZDogbm9kZS5nZXRFbmQoKX07XG4gICAgfVxuICB9XG59XG5cbmZ1bmN0aW9uIGNvbnZlcnRDaGFpbihjaGFpbjogRm9ybWF0dGVkTWVzc2FnZUNoYWluKTogRGlhZ25vc3RpY01lc3NhZ2VDaGFpbiB7XG4gIHJldHVybiB7bWVzc2FnZTogY2hhaW4ubWVzc2FnZSwgbmV4dDogY2hhaW4ubmV4dCA/IGNoYWluLm5leHQubWFwKGNvbnZlcnRDaGFpbikgOiB1bmRlZmluZWR9O1xufVxuXG5mdW5jdGlvbiBlcnJvclRvRGlhZ25vc3RpY1dpdGhDaGFpbihlcnJvcjogRm9ybWF0dGVkRXJyb3IsIHNwYW46IFNwYW4pOiBEZWNsYXJhdGlvbkVycm9yIHtcbiAgcmV0dXJuIHttZXNzYWdlOiBlcnJvci5jaGFpbiA/IGNvbnZlcnRDaGFpbihlcnJvci5jaGFpbikgOiBlcnJvci5tZXNzYWdlLCBzcGFufTtcbn1cbiJdfQ==