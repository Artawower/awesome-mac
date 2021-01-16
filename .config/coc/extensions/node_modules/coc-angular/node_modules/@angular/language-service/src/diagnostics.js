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
        define("@angular/language-service/src/diagnostics", ["require", "exports", "tslib", "path", "typescript", "@angular/language-service/src/diagnostic_messages", "@angular/language-service/src/expression_diagnostics", "@angular/language-service/src/ts_utils", "@angular/language-service/src/utils"], factory);
    }
})(function (require, exports) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    exports.ngDiagnosticToTsDiagnostic = exports.getDeclarationDiagnostics = exports.getTemplateDiagnostics = void 0;
    var tslib_1 = require("tslib");
    var path = require("path");
    var ts = require("typescript");
    var diagnostic_messages_1 = require("@angular/language-service/src/diagnostic_messages");
    var expression_diagnostics_1 = require("@angular/language-service/src/expression_diagnostics");
    var ts_utils_1 = require("@angular/language-service/src/ts_utils");
    var utils_1 = require("@angular/language-service/src/utils");
    /**
     * Return diagnostic information for the parsed AST of the template.
     * @param ast contains HTML and template AST
     */
    function getTemplateDiagnostics(ast) {
        var parseErrors = ast.parseErrors, templateAst = ast.templateAst, htmlAst = ast.htmlAst, template = ast.template;
        if (parseErrors && parseErrors.length) {
            return parseErrors.map(function (e) {
                return {
                    kind: ts.DiagnosticCategory.Error,
                    span: utils_1.offsetSpan(utils_1.spanOf(e.span), template.span.start),
                    message: e.msg,
                };
            });
        }
        return expression_diagnostics_1.getTemplateExpressionDiagnostics({
            templateAst: templateAst,
            htmlAst: htmlAst,
            offset: template.span.start,
            query: template.query,
            members: template.members,
            source: ast.template.source,
        });
    }
    exports.getTemplateDiagnostics = getTemplateDiagnostics;
    /**
     * Performs a variety diagnostics on directive declarations.
     *
     * @param declarations Angular directive declarations
     * @param modules NgModules in the project
     * @param host TypeScript service host used to perform TypeScript queries
     * @return diagnosed errors, if any
     */
    function getDeclarationDiagnostics(declarations, modules, host) {
        var e_1, _a, e_2, _b, e_3, _c, e_4, _d;
        var directives = new Set();
        try {
            for (var _e = tslib_1.__values(modules.ngModules), _f = _e.next(); !_f.done; _f = _e.next()) {
                var ngModule = _f.value;
                try {
                    for (var _g = (e_2 = void 0, tslib_1.__values(ngModule.declaredDirectives)), _h = _g.next(); !_h.done; _h = _g.next()) {
                        var directive = _h.value;
                        directives.add(directive.reference);
                    }
                }
                catch (e_2_1) { e_2 = { error: e_2_1 }; }
                finally {
                    try {
                        if (_h && !_h.done && (_b = _g.return)) _b.call(_g);
                    }
                    finally { if (e_2) throw e_2.error; }
                }
            }
        }
        catch (e_1_1) { e_1 = { error: e_1_1 }; }
        finally {
            try {
                if (_f && !_f.done && (_a = _e.return)) _a.call(_e);
            }
            finally { if (e_1) throw e_1.error; }
        }
        var results = [];
        try {
            for (var declarations_1 = tslib_1.__values(declarations), declarations_1_1 = declarations_1.next(); !declarations_1_1.done; declarations_1_1 = declarations_1.next()) {
                var declaration = declarations_1_1.value;
                var errors = declaration.errors, metadata = declaration.metadata, type = declaration.type, declarationSpan = declaration.declarationSpan;
                var sf = host.getSourceFile(type.filePath);
                if (!sf) {
                    host.error("directive " + type.name + " exists but has no source file");
                    return [];
                }
                // TypeScript identifier of the directive declaration annotation (e.g. "Component" or
                // "Directive") on a directive class.
                var directiveIdentifier = ts_utils_1.findTightestNode(sf, declarationSpan.start);
                if (!directiveIdentifier) {
                    host.error("directive " + type.name + " exists but has no identifier");
                    return [];
                }
                try {
                    for (var errors_1 = (e_4 = void 0, tslib_1.__values(errors)), errors_1_1 = errors_1.next(); !errors_1_1.done; errors_1_1 = errors_1.next()) {
                        var error = errors_1_1.value;
                        results.push({
                            kind: ts.DiagnosticCategory.Error,
                            message: error.message,
                            span: error.span,
                        });
                    }
                }
                catch (e_4_1) { e_4 = { error: e_4_1 }; }
                finally {
                    try {
                        if (errors_1_1 && !errors_1_1.done && (_d = errors_1.return)) _d.call(errors_1);
                    }
                    finally { if (e_4) throw e_4.error; }
                }
                if (!modules.ngModuleByPipeOrDirective.has(declaration.type)) {
                    results.push(diagnostic_messages_1.createDiagnostic(declarationSpan, diagnostic_messages_1.Diagnostic.directive_not_in_module, metadata.isComponent ? 'Component' : 'Directive', type.name));
                }
                if (metadata.isComponent) {
                    var _j = metadata.template, template = _j.template, templateUrl = _j.templateUrl, styleUrls = _j.styleUrls;
                    if (template === null && !templateUrl) {
                        results.push(diagnostic_messages_1.createDiagnostic(declarationSpan, diagnostic_messages_1.Diagnostic.missing_template_and_templateurl, type.name));
                    }
                    else if (templateUrl) {
                        if (template) {
                            results.push(diagnostic_messages_1.createDiagnostic(declarationSpan, diagnostic_messages_1.Diagnostic.both_template_and_templateurl, type.name));
                        }
                        // Find templateUrl value from the directive call expression, which is the parent of the
                        // directive identifier.
                        //
                        // TODO: We should create an enum of the various properties a directive can have to use
                        // instead of string literals. We can then perform a mass migration of all literal usages.
                        var templateUrlNode = ts_utils_1.findPropertyValueOfType(directiveIdentifier.parent, 'templateUrl', ts.isLiteralExpression);
                        if (!templateUrlNode) {
                            host.error("templateUrl " + templateUrl + " exists but its TypeScript node doesn't");
                            return [];
                        }
                        results.push.apply(results, tslib_1.__spread(validateUrls([templateUrlNode], host.tsLsHost)));
                    }
                    if (styleUrls.length > 0) {
                        // Find styleUrls value from the directive call expression, which is the parent of the
                        // directive identifier.
                        var styleUrlsNode = ts_utils_1.findPropertyValueOfType(directiveIdentifier.parent, 'styleUrls', ts.isArrayLiteralExpression);
                        if (!styleUrlsNode) {
                            host.error("styleUrls property exists but its TypeScript node doesn't'");
                            return [];
                        }
                        results.push.apply(results, tslib_1.__spread(validateUrls(styleUrlsNode.elements, host.tsLsHost)));
                    }
                }
            }
        }
        catch (e_3_1) { e_3 = { error: e_3_1 }; }
        finally {
            try {
                if (declarations_1_1 && !declarations_1_1.done && (_c = declarations_1.return)) _c.call(declarations_1);
            }
            finally { if (e_3) throw e_3.error; }
        }
        return results;
    }
    exports.getDeclarationDiagnostics = getDeclarationDiagnostics;
    /**
     * Checks that URLs on a directive point to a valid file.
     * Note that this diagnostic check may require a filesystem hit, and thus may be slower than other
     * checks.
     *
     * @param urls urls to check for validity
     * @param tsLsHost TS LS host used for querying filesystem information
     * @return diagnosed url errors, if any
     */
    function validateUrls(urls, tsLsHost) {
        if (!tsLsHost.fileExists) {
            return [];
        }
        var allErrors = [];
        // TODO(ayazhafiz): most of this logic can be unified with the logic in
        // definitions.ts#getUrlFromProperty. Create a utility function to be used by both.
        for (var i = 0; i < urls.length; ++i) {
            var urlNode = urls[i];
            if (!ts.isStringLiteralLike(urlNode)) {
                // If a non-string value is assigned to a URL node (like `templateUrl`), a type error will be
                // picked up by the TS Language Server.
                continue;
            }
            var curPath = urlNode.getSourceFile().fileName;
            var url = path.join(path.dirname(curPath), urlNode.text);
            if (tsLsHost.fileExists(url))
                continue;
            // Exclude opening and closing quotes in the url span.
            var urlSpan = { start: urlNode.getStart() + 1, end: urlNode.end - 1 };
            allErrors.push(diagnostic_messages_1.createDiagnostic(urlSpan, diagnostic_messages_1.Diagnostic.invalid_templateurl));
        }
        return allErrors;
    }
    /**
     * Return a recursive data structure that chains diagnostic messages.
     * @param chain
     */
    function chainDiagnostics(chain) {
        return {
            messageText: chain.message,
            category: ts.DiagnosticCategory.Error,
            code: 0,
            next: chain.next ? chain.next.map(chainDiagnostics) : undefined
        };
    }
    /**
     * Convert ng.Diagnostic to ts.Diagnostic.
     * @param d diagnostic
     * @param file
     */
    function ngDiagnosticToTsDiagnostic(d, file) {
        return {
            file: file,
            start: d.span.start,
            length: d.span.end - d.span.start,
            messageText: typeof d.message === 'string' ? d.message : chainDiagnostics(d.message),
            category: d.kind,
            code: 0,
            source: 'ng',
        };
    }
    exports.ngDiagnosticToTsDiagnostic = ngDiagnosticToTsDiagnostic;
});
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiZGlhZ25vc3RpY3MuanMiLCJzb3VyY2VSb290IjoiIiwic291cmNlcyI6WyIuLi8uLi8uLi8uLi8uLi8uLi9wYWNrYWdlcy9sYW5ndWFnZS1zZXJ2aWNlL3NyYy9kaWFnbm9zdGljcy50cyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiQUFBQTs7Ozs7O0dBTUc7Ozs7Ozs7Ozs7Ozs7O0lBR0gsMkJBQTZCO0lBQzdCLCtCQUFpQztJQUVqQyx5RkFBbUU7SUFDbkUsK0ZBQTBFO0lBQzFFLG1FQUFxRTtJQUdyRSw2REFBMkM7SUFFM0M7OztPQUdHO0lBQ0gsU0FBZ0Isc0JBQXNCLENBQUMsR0FBaUI7UUFDL0MsSUFBQSxXQUFXLEdBQW9DLEdBQUcsWUFBdkMsRUFBRSxXQUFXLEdBQXVCLEdBQUcsWUFBMUIsRUFBRSxPQUFPLEdBQWMsR0FBRyxRQUFqQixFQUFFLFFBQVEsR0FBSSxHQUFHLFNBQVAsQ0FBUTtRQUMxRCxJQUFJLFdBQVcsSUFBSSxXQUFXLENBQUMsTUFBTSxFQUFFO1lBQ3JDLE9BQU8sV0FBVyxDQUFDLEdBQUcsQ0FBQyxVQUFBLENBQUM7Z0JBQ3RCLE9BQU87b0JBQ0wsSUFBSSxFQUFFLEVBQUUsQ0FBQyxrQkFBa0IsQ0FBQyxLQUFLO29CQUNqQyxJQUFJLEVBQUUsa0JBQVUsQ0FBQyxjQUFNLENBQUMsQ0FBQyxDQUFDLElBQUksQ0FBQyxFQUFFLFFBQVEsQ0FBQyxJQUFJLENBQUMsS0FBSyxDQUFDO29CQUNyRCxPQUFPLEVBQUUsQ0FBQyxDQUFDLEdBQUc7aUJBQ2YsQ0FBQztZQUNKLENBQUMsQ0FBQyxDQUFDO1NBQ0o7UUFDRCxPQUFPLHlEQUFnQyxDQUFDO1lBQ3RDLFdBQVcsRUFBRSxXQUFXO1lBQ3hCLE9BQU8sRUFBRSxPQUFPO1lBQ2hCLE1BQU0sRUFBRSxRQUFRLENBQUMsSUFBSSxDQUFDLEtBQUs7WUFDM0IsS0FBSyxFQUFFLFFBQVEsQ0FBQyxLQUFLO1lBQ3JCLE9BQU8sRUFBRSxRQUFRLENBQUMsT0FBTztZQUN6QixNQUFNLEVBQUUsR0FB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UyxDQUFDO0lBQ25CLENBQUM7SUFFRDs7O09BR0c7SUFDSCxTQUFTLGdCQUFnQixDQUFDLEtBQWdDO1FBQ3hELE9BQU87WUFDTCxXQUFXLEVBQUUsS0FBSyxDQUFDLE9BQU87WUFDMUIsUUFBUSxFQUFFLEVBQUUsQ0FBQyxrQkFBa0IsQ0FBQyxLQUFLO1lBQ3JDLElBQUksRUFBRSxDQUFDO1lBQ1AsSUFBSSxFQUFFLEtBQUssQ0FBQyxJQUFJLENBQUMsQ0FBQyxDQUFDLEtBQUssQ0FBQyxJQUFJLENBQUMsR0FBRyxDQUFDLGdCQUFnQixDQUFDLENBQUMsQ0FBQyxDQUFDLFNBQVM7U0FDaEUsQ0FBQztJQUNKLENBQUM7SUFFRDs7OztPQUlHO0lBQ0gsU0FBZ0IsMEJBQTBCLENBQ3RDLENBQWdCLEVBQUUsSUFBNkI7UUFDakQsT0FBTztZQUNMLElBQUksTUFBQTtZQUNKLEtBQUssRUFBRSxDQUFDLENBQUMsSUFBSSxDQUFDLEtBQUs7WUFDbkIsTUFBTSxFQUFFLENBQUMsQ0FBQyxJQUFJLENBQUMsR0FBRyxHQUFHLENBQUMsQ0FBQyxJQUFJLENBQUMsS0FBSztZQUNqQyxXQUFXLEVBQUUsT0FBTyxDQUFDLENBQUMsT0FBTyxLQUFLLFFBQVEsQ0FBQyxDQUFDLENBQUMsQ0FBQyxDQUFDLE9BQU8sQ0FBQyxDQUFDLENBQUMsZ0JBQWdCLENBQUMsQ0FBQyxDQUFDLE9BQU8sQ0FBQztZQUNwRixRQUFRLEVBQUUsQ0FBQyxDQUFDLElBQUk7WUFDaEIsSUFBSSxFQUFFLENBQUM7WUFDUCxNQUFNLEVBQUUsSUFBSTtTQUNiLENBQUM7SUFDSixDQUFDO0lBWEQsZ0VBV0MiLCJzb3VyY2VzQ29udGVudCI6WyIvKipcbiAqIEBsaWNlbnNlXG4gKiBDb3B5cmlnaHQgR29vZ2xlIExMQyBBbGwgUmlnaHRzIFJlc2VydmVkLlxuICpcbiAqIFVzZSBvZiB0aGlzIHNvdXJjZSBjb2RlIGlzIGdvdmVybmVkIGJ5IGFuIE1JVC1zdHlsZSBsaWNlbnNlIHRoYXQgY2FuIGJlXG4gKiBmb3VuZCBpbiB0aGUgTElDRU5TRSBmaWxlIGF0IGh0dHBzOi8vYW5ndWxhci5pby9saWNlbnNlXG4gKi9cblxuaW1wb3J0IHtOZ0FuYWx5emVkTW9kdWxlc30gZnJvbSAnQGFuZ3VsYXIvY29tcGlsZXInO1xuaW1wb3J0ICogYXMgcGF0aCBmcm9tICdwYXRoJztcbmltcG9ydCAqIGFzIHRzIGZyb20gJ3R5cGVzY3JpcHQnO1xuXG5pbXBvcnQge2NyZWF0ZURpYWdub3N0aWMsIERpYWdub3N0aWN9IGZyb20gJy4vZGlhZ25vc3RpY19tZXNzYWdlcyc7XG5pbXBvcnQge2dldFRlbXBsYXRlRXhwcmVzc2lvbkRpYWdub3N0aWNzfSBmcm9tICcuL2V4cHJlc3Npb25fZGlhZ25vc3RpY3MnO1xuaW1wb3J0IHtmaW5kUHJvcGVydHlWYWx1ZU9mVHlwZSwgZmluZFRpZ2h0ZXN0Tm9kZX0gZnJvbSAnLi90c191dGlscyc7XG5pbXBvcnQgKiBhcyBuZyBmcm9tICcuL3R5cGVzJztcbmltcG9ydCB7VHlwZVNjcmlwdFNlcnZpY2VIb3N0fSBmcm9tICcuL3R5cGVzY3JpcHRfaG9zdCc7XG5pbXBvcnQge29mZnNldFNwYW4sIHNwYW5PZn0gZnJvbSAnLi91dGlscyc7XG5cbi8qKlxuICogUmV0dXJuIGRpYWdub3N0aWMgaW5mb3JtYXRpb24gZm9yIHRoZSBwYXJzZWQgQVNUIG9mIHRoZSB0ZW1wbGF0ZS5cbiAqIEBwYXJhbSBhc3QgY29udGFpbnMgSFRNTCBhbmQgdGVtcGxhdGUgQVNUXG4gKi9cbmV4cG9ydCBmdW5jdGlvbiBnZXRUZW1wbGF0ZURpYWdub3N0aWNzKGFzdDogbmcuQXN0UmVzdWx0KTogbmcuRGlhZ25vc3RpY1tdIHtcbiAgY29uc3Qge3BhcnNlRXJyb3JzLCB0ZW1wbGF0ZUFzdCwgaHRtbEFzdCwgdGVtcGxhdGV9ID0gYXN0O1xuICBpZiAocGFyc2VFcnJvcnMgJiYgcGFyc2VFcnJvcnMubGVuZ3RoKSB7XG4gICAgcmV0dXJuIHBhcnNlRXJyb3JzLm1hcChlID0+IHtcbiAgICAgIHJldHVybiB7XG4gICAgICAgIGtpbmQ6IHRzLkRpYWdub3N0aWNDYXRlZ29yeS5FcnJvcixcbiAgICAgICAgc3Bhbjogb2Zmc2V0U3BhbihzcGFuT2YoZS5zcGFuKSwgdGVtcGxhdGUuc3Bhbi5zdGFydCksXG4gICAgICAgIG1lc3NhZ2U6IGUubXNnLFxuICAgICAgfTtcbiAgICB9KTtcbiAgfVxuICByZXR1cm4gZ2V0VGVtcGxhdGVFeHByZXNzaW9uRGlhZ25vc3RpY3Moe1xuICAgIHRlbXBsYXRlQXN0OiB0ZW1wbGF0ZUFzdCxcbiAgICBodG1sQXN0OiBodG1sQXN0LFxuICAgIG9mZnNldDogdGVtcGxhdGUuc3Bhbi5zdGFydCxcbiAgICBxdWVyeTogdGVtcGxhdGUucXVlcnksXG4gICAgbWVtYmVyczogdGVtcGxhdGUubWVtYmVycyxcbiAgICBzb3VyY2U6IGFzdC50ZW1wbGF0ZS5zb3VyY2UsXG4gIH0pO1xufVxuXG4vKipcbiAqIFBlcmZvcm1zIGEgdmFyaWV0eSBkaWFnbm9zdGljcyBvbiBkaXJlY3RpdmUgZGVjbGFyYXRpb25zLlxuICpcbiAqIEBwYXJhbSBkZWNsYXJhdGlvbnMgQW5ndWxhciBkaXJlY3RpdmUgZGVjbGFyYXRpb25zXG4gKiBAcGFyYW0gbW9kdWxlcyBOZ01vZHVsZXMgaW4gdGhlIHByb2plY3RcbiAqIEBwYXJhbSBob3N0IFR5cGVTY3JpcHQgc2VydmljZSBob3N0IHVzZWQgdG8gcGVyZm9ybSBUeXBlU2NyaXB0IHF1ZXJpZXNcbiAqIEByZXR1cm4gZGlhZ25vc2VkIGVycm9ycywgaWYgYW55XG4gKi9cbmV4cG9ydCBmdW5jdGlvbiBnZXREZWNsYXJhdGlvbkRpYWdub3N0aWNzKFxuICAgIGRlY2xhcmF0aW9uczogbmcuRGVjbGFyYXRpb25bXSwgbW9kdWxlczogTmdBbmFseXplZE1vZHVsZXMsXG4gICAgaG9zdDogUmVhZG9ubHk8VHlwZVNjcmlwdFNlcnZpY2VIb3N0Pik6IG5nLkRpYWdub3N0aWNbXSB7XG4gIGNvbnN0IGRpcmVjdGl2ZXMgPSBuZXcgU2V0PG5nLlN0YXRpY1N5bWJvbD4oKTtcbiAgZm9yIChjb25zdCBuZ01vZHVsZSBvZiBtb2R1bGVzLm5nTW9kdWxlcykge1xuICAgIGZvciAoY29uc3QgZGlyZWN0aXZlIG9mIG5nTW9kdWxlLmRlY2xhcmVkRGlyZWN0aXZlcykge1xuICAgICAgZGlyZWN0aXZlcy5hZGQoZGlyZWN0aXZlLnJlZmVyZW5jZSk7XG4gICAgfVxuICB9XG5cbiAgY29uc3QgcmVzdWx0czogbmcuRGlhZ25vc3RpY1tdID0gW107XG5cbiAgZm9yIChjb25zdCBkZWNsYXJhdGlvbiBvZiBkZWNsYXJhdGlvbnMpIHtcbiAgICBjb25zdCB7ZXJyb3JzLCBtZXRhZGF0YSwgdHlwZSwgZGVjbGFyYXRpb25TcGFufSA9IGRlY2xhcmF0aW9uO1xuXG4gICAgY29uc3Qgc2YgPSBob3N0LmdldFNvdXJjZUZpbGUodHlwZS5maWxlUGF0aCk7XG4gICAgaWYgKCFzZikge1xuICAgICAgaG9zdC5lcnJvcihgZGlyZWN0aXZlICR7dHlwZS5uYW1lfSBleGlzdHMgYnV0IGhhcyBubyBzb3VyY2UgZmlsZWApO1xuICAgICAgcmV0dXJuIFtdO1xuICAgIH1cbiAgICAvLyBUeXBlU2NyaXB0IGlkZW50aWZpZXIgb2YgdGhlIGRpcmVjdGl2ZSBkZWNsYXJhdGlvbiBhbm5vdGF0aW9uIChlLmcuIFwiQ29tcG9uZW50XCIgb3JcbiAgICAvLyBcIkRpcmVjdGl2ZVwiKSBvbiBhIGRpcmVjdGl2ZSBjbGFzcy5cbiAgICBjb25zdCBkaXJlY3RpdmVJZGVudGlmaWVyID0gZmluZFRpZ2h0ZXN0Tm9kZShzZiwgZGVjbGFyYXRpb25TcGFuLnN0YXJ0KTtcbiAgICBpZiAoIWRpcmVjdGl2ZUlkZW50aWZpZXIpIHtcbiAgICAgIGhvc3QuZXJyb3IoYGRpcmVjdGl2ZSAke3R5cGUubmFtZX0gZXhpc3RzIGJ1dCBoYXMgbm8gaWRlbnRpZmllcmApO1xuICAgICAgcmV0dXJuIFtdO1xuICAgIH1cblxuICAgIGZvciAoY29uc3QgZXJyb3Igb2YgZXJyb3JzKSB7XG4gICAgICByZXN1bHRzLnB1c2goe1xuICAgICAgICBraW5kOiB0cy5EaWFnbm9zdGljQ2F0ZWdvcnkuRXJyb3IsXG4gICAgICAgIG1lc3NhZ2U6IGVycm9yLm1lc3NhZ2UsXG4gICAgICAgIHNwYW46IGVycm9yLnNwYW4sXG4gICAgICB9KTtcbiAgICB9XG5cbiAgICBpZiAoIW1vZHVsZXMubmdNb2R1bGVCeVBpcGVPckRpcmVjdGl2ZS5oYXMoZGVjbGFyYXRpb24udHlwZSkpIHtcbiAgICAgIHJlc3VsdHMucHVzaChjcmVhdGVEaWFnbm9zdGljKFxuICAgICAgICAgIGRlY2xhcmF0aW9uU3BhbiwgRGlhZ25vc3RpYy5kaXJlY3RpdmVfbm90X2luX21vZHVsZSxcbiAgICAgICAgICBtZXRhZGF0YS5pc0NvbXBvbmVudCA/ICdDb21wb25lbnQnIDogJ0RpcmVjdGl2ZScsIHR5cGUubmFtZSkpO1xuICAgIH1cblxuICAgIGlmIChtZXRhZGF0YS5pc0NvbXBvbmVudCkge1xuICAgICAgY29uc3Qge3RlbXBsYXRlLCB0ZW1wbGF0ZVVybCwgc3R5bGVVcmxzfSA9IG1ldGFkYXRhLnRlbXBsYXRlICE7XG4gICAgICBpZiAodGVtcGxhdGUgPT09IG51bGwgJiYgIXRlbXBsYXRlVXJsKSB7XG4gICAgICAgIHJlc3VsdHMucHVzaChjcmVhdGVEaWFnbm9zdGljKFxuICAgICAgICAgICAgZGVjbGFyYXRpb25TcGFuLCBEaWFnbm9zdGljLm1pc3NpbmdfdGVtcGxhdGVfYW5kX3RlbXBsYXRldXJsLCB0eXBlLm5hbWUpKTtcbiAgICAgIH0gZWxzZSBpZiAodGVtcGxhdGVVcmwpIHtcbiAgICAgICAgaWYgKHRlbXBsYXRlKSB7XG4gICAgICAgICAgcmVzdWx0cy5wdXNoKGNyZWF0ZURpYWdub3N0aWMoXG4gICAgICAgICAgICAgIGRlY2xhcmF0aW9uU3BhbiwgRGlhZ25vc3RpYy5ib3RoX3RlbXBsYXRlX2FuZF90ZW1wbGF0ZXVybCwgdHlwZS5uYW1lKSk7XG4gICAgICAgIH1cblxuICAgICAgICAvLyBGaW5kIHRlbXBsYXRlVXJsIHZhbHVlIGZyb20gdGhlIGRpcmVjdGl2ZSBjYWxsIGV4cHJlc3Npb24sIHdoaWNoIGlzIHRoZSBwYXJlbnQgb2YgdGhlXG4gICAgICAgIC8vIGRpcmVjdGl2ZSBpZGVudGlmaWVyLlxuICAgICAgICAvL1xuICAgICAgICAvLyBUT0RPOiBXZSBzaG91bGQgY3JlYXRlIGFuIGVudW0gb2YgdGhlIHZhcmlvdXMgcHJvcGVydGllcyBhIGRpcmVjdGl2ZSBjYW4gaGF2ZSB0byB1c2VcbiAgICAgICAgLy8gaW5zdGVhZCBvZiBzdHJpbmcgbGl0ZXJhbHMuIFdlIGNhbiB0aGVuIHBlcmZvcm0gYSBtYXNzIG1pZ3JhdGlvbiBvZiBhbGwgbGl0ZXJhbCB1c2FnZXMuXG4gICAgICAgIGNvbnN0IHRlbXBsYXRlVXJsTm9kZSA9IGZpbmRQcm9wZXJ0eVZhbHVlT2ZUeXBlKFxuICAgICAgICAgICAgZGlyZWN0aXZlSWRlbnRpZmllci5wYXJlbnQsICd0ZW1wbGF0ZVVybCcsIHRzLmlzTGl0ZXJhbEV4cHJlc3Npb24pO1xuICAgICAgICBpZiAoIXRlbXBsYXRlVXJsTm9kZSkge1xuICAgICAgICAgIGhvc3QuZXJyb3IoYHRlbXBsYXRlVXJsICR7dGVtcGxhdGVVcmx9IGV4aXN0cyBidXQgaXRzIFR5cGVTY3JpcHQgbm9kZSBkb2Vzbid0YCk7XG4gICAgICAgICAgcmV0dXJuIFtdO1xuICAgICAgICB9XG5cbiAgICAgICAgcmVzdWx0cy5wdXNoKC4uLnZhbGlkYXRlVXJscyhbdGVtcGxhdGVVcmxOb2RlXSwgaG9zdC50c0xzSG9zdCkpO1xuICAgICAgfVxuXG4gICAgICBpZiAoc3R5bGVVcmxzLmxlbmd0aCA+IDApIHtcbiAgICAgICAgLy8gRmluZCBzdHlsZVVybHMgdmFsdWUgZnJvbSB0aGUgZGlyZWN0aXZlIGNhbGwgZXhwcmVzc2lvbiwgd2hpY2ggaXMgdGhlIHBhcmVudCBvZiB0aGVcbiAgICAgICAgLy8gZGlyZWN0aXZlIGlkZW50aWZpZXIuXG4gICAgICAgIGNvbnN0IHN0eWxlVXJsc05vZGUgPSBmaW5kUHJvcGVydHlWYWx1ZU9mVHlwZShcbiAgICAgICAgICAgIGRpcmVjdGl2ZUlkZW50aWZpZXIucGFyZW50LCAnc3R5bGVVcmxzJywgdHMuaXNBcnJheUxpdGVyYWxFeHByZXNzaW9uKTtcbiAgICAgICAgaWYgKCFzdHlsZVVybHNOb2RlKSB7XG4gICAgICAgICAgaG9zdC5lcnJvcihgc3R5bGVVcmxzIHByb3BlcnR5IGV4aXN0cyBidXQgaXRzIFR5cGVTY3JpcHQgbm9kZSBkb2Vzbid0J2ApO1xuICAgICAgICAgIHJldHVybiBbXTtcbiAgICAgICAgfVxuXG4gICAgICAgIHJlc3VsdHMucHVzaCguLi52YWxpZGF0ZVVybHMoc3R5bGVVcmxzTm9kZS5lbGVtZW50cywgaG9zdC50c0xzSG9zdCkpO1xuICAgICAgfVxuICAgIH1cbiAgfVxuXG4gIHJldHVybiByZXN1bHRzO1xufVxuXG4vKipcbiAqIENoZWNrcyB0aGF0IFVSTHMgb24gYSBkaXJlY3RpdmUgcG9pbnQgdG8gYSB2YWxpZCBmaWxlLlxuICogTm90ZSB0aGF0IHRoaXMgZGlhZ25vc3RpYyBjaGVjayBtYXkgcmVxdWlyZSBhIGZpbGVzeXN0ZW0gaGl0LCBhbmQgdGh1cyBtYXkgYmUgc2xvd2VyIHRoYW4gb3RoZXJcbiAqIGNoZWNrcy5cbiAqXG4gKiBAcGFyYW0gdXJscyB1cmxzIHRvIGNoZWNrIGZvciB2YWxpZGl0eVxuICogQHBhcmFtIHRzTHNIb3N0IFRTIExTIGhvc3QgdXNlZCBmb3IgcXVlcnlpbmcgZmlsZXN5c3RlbSBpbmZvcm1hdGlvblxuICogQHJldHVybiBkaWFnbm9zZWQgdXJsIGVycm9ycywgaWYgYW55XG4gKi9cbmZ1bmN0aW9uIHZhbGlkYXRlVXJscyhcbiAgICB1cmxzOiBBcnJheUxpa2U8dHMuRXhwcmVzc2lvbj4sIHRzTHNIb3N0OiBSZWFkb25seTx0cy5MYW5ndWFnZVNlcnZpY2VIb3N0Pik6IG5nLkRpYWdub3N0aWNbXSB7XG4gIGlmICghdHNMc0hvc3QuZmlsZUV4aXN0cykge1xuICAgIHJldHVybiBbXTtcbiAgfVxuXG4gIGNvbnN0IGFsbEVycm9yczogbmcuRGlhZ25vc3RpY1tdID0gW107XG4gIC8vIFRPRE8oYXlhemhhZml6KTogbW9zdCBvZiB0aGlzIGxvZ2ljIGNhbiBiZSB1bmlmaWVkIHdpdGggdGhlIGxvZ2ljIGluXG4gIC8vIGRlZmluaXRpb25zLnRzI2dldFVybEZyb21Qcm9wZXJ0eS4gQ3JlYXRlIGEgdXRpbGl0eSBmdW5jdGlvbiB0byBiZSB1c2VkIGJ5IGJvdGguXG4gIGZvciAobGV0IGkgPSAwOyBpIDwgdXJscy5sZW5ndGg7ICsraSkge1xuICAgIGNvbnN0IHVybE5vZGUgPSB1cmxzW2ldO1xuICAgIGlmICghdHMuaXNTdHJpbmdMaXRlcmFsTGlrZSh1cmxOb2RlKSkge1xuICAgICAgLy8gSWYgYSBub24tc3RyaW5nIHZhbHVlIGlzIGFzc2lnbmVkIHRvIGEgVVJMIG5vZGUgKGxpa2UgYHRlbXBsYXRlVXJsYCksIGEgdHlwZSBlcnJvciB3aWxsIGJlXG4gICAgICAvLyBwaWNrZWQgdXAgYnkgdGhlIFRTIExhbmd1YWdlIFNlcnZlci5cbiAgICAgIGNvbnRpbnVlO1xuICAgIH1cbiAgICBjb25zdCBjdXJQYXRoID0gdXJsTm9kZS5nZXRTb3VyY2VGaWxlKCkuZmlsZU5hbWU7XG4gICAgY29uc3QgdXJsID0gcGF0aC5qb2luKHBhdGguZGlybmFtZShjdXJQYXRoKSwgdXJsTm9kZS50ZXh0KTtcbiAgICBpZiAodHNMc0hvc3QuZmlsZUV4aXN0cyh1cmwpKSBjb250aW51ZTtcblxuICAgIC8vIEV4Y2x1ZGUgb3BlbmluZyBhbmQgY2xvc2luZyBxdW90ZXMgaW4gdGhlIHVybCBzcGFuLlxuICAgIGNvbnN0IHVybFNwYW4gPSB7c3RhcnQ6IHVybE5vZGUuZ2V0U3RhcnQoKSArIDEsIGVuZDogdXJsTm9kZS5lbmQgLSAxfTtcbiAgICBhbGxFcnJvcnMucHVzaChjcmVhdGVEaWFnbm9zdGljKHVybFNwYW4sIERpYWdub3N0aWMuaW52YWxpZF90ZW1wbGF0ZXVybCkpO1xuICB9XG4gIHJldHVybiBhbGxFcnJvcnM7XG59XG5cbi8qKlxuICogUmV0dXJuIGEgcmVjdXJzaXZlIGRhdGEgc3RydWN0dXJlIHRoYXQgY2hhaW5zIGRpYWdub3N0aWMgbWVzc2FnZXMuXG4gKiBAcGFyYW0gY2hhaW5cbiAqL1xuZnVuY3Rpb24gY2hhaW5EaWFnbm9zdGljcyhjaGFpbjogbmcuRGlhZ25vc3RpY01lc3NhZ2VDaGFpbik6IHRzLkRpYWdub3N0aWNNZXNzYWdlQ2hhaW4ge1xuICByZXR1cm4ge1xuICAgIG1lc3NhZ2VUZXh0OiBjaGFpbi5tZXNzYWdlLFxuICAgIGNhdGVnb3J5OiB0cy5EaWFnbm9zdGljQ2F0ZWdvcnkuRXJyb3IsXG4gICAgY29kZTogMCxcbiAgICBuZXh0OiBjaGFpbi5uZXh0ID8gY2hhaW4ubmV4dC5tYXAoY2hhaW5EaWFnbm9zdGljcykgOiB1bmRlZmluZWRcbiAgfTtcbn1cblxuLyoqXG4gKiBDb252ZXJ0IG5nLkRpYWdub3N0aWMgdG8gdHMuRGlhZ25vc3RpYy5cbiAqIEBwYXJhbSBkIGRpYWdub3N0aWNcbiAqIEBwYXJhbSBmaWxlXG4gKi9cbmV4cG9ydCBmdW5jdGlvbiBuZ0RpYWdub3N0aWNUb1RzRGlhZ25vc3RpYyhcbiAgICBkOiBuZy5EaWFnbm9zdGljLCBmaWxlOiB0cy5Tb3VyY2VGaWxlfHVuZGVmaW5lZCk6IHRzLkRpYWdub3N0aWMge1xuICByZXR1cm4ge1xuICAgIGZpbGUsXG4gICAgc3RhcnQ6IGQuc3Bhbi5zdGFydCxcbiAgICBsZW5ndGg6IGQuc3Bhbi5lbmQgLSBkLnNwYW4uc3RhcnQsXG4gICAgbWVzc2FnZVRleHQ6IHR5cGVvZiBkLm1lc3NhZ2UgPT09ICdzdHJpbmcnID8gZC5tZXNzYWdlIDogY2hhaW5EaWFnbm9zdGljcyhkLm1lc3NhZ2UpLFxuICAgIGNhdGVnb3J5OiBkLmtpbmQsXG4gICAgY29kZTogMCxcbiAgICBzb3VyY2U6ICduZycsXG4gIH07XG59XG4iXX0=