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
        define("@angular/language-service/src/language_service", ["require", "exports", "tslib", "typescript/lib/tsserverlibrary", "@angular/language-service/src/completions", "@angular/language-service/src/definitions", "@angular/language-service/src/diagnostics", "@angular/language-service/src/hover"], factory);
    }
})(function (require, exports) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    exports.createLanguageService = void 0;
    var tslib_1 = require("tslib");
    var tss = require("typescript/lib/tsserverlibrary");
    var completions_1 = require("@angular/language-service/src/completions");
    var definitions_1 = require("@angular/language-service/src/definitions");
    var diagnostics_1 = require("@angular/language-service/src/diagnostics");
    var hover_1 = require("@angular/language-service/src/hover");
    /**
     * Create an instance of an Angular `LanguageService`.
     *
     * @publicApi
     */
    function createLanguageService(host) {
        return new LanguageServiceImpl(host);
    }
    exports.createLanguageService = createLanguageService;
    var LanguageServiceImpl = /** @class */ (function () {
        function LanguageServiceImpl(host) {
            this.host = host;
        }
        LanguageServiceImpl.prototype.getSemanticDiagnostics = function (fileName) {
            var e_1, _a;
            var analyzedModules = this.host.getAnalyzedModules(); // same role as 'synchronizeHostData'
            var ngDiagnostics = [];
            var templates = this.host.getTemplates(fileName);
            try {
                for (var templates_1 = tslib_1.__values(templates), templates_1_1 = templates_1.next(); !templates_1_1.done; templates_1_1 = templates_1.next()) {
                    var template = templates_1_1.value;
                    var ast = this.host.getTemplateAst(template);
                    if (ast) {
                        ngDiagnostics.push.apply(ngDiagnostics, tslib_1.__spread(diagnostics_1.getTemplateDiagnostics(ast)));
                    }
                }
            }
            catch (e_1_1) { e_1 = { error: e_1_1 }; }
            finally {
                try {
                    if (templates_1_1 && !templates_1_1.done && (_a = templates_1.return)) _a.call(templates_1);
                }
                finally { if (e_1) throw e_1.error; }
            }
            var declarations = this.host.getDeclarations(fileName);
            ngDiagnostics.push.apply(ngDiagnostics, tslib_1.__spread(diagnostics_1.getDeclarationDiagnostics(declarations, analyzedModules, this.host)));
            var sourceFile = fileName.endsWith('.ts') ? this.host.getSourceFile(fileName) : undefined;
            var tsDiagnostics = ngDiagnostics.map(function (d) { return diagnostics_1.ngDiagnosticToTsDiagnostic(d, sourceFile); });
            return tslib_1.__spread(tss.sortAndDeduplicateDiagnostics(tsDiagnostics));
        };
        LanguageServiceImpl.prototype.getCompletionsAtPosition = function (fileName, position, _options) {
            this.host.getAnalyzedModules(); // same role as 'synchronizeHostData'
            var ast = this.host.getTemplateAstAtPosition(fileName, position);
            if (!ast) {
                return;
            }
            var results = completions_1.getTemplateCompletions(ast, position);
            if (!results || !results.length) {
                return;
            }
            return {
                isGlobalCompletion: false,
                isMemberCompletion: false,
                isNewIdentifierLocation: false,
                // Cast CompletionEntry.kind from ng.CompletionKind to ts.ScriptElementKind
                entries: results,
            };
        };
        LanguageServiceImpl.prototype.getDefinitionAndBoundSpan = function (fileName, position) {
            this.host.getAnalyzedModules(); // same role as 'synchronizeHostData'
            var templateInfo = this.host.getTemplateAstAtPosition(fileName, position);
            if (templateInfo) {
                return definitions_1.getDefinitionAndBoundSpan(templateInfo, position);
            }
            // Attempt to get Angular-specific definitions in a TypeScript file, like templates defined
            // in a `templateUrl` property.
            if (fileName.endsWith('.ts')) {
                var sf = this.host.getSourceFile(fileName);
                if (sf) {
                    return definitions_1.getTsDefinitionAndBoundSpan(sf, position, this.host.tsLsHost);
                }
            }
        };
        LanguageServiceImpl.prototype.getQuickInfoAtPosition = function (fileName, position) {
            var analyzedModules = this.host.getAnalyzedModules(); // same role as 'synchronizeHostData'
            var templateInfo = this.host.getTemplateAstAtPosition(fileName, position);
            if (templateInfo) {
                return hover_1.getTemplateHover(templateInfo, position, analyzedModules);
            }
            // Attempt to get Angular-specific hover information in a TypeScript file, the NgModule a
            // directive belongs to.
            var declarations = this.host.getDeclarations(fileName);
            return hover_1.getTsHover(position, declarations, analyzedModules);
        };
        LanguageServiceImpl.prototype.getReferencesAtPosition = function (fileName, position) {
            var defAndSpan = this.getDefinitionAndBoundSpan(fileName, position);
            if (!(defAndSpan === null || defAndSpan === void 0 ? void 0 : defAndSpan.definitions)) {
                return;
            }
            var definitions = defAndSpan.definitions;
            var tsDef = definitions.find(function (def) { return def.fileName.endsWith('.ts'); });
            if (!tsDef) {
                return;
            }
            return this.host.tsLS.getReferencesAtPosition(tsDef.fileName, tsDef.textSpan.start);
        };
        return LanguageServiceImpl;
    }());
});
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoibGFuZ3VhZ2Vfc2VydmljZS5qcyIsInNvdXJjZVJvb3QiOiIiLCJzb3VyY2VzIjpbIi4uLy4uLy4uLy4uLy4uLy4uL3BhY2thZ2VzL2xhbmd1YWdlLXNlcnZpY2Uvc3JjL2xhbmd1YWdlX3NlcnZpY2UudHMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IkFBQUE7Ozs7OztHQU1HOzs7Ozs7Ozs7Ozs7OztJQUVILG9EQUFzRDtJQUV0RCx5RUFBcUQ7SUFDckQseUVBQXFGO0lBQ3JGLHlFQUE0RztJQUM1Ryw2REFBcUQ7SUFJckQ7Ozs7T0FJRztJQUNILFNBQWdCLHFCQUFxQixDQUFDLElBQTJCO1FBQy9ELE9BQU8sSUFBSSxtQkFBbUIsQ0FBQyxJQUFJLENBQUMsQ0FBQztJQUN2QyxDQUFDO0lBRkQsc0RBRUM7SUFFRDtRQUNFLDZCQUE2QixJQUEyQjtZQUEzQixTQUFJLEdBQUosSUFBSSxDQUF1QjtRQUFHLENBQUM7UUFFNUQsb0RBQXNCLEdBQXRCLFVBQXVCLFFBQWdCOztZQUNyQyxJQUFNLGVBQWUsR0FBRyxJQUFJLENBQUMsSUFBSSxDQUFDLGtCQUFrQixFQUFFLENBQUMsQ0FBRSxxQ0FBcUM7WUFDOUYsSUFBTSxhQUFhLEdBQW9CLEVBQUUsQ0FBQztZQUUxQyxJQUFNLFNBQVMsR0FBRyxJQUFJLENBQUMsSUFBSSxDQUFDLFlBQVksQ0FBQyxRQUFRLENBQUMsQ0FBQzs7Z0JBQ25ELEtBQXVCLElBQUEsY0FBQSxpQkFBQSxTQUFTLENBQUEsb0NBQUEsMkRBQUU7b0JBQTdCLElBQU0sUUFBUSxzQkFBQTtvQkFDakIsSUFBTSxHQUFHLEdBQUcsSUFBSSxDQUFDLElBQUksQ0FBQyxjQUFjLENBQUMsUUFBUSxDQUFDLENBQUM7b0JBQy9DLElBQUksR0FBRyxFQUFFO3dCQUNQLGFBQWEsQ0FBQyxJQUFJLE9BQWxCLGFBQWEsbUJBQVMsb0NBQXNCLENBQUMsR0FBRyxDQUFDLEdBQUU7cUJBQ3BEO2lCQUNGOzs7Ozs7Ozs7WUFFRCxJQUFNLFlBQVksR0FBRyxJQUFJLENB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LENBQUM7WUFDekQsT0FBTyxrQkFBVSxDQUFDLFFBQVEsRUFBRSxZQUFZLEVBQUUsZUFBZSxDQUFDLENBQUM7UUFDN0QsQ0FBQztRQUVELHFEQUF1QixHQUF2QixVQUF3QixRQUFnQixFQUFFLFFBQWdCO1lBQ3hELElBQU0sVUFBVSxHQUFHLElBQUksQ0FBQyx5QkFBeUIsQ0FBQyxRQUFRLEVBQUUsUUFBUSxDQUFDLENBQUM7WUFDdEUsSUFBSSxFQUFDLFVBQVUsYUFBVixVQUFVLHVCQUFWLFVBQVUsQ0FBRSxXQUFXLENBQUEsRUFBRTtnQkFDNUIsT0FBTzthQUNSO1lBQ00sSUFBQSxXQUFXLEdBQUksVUFBVSxZQUFkLENBQWU7WUFDakMsSUFBTSxLQUFLLEdBQUcsV0FBVyxDQUFDLElBQUksQ0FBQyxVQUFBLEdBQUcsSUFBSSxPQUFBLEdBQUcsQ0FBQyxRQUFRLENBQUMsUUFBUSxDQUFDLEtBQUssQ0FBQyxFQUE1QixDQUE0QixDQUFDLENBQUM7WUFDcEUsSUFBSSxDQUFDLEtBQUssRUFBRTtnQkFDVixPQUFPO2FBQ1I7WUFDRCxPQUFPLElBQUksQ0FBQyxJQUFJLENBQUMsSUFBSSxDQUFDLHVCQUF1QixDQUFDLEtBQUssQ0FBQyxRQUFRLEVBQUUsS0FBSyxDQUFDLFFBQVEsQ0FBQyxLQUFLLENBQUMsQ0FBQztRQUN0RixDQUFDO1FBQ0gsMEJBQUM7SUFBRCxDQUFDLEFBdkZELElBdUZDIiwic291cmNlc0NvbnRlbnQiOlsiLyoqXG4gKiBAbGljZW5zZVxuICogQ29weXJpZ2h0IEdvb2dsZSBMTEMgQWxsIFJpZ2h0cyBSZXNlcnZlZC5cbiAqXG4gKiBVc2Ugb2YgdGhpcyBzb3VyY2UgY29kZSBpcyBnb3Zlcm5lZCBieSBhbiBNSVQtc3R5bGUgbGljZW5zZSB0aGF0IGNhbiBiZVxuICogZm91bmQgaW4gdGhlIExJQ0VOU0UgZmlsZSBhdCBodHRwczovL2FuZ3VsYXIuaW8vbGljZW5zZVxuICovXG5cbmltcG9ydCAqIGFzIHRzcyBmcm9tICd0eXBlc2NyaXB0L2xpYi90c3NlcnZlcmxpYnJhcnknO1xuXG5pbXBvcnQge2dldFRlbXBsYXRlQ29tcGxldGlvbnN9IGZyb20gJy4vY29tcGxldGlvbnMnO1xuaW1wb3J0IHtnZXREZWZpbml0aW9uQW5kQm91bmRTcGFuLCBnZXRUc0RlZmluaXRpb25BbmRCb3VuZFNwYW59IGZyb20gJy4vZGVmaW5pdGlvbnMnO1xuaW1wb3J0IHtnZXREZWNsYXJhdGlvbkRpYWdub3N0aWNzLCBnZXRUZW1wbGF0ZURpYWdub3N0aWNzLCBuZ0RpYWdub3N0aWNUb1RzRGlhZ25vc3RpY30gZnJvbSAnLi9kaWFnbm9zdGljcyc7XG5pbXBvcnQge2dldFRlbXBsYXRlSG92ZXIsIGdldFRzSG92ZXJ9IGZyb20gJy4vaG92ZXInO1xuaW1wb3J0ICogYXMgbmcgZnJvbSAnLi90eXBlcyc7XG5pbXBvcnQge1R5cGVTY3JpcHRTZXJ2aWNlSG9zdH0gZnJvbSAnLi90eXBlc2NyaXB0X2hvc3QnO1xuXG4vKipcbiAqIENyZWF0ZSBhbiBpbnN0YW5jZSBvZiBhbiBBbmd1bGFyIGBMYW5ndWFnZVNlcnZpY2VgLlxuICpcbiAqIEBwdWJsaWNBcGlcbiAqL1xuZXhwb3J0IGZ1bmN0aW9uIGNyZWF0ZUxhbmd1YWdlU2VydmljZShob3N0OiBUeXBlU2NyaXB0U2VydmljZUhvc3QpIHtcbiAgcmV0dXJuIG5ldyBMYW5ndWFnZVNlcnZpY2VJbXBsKGhvc3QpO1xufVxuXG5jbGFzcyBMYW5ndWFnZVNlcnZpY2VJbXBsIGltcGxlbWVudHMgbmcuTGFuZ3VhZ2VTZXJ2aWNlIHtcbiAgY29uc3RydWN0b3IocHJpdmF0ZSByZWFkb25seSBob3N0OiBUeXBlU2NyaXB0U2VydmljZUhvc3QpIHt9XG5cbiAgZ2V0U2VtYW50aWNEaWFnbm9zdGljcyhmaWxlTmFtZTogc3RyaW5nKTogdHNzLkRpYWdub3N0aWNbXSB7XG4gICAgY29uc3QgYW5hbHl6ZWRNb2R1bGVzID0gdGhpcy5ob3N0LmdldEFuYWx5emVkTW9kdWxlcygpOyAgLy8gc2FtZSByb2xlIGFzICdzeW5jaHJvbml6ZUhvc3REYXRhJ1xuICAgIGNvbnN0IG5nRGlhZ25vc3RpY3M6IG5nLkRpYWdub3N0aWNbXSA9IFtdO1xuXG4gICAgY29uc3QgdGVtcGxhdGVzID0gdGhpcy5ob3N0LmdldFRlbXBsYXRlcyhmaWxlTmFtZSk7XG4gICAgZm9yIChjb25zdCB0ZW1wbGF0ZSBvZiB0ZW1wbGF0ZXMpIHtcbiAgICAgIGNvbnN0IGFzdCA9IHRoaXMuaG9zdC5nZXRUZW1wbGF0ZUFzdCh0ZW1wbGF0ZSk7XG4gICAgICBpZiAoYXN0KSB7XG4gICAgICAgIG5nRGlhZ25vc3RpY3MucHVzaCguLi5nZXRUZW1wbGF0ZURpYWdub3N0aWNzKGFzdCkpO1xuICAgICAgfVxuICAgIH1cblxuICAgIGNvbnN0IGRlY2xhcmF0aW9ucyA9IHRoaXMuaG9zdC5nZXREZWNsYXJhdGlvbnMoZmlsZU5hbWUpO1xuICAgIG5nRGlhZ25vc3RpY3MucHVzaCguLi5nZXREZWNsYXJhdGlvbkRpYWdub3N0aWNzKGRlY2xhcmF0aW9ucywgYW5hbHl6ZWRNb2R1bGVzLCB0aGlzLmhvc3QpKTtcblxuICAgIGNvbnN0IHNvdXJjZUZpbGUgPSBmaWxlTmFtZS5lbmRzV2l0aCgnLnRzJykgPyB0aGlzLmhvc3QuZ2V0U291cmNlRmlsZShmaWxlTmFtZSkgOiB1bmRlZmluZWQ7XG4gICAgY29uc3QgdHNEaWFnbm9zdGljcyA9IG5nRGlhZ25vc3RpY3MubWFwKGQgPT4gbmdEaWFnbm9zdGljVG9Uc0RpYWdub3N0aWMoZCwgc291cmNlRmlsZSkpO1xuICAgIHJldHVybiBbLi4udHNzLnNvcnRBbmREZWR1cGxpY2F0ZURpYWdub3N0aWNzKHRzRGlhZ25vc3RpY3MpXTtcbiAgfVxuXG4gIGdldENvbXBsZXRpb25zQXRQb3NpdGlvbihcbiAgICAgIGZpbGVOYW1lOiBzdHJpbmcsIHBvc2l0aW9uOiBudW1iZXIsXG4gICAgICBfb3B0aW9ucz86IHRzcy5HZXRDb21wbGV0aW9uc0F0UG9zaXRpb25PcHRpb25zKTogdHNzLkNvbXBsZXRpb25JbmZvfHVuZGVmaW5lZCB7XG4gICAgdGhpcy5ob3N0LmdldEFuYWx5emVkTW9kdWxlcygpOyAgLy8gc2FtZSByb2xlIGFzICdzeW5jaHJvbml6ZUhvc3REYXRhJ1xuICAgIGNvbnN0IGFzdCA9IHRoaXMuaG9zdC5nZXRUZW1wbGF0ZUFzdEF0UG9zaXRpb24oZmlsZU5hbWUsIHBvc2l0aW9uKTtcbiAgICBpZiAoIWFzdCkge1xuICAgICAgcmV0dXJuO1xuICAgIH1cbiAgICBjb25zdCByZXN1bHRzID0gZ2V0VGVtcGxhdGVDb21wbGV0aW9ucyhhc3QsIHBvc2l0aW9uKTtcbiAgICBpZiAoIXJlc3VsdHMgfHwgIXJlc3VsdHMubGVuZ3RoKSB7XG4gICAgICByZXR1cm47XG4gICAgfVxuICAgIHJldHVybiB7XG4gICAgICBpc0dsb2JhbENvbXBsZXRpb246IGZhbHNlLFxuICAgICAgaXNNZW1iZXJDb21wbGV0aW9uOiBmYWxzZSxcbiAgICAgIGlzTmV3SWRlbnRpZmllckxvY2F0aW9uOiBmYWxzZSxcbiAgICAgIC8vIENhc3QgQ29tcGxldGlvbkVudHJ5LmtpbmQgZnJvbSBuZy5Db21wbGV0aW9uS2luZCB0byB0cy5TY3JpcHRFbGVtZW50S2luZFxuICAgICAgZW50cmllczogcmVzdWx0cyBhcyB1bmtub3duIGFzIHRzLkNvbXBsZXRpb25FbnRyeVtdLFxuICAgIH07XG4gIH1cblxuICBnZXREZWZpbml0aW9uQW5kQm91bmRTcGFuKGZpbGVOYW1lOiBzdHJpbmcsIHBvc2l0aW9uOiBudW1iZXIpOiB0c3MuRGVmaW5pdGlvbkluZm9BbmRCb3VuZFNwYW5cbiAgICAgIHx1bmRlZmluZWQge1xuICAgIHRoaXMuaG9zdC5nZXRBbmFseXplZE1vZHVsZXMoKTsgIC8vIHNhbWUgcm9sZSBhcyAnc3luY2hyb25pemVIb3N0RGF0YSdcbiAgICBjb25zdCB0ZW1wbGF0ZUluZm8gPSB0aGlzLmhvc3QuZ2V0VGVtcGxhdGVBc3RBdFBvc2l0aW9uKGZpbGVOYW1lLCBwb3NpdGlvbik7XG4gICAgaWYgKHRlbXBsYXRlSW5mbykge1xuICAgICAgcmV0dXJuIGdldERlZmluaXRpb25BbmRCb3VuZFNwYW4odGVtcGxhdGVJbmZvLCBwb3NpdGlvbik7XG4gICAgfVxuXG4gICAgLy8gQXR0ZW1wdCB0byBnZXQgQW5ndWxhci1zcGVjaWZpYyBkZWZpbml0aW9ucyBpbiBhIFR5cGVTY3JpcHQgZmlsZSwgbGlrZSB0ZW1wbGF0ZXMgZGVmaW5lZFxuICAgIC8vIGluIGEgYHRlbXBsYXRlVXJsYCBwcm9wZXJ0eS5cbiAgICBpZiAoZmlsZU5hbWUuZW5kc1dpdGgoJy50cycpKSB7XG4gICAgICBjb25zdCBzZiA9IHRoaXMuaG9zdC5nZXRTb3VyY2VGaWxlKGZpbGVOYW1lKTtcbiAgICAgIGlmIChzZikge1xuICAgICAgICByZXR1cm4gZ2V0VHNEZWZpbml0aW9uQW5kQm91bmRTcGFuKHNmLCBwb3NpdGlvbiwgdGhpcy5ob3N0LnRzTHNIb3N0KTtcbiAgICAgIH1cbiAgICB9XG4gIH1cblxuICBnZXRRdWlja0luZm9BdFBvc2l0aW9uKGZpbGVOYW1lOiBzdHJpbmcsIHBvc2l0aW9uOiBudW1iZXIpOiB0c3MuUXVpY2tJbmZvfHVuZGVmaW5lZCB7XG4gICAgY29uc3QgYW5hbHl6ZWRNb2R1bGVzID0gdGhpcy5ob3N0LmdldEFuYWx5emVkTW9kdWxlcygpOyAgLy8gc2FtZSByb2xlIGFzICdzeW5jaHJvbml6ZUhvc3REYXRhJ1xuICAgIGNvbnN0IHRlbXBsYXRlSW5mbyA9IHRoaXMuaG9zdC5nZXRUZW1wbGF0ZUFzdEF0UG9zaXRpb24oZmlsZU5hbWUsIHBvc2l0aW9uKTtcbiAgICBpZiAodGVtcGxhdGVJbmZvKSB7XG4gICAgICByZXR1cm4gZ2V0VGVtcGxhdGVIb3Zlcih0ZW1wbGF0ZUluZm8sIHBvc2l0aW9uLCBhbmFseXplZE1vZHVsZXMpO1xuICAgIH1cblxuICAgIC8vIEF0dGVtcHQgdG8gZ2V0IEFuZ3VsYXItc3BlY2lmaWMgaG92ZXIgaW5mb3JtYXRpb24gaW4gYSBUeXBlU2NyaXB0IGZpbGUsIHRoZSBOZ01vZHVsZSBhXG4gICAgLy8gZGlyZWN0aXZlIGJlbG9uZ3MgdG8uXG4gICAgY29uc3QgZGVjbGFyYXRpb25zID0gdGhpcy5ob3N0LmdldERlY2xhcmF0aW9ucyhmaWxlTmFtZSk7XG4gICAgcmV0dXJuIGdldFRzSG92ZXIocG9zaXRpb24sIGRlY2xhcmF0aW9ucywgYW5hbHl6ZWRNb2R1bGVzKTtcbiAgfVxuXG4gIGdldFJlZmVyZW5jZXNBdFBvc2l0aW9uKGZpbGVOYW1lOiBzdHJpbmcsIHBvc2l0aW9uOiBudW1iZXIpOiB0c3MuUmVmZXJlbmNlRW50cnlbXXx1bmRlZmluZWQge1xuICAgIGNvbnN0IGRlZkFuZFNwYW4gPSB0aGlzLmdldERlZmluaXRpb25BbmRCb3VuZFNwYW4oZmlsZU5hbWUsIHBvc2l0aW9uKTtcbiAgICBpZiAoIWRlZkFuZFNwYW4/LmRlZmluaXRpb25zKSB7XG4gICAgICByZXR1cm47XG4gICAgfVxuICAgIGNvbnN0IHtkZWZpbml0aW9uc30gPSBkZWZBbmRTcGFuO1xuICAgIGNvbnN0IHRzRGVmID0gZGVmaW5pdGlvbnMuZmluZChkZWYgPT4gZGVmLmZpbGVOYW1lLmVuZHNXaXRoKCcudHMnKSk7XG4gICAgaWYgKCF0c0RlZikge1xuICAgICAgcmV0dXJuO1xuICAgIH1cbiAgICByZXR1cm4gdGhpcy5ob3N0LnRzTFMuZ2V0UmVmZXJlbmNlc0F0UG9zaXRpb24odHNEZWYuZmlsZU5hbWUsIHRzRGVmLnRleHRTcGFuLnN0YXJ0KTtcbiAgfVxufVxuIl19