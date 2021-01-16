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
        define("@angular/language-service/src/expression_diagnostics", ["require", "exports", "tslib", "@angular/compiler", "@angular/language-service/src/diagnostic_messages", "@angular/language-service/src/expression_type", "@angular/language-service/src/symbols", "@angular/language-service/src/utils"], factory);
    }
})(function (require, exports) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    exports.getExpressionScope = exports.getTemplateExpressionDiagnostics = void 0;
    var tslib_1 = require("tslib");
    var compiler_1 = require("@angular/compiler");
    var diagnostic_messages_1 = require("@angular/language-service/src/diagnostic_messages");
    var expression_type_1 = require("@angular/language-service/src/expression_type");
    var symbols_1 = require("@angular/language-service/src/symbols");
    var utils_1 = require("@angular/language-service/src/utils");
    function getTemplateExpressionDiagnostics(info) {
        var visitor = new ExpressionDiagnosticsVisitor(info, function (path) { return getExpressionScope(info, path); });
        compiler_1.templateVisitAll(visitor, info.templateAst);
        return visitor.diagnostics;
    }
    exports.getTemplateExpressionDiagnostics = getTemplateExpressionDiagnostics;
    function getReferences(info) {
        var result = [];
        function processReferences(references) {
            var e_1, _a;
            var _loop_1 = function (reference) {
                var type = undefined;
                if (reference.value) {
                    type = info.query.getTypeSymbol(compiler_1.tokenReference(reference.value));
                }
                result.push({
                    name: reference.name,
                    kind: 'reference',
                    type: type || info.query.getBuiltinType(symbols_1.BuiltinType.Any),
                    get definition() {
                        return getDefinitionOf(info, reference);
                    }
                });
            };
            try {
                for (var references_1 = tslib_1.__values(references), references_1_1 = references_1.next(); !references_1_1.done; references_1_1 = references_1.next()) {
                    var reference = references_1_1.value;
                    _loop_1(reference);
                }
            }
            catch (e_1_1) { e_1 = { error: e_1_1 }; }
            finally {
                try {
                    if (references_1_1 && !references_1_1.done && (_a = references_1.return)) _a.call(references_1);
                }
                finally { if (e_1) throw e_1.error; }
            }
        }
        var visitor = new /** @class */ (function (_super) {
            tslib_1.__extends(class_1, _super);
            function class_1() {
                return _super !== null && _super.apply(this, arguments) || this;
            }
            class_1.prototype.visitEmbeddedTemplate = function (ast, context) {
                _super.prototype.visitEmbeddedTemplate.call(this, ast, context);
                processReferences(ast.references);
            };
            class_1.prototype.visitElement = function (ast, context) {
                _super.prototype.visitElement.call(this, ast, context);
                processReferences(ast.references);
            };
            return class_1;
        }(compiler_1.RecursiveTemplateAstVisitor));
        compiler_1.templateVisitAll(visitor, info.templateAst);
        return result;
    }
    function getDefinitionOf(info, ast) {
        if (info.fileName) {
            var templateOffset = info.offset;
            return [{
                    fileName: info.fileName,
                    span: {
                        start: ast.sourceSpan.start.offset + templateOffset,
                        end: ast.sourceSpan.end.offset + templateOffset
                    }
                }];
        }
    }
    /**
     * Resolve all variable declarations in a template by traversing the specified
     * `path`.
     * @param info
     * @param path template AST path
     */
    function getVarDeclarations(info, path) {
        var e_2, _a;
        var results = [];
        for (var current = path.head; current; current = path.childOf(current)) {
            if (!(current instanceof compiler_1.EmbeddedTemplateAst)) {
                continue;
            }
            var _loop_2 = function (variable) {
                var symbol = getVariableTypeFromDirectiveContext(variable.value, info.query, current);
                var kind = info.query.getTypeKind(symbol);
                if (kind === symbols_1.BuiltinType.Any || kind === symbols_1.BuiltinType.Unbound) {
                    // For special cases such as ngFor and ngIf, the any type is not very useful.
                    // We can do better by resolving the binding value.
                    var symbolsInScope = info.query.mergeSymbolTable([
                        info.members,
                        // Since we are traversing the AST path from head to tail, any variables
                        // that have been declared so far are also in scope.
                        info.query.createSymbolTable(results),
                    ]);
                    symbol = refinedVariableType(variable.value, symbolsInScope, info, current);
                }
                results.push({
                    name: variable.name,
                    kind: 'variable',
                    type: symbol,
                    get definition() {
                        return getDefinitionOf(info, variable);
                    },
                });
            };
            try {
                for (var _b = (e_2 = void 0, tslib_1.__values(current.variables)), _c = _b.next(); !_c.done; _c = _b.next()) {
                    var variable = _c.value;
                    _loop_2(variable);
                }
            }
            catch (e_2_1) { e_2 = { error: e_2_1 }; }
            finally {
                try {
                    if (_c && !_c.done && (_a = _b.return)) _a.call(_b);
                }
                finally { if (e_2) throw e_2.error; }
            }
        }
        return results;
    }
    /**
     * Resolve the type for the variable in `templateElement` by finding the structural
     * directive which has the context member. Returns any when not found.
     * @param value variable value name
     * @param query type symbol query
     * @param templateElement
     */
    function getVariableTypeFromDirectiveContext(value, query, templateElement) {
        var e_3, _a;
        try {
            for (var _b = tslib_1.__values(templateElement.directives), _c = _b.next(); !_c.done; _c = _b.next()) {
                var directive = _c.value.directive;
                var context = query.getTemplateContext(directive.type.reference);
                if (context) {
                    var member = context.get(value);
                    if (member && member.type) {
                        return member.type;
                    }
                }
            }
        }
        catch (e_3_1) { e_3 = { error: e_3_1 }; }
        finally {
            try {
                if (_c && !_c.done && (_a = _b.return)) _a.call(_b);
            }
            finally { if (e_3) throw e_3.error; }
        }
        return query.getBuiltinType(symbols_1.BuiltinType.Any);
    }
    /**
     * Resolve a more specific type for the variable in `templateElement` by inspecting
     * all variables that are in scope in the `mergedTable`. This function is a special
     * case for `ngFor` and `ngIf`. If resolution fails, return the `any` type.
     * @param value variable value name
     * @param mergedTable symbol table for all variables in scope
     * @param info available template information
     * @param templateElement
     */
    function refinedVariableType(value, mergedTable, info, templateElement) {
        if (value === '$implicit') {
            // Special case: ngFor directive
            var ngForDirective = templateElement.directives.find(function (d) {
                var name = compiler_1.identifierName(d.directive.type);
                return name == 'NgFor' || name == 'NgForOf';
            });
            if (ngForDirective) {
                var ngForOfBinding = ngForDirective.inputs.find(function (i) { return i.directiveName == 'ngForOf'; });
                if (ngForOfBinding) {
                    // Check if there is a known type for the ngFor binding.
                    var bindingType = new expression_type_1.AstType(mergedTable, info.query, {}, info.source).getType(ngForOfBinding.value);
                    if (bindingType) {
                        var result = info.query.getElementType(bindingType);
                        if (result) {
                            return result;
                        }
                    }
                }
            }
        }
        if (value === 'ngIf' || value === '$implicit') {
            var ngIfDirective = templateElement.directives.find(function (d) { return compiler_1.identifierName(d.directive.type) === 'NgIf'; });
            if (ngIfDirective) {
                // Special case: ngIf directive. The NgIf structural directive owns a template context with
                // "$implicit" and "ngIf" members. These properties are typed as generics. Until the language
                // service uses an Ivy and TypecheckBlock backend, we cannot bind these values to a concrete
                // type without manual inference. To get the concrete type, look up the type of the "ngIf"
                // import on the NgIf directive bound to the template.
                //
                // See @angular/common/ng_if.ts for more information.
                var ngIfBinding = ngIfDirective.inputs.find(function (i) { return i.directiveName === 'ngIf'; });
                if (ngIfBinding) {
                    // Check if there is a known type bound to the ngIf input.
                    var bindingType = new expression_type_1.AstType(mergedTable, info.query, {}, info.source).getType(ngIfBinding.value);
                    if (bindingType) {
                        return bindingType;
                    }
                }
            }
        }
        // We can't do better, return any
        return info.query.getBuiltinType(symbols_1.BuiltinType.Any);
    }
    function getEventDeclaration(info, path) {
        var event = path.tail;
        if (!(event instanceof compiler_1.BoundEventAst)) {
            // No event available in this context.
            return;
        }
        var genericEvent = {
            name: '$event',
            kind: 'variable',
            type: info.query.getBuiltinType(symbols_1.BuiltinType.Any),
        };
        var outputSymbol = utils_1.findOutputBinding(event, path, info.query);
        if (!outputSymbol) {
            // The `$event` variable doesn't belong to an output, so its type can't be refined.
            // TODO: type `$event` variables in bindings to DOM events.
            return genericEvent;
        }
        // The raw event type is wrapped in a generic, like EventEmitter<T> or Observable<T>.
        var ta = outputSymbol.typeArguments();
        if (!ta || ta.length !== 1)
            return genericEvent;
        var eventType = ta[0];
        return tslib_1.__assign(tslib_1.__assign({}, genericEvent), { type: eventType });
    }
    /**
     * Returns the symbols available in a particular scope of a template.
     * @param info parsed template information
     * @param path path of template nodes narrowing to the context the expression scope should be
     * derived for.
     */
    function getExpressionScope(info, path) {
        var result = info.members;
        var references = getReferences(info);
        var variables = getVarDeclarations(info, path);
        var event = getEventDeclaration(info, path);
        if (references.length || variables.length || event) {
            var referenceTable = info.query.createSymbolTable(references);
            var variableTable = info.query.createSymbolTable(variables);
            var eventsTable = info.query.createSymbolTable(event ? [event] : []);
            result = info.query.mergeSymbolTable([result, referenceTable, variableTable, eventsTable]);
        }
        return result;
    }
    exports.getExpressionScope = getExpressionScope;
    var ExpressionDiagnosticsVisitor = /** @class */ (function (_super) {
        tslib_1.__extends(ExpressionDiagnosticsVisitor, _super);
        function ExpressionDiagnosticsVisitor(info, getExpressionScope) {
            var _this = _super.call(this) || this;
            _this.info = info;
            _this.getExpressionScope = getExpressionScope;
            _this.diagnostics = [];
            _this.path = new compiler_1.AstPath([]);
            return _this;
        }
        ExpressionDiagnosticsVisitor.prototype.visitDirective = function (ast, context) {
            // Override the default child visitor to ignore the host properties of a directive.
            if (ast.inputs && ast.inputs.length) {
                compiler_1.templateVisitAll(this, ast.inputs, context);
            }
        };
        ExpressionDiagnosticsVisitor.prototype.visitBoundText = function (ast) {
            this.push(ast);
            this.diagnoseExpression(ast.value, ast.sourceSpan.start.offset, false);
            this.pop();
        };
        ExpressionDiagnosticsVisitor.prototype.visitDirectiveProperty = function (ast) {
            this.push(ast);
            this.diagnoseExpression(ast.value, this.attributeValueLocation(ast), false);
            this.pop();
        };
        ExpressionDiagnosticsVisitor.prototype.visitElementProperty = function (ast) {
            this.push(ast);
            this.diagnoseExpression(ast.value, this.attributeValueLocation(ast), false);
            this.pop();
        };
        ExpressionDiagnosticsVisitor.prototype.visitEvent = function (ast) {
            this.push(ast);
            this.diagnoseExpression(ast.handler, this.attributeValueLocation(ast), true);
            this.pop();
        };
        ExpressionDiagnosticsVisitor.prototype.visitVariable = function (ast) {
            var directive = this.directiveSummary;
            if (directive && ast.value) {
                var context = this.info.query.getTemplateContext(directive.type.reference);
                if (context && !context.has(ast.value)) {
                    var missingMember = ast.value === '$implicit' ? 'an implicit value' : "a member called '" + ast.value + "'";
                    var span = this.absSpan(spanOf(ast.sourceSpan));
                    this.diagnostics.push(diagnostic_messages_1.createDiagnostic(span, diagnostic_messages_1.Diagnostic.template_context_missing_member, directive.type.reference.name, missingMember));
                }
            }
        };
        ExpressionDiagnosticsVisitor.prototype.visitElement = function (ast, context) {
            this.push(ast);
            _super.prototype.visitElement.call(this, ast, context);
            this.pop();
        };
        ExpressionDiagnosticsVisitor.prototype.visitEmbeddedTemplate = function (ast, context) {
            var previousDirectiveSummary = this.directiveSummary;
            this.push(ast);
            // Find directive that references this template
            this.directiveSummary =
                ast.directives.map(function (d) { return d.directive; }).find(function (d) { return hasTemplateReference(d.type); });
            // Process children
            _super.prototype.visitEmbeddedTemplate.call(this, ast, context);
            this.pop();
            this.directiveSummary = previousDirectiveSummary;
        };
        ExpressionDiagnosticsVisitor.prototype.attributeValueLocation = function (ast) {
            var path = utils_1.getPathToNodeAtPosition(this.info.htmlAst, ast.sourceSpan.start.offset);
            var last = path.tail;
            if (last instanceof compiler_1.Attribute && last.valueSpan) {
                return last.valueSpan.start.offset;
            }
            return ast.sourceSpan.start.offset;
        };
        ExpressionDiagnosticsVisitor.prototype.diagnoseExpression = function (ast, offset, inEvent) {
            var e_4, _a;
            var scope = this.getExpressionScope(this.path, inEvent);
            var analyzer = new expression_type_1.AstType(scope, this.info.query, { inEvent: inEvent }, this.info.source);
            try {
                for (var _b = tslib_1.__values(analyzer.getDiagnostics(ast)), _c = _b.next(); !_c.done; _c = _b.next()) {
                    var diagnostic = _c.value;
                    diagnostic.span = this.absSpan(diagnostic.span, offset);
                    this.diagnostics.push(diagnostic);
                }
            }
            catch (e_4_1) { e_4 = { error: e_4_1 }; }
            finally {
                try {
                    if (_c && !_c.done && (_a = _b.return)) _a.call(_b);
                }
                finally { if (e_4) throw e_4.error; }
            }
        };
        ExpressionDiagnosticsVisitor.prototype.push = function (ast) {
            this.path.push(ast);
        };
        ExpressionDiagnosticsVisitor.prototype.pop = function () {
            this.path.pop();
        };
        ExpressionDiagnosticsVisitor.prototype.absSpan = function (span, additionalOffset) {
            if (additionalOffset === void 0) { additionalOffset = 0; }
            return {
                start: span.start + this.info.offset + additionalOffset,
                end: span.end + this.info.offset + additionalOffset,
            };
        };
        return ExpressionDiagnosticsVisitor;
    }(compiler_1.RecursiveTemplateAstVisitor));
    function hasTemplateReference(type) {
        var e_5, _a;
        if (type.diDeps) {
            try {
                for (var _b = tslib_1.__values(type.diDeps), _c = _b.next(); !_c.done; _c = _b.next()) {
                    var diDep = _c.value;
                    if (diDep.token && diDep.token.identifier &&
                        compiler_1.identifierName(diDep.token.identifier) == 'TemplateRef')
                        return true;
                }
            }
            catch (e_5_1) { e_5 = { error: e_5_1 }; }
            finally {
                try {
                    if (_c && !_c.done && (_a = _b.return)) _a.call(_b);
                }
                finally { if (e_5) throw e_5.error; }
            }
        }
        return false;
    }
    function spanOf(sourceSpan) {
        return { start: sourceSpan.start.offset, end: sourceSpan.end.offset };
    }
});
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiZXhwcmVzc2lvbl9kaWFnbm9zdGljcy5qcyIsInNvdXJjZVJvb3QiOiIiLCJzb3VyY2VzIjpbIi4uLy4uLy4uLy4uLy4uLy4uL3BhY2thZ2VzL2xhbmd1YWdlLXNlcnZpY2Uvc3JjL2V4cHJlc3Npb25fZGlhZ25vc3RpY3MudHMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IkFBQUE7Ozs7OztHQU1HOzs7Ozs7Ozs7Ozs7OztJQUVILDhDQUFpWTtJQUVqWSx5RkFBbUU7SUFDbkUsaUZBQTBDO0lBQzFDLGlFQUE2RztJQUU3Ryw2REFBbUU7SUFFbkUsU0FBZ0IsZ0NBQWdDLENBQUMsSUFBK0I7UUFDOUUsSUFBTSxPQUFPLEdBQUcsSUFBSSw0QkFBNEIsQ0FDNUMsSUFBSSxFQUFFLFVBQUMsSUFBcUIsSUFBSyxPQUFBLGtCQUFrQixDQUFDLElBQUksRUFBRSxJQUFJLENBQUMsRUFBOUIsQ0FBOEIsQ0FBQyxDQUFDO1FBQ3JFLDJCQUFnQixDQUFDLE9BQU8sRUFBRSxJQUFJLENBQUMsV0FBVyxDQUFDLENBQUM7UUFDNUMsT0FBTyxPQUFPLENBQUMsV0FBVyxDQUFDO0lBQzdCLENBQUM7SUFMRCw0RUFLQztJQUVELFNBQVMsYUFBYSxDQUFDLElBQStCO1FBQ3BELElBQU0sTUFBTSxHQUF3QixFQUFFLENBQUM7UUFFdkMsU0FBUyxpQkFBaUIsQ0FBQyxVQUEwQjs7b0NBQ3hDLFNBQVM7Z0JBQ2xCLElBQUksSUFBSSxHQUFxQixTQUFTLENBQUM7Z0JBQ3ZDLElBQUksU0FBUyxDQUFDLEtBQUssRUFBRTtvQkFDbkIsSUFBSSxHQUFHLElBQUksQ0FBQyxLQUFLLENBQUMsYUFBYSxDQUFDLHlCQUFjLENBQUMsU0FBUyxDQUFDLEtBQUssQ0FBQyxDQUFDLENBQUM7aUJBQ2xFO2dCQUNELE1BQU0sQ0FBQyxJQUFJLENBQUM7b0JBQ1YsSUFBSSxFQUFFLFNBQVMsQ0FBQyxJQUFJO29CQUNwQixJQUFJLEVBQUUsV0FBVztvQkFDakIsSUFBSSxFQUFFLElBQUksSUFBSSxJQUFJLENB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QUMsTUFBTSxFQUFDLENBQUM7SUFDdEUsQ0FBQyIsInNvdXJjZXNDb250ZW50IjpbIi8qKlxuICogQGxpY2Vuc2VcbiAqIENvcHlyaWdodCBHb29nbGUgTExDIEFsbCBSaWdodHMgUmVzZXJ2ZWQuXG4gKlxuICogVXNlIG9mIHRoaXMgc291cmNlIGNvZGUgaXMgZ292ZXJuZWQgYnkgYW4gTUlULXN0eWxlIGxpY2Vuc2UgdGhhdCBjYW4gYmVcbiAqIGZvdW5kIGluIHRoZSBMSUNFTlNFIGZpbGUgYXQgaHR0cHM6Ly9hbmd1bGFyLmlvL2xpY2Vuc2VcbiAqL1xuXG5pbXBvcnQge0FTVCwgQXN0UGF0aCwgQXR0cmlidXRlLCBCb3VuZERpcmVjdGl2ZVByb3BlcnR5QXN0LCBCb3VuZEVsZW1lbnRQcm9wZXJ0eUFzdCwgQm91bmRFdmVudEFzdCwgQm91bmRUZXh0QXN0LCBDb21waWxlRGlyZWN0aXZlU3VtbWFyeSwgQ29tcGlsZVR5cGVNZXRhZGF0YSwgRGlyZWN0aXZlQXN0LCBFbGVtZW50QXN0LCBFbWJlZGRlZFRlbXBsYXRlQXN0LCBpZGVudGlmaWVyTmFtZSwgUGFyc2VTb3VyY2VTcGFuLCBSZWN1cnNpdmVUZW1wbGF0ZUFzdFZpc2l0b3IsIFJlZmVyZW5jZUFzdCwgVGVtcGxhdGVBc3QsIFRlbXBsYXRlQXN0UGF0aCwgdGVtcGxhdGVWaXNpdEFsbCwgdG9rZW5SZWZlcmVuY2UsIFZhcmlhYmxlQXN0fSBmcm9tICdAYW5ndWxhci9jb21waWxlcic7XG5cbmltcG9ydCB7Y3JlYXRlRGlhZ25vc3RpYywgRGlhZ25vc3RpY30gZnJvbSAnLi9kaWFnbm9zdGljX21lc3NhZ2VzJztcbmltcG9ydCB7QXN0VHlwZX0gZnJvbSAnLi9leHByZXNzaW9uX3R5cGUnO1xuaW1wb3J0IHtCdWlsdGluVHlwZSwgRGVmaW5pdGlvbiwgU3BhbiwgU3ltYm9sLCBTeW1ib2xEZWNsYXJhdGlvbiwgU3ltYm9sUXVlcnksIFN5bWJvbFRhYmxlfSBmcm9tICcuL3N5bWJvbHMnO1xuaW1wb3J0ICogYXMgbmcgZnJvbSAnLi90eXBlcyc7XG5pbXBvcnQge2ZpbmRPdXRwdXRCaW5kaW5nLCBnZXRQYXRoVG9Ob2RlQXRQb3NpdGlvbn0gZnJvbSAnLi91dGlscyc7XG5cbmV4cG9ydCBmdW5jdGlvbiBnZXRUZW1wbGF0ZUV4cHJlc3Npb25EaWFnbm9zdGljcyhpbmZvOiBuZy5EaWFnbm9zdGljVGVtcGxhdGVJbmZvKTogbmcuRGlhZ25vc3RpY1tdIHtcbiAgY29uc3QgdmlzaXRvciA9IG5ldyBFeHByZXNzaW9uRGlhZ25vc3RpY3NWaXNpdG9yKFxuICAgICAgaW5mbywgKHBhdGg6IFRlbXBsYXRlQXN0UGF0aCkgPT4gZ2V0RXhwcmVzc2lvblNjb3BlKGluZm8sIHBhdGgpKTtcbiAgdGVtcGxhdGVWaXNpdEFsbCh2aXNpdG9yLCBpbmZvLnRlbXBsYXRlQXN0KTtcbiAgcmV0dXJuIHZpc2l0b3IuZGlhZ25vc3RpY3M7XG59XG5cbmZ1bmN0aW9uIGdldFJlZmVyZW5jZXMoaW5mbzogbmcuRGlhZ25vc3RpY1RlbXBsYXRlSW5mbyk6IFN5bWJvbERlY2xhcmF0aW9uW10ge1xuICBjb25zdCByZXN1bHQ6IFN5bWJvbERlY2xhcmF0aW9uW10gPSBbXTtcblxuICBmdW5jdGlvbiBwcm9jZXNzUmVmZXJlbmNlcyhyZWZlcmVuY2VzOiBSZWZlcmVuY2VBc3RbXSkge1xuICAgIGZvciAoY29uc3QgcmVmZXJlbmNlIG9mIHJlZmVyZW5jZXMpIHtcbiAgICAgIGxldCB0eXBlOiBTeW1ib2x8dW5kZWZpbmVkID0gdW5kZWZpbmVkO1xuICAgICAgaWYgKHJlZmVyZW5jZS52YWx1ZSkge1xuICAgICAgICB0eXBlID0gaW5mby5xdWVyeS5nZXRUeXBlU3ltYm9sKHRva2VuUmVmZXJlbmNlKHJlZmVyZW5jZS52YWx1ZSkpO1xuICAgICAgfVxuICAgICAgcmVzdWx0LnB1c2goe1xuICAgICAgICBuYW1lOiByZWZlcmVuY2UubmFtZSxcbiAgICAgICAga2luZDogJ3JlZmVyZW5jZScsXG4gICAgICAgIHR5cGU6IHR5cGUgfHwgaW5mby5xdWVyeS5nZXRCdWlsdGluVHlwZShCdWlsdGluVHlwZS5BbnkpLFxuICAgICAgICBnZXQgZGVmaW5pdGlvbigpIHtcbiAgICAgICAgICByZXR1cm4gZ2V0RGVmaW5pdGlvbk9mKGluZm8sIHJlZmVyZW5jZSk7XG4gICAgICAgIH1cbiAgICAgIH0pO1xuICAgIH1cbiAgfVxuXG4gIGNvbnN0IHZpc2l0b3IgPSBuZXcgY2xhc3MgZXh0ZW5kcyBSZWN1cnNpdmVUZW1wbGF0ZUFzdFZpc2l0b3Ige1xuICAgIHZpc2l0RW1iZWRkZWRUZW1wbGF0ZShhc3Q6IEVtYmVkZGVkVGVtcGxhdGVBc3QsIGNvbnRleHQ6IGFueSk6IGFueSB7XG4gICAgICBzdXBlci52aXNpdEVtYmVkZGVkVGVtcGxhdGUoYXN0LCBjb250ZXh0KTtcbiAgICAgIHByb2Nlc3NSZWZlcmVuY2VzKGFzdC5yZWZlcmVuY2VzKTtcbiAgICB9XG4gICAgdmlzaXRFbGVtZW50KGFzdDogRWxlbWVudEFzdCwgY29udGV4dDogYW55KTogYW55IHtcbiAgICAgIHN1cGVyLnZpc2l0RWxlbWVudChhc3QsIGNvbnRleHQpO1xuICAgICAgcHJvY2Vzc1JlZmVyZW5jZXMoYXN0LnJlZmVyZW5jZXMpO1xuICAgIH1cbiAgfTtcblxuICB0ZW1wbGF0ZVZpc2l0QWxsKHZpc2l0b3IsIGluZm8udGVtcGxhdGVBc3QpO1xuXG4gIHJldHVybiByZXN1bHQ7XG59XG5cbmZ1bmN0aW9uIGdldERlZmluaXRpb25PZihpbmZvOiBuZy5EaWFnbm9zdGljVGVtcGxhdGVJbmZvLCBhc3Q6IFRlbXBsYXRlQXN0KTogRGVmaW5pdGlvbnx1bmRlZmluZWQge1xuICBpZiAoaW5mby5maWxlTmFtZSkge1xuICAgIGNvbnN0IHRlbXBsYXRlT2Zmc2V0ID0gaW5mby5vZmZzZXQ7XG4gICAgcmV0dXJuIFt7XG4gICAgICBmaWxlTmFtZTogaW5mby5maWxlTmFtZSxcbiAgICAgIHNwYW46IHtcbiAgICAgICAgc3RhcnQ6IGFzdC5zb3VyY2VTcGFuLnN0YXJ0Lm9mZnNldCArIHRlbXBsYXRlT2Zmc2V0LFxuICAgICAgICBlbmQ6IGFzdC5zb3VyY2VTcGFuLmVuZC5vZmZzZXQgKyB0ZW1wbGF0ZU9mZnNldFxuICAgICAgfVxuICAgIH1dO1xuICB9XG59XG5cbi8qKlxuICogUmVzb2x2ZSBhbGwgdmFyaWFibGUgZGVjbGFyYXRpb25zIGluIGEgdGVtcGxhdGUgYnkgdHJhdmVyc2luZyB0aGUgc3BlY2lmaWVkXG4gKiBgcGF0aGAuXG4gKiBAcGFyYW0gaW5mb1xuICogQHBhcmFtIHBhdGggdGVtcGxhdGUgQVNUIHBhdGhcbiAqL1xuZnVuY3Rpb24gZ2V0VmFyRGVjbGFyYXRpb25zKFxuICAgIGluZm86IG5nLkRpYWdub3N0aWNUZW1wbGF0ZUluZm8sIHBhdGg6IFRlbXBsYXRlQXN0UGF0aCk6IFN5bWJvbERlY2xhcmF0aW9uW10ge1xuICBjb25zdCByZXN1bHRzOiBTeW1ib2xEZWNsYXJhdGlvbltdID0gW107XG4gIGZvciAobGV0IGN1cnJlbnQgPSBwYXRoLmhlYWQ7IGN1cnJlbnQ7IGN1cnJlbnQgPSBwYXRoLmNoaWxkT2YoY3VycmVudCkpIHtcbiAgICBpZiAoIShjdXJyZW50IGluc3RhbmNlb2YgRW1iZWRkZWRUZW1wbGF0ZUFzdCkpIHtcbiAgICAgIGNvbnRpbnVlO1xuICAgIH1cbiAgICBmb3IgKGNvbnN0IHZhcmlhYmxlIG9mIGN1cnJlbnQudmFyaWFibGVzKSB7XG4gICAgICBsZXQgc3ltYm9sID0gZ2V0VmFyaWFibGVUeXBlRnJvbURpcmVjdGl2ZUNvbnRleHQodmFyaWFibGUudmFsdWUsIGluZm8ucXVlcnksIGN1cnJlbnQpO1xuXG4gICAgICBjb25zdCBraW5kID0gaW5mby5xdWVyeS5nZXRUeXBlS2luZChzeW1ib2wpO1xuICAgICAgaWYgKGtpbmQgPT09IEJ1aWx0aW5UeXBlLkFueSB8fCBraW5kID09PSBCdWlsdGluVHlwZS5VbmJvdW5kKSB7XG4gICAgICAgIC8vIEZvciBzcGVjaWFsIGNhc2VzIHN1Y2ggYXMgbmdGb3IgYW5kIG5nSWYsIHRoZSBhbnkgdHlwZSBpcyBub3QgdmVyeSB1c2VmdWwuXG4gICAgICAgIC8vIFdlIGNhbiBkbyBiZXR0ZXIgYnkgcmVzb2x2aW5nIHRoZSBiaW5kaW5nIHZhbHVlLlxuICAgICAgICBjb25zdCBzeW1ib2xzSW5TY29wZSA9IGluZm8ucXVlcnkubWVyZ2VTeW1ib2xUYWJsZShbXG4gICAgICAgICAgaW5mby5tZW1iZXJzLFxuICAgICAgICAgIC8vIFNpbmNlIHdlIGFyZSB0cmF2ZXJzaW5nIHRoZSBBU1QgcGF0aCBmcm9tIGhlYWQgdG8gdGFpbCwgYW55IHZhcmlhYmxlc1xuICAgICAgICAgIC8vIHRoYXQgaGF2ZSBiZWVuIGRlY2xhcmVkIHNvIGZhciBhcmUgYWxzbyBpbiBzY29wZS5cbiAgICAgICAgICBpbmZvLnF1ZXJ5LmNyZWF0ZVN5bWJvbFRhYmxlKHJlc3VsdHMpLFxuICAgICAgICBdKTtcbiAgICAgICAgc3ltYm9sID0gcmVmaW5lZFZhcmlhYmxlVHlwZSh2YXJpYWJsZS52YWx1ZSwgc3ltYm9sc0luU2NvcGUsIGluZm8sIGN1cnJlbnQpO1xuICAgICAgfVxuICAgICAgcmVzdWx0cy5wdXNoKHtcbiAgICAgICAgbmFtZTogdmFyaWFibGUubmFtZSxcbiAgICAgICAga2luZDogJ3ZhcmlhYmxlJyxcbiAgICAgICAgdHlwZTogc3ltYm9sLFxuICAgICAgICBnZXQgZGVmaW5pdGlvbigpIHtcbiAgICAgICAgICByZXR1cm4gZ2V0RGVmaW5pdGlvbk9mKGluZm8sIHZhcmlhYmxlKTtcbiAgICAgICAgfSxcbiAgICAgIH0pO1xuICAgIH1cbiAgfVxuICByZXR1cm4gcmVzdWx0cztcbn1cblxuLyoqXG4gKiBSZXNvbHZlIHRoZSB0eXBlIGZvciB0aGUgdmFyaWFibGUgaW4gYHRlbXBsYXRlRWxlbWVudGAgYnkgZmluZGluZyB0aGUgc3RydWN0dXJhbFxuICogZGlyZWN0aXZlIHdoaWNoIGhhcyB0aGUgY29udGV4dCBtZW1iZXIuIFJldHVybnMgYW55IHdoZW4gbm90IGZvdW5kLlxuICogQHBhcmFtIHZhbHVlIHZhcmlhYmxlIHZhbHVlIG5hbWVcbiAqIEBwYXJhbSBxdWVyeSB0eXBlIHN5bWJvbCBxdWVyeVxuICogQHBhcmFtIHRlbXBsYXRlRWxlbWVudFxuICovXG5mdW5jdGlvbiBnZXRWYXJpYWJsZVR5cGVGcm9tRGlyZWN0aXZlQ29udGV4dChcbiAgICB2YWx1ZTogc3RyaW5nLCBxdWVyeTogU3ltYm9sUXVlcnksIHRlbXBsYXRlRWxlbWVudDogRW1iZWRkZWRUZW1wbGF0ZUFzdCk6IFN5bWJvbCB7XG4gIGZvciAoY29uc3Qge2RpcmVjdGl2ZX0gb2YgdGVtcGxhdGVFbGVtZW50LmRpcmVjdGl2ZXMpIHtcbiAgICBjb25zdCBjb250ZXh0ID0gcXVlcnkuZ2V0VGVtcGxhdGVDb250ZXh0KGRpcmVjdGl2ZS50eXBlLnJlZmVyZW5jZSk7XG4gICAgaWYgKGNvbnRleHQpIHtcbiAgICAgIGNvbnN0IG1lbWJlciA9IGNvbnRleHQuZ2V0KHZhbHVlKTtcbiAgICAgIGlmIChtZW1iZXIgJiYgbWVtYmVyLnR5cGUpIHtcbiAgICAgICAgcmV0dXJuIG1lbWJlci50eXBlO1xuICAgICAgfVxuICAgIH1cbiAgfVxuXG4gIHJldHVybiBxdWVyeS5nZXRCdWlsdGluVHlwZShCdWlsdGluVHlwZS5BbnkpO1xufVxuXG4vKipcbiAqIFJlc29sdmUgYSBtb3JlIHNwZWNpZmljIHR5cGUgZm9yIHRoZSB2YXJpYWJsZSBpbiBgdGVtcGxhdGVFbGVtZW50YCBieSBpbnNwZWN0aW5nXG4gKiBhbGwgdmFyaWFibGVzIHRoYXQgYXJlIGluIHNjb3BlIGluIHRoZSBgbWVyZ2VkVGFibGVgLiBUaGlzIGZ1bmN0aW9uIGlzIGEgc3BlY2lhbFxuICogY2FzZSBmb3IgYG5nRm9yYCBhbmQgYG5nSWZgLiBJZiByZXNvbHV0aW9uIGZhaWxzLCByZXR1cm4gdGhlIGBhbnlgIHR5cGUuXG4gKiBAcGFyYW0gdmFsdWUgdmFyaWFibGUgdmFsdWUgbmFtZVxuICogQHBhcmFtIG1lcmdlZFRhYmxlIHN5bWJvbCB0YWJsZSBmb3IgYWxsIHZhcmlhYmxlcyBpbiBzY29wZVxuICogQHBhcmFtIGluZm8gYXZhaWxhYmxlIHRlbXBsYXRlIGluZm9ybWF0aW9uXG4gKiBAcGFyYW0gdGVtcGxhdGVFbGVtZW50XG4gKi9cbmZ1bmN0aW9uIHJlZmluZWRWYXJpYWJsZVR5cGUoXG4gICAgdmFsdWU6IHN0cmluZywgbWVyZ2VkVGFibGU6IFN5bWJvbFRhYmxlLCBpbmZvOiBuZy5EaWFnbm9zdGljVGVtcGxhdGVJbmZvLFxuICAgIHRlbXBsYXRlRWxlbWVudDogRW1iZWRkZWRUZW1wbGF0ZUFzdCk6IFN5bWJvbCB7XG4gIGlmICh2YWx1ZSA9PT0gJyRpbXBsaWNpdCcpIHtcbiAgICAvLyBTcGVjaWFsIGNhc2U6IG5nRm9yIGRpcmVjdGl2ZVxuICAgIGNvbnN0IG5nRm9yRGlyZWN0aXZlID0gdGVtcGxhdGVFbGVtZW50LmRpcmVjdGl2ZXMuZmluZChkID0+IHtcbiAgICAgIGNvbnN0IG5hbWUgPSBpZGVudGlmaWVyTmFtZShkLmRpcmVjdGl2ZS50eXBlKTtcbiAgICAgIHJldHVybiBuYW1lID09ICdOZ0ZvcicgfHwgbmFtZSA9PSAnTmdGb3JPZic7XG4gICAgfSk7XG4gICAgaWYgKG5nRm9yRGlyZWN0aXZlKSB7XG4gICAgICBjb25zdCBuZ0Zvck9mQmluZGluZyA9IG5nRm9yRGlyZWN0aXZlLmlucHV0cy5maW5kKGkgPT4gaS5kaXJlY3RpdmVOYW1lID09ICduZ0Zvck9mJyk7XG4gICAgICBpZiAobmdGb3JPZkJpbmRpbmcpIHtcbiAgICAgICAgLy8gQ2hlY2sgaWYgdGhlcmUgaXMgYSBrbm93biB0eXBlIGZvciB0aGUgbmdGb3IgYmluZGluZy5cbiAgICAgICAgY29uc3QgYmluZGluZ1R5cGUgPVxuICAgICAgICAgICAgbmV3IEFzdFR5cGUobWVyZ2VkVGFibGUsIGluZm8ucXVlcnksIHt9LCBpbmZvLnNvdXJjZSkuZ2V0VHlwZShuZ0Zvck9mQmluZGluZy52YWx1ZSk7XG4gICAgICAgIGlmIChiaW5kaW5nVHlwZSkge1xuICAgICAgICAgIGNvbnN0IHJlc3VsdCA9IGluZm8ucXVlcnkuZ2V0RWxlbWVudFR5cGUoYmluZGluZ1R5cGUpO1xuICAgICAgICAgIGlmIChyZXN1bHQpIHtcbiAgICAgICAgICAgIHJldHVybiByZXN1bHQ7XG4gICAgICAgICAgfVxuICAgICAgICB9XG4gICAgICB9XG4gICAgfVxuICB9XG5cbiAgaWYgKHZhbHVlID09PSAnbmdJZicgfHwgdmFsdWUgPT09ICckaW1wbGljaXQnKSB7XG4gICAgY29uc3QgbmdJZkRpcmVjdGl2ZSA9XG4gICAgICAgIHRlbXBsYXRlRWxlbWVudC5kaXJlY3RpdmVzLmZpbmQoZCA9PiBpZGVudGlmaWVyTmFtZShkLmRpcmVjdGl2ZS50eXBlKSA9PT0gJ05nSWYnKTtcbiAgICBpZiAobmdJZkRpcmVjdGl2ZSkge1xuICAgICAgLy8gU3BlY2lhbCBjYXNlOiBuZ0lmIGRpcmVjdGl2ZS4gVGhlIE5nSWYgc3RydWN0dXJhbCBkaXJlY3RpdmUgb3ducyBhIHRlbXBsYXRlIGNvbnRleHQgd2l0aFxuICAgICAgLy8gXCIkaW1wbGljaXRcIiBhbmQgXCJuZ0lmXCIgbWVtYmVycy4gVGhlc2UgcHJvcGVydGllcyBhcmUgdHlwZWQgYXMgZ2VuZXJpY3MuIFVudGlsIHRoZSBsYW5ndWFnZVxuICAgICAgLy8gc2VydmljZSB1c2VzIGFuIEl2eSBhbmQgVHlwZWNoZWNrQmxvY2sgYmFja2VuZCwgd2UgY2Fubm90IGJpbmQgdGhlc2UgdmFsdWVzIHRvIGEgY29uY3JldGVcbiAgICAgIC8vIHR5cGUgd2l0aG91dCBtYW51YWwgaW5mZXJlbmNlLiBUbyBnZXQgdGhlIGNvbmNyZXRlIHR5cGUsIGxvb2sgdXAgdGhlIHR5cGUgb2YgdGhlIFwibmdJZlwiXG4gICAgICAvLyBpbXBvcnQgb24gdGhlIE5nSWYgZGlyZWN0aXZlIGJvdW5kIHRvIHRoZSB0ZW1wbGF0ZS5cbiAgICAgIC8vXG4gICAgICAvLyBTZWUgQGFuZ3VsYXIvY29tbW9uL25nX2lmLnRzIGZvciBtb3JlIGluZm9ybWF0aW9uLlxuICAgICAgY29uc3QgbmdJZkJpbmRpbmcgPSBuZ0lmRGlyZWN0aXZlLmlucHV0cy5maW5kKGkgPT4gaS5kaXJlY3RpdmVOYW1lID09PSAnbmdJZicpO1xuICAgICAgaWYgKG5nSWZCaW5kaW5nKSB7XG4gICAgICAgIC8vIENoZWNrIGlmIHRoZXJlIGlzIGEga25vd24gdHlwZSBib3VuZCB0byB0aGUgbmdJZiBpbnB1dC5cbiAgICAgICAgY29uc3QgYmluZGluZ1R5cGUgPVxuICAgICAgICAgICAgbmV3IEFzdFR5cGUobWVyZ2VkVGFibGUsIGluZm8ucXVlcnksIHt9LCBpbmZvLnNvdXJjZSkuZ2V0VHlwZShuZ0lmQmluZGluZy52YWx1ZSk7XG4gICAgICAgIGlmIChiaW5kaW5nVHlwZSkge1xuICAgICAgICAgIHJldHVybiBiaW5kaW5nVHlwZTtcbiAgICAgICAgfVxuICAgICAgfVxuICAgIH1cbiAgfVxuXG4gIC8vIFdlIGNhbid0IGRvIGJldHRlciwgcmV0dXJuIGFueVxuICByZXR1cm4gaW5mby5xdWVyeS5nZXRCdWlsdGluVHlwZShCdWlsdGluVHlwZS5BbnkpO1xufVxuXG5mdW5jdGlvbiBnZXRFdmVudERlY2xhcmF0aW9uKFxuICAgIGluZm86IG5nLkRpYWdub3N0aWNUZW1wbGF0ZUluZm8sIHBhdGg6IFRlbXBsYXRlQXN0UGF0aCk6IFN5bWJvbERlY2xhcmF0aW9ufHVuZGVmaW5lZCB7XG4gIGNvbnN0IGV2ZW50ID0gcGF0aC50YWlsO1xuICBpZiAoIShldmVudCBpbnN0YW5jZW9mIEJvdW5kRXZlbnRBc3QpKSB7XG4gICAgLy8gTm8gZXZlbnQgYXZhaWxhYmxlIGluIHRoaXMgY29udGV4dC5cbiAgICByZXR1cm47XG4gIH1cblxuICBjb25zdCBnZW5lcmljRXZlbnQ6IFN5bWJvbERlY2xhcmF0aW9uID0ge1xuICAgIG5hbWU6ICckZXZlbnQnLFxuICAgIGtpbmQ6ICd2YXJpYWJsZScsXG4gICAgdHlwZTogaW5mby5xdWVyeS5nZXRCdWlsdGluVHlwZShCdWlsdGluVHlwZS5BbnkpLFxuICB9O1xuXG4gIGNvbnN0IG91dHB1dFN5bWJvbCA9IGZpbmRPdXRwdXRCaW5kaW5nKGV2ZW50LCBwYXRoLCBpbmZvLnF1ZXJ5KTtcbiAgaWYgKCFvdXRwdXRTeW1ib2wpIHtcbiAgICAvLyBUaGUgYCRldmVudGAgdmFyaWFibGUgZG9lc24ndCBiZWxvbmcgdG8gYW4gb3V0cHV0LCBzbyBpdHMgdHlwZSBjYW4ndCBiZSByZWZpbmVkLlxuICAgIC8vIFRPRE86IHR5cGUgYCRldmVudGAgdmFyaWFibGVzIGluIGJpbmRpbmdzIHRvIERPTSBldmVudHMuXG4gICAgcmV0dXJuIGdlbmVyaWNFdmVudDtcbiAgfVxuXG4gIC8vIFRoZSByYXcgZXZlbnQgdHlwZSBpcyB3cmFwcGVkIGluIGEgZ2VuZXJpYywgbGlrZSBFdmVudEVtaXR0ZXI8VD4gb3IgT2JzZXJ2YWJsZTxUPi5cbiAgY29uc3QgdGEgPSBvdXRwdXRTeW1ib2wudHlwZUFyZ3VtZW50cygpO1xuICBpZiAoIXRhIHx8IHRhLmxlbmd0aCAhPT0gMSkgcmV0dXJuIGdlbmVyaWNFdmVudDtcbiAgY29uc3QgZXZlbnRUeXBlID0gdGFbMF07XG5cbiAgcmV0dXJuIHsuLi5nZW5lcmljRXZlbnQsIHR5cGU6IGV2ZW50VHlwZX07XG59XG5cbi8qKlxuICogUmV0dXJucyB0aGUgc3ltYm9scyBhdmFpbGFibGUgaW4gYSBwYXJ0aWN1bGFyIHNjb3BlIG9mIGEgdGVtcGxhdGUuXG4gKiBAcGFyYW0gaW5mbyBwYXJzZWQgdGVtcGxhdGUgaW5mb3JtYXRpb25cbiAqIEBwYXJhbSBwYXRoIHBhdGggb2YgdGVtcGxhdGUgbm9kZXMgbmFycm93aW5nIHRvIHRoZSBjb250ZXh0IHRoZSBleHByZXNzaW9uIHNjb3BlIHNob3VsZCBiZVxuICogZGVyaXZlZCBmb3IuXG4gKi9cbmV4cG9ydCBmdW5jdGlvbiBnZXRFeHByZXNzaW9uU2NvcGUoXG4gICAgaW5mbzogbmcuRGlhZ25vc3RpY1RlbXBsYXRlSW5mbywgcGF0aDogVGVtcGxhdGVBc3RQYXRoKTogU3ltYm9sVGFibGUge1xuICBsZXQgcmVzdWx0ID0gaW5mby5tZW1iZXJzO1xuICBjb25zdCByZWZlcmVuY2VzID0gZ2V0UmVmZXJlbmNlcyhpbmZvKTtcbiAgY29uc3QgdmFyaWFibGVzID0gZ2V0VmFyRGVjbGFyYXRpb25zKGluZm8sIHBhdGgpO1xuICBjb25zdCBldmVudCA9IGdldEV2ZW50RGVjbGFyYXRpb24oaW5mbywgcGF0aCk7XG4gIGlmIChyZWZlcmVuY2VzLmxlbmd0aCB8fCB2YXJpYWJsZXMubGVuZ3RoIHx8IGV2ZW50KSB7XG4gICAgY29uc3QgcmVmZXJlbmNlVGFibGUgPSBpbmZvLnF1ZXJ5LmNyZWF0ZVN5bWJvbFRhYmxlKHJlZmVyZW5jZXMpO1xuICAgIGNvbnN0IHZhcmlhYmxlVGFibGUgPSBpbmZvLnF1ZXJ5LmNyZWF0ZVN5bWJvbFRhYmxlKHZhcmlhYmxlcyk7XG4gICAgY29uc3QgZXZlbnRzVGFibGUgPSBpbmZvLnF1ZXJ5LmNyZWF0ZVN5bWJvbFRhYmxlKGV2ZW50ID8gW2V2ZW50XSA6IFtdKTtcbiAgICByZXN1bHQgPSBpbmZvLnF1ZXJ5Lm1lcmdlU3ltYm9sVGFibGUoW3Jlc3VsdCwgcmVmZXJlbmNlVGFibGUsIHZhcmlhYmxlVGFibGUsIGV2ZW50c1RhYmxlXSk7XG4gIH1cbiAgcmV0dXJuIHJlc3VsdDtcbn1cblxuY2xhc3MgRXhwcmVzc2lvbkRpYWdub3N0aWNzVmlzaXRvciBleHRlbmRzIFJlY3Vyc2l2ZVRlbXBsYXRlQXN0VmlzaXRvciB7XG4gIHByaXZhdGUgcGF0aDogVGVtcGxhdGVBc3RQYXRoO1xuICBwcml2YXRlIGRpcmVjdGl2ZVN1bW1hcnk6IENvbXBpbGVEaXJlY3RpdmVTdW1tYXJ5fHVuZGVmaW5lZDtcblxuICBkaWFnbm9zdGljczogbmcuRGlhZ25vc3RpY1tdID0gW107XG5cbiAgY29uc3RydWN0b3IoXG4gICAgICBwcml2YXRlIGluZm86IG5nLkRpYWdub3N0aWNUZW1wbGF0ZUluZm8sXG4gICAgICBwcml2YXRlIGdldEV4cHJlc3Npb25TY29wZTogKHBhdGg6IFRlbXBsYXRlQXN0UGF0aCwgaW5jbHVkZUV2ZW50OiBib29sZWFuKSA9PiBTeW1ib2xUYWJsZSkge1xuICAgIHN1cGVyKCk7XG4gICAgdGhpcy5wYXRoID0gbmV3IEFzdFBhdGg8VGVtcGxhdGVBc3Q+KFtdKTtcbiAgfVxuXG4gIHZpc2l0RGlyZWN0aXZlKGFzdDogRGlyZWN0aXZlQXN0LCBjb250ZXh0OiBhbnkpOiBhbnkge1xuICAgIC8vIE92ZXJyaWRlIHRoZSBkZWZhdWx0IGNoaWxkIHZpc2l0b3IgdG8gaWdub3JlIHRoZSBob3N0IHByb3BlcnRpZXMgb2YgYSBkaXJlY3RpdmUuXG4gICAgaWYgKGFzdC5pbnB1dHMgJiYgYXN0LmlucHV0cy5sZW5ndGgpIHtcbiAgICAgIHRlbXBsYXRlVmlzaXRBbGwodGhpcywgYXN0LmlucHV0cywgY29udGV4dCk7XG4gICAgfVxuICB9XG5cbiAgdmlzaXRCb3VuZFRleHQoYXN0OiBCb3VuZFRleHRBc3QpOiB2b2lkIHtcbiAgICB0aGlzLnB1c2goYXN0KTtcbiAgICB0aGlzLmRpYWdub3NlRXhwcmVzc2lvbihhc3QudmFsdWUsIGFzdC5zb3VyY2VTcGFuLnN0YXJ0Lm9mZnNldCwgZmFsc2UpO1xuICAgIHRoaXMucG9wKCk7XG4gIH1cblxuICB2aXNpdERpcmVjdGl2ZVByb3BlcnR5KGFzdDogQm91bmREaXJlY3RpdmVQcm9wZXJ0eUFzdCk6IHZvaWQge1xuICAgIHRoaXMucHVzaChhc3QpO1xuICAgIHRoaXMuZGlhZ25vc2VFeHByZXNzaW9uKGFzdC52YWx1ZSwgdGhpcy5hdHRyaWJ1dGVWYWx1ZUxvY2F0aW9uKGFzdCksIGZhbHNlKTtcbiAgICB0aGlzLnBvcCgpO1xuICB9XG5cbiAgdmlzaXRFbGVtZW50UHJvcGVydHkoYXN0OiBCb3VuZEVsZW1lbnRQcm9wZXJ0eUFzdCk6IHZvaWQge1xuICAgIHRoaXMucHVzaChhc3QpO1xuICAgIHRoaXMuZGlhZ25vc2VFeHByZXNzaW9uKGFzdC52YWx1ZSwgdGhpcy5hdHRyaWJ1dGVWYWx1ZUxvY2F0aW9uKGFzdCksIGZhbHNlKTtcbiAgICB0aGlzLnBvcCgpO1xuICB9XG5cbiAgdmlzaXRFdmVudChhc3Q6IEJvdW5kRXZlbnRBc3QpOiB2b2lkIHtcbiAgICB0aGlzLnB1c2goYXN0KTtcbiAgICB0aGlzLmRpYWdub3NlRXhwcmVzc2lvbihhc3QuaGFuZGxlciwgdGhpcy5hdHRyaWJ1dGVWYWx1ZUxvY2F0aW9uKGFzdCksIHRydWUpO1xuICAgIHRoaXMucG9wKCk7XG4gIH1cblxuICB2aXNpdFZhcmlhYmxlKGFzdDogVmFyaWFibGVBc3QpOiB2b2lkIHtcbiAgICBjb25zdCBkaXJlY3RpdmUgPSB0aGlzLmRpcmVjdGl2ZVN1bW1hcnk7XG4gICAgaWYgKGRpcmVjdGl2ZSAmJiBhc3QudmFsdWUpIHtcbiAgICAgIGNvbnN0IGNvbnRleHQgPSB0aGlzLmluZm8ucXVlcnkuZ2V0VGVtcGxhdGVDb250ZXh0KGRpcmVjdGl2ZS50eXBlLnJlZmVyZW5jZSkhO1xuICAgICAgaWYgKGNvbnRleHQgJiYgIWNvbnRleHQuaGFzKGFzdC52YWx1ZSkpIHtcbiAgICAgICAgY29uc3QgbWlzc2luZ01lbWJlciA9XG4gICAgICAgICAgICBhc3QudmFsdWUgPT09ICckaW1wbGljaXQnID8gJ2FuIGltcGxpY2l0IHZhbHVlJyA6IGBhIG1lbWJlciBjYWxsZWQgJyR7YXN0LnZhbHVlfSdgO1xuXG4gICAgICAgIGNvbnN0IHNwYW4gPSB0aGlzLmFic1NwYW4oc3Bhbk9mKGFzdC5zb3VyY2VTcGFuKSk7XG4gICAgICAgIHRoaXMuZGlhZ25vc3RpY3MucHVzaChjcmVhdGVEaWFnbm9zdGljKFxuICAgICAgICAgICAgc3BhbiwgRGlhZ25vc3RpYy50ZW1wbGF0ZV9jb250ZXh0X21pc3NpbmdfbWVtYmVyLCBkaXJlY3RpdmUudHlwZS5yZWZlcmVuY2UubmFtZSxcbiAgICAgICAgICAgIG1pc3NpbmdNZW1iZXIpKTtcbiAgICAgIH1cbiAgICB9XG4gIH1cblxuICB2aXNpdEVsZW1lbnQoYXN0OiBFbGVtZW50QXN0LCBjb250ZXh0OiBhbnkpOiB2b2lkIHtcbiAgICB0aGlzLnB1c2goYXN0KTtcbiAgICBzdXBlci52aXNpdEVsZW1lbnQoYXN0LCBjb250ZXh0KTtcbiAgICB0aGlzLnBvcCgpO1xuICB9XG5cbiAgdmlzaXRFbWJlZGRlZFRlbXBsYXRlKGFzdDogRW1iZWRkZWRUZW1wbGF0ZUFzdCwgY29udGV4dDogYW55KTogYW55IHtcbiAgICBjb25zdCBwcmV2aW91c0RpcmVjdGl2ZVN1bW1hcnkgPSB0aGlzLmRpcmVjdGl2ZVN1bW1hcnk7XG5cbiAgICB0aGlzLnB1c2goYXN0KTtcblxuICAgIC8vIEZpbmQgZGlyZWN0aXZlIHRoYXQgcmVmZXJlbmNlcyB0aGlzIHRlbXBsYXRlXG4gICAgdGhpcy5kaXJlY3RpdmVTdW1tYXJ5ID1cbiAgICAgICAgYXN0LmRpcmVjdGl2ZXMubWFwKGQgPT4gZC5kaXJlY3RpdmUpLmZpbmQoZCA9PiBoYXNUZW1wbGF0ZVJlZmVyZW5jZShkLnR5cGUpKSE7XG5cbiAgICAvLyBQcm9jZXNzIGNoaWxkcmVuXG4gICAgc3VwZXIudmlzaXRFbWJlZGRlZFRlbXBsYXRlKGFzdCwgY29udGV4dCk7XG5cbiAgICB0aGlzLnBvcCgpO1xuXG4gICAgdGhpcy5kaXJlY3RpdmVTdW1tYXJ5ID0gcHJldmlvdXNEaXJlY3RpdmVTdW1tYXJ5O1xuICB9XG5cbiAgcHJpdmF0ZSBhdHRyaWJ1dGVWYWx1ZUxvY2F0aW9uKGFzdDogVGVtcGxhdGVBc3QpIHtcbiAgICBjb25zdCBwYXRoID0gZ2V0UGF0aFRvTm9kZUF0UG9zaXRpb24odGhpcy5pbmZvLmh0bWxBc3QsIGFzdC5zb3VyY2VTcGFuLnN0YXJ0Lm9mZnNldCk7XG4gICAgY29uc3QgbGFzdCA9IHBhdGgudGFpbDtcbiAgICBpZiAobGFzdCBpbnN0YW5jZW9mIEF0dHJpYnV0ZSAmJiBsYXN0LnZhbHVlU3Bhbikge1xuICAgICAgcmV0dXJuIGxhc3QudmFsdWVTcGFuLnN0YXJ0Lm9mZnNldDtcbiAgICB9XG4gICAgcmV0dXJuIGFzdC5zb3VyY2VTcGFuLnN0YXJ0Lm9mZnNldDtcbiAgfVxuXG4gIHByaXZhdGUgZGlhZ25vc2VFeHByZXNzaW9uKGFzdDogQVNULCBvZmZzZXQ6IG51bWJlciwgaW5FdmVudDogYm9vbGVhbikge1xuICAgIGNvbnN0IHNjb3BlID0gdGhpcy5nZXRFeHByZXNzaW9uU2NvcGUodGhpcy5wYXRoLCBpbkV2ZW50KTtcbiAgICBjb25zdCBhbmFseXplciA9IG5ldyBBc3RUeXBlKHNjb3BlLCB0aGlzLmluZm8ucXVlcnksIHtpbkV2ZW50fSwgdGhpcy5pbmZvLnNvdXJjZSk7XG4gICAgZm9yIChjb25zdCBkaWFnbm9zdGljIG9mIGFuYWx5emVyLmdldERpYWdub3N0aWNzKGFzdCkpIHtcbiAgICAgIGRpYWdub3N0aWMuc3BhbiA9IHRoaXMuYWJzU3BhbihkaWFnbm9zdGljLnNwYW4sIG9mZnNldCk7XG4gICAgICB0aGlzLmRpYWdub3N0aWNzLnB1c2goZGlhZ25vc3RpYyk7XG4gICAgfVxuICB9XG5cbiAgcHJpdmF0ZSBwdXNoKGFzdDogVGVtcGxhdGVBc3QpIHtcbiAgICB0aGlzLnBhdGgucHVzaChhc3QpO1xuICB9XG5cbiAgcHJpdmF0ZSBwb3AoKSB7XG4gICAgdGhpcy5wYXRoLnBvcCgpO1xuICB9XG5cbiAgcHJpdmF0ZSBhYnNTcGFuKHNwYW46IFNwYW4sIGFkZGl0aW9uYWxPZmZzZXQ6IG51bWJlciA9IDApOiBTcGFuIHtcbiAgICByZXR1cm4ge1xuICAgICAgc3RhcnQ6IHNwYW4uc3RhcnQgKyB0aGlzLmluZm8ub2Zmc2V0ICsgYWRkaXRpb25hbE9mZnNldCxcbiAgICAgIGVuZDogc3Bhbi5lbmQgKyB0aGlzLmluZm8ub2Zmc2V0ICsgYWRkaXRpb25hbE9mZnNldCxcbiAgICB9O1xuICB9XG59XG5cbmZ1bmN0aW9uIGhhc1RlbXBsYXRlUmVmZXJlbmNlKHR5cGU6IENvbXBpbGVUeXBlTWV0YWRhdGEpOiBib29sZWFuIHtcbiAgaWYgKHR5cGUuZGlEZXBzKSB7XG4gICAgZm9yIChsZXQgZGlEZXAgb2YgdHlwZS5kaURlcHMpIHtcbiAgICAgIGlmIChkaURlcC50b2tlbiAmJiBkaURlcC50b2tlbi5pZGVudGlmaWVyICYmXG4gICAgICAgICAgaWRlbnRpZmllck5hbWUoZGlEZXAudG9rZW4hLmlkZW50aWZpZXIhKSA9PSAnVGVtcGxhdGVSZWYnKVxuICAgICAgICByZXR1cm4gdHJ1ZTtcbiAgICB9XG4gIH1cbiAgcmV0dXJuIGZhbHNlO1xufVxuXG5mdW5jdGlvbiBzcGFuT2Yoc291cmNlU3BhbjogUGFyc2VTb3VyY2VTcGFuKTogU3BhbiB7XG4gIHJldHVybiB7c3RhcnQ6IHNvdXJjZVNwYW4uc3RhcnQub2Zmc2V0LCBlbmQ6IHNvdXJjZVNwYW4uZW5kLm9mZnNldH07XG59XG4iXX0=