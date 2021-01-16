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
        define("@angular/language-service/src/locate_symbol", ["require", "exports", "tslib", "@angular/compiler", "typescript/lib/tsserverlibrary", "@angular/language-service/src/expression_diagnostics", "@angular/language-service/src/expressions", "@angular/language-service/src/types", "@angular/language-service/src/utils"], factory);
    }
})(function (require, exports) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    exports.locateSymbols = void 0;
    var tslib_1 = require("tslib");
    var compiler_1 = require("@angular/compiler");
    var tss = require("typescript/lib/tsserverlibrary");
    var expression_diagnostics_1 = require("@angular/language-service/src/expression_diagnostics");
    var expressions_1 = require("@angular/language-service/src/expressions");
    var types_1 = require("@angular/language-service/src/types");
    var utils_1 = require("@angular/language-service/src/utils");
    /**
     * Traverses a template AST and locates symbol(s) at a specified position.
     * @param info template AST information set
     * @param position location to locate symbols at
     */
    function locateSymbols(info, position) {
        var templatePosition = position - info.template.span.start;
        // TODO: update `findTemplateAstAt` to use absolute positions.
        var path = utils_1.findTemplateAstAt(info.templateAst, templatePosition);
        var attribute = findAttribute(info, position);
        if (!path.tail)
            return [];
        var narrowest = utils_1.spanOf(path.tail);
        var toVisit = [];
        for (var node = path.tail; node && utils_1.isNarrower(utils_1.spanOf(node.sourceSpan), narrowest); node = path.parentOf(node)) {
            toVisit.push(node);
        }
        // For the structural directive, only care about the last template AST.
        if (attribute === null || attribute === void 0 ? void 0 : attribute.name.startsWith('*')) {
            toVisit.splice(0, toVisit.length - 1);
        }
        return toVisit.map(function (ast) { return locateSymbol(ast, path, info); })
            .filter(function (sym) { return sym !== undefined; });
    }
    exports.locateSymbols = locateSymbols;
    /**
     * Visits a template node and locates the symbol in that node at a path position.
     * @param ast template AST node to visit
     * @param path non-empty set of narrowing AST nodes at a position
     * @param info template AST information set
     */
    function locateSymbol(ast, path, info) {
        var templatePosition = path.position;
        var position = templatePosition + info.template.span.start;
        var symbol;
        var span;
        var staticSymbol;
        var attributeValueSymbol = function (ast) {
            var attribute = findAttribute(info, position);
            if (attribute) {
                if (utils_1.inSpan(templatePosition, utils_1.spanOf(attribute.valueSpan))) {
                    var result = void 0;
                    if (attribute.name.startsWith('*')) {
                        result = getSymbolInMicrosyntax(info, path, attribute);
                    }
                    else {
                        var dinfo = utils_1.diagnosticInfoFromTemplateInfo(info);
                        var scope = expression_diagnostics_1.getExpressionScope(dinfo, path);
                        result = expressions_1.getExpressionSymbol(scope, ast, templatePosition, info.template);
                    }
                    if (result) {
                        symbol = result.symbol;
                        span = utils_1.offsetSpan(result.span, attribute.valueSpan.start.offset);
                    }
                    return true;
                }
            }
            return false;
        };
        ast.visit({
            visitNgContent: function (_ast) { },
            visitEmbeddedTemplate: function (_ast) { },
            visitElement: function (ast) {
                var component = ast.directives.find(function (d) { return d.directive.isComponent; });
                if (component) {
                    // Need to cast because 'reference' is typed as any
                    staticSymbol = component.directive.type.reference;
                    symbol = info.template.query.getTypeSymbol(staticSymbol);
                    symbol = symbol && new OverrideKindSymbol(symbol, types_1.DirectiveKind.COMPONENT);
                    span = utils_1.spanOf(ast);
                }
                else {
                    // Find a directive that matches the element name
                    var directive = ast.directives.find(function (d) { return d.directive.selector != null && d.directive.selector.indexOf(ast.name) >= 0; });
                    if (directive) {
                        // Need to cast because 'reference' is typed as any
                        staticSymbol = directive.directive.type.reference;
                        symbol = info.template.query.getTypeSymbol(staticSymbol);
                        symbol = symbol && new OverrideKindSymbol(symbol, types_1.DirectiveKind.DIRECTIVE);
                        span = utils_1.spanOf(ast);
                    }
                }
            },
            visitReference: function (ast) {
                symbol = ast.value && info.template.query.getTypeSymbol(compiler_1.tokenReference(ast.value));
                span = utils_1.spanOf(ast);
            },
            visitVariable: function (_ast) { },
            visitEvent: function (ast) {
                if (!attributeValueSymbol(ast.handler)) {
                    symbol = utils_1.findOutputBinding(ast, path, info.template.query);
                    symbol = symbol && new OverrideKindSymbol(symbol, types_1.DirectiveKind.EVENT);
                    span = utils_1.spanOf(ast);
                }
            },
            visitElementProperty: function (ast) {
                attributeValueSymbol(ast.value);
            },
            visitAttr: function (ast) {
                var e_1, _a;
                var element = path.first(compiler_1.ElementAst);
                if (!element)
                    return;
                // Create a mapping of all directives applied to the element from their selectors.
                var matcher = new compiler_1.SelectorMatcher();
                try {
                    for (var _b = tslib_1.__values(element.directives), _c = _b.next(); !_c.done; _c = _b.next()) {
                        var dir = _c.value;
                        if (!dir.directive.selector)
                            continue;
                        matcher.addSelectables(compiler_1.CssSelector.parse(dir.directive.selector), dir);
                    }
                }
                catch (e_1_1) { e_1 = { error: e_1_1 }; }
                finally {
                    try {
                        if (_c && !_c.done && (_a = _b.return)) _a.call(_b);
                    }
                    finally { if (e_1) throw e_1.error; }
                }
                // See if this attribute matches the selector of any directive on the element.
                var attributeSelector = "[" + ast.name + "=" + ast.value + "]";
                var parsedAttribute = compiler_1.CssSelector.parse(attributeSelector);
                if (!parsedAttribute.length)
                    return;
                matcher.match(parsedAttribute[0], function (_, _a) {
                    var directive = _a.directive;
                    // Need to cast because 'reference' is typed as any
                    staticSymbol = directive.type.reference;
                    symbol = info.template.query.getTypeSymbol(staticSymbol);
                    symbol = symbol && new OverrideKindSymbol(symbol, types_1.DirectiveKind.DIRECTIVE);
                    span = utils_1.spanOf(ast);
                });
            },
            visitBoundText: function (ast) {
                var expressionPosition = templatePosition - ast.sourceSpan.start.offset;
                if (utils_1.inSpan(expressionPosition, ast.value.span)) {
                    var dinfo = utils_1.diagnosticInfoFromTemplateInfo(info);
                    var scope = expression_diagnostics_1.getExpressionScope(dinfo, path);
                    var result = expressions_1.getExpressionSymbol(scope, ast.value, templatePosition, info.template);
                    if (result) {
                        symbol = result.symbol;
                        span = utils_1.offsetSpan(result.span, ast.sourceSpan.start.offset);
                    }
                }
            },
            visitText: function (_ast) { },
            visitDirective: function (ast) {
                // Need to cast because 'reference' is typed as any
                staticSymbol = ast.directive.type.reference;
                symbol = info.template.query.getTypeSymbol(staticSymbol);
                span = utils_1.spanOf(ast);
            },
            visitDirectiveProperty: function (ast) {
                if (!attributeValueSymbol(ast.value)) {
                    var directive = findParentOfBinding(info.templateAst, ast, templatePosition);
                    var attribute = findAttribute(info, position);
                    if (directive && attribute) {
                        if (attribute.name.startsWith('*')) {
                            var compileTypeSummary = directive.directive;
                            symbol = info.template.query.getTypeSymbol(compileTypeSummary.type.reference);
                            symbol = symbol && new OverrideKindSymbol(symbol, types_1.DirectiveKind.DIRECTIVE);
                            // Use 'attribute.sourceSpan' instead of the directive's,
                            // because the span of the directive is the whole opening tag of an element.
                            span = utils_1.spanOf(attribute.sourceSpan);
                        }
                        else {
                            symbol = findInputBinding(info, ast.templateName, directive);
                            span = utils_1.spanOf(ast);
                        }
                    }
                }
            }
        }, null);
        if (symbol && span) {
            var _a = utils_1.offsetSpan(span, info.template.span.start), start = _a.start, end = _a.end;
            return {
                symbol: symbol,
                span: tss.createTextSpanFromBounds(start, end),
                staticSymbol: staticSymbol,
            };
        }
    }
    // Get the symbol in microsyntax at template position.
    function getSymbolInMicrosyntax(info, path, attribute) {
        var e_2, _a;
        var _b;
        if (!attribute.valueSpan) {
            return;
        }
        var absValueOffset = attribute.valueSpan.start.offset;
        var result;
        var templateBindings = info.expressionParser.parseTemplateBindings(attribute.name, attribute.value, attribute.sourceSpan.toString(), attribute.sourceSpan.start.offset, attribute.valueSpan.start.offset).templateBindings;
        try {
            // Find the symbol that contains the position.
            for (var templateBindings_1 = tslib_1.__values(templateBindings), templateBindings_1_1 = templateBindings_1.next(); !templateBindings_1_1.done; templateBindings_1_1 = templateBindings_1.next()) {
                var tb = templateBindings_1_1.value;
                if (tb instanceof compiler_1.VariableBinding) {
                    // TODO(kyliau): if binding is variable we should still look for the value
                    // of the key. For example, "let i=index" => "index" should point to
                    // NgForOfContext.index
                    continue;
                }
                if (utils_1.inSpan(path.position, (_b = tb.value) === null || _b === void 0 ? void 0 : _b.ast.sourceSpan)) {
                    var dinfo = utils_1.diagnosticInfoFromTemplateInfo(info);
                    var scope = expression_diagnostics_1.getExpressionScope(dinfo, path);
                    result = expressions_1.getExpressionSymbol(scope, tb.value, path.position, info.template);
                }
                else if (utils_1.inSpan(path.position, tb.sourceSpan)) {
                    var template = path.first(compiler_1.EmbeddedTemplateAst);
                    if (template) {
                        // One element can only have one template binding.
                        var directiveAst = template.directives[0];
                        if (directiveAst) {
                            var symbol = findInputBinding(info, tb.key.source.substring(1), directiveAst);
                            if (symbol) {
                                result = {
                                    symbol: symbol,
                                    // the span here has to be relative to the start of the template
                                    // value so deduct the absolute offset.
                                    // TODO(kyliau): Use absolute source span throughout completions.
                                    span: utils_1.offsetSpan(tb.key.span, -absValueOffset),
                                };
                            }
                        }
                    }
                }
            }
        }
        catch (e_2_1) { e_2 = { error: e_2_1 }; }
        finally {
            try {
                if (templateBindings_1_1 && !templateBindings_1_1.done && (_a = templateBindings_1.return)) _a.call(templateBindings_1);
            }
            finally { if (e_2) throw e_2.error; }
        }
        return result;
    }
    function findAttribute(info, position) {
        var templatePosition = position - info.template.span.start;
        var path = utils_1.getPathToNodeAtPosition(info.htmlAst, templatePosition);
        return path.first(compiler_1.Attribute);
    }
    // TODO: remove this function after the path includes 'DirectiveAst'.
    // Find the directive that corresponds to the specified 'binding'
    // at the specified 'position' in the 'ast'.
    function findParentOfBinding(ast, binding, position) {
        var res;
        var visitor = new /** @class */ (function (_super) {
            tslib_1.__extends(class_1, _super);
            function class_1() {
                return _super !== null && _super.apply(this, arguments) || this;
            }
            class_1.prototype.visit = function (ast) {
                var span = utils_1.spanOf(ast);
                if (!utils_1.inSpan(position, span)) {
                    // Returning a value here will result in the children being skipped.
                    return true;
                }
            };
            class_1.prototype.visitEmbeddedTemplate = function (ast, context) {
                return this.visitChildren(context, function (visit) {
                    visit(ast.directives);
                    visit(ast.children);
                });
            };
            class_1.prototype.visitElement = function (ast, context) {
                return this.visitChildren(context, function (visit) {
                    visit(ast.directives);
                    visit(ast.children);
                });
            };
            class_1.prototype.visitDirective = function (ast) {
                var result = this.visitChildren(ast, function (visit) {
                    visit(ast.inputs);
                });
                return result;
            };
            class_1.prototype.visitDirectiveProperty = function (ast, context) {
                if (ast === binding) {
                    res = context;
                }
            };
            return class_1;
        }(compiler_1.RecursiveTemplateAstVisitor));
        compiler_1.templateVisitAll(visitor, ast);
        return res;
    }
    // Find the symbol of input binding in 'directiveAst' by 'name'.
    function findInputBinding(info, name, directiveAst) {
        var invertedInput = utils_1.invertMap(directiveAst.directive.inputs);
        var fieldName = invertedInput[name];
        if (fieldName) {
            var classSymbol = info.template.query.getTypeSymbol(directiveAst.directive.type.reference);
            if (classSymbol) {
                return classSymbol.members().get(fieldName);
            }
        }
    }
    /**
     * Wrap a symbol and change its kind to component.
     */
    var OverrideKindSymbol = /** @class */ (function () {
        function OverrideKindSymbol(sym, kindOverride) {
            this.sym = sym;
            this.kind = kindOverride;
        }
        Object.defineProperty(OverrideKindSymbol.prototype, "name", {
            get: function () {
                return this.sym.name;
            },
            enumerable: false,
            configurable: true
        });
        Object.defineProperty(OverrideKindSymbol.prototype, "language", {
            get: function () {
                return this.sym.language;
            },
            enumerable: false,
            configurable: true
        });
        Object.defineProperty(OverrideKindSymbol.prototype, "type", {
            get: function () {
                return this.sym.type;
            },
            enumerable: false,
            configurable: true
        });
        Object.defineProperty(OverrideKindSymbol.prototype, "container", {
            get: function () {
                return this.sym.container;
            },
            enumerable: false,
            configurable: true
        });
        Object.defineProperty(OverrideKindSymbol.prototype, "public", {
            get: function () {
                return this.sym.public;
            },
            enumerable: false,
            configurable: true
        });
        Object.defineProperty(OverrideKindSymbol.prototype, "callable", {
            get: function () {
                return this.sym.callable;
            },
            enumerable: false,
            configurable: true
        });
        Object.defineProperty(OverrideKindSymbol.prototype, "nullable", {
            get: function () {
                return this.sym.nullable;
            },
            enumerable: false,
            configurable: true
        });
        Object.defineProperty(OverrideKindSymbol.prototype, "definition", {
            get: function () {
                return this.sym.definition;
            },
            enumerable: false,
            configurable: true
        });
        Object.defineProperty(OverrideKindSymbol.prototype, "documentation", {
            get: function () {
                return this.sym.documentation;
            },
            enumerable: false,
            configurable: true
        });
        OverrideKindSymbol.prototype.members = function () {
            return this.sym.members();
        };
        OverrideKindSymbol.prototype.signatures = function () {
            return this.sym.signatures();
        };
        OverrideKindSymbol.prototype.selectSignature = function (types) {
            return this.sym.selectSignature(types);
        };
        OverrideKindSymbol.prototype.indexed = function (argument) {
            return this.sym.indexed(argument);
        };
        OverrideKindSymbol.prototype.typeArguments = function () {
            return this.sym.typeArguments();
        };
        return OverrideKindSymbol;
    }());
});
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoibG9jYXRlX3N5bWJvbC5qcyIsInNvdXJjZVJvb3QiOiIiLCJzb3VyY2VzIjpbIi4uLy4uLy4uLy4uLy4uLy4uL3BhY2thZ2VzL2xhbmd1YWdlLXNlcnZpY2Uvc3JjL2xvY2F0ZV9zeW1ib2wudHMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IkFBQUE7Ozs7OztHQU1HOzs7Ozs7Ozs7Ozs7OztJQUVILDhDQUFxUjtJQUNyUixvREFBc0Q7SUFFdEQsK0ZBQTREO0lBQzVELHlFQUFrRDtJQUNsRCw2REFBdUY7SUFDdkYsNkRBQXlLO0lBRXpLOzs7O09BSUc7SUFDSCxTQUFnQixhQUFhLENBQUMsSUFBZSxFQUFFLFFBQWdCO1FBQzdELElBQU0sZ0JBQWdCLEdBQUcsUUFBUSxHQUFHLElBQUksQ0FBQyxRQUFRLENBQUMsSUFBSSxDQUFDLEtBQUssQ0FBQztRQUM3RCw4REFBOEQ7UUFDOUQsSUFBTSxJQUFJLEdBQUcseUJBQWlCLENBQUMsSUFBSSxDQUFDLFdBQVcsRUFBRSxnQkFBZ0IsQ0FBQyxDQUFDO1FBQ25FLElBQU0sU0FBUyxHQUFHLGFBQWEsQ0FBQyxJQUFJLEVBQUUsUUFBUSxDQUFDLENBQUM7UUFFaEQsSUFBSSxDQUFDLElBQUksQ0FBQyxJQUFJO1lBQUUsT0FBTyxFQUFFLENBQUM7UUFFMUIsSUFBTSxTQUFTLEdBQUcsY0FBTSxDQUFDLElBQUksQ0FBQyxJQUFJLENBQUMsQ0FBQztRQUNwQyxJQUFNLE9BQU8sR0FBa0IsRUFBRSxDQUFDO1FBQ2xDLEtBQUssSUFBSSxJQUFJLEdBQTBCLElBQUksQ0FBQyxJQUFJLEVBQzNDLElBQUksSUFBSSxrQkFBVSxDQUFDLGNBQU0sQ0FBQyxJQUFJLENBQUMsVUFBVSxDQUFDLEVBQUUsU0FBUyxDQUFDLEVBQUUsSUFBSSxHQUFHLElBQUksQ0FBQyxRQUFRLENBQUMsSUFBSSxDQUFDLEVBQUU7WUFDdkYsT0FBTyxDQUFDLElBQUksQ0FBQyxJQUFJLENBQUMsQ0FBQztTQUNwQjtRQUVELHVFQUF1RTtRQUN2RSxJQUFJLFNB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Z0JBQ0UsT0FBTyxJQUFJLENBQUMsR0FBRyxDQUFDLFVBQVUsQ0FBQztZQUM3QixDQUFDOzs7V0FBQTtRQUVELHNCQUFJLDZDQUFhO2lCQUFqQjtnQkFDRSxPQUFPLElBQUksQ0FBQyxHQUFHLENBQUMsYUFBYSxDQUFDO1lBQ2hDLENBQUM7OztXQUFBO1FBRUQsb0NBQU8sR0FBUDtZQUNFLE9BQU8sSUFBSSxDQUFDLEdBQUcsQ0FBQyxPQUFPLEVBQUUsQ0FBQztRQUM1QixDQUFDO1FBRUQsdUNBQVUsR0FBVjtZQUNFLE9BQU8sSUFBSSxDQUFDLEdBQUcsQ0FBQyxVQUFVLEVBQUUsQ0FBQztRQUMvQixDQUFDO1FBRUQsNENBQWUsR0FBZixVQUFnQixLQUFlO1lBQzdCLE9BQU8sSUFBSSxDQUFDLEdBQUcsQ0FBQyxlQUFlLENBQUMsS0FBSyxDQUFDLENBQUM7UUFDekMsQ0FBQztRQUVELG9DQUFPLEdBQVAsVUFBUSxRQUFnQjtZQUN0QixPQUFPLElBQUksQ0FBQyxHQUFHLENBQUMsT0FBTyxDQUFDLFFBQVEsQ0FBQyxDQUFDO1FBQ3BDLENBQUM7UUFFRCwwQ0FBYSxHQUFiO1lBQ0UsT0FBTyxJQUFJLENBQUMsR0FBRyxDQUFDLGFBQWEsRUFBRSxDQUFDO1FBQ2xDLENBQUM7UUFDSCx5QkFBQztJQUFELENBQUMsQUE3REQsSUE2REMiLCJzb3VyY2VzQ29udGVudCI6WyIvKipcbiAqIEBsaWNlbnNlXG4gKiBDb3B5cmlnaHQgR29vZ2xlIExMQyBBbGwgUmlnaHRzIFJlc2VydmVkLlxuICpcbiAqIFVzZSBvZiB0aGlzIHNvdXJjZSBjb2RlIGlzIGdvdmVybmVkIGJ5IGFuIE1JVC1zdHlsZSBsaWNlbnNlIHRoYXQgY2FuIGJlXG4gKiBmb3VuZCBpbiB0aGUgTElDRU5TRSBmaWxlIGF0IGh0dHBzOi8vYW5ndWxhci5pby9saWNlbnNlXG4gKi9cblxuaW1wb3J0IHtBU1QsIEF0dHJpYnV0ZSwgQm91bmREaXJlY3RpdmVQcm9wZXJ0eUFzdCwgQ3NzU2VsZWN0b3IsIERpcmVjdGl2ZUFzdCwgRWxlbWVudEFzdCwgRW1iZWRkZWRUZW1wbGF0ZUFzdCwgUmVjdXJzaXZlVGVtcGxhdGVBc3RWaXNpdG9yLCBTZWxlY3Rvck1hdGNoZXIsIFN0YXRpY1N5bWJvbCwgVGVtcGxhdGVBc3QsIFRlbXBsYXRlQXN0UGF0aCwgdGVtcGxhdGVWaXNpdEFsbCwgdG9rZW5SZWZlcmVuY2UsIFZhcmlhYmxlQmluZGluZ30gZnJvbSAnQGFuZ3VsYXIvY29tcGlsZXInO1xuaW1wb3J0ICogYXMgdHNzIGZyb20gJ3R5cGVzY3JpcHQvbGliL3Rzc2VydmVybGlicmFyeSc7XG5cbmltcG9ydCB7Z2V0RXhwcmVzc2lvblNjb3BlfSBmcm9tICcuL2V4cHJlc3Npb25fZGlhZ25vc3RpY3MnO1xuaW1wb3J0IHtnZXRFeHByZXNzaW9uU3ltYm9sfSBmcm9tICcuL2V4cHJlc3Npb25zJztcbmltcG9ydCB7QXN0UmVzdWx0LCBEZWZpbml0aW9uLCBEaXJlY3RpdmVLaW5kLCBTcGFuLCBTeW1ib2wsIFN5bWJvbEluZm99IGZyb20gJy4vdHlwZXMnO1xuaW1wb3J0IHtkaWFnbm9zdGljSW5mb0Zyb21UZW1wbGF0ZUluZm8sIGZpbmRPdXRwdXRCaW5kaW5nLCBmaW5kVGVtcGxhdGVBc3RBdCwgZ2V0UGF0aFRvTm9kZUF0UG9zaXRpb24sIGluU3BhbiwgaW52ZXJ0TWFwLCBpc05hcnJvd2VyLCBvZmZzZXRTcGFuLCBzcGFuT2Z9IGZyb20gJy4vdXRpbHMnO1xuXG4vKipcbiAqIFRyYXZlcnNlcyBhIHRlbXBsYXRlIEFTVCBhbmQgbG9jYXRlcyBzeW1ib2wocykgYXQgYSBzcGVjaWZpZWQgcG9zaXRpb24uXG4gKiBAcGFyYW0gaW5mbyB0ZW1wbGF0ZSBBU1QgaW5mb3JtYXRpb24gc2V0XG4gKiBAcGFyYW0gcG9zaXRpb24gbG9jYXRpb24gdG8gbG9jYXRlIHN5bWJvbHMgYXRcbiAqL1xuZXhwb3J0IGZ1bmN0aW9uIGxvY2F0ZVN5bWJvbHMoaW5mbzogQXN0UmVzdWx0LCBwb3NpdGlvbjogbnVtYmVyKTogU3ltYm9sSW5mb1tdIHtcbiAgY29uc3QgdGVtcGxhdGVQb3NpdGlvbiA9IHBvc2l0aW9uIC0gaW5mby50ZW1wbGF0ZS5zcGFuLnN0YXJ0O1xuICAvLyBUT0RPOiB1cGRhdGUgYGZpbmRUZW1wbGF0ZUFzdEF0YCB0byB1c2UgYWJzb2x1dGUgcG9zaXRpb25zLlxuICBjb25zdCBwYXRoID0gZmluZFRlbXBsYXRlQXN0QXQoaW5mby50ZW1wbGF0ZUFzdCwgdGVtcGxhdGVQb3NpdGlvbik7XG4gIGNvbnN0IGF0dHJpYnV0ZSA9IGZpbmRBdHRyaWJ1dGUoaW5mbywgcG9zaXRpb24pO1xuXG4gIGlmICghcGF0aC50YWlsKSByZXR1cm4gW107XG5cbiAgY29uc3QgbmFycm93ZXN0ID0gc3Bhbk9mKHBhdGgudGFpbCk7XG4gIGNvbnN0IHRvVmlzaXQ6IFRlbXBsYXRlQXN0W10gPSBbXTtcbiAgZm9yIChsZXQgbm9kZTogVGVtcGxhdGVBc3R8dW5kZWZpbmVkID0gcGF0aC50YWlsO1xuICAgICAgIG5vZGUgJiYgaXNOYXJyb3dlcihzcGFuT2Yobm9kZS5zb3VyY2VTcGFuKSwgbmFycm93ZXN0KTsgbm9kZSA9IHBhdGgucGFyZW50T2Yobm9kZSkpIHtcbiAgICB0b1Zpc2l0LnB1c2gobm9kZSk7XG4gIH1cblxuICAvLyBGb3IgdGhlIHN0cnVjdHVyYWwgZGlyZWN0aXZlLCBvbmx5IGNhcmUgYWJvdXQgdGhlIGxhc3QgdGVtcGxhdGUgQVNULlxuICBpZiAoYXR0cmlidXRlPy5uYW1lLnN0YXJ0c1dpdGgoJyonKSkge1xuICAgIHRvVmlzaXQuc3BsaWNlKDAsIHRvVmlzaXQubGVuZ3RoIC0gMSk7XG4gIH1cblxuICByZXR1cm4gdG9WaXNpdC5tYXAoYXN0ID0+IGxvY2F0ZVN5bWJvbChhc3QsIHBhdGgsIGluZm8pKVxuICAgICAgLmZpbHRlcigoc3ltKTogc3ltIGlzIFN5bWJvbEluZm8gPT4gc3ltICE9PSB1bmRlZmluZWQpO1xufVxuXG4vKipcbiAqIFZpc2l0cyBhIHRlbXBsYXRlIG5vZGUgYW5kIGxvY2F0ZXMgdGhlIHN5bWJvbCBpbiB0aGF0IG5vZGUgYXQgYSBwYXRoIHBvc2l0aW9uLlxuICogQHBhcmFtIGFzdCB0ZW1wbGF0ZSBBU1Qgbm9kZSB0byB2aXNpdFxuICogQHBhcmFtIHBhdGggbm9uLWVtcHR5IHNldCBvZiBuYXJyb3dpbmcgQVNUIG5vZGVzIGF0IGEgcG9zaXRpb25cbiAqIEBwYXJhbSBpbmZvIHRlbXBsYXRlIEFTVCBpbmZvcm1hdGlvbiBzZXRcbiAqL1xuZnVuY3Rpb24gbG9jYXRlU3ltYm9sKGFzdDogVGVtcGxhdGVBc3QsIHBhdGg6IFRlbXBsYXRlQXN0UGF0aCwgaW5mbzogQXN0UmVzdWx0KTogU3ltYm9sSW5mb3xcbiAgICB1bmRlZmluZWQge1xuICBjb25zdCB0ZW1wbGF0ZVBvc2l0aW9uID0gcGF0aC5wb3NpdGlvbjtcbiAgY29uc3QgcG9zaXRpb24gPSB0ZW1wbGF0ZVBvc2l0aW9uICsgaW5mby50ZW1wbGF0ZS5zcGFuLnN0YXJ0O1xuICBsZXQgc3ltYm9sOiBTeW1ib2x8dW5kZWZpbmVkO1xuICBsZXQgc3BhbjogU3Bhbnx1bmRlZmluZWQ7XG4gIGxldCBzdGF0aWNTeW1ib2w6IFN0YXRpY1N5bWJvbHx1bmRlZmluZWQ7XG4gIGNvbnN0IGF0dHJpYnV0ZVZhbHVlU3ltYm9sID0gKGFzdDogQVNUKTogYm9vbGVhbiA9PiB7XG4gICAgY29uc3QgYXR0cmlidXRlID0gZmluZEF0dHJpYnV0ZShpbmZvLCBwb3NpdGlvbik7XG4gICAgaWYgKGF0dHJpYnV0ZSkge1xuICAgICAgaWYgKGluU3Bhbih0ZW1wbGF0ZVBvc2l0aW9uLCBzcGFuT2YoYXR0cmlidXRlLnZhbHVlU3BhbikpKSB7XG4gICAgICAgIGxldCByZXN1bHQ6IHtzeW1ib2w6IFN5bWJvbCwgc3BhbjogU3Bhbn18dW5kZWZpbmVkO1xuICAgICAgICBpZiAoYXR0cmlidXRlLm5hbWUuc3RhcnRzV2l0aCgnKicpKSB7XG4gICAgICAgICAgcmVzdWx0ID0gZ2V0U3ltYm9sSW5NaWNyb3N5bnRheChpbmZvLCBwYXRoLCBhdHRyaWJ1dGUpO1xuICAgICAgICB9IGVsc2Uge1xuICAgICAgICAgIGNvbnN0IGRpbmZvID0gZGlhZ25vc3RpY0luZm9Gcm9tVGVtcGxhdGVJbmZvKGluZm8pO1xuICAgICAgICAgIGNvbnN0IHNjb3BlID0gZ2V0RXhwcmVzc2lvblNjb3BlKGRpbmZvLCBwYXRoKTtcbiAgICAgICAgICByZXN1bHQgPSBnZXRFeHByZXNzaW9uU3ltYm9sKHNjb3BlLCBhc3QsIHRlbXBsYXRlUG9zaXRpb24sIGluZm8udGVtcGxhdGUpO1xuICAgICAgICB9XG4gICAgICAgIGlmIChyZXN1bHQpIHtcbiAgICAgICAgICBzeW1ib2wgPSByZXN1bHQuc3ltYm9sO1xuICAgICAgICAgIHNwYW4gPSBvZmZzZXRTcGFuKHJlc3VsdC5zcGFuLCBhdHRyaWJ1dGUudmFsdWVTcGFuIS5zdGFydC5vZmZzZXQpO1xuICAgICAgICB9XG4gICAgICAgIHJldHVybiB0cnVlO1xuICAgICAgfVxuICAgIH1cbiAgICByZXR1cm4gZmFsc2U7XG4gIH07XG4gIGFzdC52aXNpdChcbiAgICAgIHtcbiAgICAgICAgdmlzaXROZ0NvbnRlbnQoX2FzdCkge30sXG4gICAgICAgIHZpc2l0RW1iZWRkZWRUZW1wbGF0ZShfYXN0KSB7fSxcbiAgICAgICAgdmlzaXRFbGVtZW50KGFzdCkge1xuICAgICAgICAgIGNvbnN0IGNvbXBvbmVudCA9IGFzdC5kaXJlY3RpdmVzLmZpbmQoZCA9PiBkLmRpcmVjdGl2ZS5pc0NvbXBvbmVudCk7XG4gICAgICAgICAgaWYgKGNvbXBvbmVudCkge1xuICAgICAgICAgICAgLy8gTmVlZCB0byBjYXN0IGJlY2F1c2UgJ3JlZmVyZW5jZScgaXMgdHlwZWQgYXMgYW55XG4gICAgICAgICAgICBzdGF0aWNTeW1ib2wgPSBjb21wb25lbnQuZGlyZWN0aXZlLnR5cGUucmVmZXJlbmNlIGFzIFN0YXRpY1N5bWJvbDtcbiAgICAgICAgICAgIHN5bWJvbCA9IGluZm8udGVtcGxhdGUucXVlcnkuZ2V0VHlwZVN5bWJvbChzdGF0aWNTeW1ib2wpO1xuICAgICAgICAgICAgc3ltYm9sID0gc3ltYm9sICYmIG5ldyBPdmVycmlkZUtpbmRTeW1ib2woc3ltYm9sLCBEaXJlY3RpdmVLaW5kLkNPTVBPTkVOVCk7XG4gICAgICAgICAgICBzcGFuID0gc3Bhbk9mKGFzdCk7XG4gICAgICAgICAgfSBlbHNlIHtcbiAgICAgICAgICAgIC8vIEZpbmQgYSBkaXJlY3RpdmUgdGhhdCBtYXRjaGVzIHRoZSBlbGVtZW50IG5hbWVcbiAgICAgICAgICAgIGNvbnN0IGRpcmVjdGl2ZSA9IGFzdC5kaXJlY3RpdmVzLmZpbmQoXG4gICAgICAgICAgICAgICAgZCA9PiBkLmRpcmVjdGl2ZS5zZWxlY3RvciAhPSBudWxsICYmIGQuZGlyZWN0aXZlLnNlbGVjdG9yLmluZGV4T2YoYXN0Lm5hbWUpID49IDApO1xuICAgICAgICAgICAgaWYgKGRpcmVjdGl2ZSkge1xuICAgICAgICAgICAgICAvLyBOZWVkIHRvIGNhc3QgYmVjYXVzZSAncmVmZXJlbmNlJyBpcyB0eXBlZCBhcyBhbnlcbiAgICAgICAgICAgICAgc3RhdGljU3ltYm9sID0gZGlyZWN0aXZlLmRpcmVjdGl2ZS50eXBlLnJlZmVyZW5jZSBhcyBTdGF0aWNTeW1ib2w7XG4gICAgICAgICAgICAgIHN5bWJvbCA9IGluZm8udGVtcGxhdGUucXVlcnkuZ2V0VHlwZVN5bWJvbChzdGF0aWNTeW1ib2wpO1xuICAgICAgICAgICAgICBzeW1ib2wgPSBzeW1ib2wgJiYgbmV3IE92ZXJyaWRlS2luZFN5bWJvbChzeW1ib2wsIERpcmVjdGl2ZUtpbmQuRElSRUNUSVZFKTtcbiAgICAgICAgICAgICAgc3BhbiA9IHNwYW5PZihhc3QpO1xuICAgICAgICAgICAgfVxuICAgICAgICAgIH1cbiAgICAgICAgfSxcbiAgICAgICAgdmlzaXRSZWZlcmVuY2UoYXN0KSB7XG4gICAgICAgICAgc3ltYm9sID0gYXN0LnZhbHVlICYmIGluZm8udGVtcGxhdGUucXVlcnkuZ2V0VHlwZVN5bWJvbCh0b2tlblJlZmVyZW5jZShhc3QudmFsdWUpKTtcbiAgICAgICAgICBzcGFuID0gc3Bhbk9mKGFzdCk7XG4gICAgICAgIH0sXG4gICAgICAgIHZpc2l0VmFyaWFibGUoX2FzdCkge30sXG4gICAgICAgIHZpc2l0RXZlbnQoYXN0KSB7XG4gICAgICAgICAgaWYgKCFhdHRyaWJ1dGVWYWx1ZVN5bWJvbChhc3QuaGFuZGxlcikpIHtcbiAgICAgICAgICAgIHN5bWJvbCA9IGZpbmRPdXRwdXRCaW5kaW5nKGFzdCwgcGF0aCwgaW5mby50ZW1wbGF0ZS5xdWVyeSk7XG4gICAgICAgICAgICBzeW1ib2wgPSBzeW1ib2wgJiYgbmV3IE92ZXJyaWRlS2luZFN5bWJvbChzeW1ib2wsIERpcmVjdGl2ZUtpbmQuRVZFTlQpO1xuICAgICAgICAgICAgc3BhbiA9IHNwYW5PZihhc3QpO1xuICAgICAgICAgIH1cbiAgICAgICAgfSxcbiAgICAgICAgdmlzaXRFbGVtZW50UHJvcGVydHkoYXN0KSB7XG4gICAgICAgICAgYXR0cmlidXRlVmFsdWVTeW1ib2woYXN0LnZhbHVlKTtcbiAgICAgICAgfSxcbiAgICAgICAgdmlzaXRBdHRyKGFzdCkge1xuICAgICAgICAgIGNvbnN0IGVsZW1lbnQgPSBwYXRoLmZpcnN0KEVsZW1lbnRBc3QpO1xuICAgICAgICAgIGlmICghZWxlbWVudCkgcmV0dXJuO1xuICAgICAgICAgIC8vIENyZWF0ZSBhIG1hcHBpbmcgb2YgYWxsIGRpcmVjdGl2ZXMgYXBwbGllZCB0byB0aGUgZWxlbWVudCBmcm9tIHRoZWlyIHNlbGVjdG9ycy5cbiAgICAgICAgICBjb25zdCBtYXRjaGVyID0gbmV3IFNlbGVjdG9yTWF0Y2hlcjxEaXJlY3RpdmVBc3Q+KCk7XG4gICAgICAgICAgZm9yIChjb25zdCBkaXIgb2YgZWxlbWVudC5kaXJlY3RpdmVzKSB7XG4gICAgICAgICAgICBpZiAoIWRpci5kaXJlY3RpdmUuc2VsZWN0b3IpIGNvbnRpbnVlO1xuICAgICAgICAgICAgbWF0Y2hlci5hZGRTZWxlY3RhYmxlcyhDc3NTZWxlY3Rvci5wYXJzZShkaXIuZGlyZWN0aXZlLnNlbGVjdG9yKSwgZGlyKTtcbiAgICAgICAgICB9XG5cbiAgICAgICAgICAvLyBTZWUgaWYgdGhpcyBhdHRyaWJ1dGUgbWF0Y2hlcyB0aGUgc2VsZWN0b3Igb2YgYW55IGRpcmVjdGl2ZSBvbiB0aGUgZWxlbWVudC5cbiAgICAgICAgICBjb25zdCBhdHRyaWJ1dGVTZWxlY3RvciA9IGBbJHthc3QubmFtZX09JHthc3QudmFsdWV9XWA7XG4gICAgICAgICAgY29uc3QgcGFyc2VkQXR0cmlidXRlID0gQ3NzU2VsZWN0b3IucGFyc2UoYXR0cmlidXRlU2VsZWN0b3IpO1xuICAgICAgICAgIGlmICghcGFyc2VkQXR0cmlidXRlLmxlbmd0aCkgcmV0dXJuO1xuICAgICAgICAgIG1hdGNoZXIubWF0Y2gocGFyc2VkQXR0cmlidXRlWzBdLCAoXywge2RpcmVjdGl2ZX0pID0+IHtcbiAgICAgICAgICAgIC8vIE5lZWQgdG8gY2FzdCBiZWNhdXNlICdyZWZlcmVuY2UnIGlzIHR5cGVkIGFzIGFueVxuICAgICAgICAgICAgc3RhdGljU3ltYm9sID0gZGlyZWN0aXZlLnR5cGUucmVmZXJlbmNlIGFzIFN0YXRpY1N5bWJvbDtcbiAgICAgICAgICAgIHN5bWJvbCA9IGluZm8udGVtcGxhdGUucXVlcnkuZ2V0VHlwZVN5bWJvbChzdGF0aWNTeW1ib2wpO1xuICAgICAgICAgICAgc3ltYm9sID0gc3ltYm9sICYmIG5ldyBPdmVycmlkZUtpbmRTeW1ib2woc3ltYm9sLCBEaXJlY3RpdmVLaW5kLkRJUkVDVElWRSk7XG4gICAgICAgICAgICBzcGFuID0gc3Bhbk9mKGFzdCk7XG4gICAgICAgICAgfSk7XG4gICAgICAgIH0sXG4gICAgICAgIHZpc2l0Qm91bmRUZXh0KGFzdCkge1xuICAgICAgICAgIGNvbnN0IGV4cHJlc3Npb25Qb3NpdGlvbiA9IHRlbXBsYXRlUG9zaXRpb24gLSBhc3Quc291cmNlU3Bhbi5zdGFydC5vZmZzZXQ7XG4gICAgICAgICAgaWYgKGluU3BhbihleHByZXNzaW9uUG9zaXRpb24sIGFzdC52YWx1ZS5zcGFuKSkge1xuICAgICAgICAgICAgY29uc3QgZGluZm8gPSBkaWFnbm9zdGljSW5mb0Zyb21UZW1wbGF0ZUluZm8oaW5mbyk7XG4gICAgICAgICAgICBjb25zdCBzY29wZSA9IGdldEV4cHJlc3Npb25TY29wZShkaW5mbywgcGF0aCk7XG4gICAgICAgICAgICBjb25zdCByZXN1bHQgPSBnZXRFeHByZXNzaW9uU3ltYm9sKHNjb3BlLCBhc3QudmFsdWUsIHRlbXBsYXRlUG9zaXRpb24sIGluZm8udGVtcGxhdGUpO1xuICAgICAgICAgICAgaWYgKHJlc3VsdCkge1xuICAgICAgICAgICAgICBzeW1ib2wgPSByZXN1bHQuc3ltYm9sO1xuICAgICAgICAgICAgICBzcGFuID0gb2Zmc2V0U3BhbihyZXN1bHQuc3BhbiwgYXN0LnNvdXJjZVNwYW4uc3RhcnQub2Zmc2V0KTtcbiAgICAgICAgICAgIH1cbiAgICAgICAgICB9XG4gICAgICAgIH0sXG4gICAgICAgIHZpc2l0VGV4dChfYXN0KSB7fSxcbiAgICAgICAgdmlzaXREaXJlY3RpdmUoYXN0KSB7XG4gICAgICAgICAgLy8gTmVlZCB0byBjYXN0IGJlY2F1c2UgJ3JlZmVyZW5jZScgaXMgdHlwZWQgYXMgYW55XG4gICAgICAgICAgc3RhdGljU3ltYm9sID0gYXN0LmRpcmVjdGl2ZS50eXBlLnJlZmVyZW5jZSBhcyBTdGF0aWNTeW1ib2w7XG4gICAgICAgICAgc3ltYm9sID0gaW5mby50ZW1wbGF0ZS5xdWVyeS5nZXRUeXBlU3ltYm9sKHN0YXRpY1N5bWJvbCk7XG4gICAgICAgICAgc3BhbiA9IHNwYW5PZihhc3QpO1xuICAgICAgICB9LFxuICAgICAgICB2aXNpdERpcmVjdGl2ZVByb3BlcnR5KGFzdCkge1xuICAgICAgICAgIGlmICghYXR0cmlidXRlVmFsdWVTeW1ib2woYXN0LnZhbHVlKSkge1xuICAgICAgICAgICAgY29uc3QgZGlyZWN0aXZlID0gZmluZFBhcmVudE9mQmluZGluZyhpbmZvLnRlbXBsYXRlQXN0LCBhc3QsIHRlbXBsYXRlUG9zaXRpb24pO1xuICAgICAgICAgICAgY29uc3QgYXR0cmlidXRlID0gZmluZEF0dHJpYnV0ZShpbmZvLCBwb3NpdGlvbik7XG4gICAgICAgICAgICBpZiAoZGlyZWN0aXZlICYmIGF0dHJpYnV0ZSkge1xuICAgICAgICAgICAgICBpZiAoYXR0cmlidXRlLm5hbWUuc3RhcnRzV2l0aCgnKicpKSB7XG4gICAgICAgICAgICAgICAgY29uc3QgY29tcGlsZVR5cGVTdW1tYXJ5ID0gZGlyZWN0aXZlLmRpcmVjdGl2ZTtcbiAgICAgICAgICAgICAgICBzeW1ib2wgPSBpbmZvLnRlbXBsYXRlLnF1ZXJ5LmdldFR5cGVTeW1ib2woY29tcGlsZVR5cGVTdW1tYXJ5LnR5cGUucmVmZXJlbmNlKTtcbiAgICAgICAgICAgICAgICBzeW1ib2wgPSBzeW1ib2wgJiYgbmV3IE92ZXJyaWRlS2luZFN5bWJvbChzeW1ib2wsIERpcmVjdGl2ZUtpbmQuRElSRUNUSVZFKTtcbiAgICAgICAgICAgICAgICAvLyBVc2UgJ2F0dHJpYnV0ZS5zb3VyY2VTcGFuJyBpbnN0ZWFkIG9mIHRoZSBkaXJlY3RpdmUncyxcbiAgICAgICAgICAgICAgICAvLyBiZWNhdXNlIHRoZSBzcGFuIG9mIHRoZSBkaXJlY3RpdmUgaXMgdGhlIHdob2xlIG9wZW5pbmcgdGFnIG9mIGFuIGVsZW1lbnQuXG4gICAgICAgICAgICAgICAgc3BhbiA9IHNwYW5PZihhdHRyaWJ1dGUuc291cmNlU3Bhbik7XG4gICAgICAgICAgICAgIH0gZWxzZSB7XG4gICAgICAgICAgICAgICAgc3ltYm9sID0gZmluZElucHV0QmluZGluZyhpbmZvLCBhc3QudGVtcGxhdGVOYW1lLCBkaXJlY3RpdmUpO1xuICAgICAgICAgICAgICAgIHNwYW4gPSBzcGFuT2YoYXN0KTtcbiAgICAgICAgICAgICAgfVxuICAgICAgICAgICAgfVxuICAgICAgICAgIH1cbiAgICAgICAgfVxuICAgICAgfSxcbiAgICAgIG51bGwpO1xuICBpZiAoc3ltYm9sICYmIHNwYW4pIHtcbiAgICBjb25zdCB7c3RhcnQsIGVuZH0gPSBvZmZzZXRTcGFuKHNwYW4sIGluZm8udGVtcGxhdGUuc3Bhbi5zdGFydCk7XG4gICAgcmV0dXJuIHtcbiAgICAgIHN5bWJvbCxcbiAgICAgIHNwYW46IHRzcy5jcmVhdGVUZXh0U3BhbkZyb21Cb3VuZHMoc3RhcnQsIGVuZCksXG4gICAgICBzdGF0aWNTeW1ib2wsXG4gICAgfTtcbiAgfVxufVxuXG4vLyBHZXQgdGhlIHN5bWJvbCBpbiBtaWNyb3N5bnRheCBhdCB0ZW1wbGF0ZSBwb3NpdGlvbi5cbmZ1bmN0aW9uIGdldFN5bWJvbEluTWljcm9zeW50YXgoaW5mbzogQXN0UmVzdWx0LCBwYXRoOiBUZW1wbGF0ZUFzdFBhdGgsIGF0dHJpYnV0ZTogQXR0cmlidXRlKTpcbiAgICB7c3ltYm9sOiBTeW1ib2wsIHNwYW46IFNwYW59fHVuZGVmaW5lZCB7XG4gIGlmICghYXR0cmlidXRlLnZhbHVlU3Bhbikge1xuICAgIHJldHVybjtcbiAgfVxuICBjb25zdCBhYnNWYWx1ZU9mZnNldCA9IGF0dHJpYnV0ZS52YWx1ZVNwYW4uc3RhcnQub2Zmc2V0O1xuICBsZXQgcmVzdWx0OiB7c3ltYm9sOiBTeW1ib2wsIHNwYW46IFNwYW59fHVuZGVmaW5lZDtcbiAgY29uc3Qge3RlbXBsYXRlQmluZGluZ3N9ID0gaW5mby5leHByZXNzaW9uUGFyc2VyLnBhcnNlVGVtcGxhdGVCaW5kaW5ncyhcbiAgICAgIGF0dHJpYnV0ZS5uYW1lLCBhdHRyaWJ1dGUudmFsdWUsIGF0dHJpYnV0ZS5zb3VyY2VTcGFuLnRvU3RyaW5nKCksXG4gICAgICBhdHRyaWJ1dGUuc291cmNlU3Bhbi5zdGFydC5vZmZzZXQsIGF0dHJpYnV0ZS52YWx1ZVNwYW4uc3RhcnQub2Zmc2V0KTtcblxuICAvLyBGaW5kIHRoZSBzeW1ib2wgdGhhdCBjb250YWlucyB0aGUgcG9zaXRpb24uXG4gIGZvciAoY29uc3QgdGIgb2YgdGVtcGxhdGVCaW5kaW5ncykge1xuICAgIGlmICh0YiBpbnN0YW5jZW9mIFZhcmlhYmxlQmluZGluZykge1xuICAgICAgLy8gVE9ETyhreWxpYXUpOiBpZiBiaW5kaW5nIGlzIHZhcmlhYmxlIHdlIHNob3VsZCBzdGlsbCBsb29rIGZvciB0aGUgdmFsdWVcbiAgICAgIC8vIG9mIHRoZSBrZXkuIEZvciBleGFtcGxlLCBcImxldCBpPWluZGV4XCIgPT4gXCJpbmRleFwiIHNob3VsZCBwb2ludCB0b1xuICAgICAgLy8gTmdGb3JPZkNvbnRleHQuaW5kZXhcbiAgICAgIGNvbnRpbnVlO1xuICAgIH1cbiAgICBpZiAoaW5TcGFuKHBhdGgucG9zaXRpb24sIHRiLnZhbHVlPy5hc3Quc291cmNlU3BhbikpIHtcbiAgICAgIGNvbnN0IGRpbmZvID0gZGlhZ25vc3RpY0luZm9Gcm9tVGVtcGxhdGVJbmZvKGluZm8pO1xuICAgICAgY29uc3Qgc2NvcGUgPSBnZXRFeHByZXNzaW9uU2NvcGUoZGluZm8sIHBhdGgpO1xuICAgICAgcmVzdWx0ID0gZ2V0RXhwcmVzc2lvblN5bWJvbChzY29wZSwgdGIudmFsdWUhLCBwYXRoLnBvc2l0aW9uLCBpbmZvLnRlbXBsYXRlKTtcbiAgICB9IGVsc2UgaWYgKGluU3BhbihwYXRoLnBvc2l0aW9uLCB0Yi5zb3VyY2VTcGFuKSkge1xuICAgICAgY29uc3QgdGVtcGxhdGUgPSBwYXRoLmZpcnN0KEVtYmVkZGVkVGVtcGxhdGVBc3QpO1xuICAgICAgaWYgKHRlbXBsYXRlKSB7XG4gICAgICAgIC8vIE9uZSBlbGVtZW50IGNhbiBvbmx5IGhhdmUgb25lIHRlbXBsYXRlIGJpbmRpbmcuXG4gICAgICAgIGNvbnN0IGRpcmVjdGl2ZUFzdCA9IHRlbXBsYXRlLmRpcmVjdGl2ZXNbMF07XG4gICAgICAgIGlmIChkaXJlY3RpdmVBc3QpIHtcbiAgICAgICAgICBjb25zdCBzeW1ib2wgPSBmaW5kSW5wdXRCaW5kaW5nKGluZm8sIHRiLmtleS5zb3VyY2Uuc3Vic3RyaW5nKDEpLCBkaXJlY3RpdmVBc3QpO1xuICAgICAgICAgIGlmIChzeW1ib2wpIHtcbiAgICAgICAgICAgIHJlc3VsdCA9IHtcbiAgICAgICAgICAgICAgc3ltYm9sLFxuICAgICAgICAgICAgICAvLyB0aGUgc3BhbiBoZXJlIGhhcyB0byBiZSByZWxhdGl2ZSB0byB0aGUgc3RhcnQgb2YgdGhlIHRlbXBsYXRlXG4gICAgICAgICAgICAgIC8vIHZhbHVlIHNvIGRlZHVjdCB0aGUgYWJzb2x1dGUgb2Zmc2V0LlxuICAgICAgICAgICAgICAvLyBUT0RPKGt5bGlhdSk6IFVzZSBhYnNvbHV0ZSBzb3VyY2Ugc3BhbiB0aHJvdWdob3V0IGNvbXBsZXRpb25zLlxuICAgICAgICAgICAgICBzcGFuOiBvZmZzZXRTcGFuKHRiLmtleS5zcGFuLCAtYWJzVmFsdWVPZmZzZXQpLFxuICAgICAgICAgICAgfTtcbiAgICAgICAgICB9XG4gICAgICAgIH1cbiAgICAgIH1cbiAgICB9XG4gIH1cbiAgcmV0dXJuIHJlc3VsdDtcbn1cblxuZnVuY3Rpb24gZmluZEF0dHJpYnV0ZShpbmZvOiBBc3RSZXN1bHQsIHBvc2l0aW9uOiBudW1iZXIpOiBBdHRyaWJ1dGV8dW5kZWZpbmVkIHtcbiAgY29uc3QgdGVtcGxhdGVQb3NpdGlvbiA9IHBvc2l0aW9uIC0gaW5mby50ZW1wbGF0ZS5zcGFuLnN0YXJ0O1xuICBjb25zdCBwYXRoID0gZ2V0UGF0aFRvTm9kZUF0UG9zaXRpb24oaW5mby5odG1sQXN0LCB0ZW1wbGF0ZVBvc2l0aW9uKTtcbiAgcmV0dXJuIHBhdGguZmlyc3QoQXR0cmlidXRlKTtcbn1cblxuLy8gVE9ETzogcmVtb3ZlIHRoaXMgZnVuY3Rpb24gYWZ0ZXIgdGhlIHBhdGggaW5jbHVkZXMgJ0RpcmVjdGl2ZUFzdCcuXG4vLyBGaW5kIHRoZSBkaXJlY3RpdmUgdGhhdCBjb3JyZXNwb25kcyB0byB0aGUgc3BlY2lmaWVkICdiaW5kaW5nJ1xuLy8gYXQgdGhlIHNwZWNpZmllZCAncG9zaXRpb24nIGluIHRoZSAnYXN0Jy5cbmZ1bmN0aW9uIGZpbmRQYXJlbnRPZkJpbmRpbmcoXG4gICAgYXN0OiBUZW1wbGF0ZUFzdFtdLCBiaW5kaW5nOiBCb3VuZERpcmVjdGl2ZVByb3BlcnR5QXN0LCBwb3NpdGlvbjogbnVtYmVyKTogRGlyZWN0aXZlQXN0fFxuICAgIHVuZGVmaW5lZCB7XG4gIGxldCByZXM6IERpcmVjdGl2ZUFzdHx1bmRlZmluZWQ7XG4gIGNvbnN0IHZpc2l0b3IgPSBuZXcgY2xhc3MgZXh0ZW5kcyBSZWN1cnNpdmVUZW1wbGF0ZUFzdFZpc2l0b3Ige1xuICAgIHZpc2l0KGFzdDogVGVtcGxhdGVBc3QpOiBhbnkge1xuICAgICAgY29uc3Qgc3BhbiA9IHNwYW5PZihhc3QpO1xuICAgICAgaWYgKCFpblNwYW4ocG9zaXRpb24sIHNwYW4pKSB7XG4gICAgICAgIC8vIFJldHVybmluZyBhIHZhbHVlIGhlcmUgd2lsbCByZXN1bHQgaW4gdGhlIGNoaWxkcmVuIGJlaW5nIHNraXBwZWQuXG4gICAgICAgIHJldHVybiB0cnVlO1xuICAgICAgfVxuICAgIH1cblxuICAgIHZpc2l0RW1iZWRkZWRUZW1wbGF0ZShhc3Q6IEVtYmVkZGVkVGVtcGxhdGVBc3QsIGNvbnRleHQ6IGFueSk6IGFueSB7XG4gICAgICByZXR1cm4gdGhpcy52aXNpdENoaWxkcmVuKGNvbnRleHQsIHZpc2l0ID0+IHtcbiAgICAgICAgdmlzaXQoYXN0LmRpcmVjdGl2ZXMpO1xuICAgICAgICB2aXNpdChhc3QuY2hpbGRyZW4pO1xuICAgICAgfSk7XG4gICAgfVxuXG4gICAgdmlzaXRFbGVtZW50KGFzdDogRWxlbWVudEFzdCwgY29udGV4dDogYW55KTogYW55IHtcbiAgICAgIHJldHVybiB0aGlzLnZpc2l0Q2hpbGRyZW4oY29udGV4dCwgdmlzaXQgPT4ge1xuICAgICAgICB2aXNpdChhc3QuZGlyZWN0aXZlcyk7XG4gICAgICAgIHZpc2l0KGFzdC5jaGlsZHJlbik7XG4gICAgICB9KTtcbiAgICB9XG5cbiAgICB2aXNpdERpcmVjdGl2ZShhc3Q6IERpcmVjdGl2ZUFzdCkge1xuICAgICAgY29uc3QgcmVzdWx0ID0gdGhpcy52aXNpdENoaWxkcmVuKGFzdCwgdmlzaXQgPT4ge1xuICAgICAgICB2aXNpdChhc3QuaW5wdXRzKTtcbiAgICAgIH0pO1xuICAgICAgcmV0dXJuIHJlc3VsdDtcbiAgICB9XG5cbiAgICB2aXNpdERpcmVjdGl2ZVByb3BlcnR5KGFzdDogQm91bmREaXJlY3RpdmVQcm9wZXJ0eUFzdCwgY29udGV4dDogRGlyZWN0aXZlQXN0KSB7XG4gICAgICBpZiAoYXN0ID09PSBiaW5kaW5nKSB7XG4gICAgICAgIHJlcyA9IGNvbnRleHQ7XG4gICAgICB9XG4gICAgfVxuICB9O1xuICB0ZW1wbGF0ZVZpc2l0QWxsKHZpc2l0b3IsIGFzdCk7XG4gIHJldHVybiByZXM7XG59XG5cbi8vIEZpbmQgdGhlIHN5bWJvbCBvZiBpbnB1dCBiaW5kaW5nIGluICdkaXJlY3RpdmVBc3QnIGJ5ICduYW1lJy5cbmZ1bmN0aW9uIGZpbmRJbnB1dEJpbmRpbmcoaW5mbzogQXN0UmVzdWx0LCBuYW1lOiBzdHJpbmcsIGRpcmVjdGl2ZUFzdDogRGlyZWN0aXZlQXN0KTogU3ltYm9sfFxuICAgIHVuZGVmaW5lZCB7XG4gIGNvbnN0IGludmVydGVkSW5wdXQgPSBpbnZlcnRNYXAoZGlyZWN0aXZlQXN0LmRpcmVjdGl2ZS5pbnB1dHMpO1xuICBjb25zdCBmaWVsZE5hbWUgPSBpbnZlcnRlZElucHV0W25hbWVdO1xuICBpZiAoZmllbGROYW1lKSB7XG4gICAgY29uc3QgY2xhc3NTeW1ib2wgPSBpbmZvLnRlbXBsYXRlLnF1ZXJ5LmdldFR5cGVTeW1ib2woZGlyZWN0aXZlQXN0LmRpcmVjdGl2ZS50eXBlLnJlZmVyZW5jZSk7XG4gICAgaWYgKGNsYXNzU3ltYm9sKSB7XG4gICAgICByZXR1cm4gY2xhc3NTeW1ib2wubWVtYmVycygpLmdldChmaWVsZE5hbWUpO1xuICAgIH1cbiAgfVxufVxuXG4vKipcbiAqIFdyYXAgYSBzeW1ib2wgYW5kIGNoYW5nZSBpdHMga2luZCB0byBjb21wb25lbnQuXG4gKi9cbmNsYXNzIE92ZXJyaWRlS2luZFN5bWJvbCBpbXBsZW1lbnRzIFN5bWJvbCB7XG4gIHB1YmxpYyByZWFkb25seSBraW5kOiBEaXJlY3RpdmVLaW5kO1xuICBjb25zdHJ1Y3Rvcihwcml2YXRlIHN5bTogU3ltYm9sLCBraW5kT3ZlcnJpZGU6IERpcmVjdGl2ZUtpbmQpIHtcbiAgICB0aGlzLmtpbmQgPSBraW5kT3ZlcnJpZGU7XG4gIH1cblxuICBnZXQgbmFtZSgpOiBzdHJpbmcge1xuICAgIHJldHVybiB0aGlzLnN5bS5uYW1lO1xuICB9XG5cbiAgZ2V0IGxhbmd1YWdlKCk6IHN0cmluZyB7XG4gICAgcmV0dXJuIHRoaXMuc3ltLmxhbmd1YWdlO1xuICB9XG5cbiAgZ2V0IHR5cGUoKTogU3ltYm9sfHVuZGVmaW5lZCB7XG4gICAgcmV0dXJuIHRoaXMuc3ltLnR5cGU7XG4gIH1cblxuICBnZXQgY29udGFpbmVyKCk6IFN5bWJvbHx1bmRlZmluZWQge1xuICAgIHJldHVybiB0aGlzLnN5bS5jb250YWluZXI7XG4gIH1cblxuICBnZXQgcHVibGljKCk6IGJvb2xlYW4ge1xuICAgIHJldHVybiB0aGlzLnN5bS5wdWJsaWM7XG4gIH1cblxuICBnZXQgY2FsbGFibGUoKTogYm9vbGVhbiB7XG4gICAgcmV0dXJuIHRoaXMuc3ltLmNhbGxhYmxlO1xuICB9XG5cbiAgZ2V0IG51bGxhYmxlKCk6IGJvb2xlYW4ge1xuICAgIHJldHVybiB0aGlzLnN5bS5udWxsYWJsZTtcbiAgfVxuXG4gIGdldCBkZWZpbml0aW9uKCk6IERlZmluaXRpb24ge1xuICAgIHJldHVybiB0aGlzLnN5bS5kZWZpbml0aW9uO1xuICB9XG5cbiAgZ2V0IGRvY3VtZW50YXRpb24oKTogdHMuU3ltYm9sRGlzcGxheVBhcnRbXSB7XG4gICAgcmV0dXJuIHRoaXMuc3ltLmRvY3VtZW50YXRpb247XG4gIH1cblxuICBtZW1iZXJzKCkge1xuICAgIHJldHVybiB0aGlzLnN5bS5tZW1iZXJzKCk7XG4gIH1cblxuICBzaWduYXR1cmVzKCkge1xuICAgIHJldHVybiB0aGlzLnN5bS5zaWduYXR1cmVzKCk7XG4gIH1cblxuICBzZWxlY3RTaWduYXR1cmUodHlwZXM6IFN5bWJvbFtdKSB7XG4gICAgcmV0dXJuIHRoaXMuc3ltLnNlbGVjdFNpZ25hdHVyZSh0eXBlcyk7XG4gIH1cblxuICBpbmRleGVkKGFyZ3VtZW50OiBTeW1ib2wpIHtcbiAgICByZXR1cm4gdGhpcy5zeW0uaW5kZXhlZChhcmd1bWVudCk7XG4gIH1cblxuICB0eXBlQXJndW1lbnRzKCk6IFN5bWJvbFtdfHVuZGVmaW5lZCB7XG4gICAgcmV0dXJuIHRoaXMuc3ltLnR5cGVBcmd1bWVudHMoKTtcbiAgfVxufVxuIl19