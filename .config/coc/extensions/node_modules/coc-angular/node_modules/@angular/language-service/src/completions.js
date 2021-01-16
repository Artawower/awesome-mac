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
        define("@angular/language-service/src/completions", ["require", "exports", "tslib", "@angular/compiler", "@angular/compiler/src/chars", "@angular/language-service/src/binding_utils", "@angular/language-service/src/expression_diagnostics", "@angular/language-service/src/expressions", "@angular/language-service/src/html_info", "@angular/language-service/src/template", "@angular/language-service/src/types", "@angular/language-service/src/utils"], factory);
    }
})(function (require, exports) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    exports.getTemplateCompletions = void 0;
    var tslib_1 = require("tslib");
    var compiler_1 = require("@angular/compiler");
    var chars_1 = require("@angular/compiler/src/chars");
    var binding_utils_1 = require("@angular/language-service/src/binding_utils");
    var expression_diagnostics_1 = require("@angular/language-service/src/expression_diagnostics");
    var expressions_1 = require("@angular/language-service/src/expressions");
    var html_info_1 = require("@angular/language-service/src/html_info");
    var template_1 = require("@angular/language-service/src/template");
    var ng = require("@angular/language-service/src/types");
    var utils_1 = require("@angular/language-service/src/utils");
    var HIDDEN_HTML_ELEMENTS = new Set(['html', 'script', 'noscript', 'base', 'body', 'title', 'head', 'link']);
    var HTML_ELEMENTS = html_info_1.elementNames().filter(function (name) { return !HIDDEN_HTML_ELEMENTS.has(name); }).map(function (name) {
        return {
            name: name,
            kind: ng.CompletionKind.HTML_ELEMENT,
            sortText: name,
        };
    });
    var ANGULAR_ELEMENTS = [
        {
            name: 'ng-container',
            kind: ng.CompletionKind.ANGULAR_ELEMENT,
            sortText: 'ng-container',
        },
        {
            name: 'ng-content',
            kind: ng.CompletionKind.ANGULAR_ELEMENT,
            sortText: 'ng-content',
        },
        {
            name: 'ng-template',
            kind: ng.CompletionKind.ANGULAR_ELEMENT,
            sortText: 'ng-template',
        },
    ];
    function isIdentifierPart(code) {
        // Identifiers consist of alphanumeric characters, '_', or '$'.
        return chars_1.isAsciiLetter(code) || chars_1.isDigit(code) || code == chars_1.$$ || code == chars_1.$_;
    }
    /**
     * Gets the span of word in a template that surrounds `position`. If there is no word around
     * `position`, nothing is returned.
     */
    function getBoundedWordSpan(templateInfo, position, ast) {
        var template = templateInfo.template;
        var templateSrc = template.source;
        if (!templateSrc)
            return;
        if (ast instanceof compiler_1.Element) {
            // The HTML tag may include `-` (e.g. `app-root`),
            // so use the HtmlAst to get the span before ayazhafiz refactor the code.
            return {
                start: templateInfo.template.span.start + ast.startSourceSpan.start.offset + 1,
                length: ast.name.length
            };
        }
        // TODO(ayazhafiz): A solution based on word expansion will always be expensive compared to one
        // based on ASTs. Whatever penalty we incur is probably manageable for small-length (i.e. the
        // majority of) identifiers, but the current solution involes a number of branchings and we can't
        // control potentially very long identifiers. Consider moving to an AST-based solution once
        // existing difficulties with AST spans are more clearly resolved (see #31898 for discussion of
        // known problems, and #33091 for how they affect text replacement).
        //
        // `templatePosition` represents the right-bound location of a cursor in the template.
        //    key.ent|ry
        //           ^---- cursor, at position `r` is at.
        // A cursor is not itself a character in the template; it has a left (lower) and right (upper)
        // index bound that hugs the cursor itself.
        var templatePosition = position - template.span.start;
        // To perform word expansion, we want to determine the left and right indices that hug the cursor.
        // There are three cases here.
        var left, right;
        if (templatePosition === 0) {
            // 1. Case like
            //      |rest of template
            //    the cursor is at the start of the template, hugged only by the right side (0-index).
            left = right = 0;
        }
        else if (templatePosition === templateSrc.length) {
            // 2. Case like
            //      rest of template|
            //    the cursor is at the end of the template, hugged only by the left side (last-index).
            left = right = templateSrc.length - 1;
        }
        else {
            // 3. Case like
            //      wo|rd
            //    there is a clear left and right index.
            left = templatePosition - 1;
            right = templatePosition;
        }
        if (!isIdentifierPart(templateSrc.charCodeAt(left)) &&
            !isIdentifierPart(templateSrc.charCodeAt(right))) {
            // Case like
            //         .|.
            // left ---^ ^--- right
            // There is no word here.
            return;
        }
        // Expand on the left and right side until a word boundary is hit. Back up one expansion on both
        // side to stay inside the word.
        while (left >= 0 && isIdentifierPart(templateSrc.charCodeAt(left)))
            --left;
        ++left;
        while (right < templateSrc.length && isIdentifierPart(templateSrc.charCodeAt(right)))
            ++right;
        --right;
        var absoluteStartPosition = position - (templatePosition - left);
        var length = right - left + 1;
        return { start: absoluteStartPosition, length: length };
    }
    function getTemplateCompletions(templateInfo, position) {
        var htmlAst = templateInfo.htmlAst, template = templateInfo.template;
        // Calculate the position relative to the start of the template. This is needed
        // because spans in HTML AST are relative. Inline template has non-zero start position.
        var templatePosition = position - template.span.start;
        var htmlPath = utils_1.getPathToNodeAtPosition(htmlAst, templatePosition);
        var mostSpecific = htmlPath.tail;
        var visitor = new HtmlVisitor(templateInfo, htmlPath);
        var results = mostSpecific ?
            mostSpecific.visit(visitor, null /* context */) :
            elementCompletions(templateInfo);
        var replacementSpan = getBoundedWordSpan(templateInfo, position, mostSpecific);
        return results.map(function (entry) {
            return tslib_1.__assign(tslib_1.__assign({}, entry), { replacementSpan: replacementSpan });
        });
    }
    exports.getTemplateCompletions = getTemplateCompletions;
    var HtmlVisitor = /** @class */ (function () {
        function HtmlVisitor(templateInfo, htmlPath) {
            this.templateInfo = templateInfo;
            this.htmlPath = htmlPath;
            this.relativePosition = htmlPath.position;
        }
        // Note that every visitor method must explicitly specify return type because
        // Visitor returns `any` for all methods.
        HtmlVisitor.prototype.visitElement = function (ast) {
            var startTagSpan = utils_1.spanOf(ast.sourceSpan);
            var tagLen = ast.name.length;
            // + 1 for the opening angle bracket
            if (this.relativePosition <= startTagSpan.start + tagLen + 1) {
                // If we are in the tag then return the element completions.
                return elementCompletions(this.templateInfo);
            }
            if (this.relativePosition < startTagSpan.end) {
                // We are in the attribute section of the element (but not in an attribute).
                // Return the attribute completions.
                return attributeCompletionsForElement(this.templateInfo, ast.name);
            }
            return [];
        };
        HtmlVisitor.prototype.visitAttribute = function (ast) {
            // An attribute consists of two parts, LHS="RHS".
            // Determine if completions are requested for LHS or RHS
            if (ast.valueSpan && utils_1.inSpan(this.relativePosition, utils_1.spanOf(ast.valueSpan))) {
                // RHS completion
                return attributeValueCompletions(this.templateInfo, this.htmlPath);
            }
            // LHS completion
            return attributeCompletions(this.templateInfo, this.htmlPath);
        };
        HtmlVisitor.prototype.visitText = function () {
            var _this = this;
            var _a;
            var templatePath = utils_1.findTemplateAstAt(this.templateInfo.templateAst, this.relativePosition);
            if (templatePath.tail instanceof compiler_1.BoundTextAst) {
                // If we know that this is an interpolation then do not try other scenarios.
                var visitor = new ExpressionVisitor(this.templateInfo, this.relativePosition, function () {
                    return expression_diagnostics_1.getExpressionScope(utils_1.diagnosticInfoFromTemplateInfo(_this.templateInfo), templatePath);
                });
                (_a = templatePath.tail) === null || _a === void 0 ? void 0 : _a.visit(visitor, null);
                return visitor.results;
            }
            // TODO(kyliau): Not sure if this check is really needed since we don't have
            // any test cases for it.
            var element = this.htmlPath.first(compiler_1.Element);
            if (element &&
                compiler_1.getHtmlTagDefinition(element.name).contentType !== compiler_1.TagContentType.PARSABLE_DATA) {
                return [];
            }
            // This is to account for cases like <h1> <a> text | </h1> where the
            // closest element has no closing tag and thus is considered plain text.
            var results = voidElementAttributeCompletions(this.templateInfo, this.htmlPath);
            if (results.length) {
                return results;
            }
            return elementCompletions(this.templateInfo);
        };
        HtmlVisitor.prototype.visitComment = function () {
            return [];
        };
        HtmlVisitor.prototype.visitExpansion = function () {
            return [];
        };
        HtmlVisitor.prototype.visitExpansionCase = function () {
            return [];
        };
        return HtmlVisitor;
    }());
    function attributeCompletions(info, path) {
        var attr = path.tail;
        var elem = path.parentOf(attr);
        if (!(attr instanceof compiler_1.Attribute) || !(elem instanceof compiler_1.Element)) {
            return [];
        }
        // TODO: Consider parsing the attrinute name to a proper AST instead of
        // matching using regex. This is because the regexp would incorrectly identify
        // bind parts for cases like [()|]
        //                              ^ cursor is here
        var binding = binding_utils_1.getBindingDescriptor(attr.name);
        if (!binding) {
            // This is a normal HTML attribute, not an Angular attribute.
            return attributeCompletionsForElement(info, elem.name);
        }
        var results = [];
        var ngAttrs = angularAttributes(info, elem.name);
        switch (binding.kind) {
            case binding_utils_1.ATTR.KW_MICROSYNTAX:
                // template reference attribute: *attrName
                results.push.apply(results, tslib_1.__spread(ngAttrs.templateRefs));
                break;
            case binding_utils_1.ATTR.KW_BIND:
            case binding_utils_1.ATTR.IDENT_PROPERTY:
                // property binding via bind- or []
                results.push.apply(results, tslib_1.__spread(html_info_1.propertyNames(elem.name), ngAttrs.inputs));
                break;
            case binding_utils_1.ATTR.KW_ON:
            case binding_utils_1.ATTR.IDENT_EVENT:
                // event binding via on- or ()
                results.push.apply(results, tslib_1.__spread(html_info_1.eventNames(elem.name), ngAttrs.outputs));
                break;
            case binding_utils_1.ATTR.KW_BINDON:
            case binding_utils_1.ATTR.IDENT_BANANA_BOX:
                // banana-in-a-box binding via bindon- or [()]
                results.push.apply(results, tslib_1.__spread(ngAttrs.bananas));
                break;
        }
        return results.map(function (name) {
            return {
                name: name,
                kind: ng.CompletionKind.ATTRIBUTE,
                sortText: name,
            };
        });
    }
    function attributeCompletionsForElement(info, elementName) {
        var e_1, _a, e_2, _b;
        var results = [];
        if (info.template instanceof template_1.InlineTemplate) {
            try {
                // Provide HTML attributes completion only for inline templates
                for (var _c = tslib_1.__values(html_info_1.attributeNames(elementName)), _d = _c.next(); !_d.done; _d = _c.next()) {
                    var name_1 = _d.value;
                    results.push({
                        name: name_1,
                        kind: ng.CompletionKind.HTML_ATTRIBUTE,
                        sortText: name_1,
                    });
                }
            }
            catch (e_1_1) { e_1 = { error: e_1_1 }; }
            finally {
                try {
                    if (_d && !_d.done && (_a = _c.return)) _a.call(_c);
                }
                finally { if (e_1) throw e_1.error; }
            }
        }
        // Add Angular attributes
        var ngAttrs = angularAttributes(info, elementName);
        try {
            for (var _e = tslib_1.__values(ngAttrs.others), _f = _e.next(); !_f.done; _f = _e.next()) {
                var name_2 = _f.value;
                results.push({
                    name: name_2,
                    kind: ng.CompletionKind.ATTRIBUTE,
                    sortText: name_2,
                });
            }
        }
        catch (e_2_1) { e_2 = { error: e_2_1 }; }
        finally {
            try {
                if (_f && !_f.done && (_b = _e.return)) _b.call(_e);
            }
            finally { if (e_2) throw e_2.error; }
        }
        return results;
    }
    /**
     * Provide completions to the RHS of an attribute, which is of the form
     * LHS="RHS". The template path is computed from the specified `info` whereas
     * the context is determined from the specified `htmlPath`.
     * @param info Object that contains the template AST
     * @param htmlPath Path to the HTML node
     */
    function attributeValueCompletions(info, htmlPath) {
        // Find the corresponding Template AST path.
        var templatePath = utils_1.findTemplateAstAt(info.templateAst, htmlPath.position);
        var visitor = new ExpressionVisitor(info, htmlPath.position, function () {
            var dinfo = utils_1.diagnosticInfoFromTemplateInfo(info);
            return expression_diagnostics_1.getExpressionScope(dinfo, templatePath);
        });
        if (templatePath.tail instanceof compiler_1.AttrAst ||
            templatePath.tail instanceof compiler_1.BoundElementPropertyAst ||
            templatePath.tail instanceof compiler_1.BoundEventAst) {
            templatePath.tail.visit(visitor, null);
            return visitor.results;
        }
        // In order to provide accurate attribute value completion, we need to know
        // what the LHS is, and construct the proper AST if it is missing.
        var htmlAttr = htmlPath.tail;
        var binding = binding_utils_1.getBindingDescriptor(htmlAttr.name);
        if (binding && binding.kind === binding_utils_1.ATTR.KW_REF) {
            var refAst = void 0;
            var elemAst = void 0;
            if (templatePath.tail instanceof compiler_1.ReferenceAst) {
                refAst = templatePath.tail;
                var parent_1 = templatePath.parentOf(refAst);
                if (parent_1 instanceof compiler_1.ElementAst) {
                    elemAst = parent_1;
                }
            }
            else if (templatePath.tail instanceof compiler_1.ElementAst) {
                refAst = new compiler_1.ReferenceAst(htmlAttr.name, null, htmlAttr.value, htmlAttr.valueSpan);
                elemAst = templatePath.tail;
            }
            if (refAst && elemAst) {
                refAst.visit(visitor, elemAst);
            }
        }
        else {
            // HtmlAst contains the `Attribute` node, however the corresponding `AttrAst`
            // node is missing from the TemplateAst.
            var attrAst = new compiler_1.AttrAst(htmlAttr.name, htmlAttr.value, htmlAttr.valueSpan);
            attrAst.visit(visitor, null);
        }
        return visitor.results;
    }
    function elementCompletions(info) {
        var e_3, _a;
        var results = tslib_1.__spread(ANGULAR_ELEMENTS);
        if (info.template instanceof template_1.InlineTemplate) {
            // Provide HTML elements completion only for inline templates
            results.push.apply(results, tslib_1.__spread(HTML_ELEMENTS));
        }
        // Collect the elements referenced by the selectors
        var components = new Set();
        try {
            for (var _b = tslib_1.__values(utils_1.getSelectors(info).selectors), _c = _b.next(); !_c.done; _c = _b.next()) {
                var selector = _c.value;
                var name_3 = selector.element;
                if (name_3 && !components.has(name_3)) {
                    components.add(name_3);
                    results.push({
                        name: name_3,
                        kind: ng.CompletionKind.COMPONENT,
                        sortText: name_3,
                    });
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
        return results;
    }
    // There is a special case of HTML where text that contains a unclosed tag is treated as
    // text. For exaple '<h1> Some <a text </h1>' produces a text nodes inside of the H1
    // element "Some <a text". We, however, want to treat this as if the user was requesting
    // the attributes of an "a" element, not requesting completion in the a text element. This
    // code checks for this case and returns element completions if it is detected or undefined
    // if it is not.
    function voidElementAttributeCompletions(info, path) {
        var tail = path.tail;
        if (tail instanceof compiler_1.Text) {
            var match = tail.value.match(/<(\w(\w|\d|-)*:)?(\w(\w|\d|-)*)\s/);
            // The position must be after the match, otherwise we are still in a place where elements
            // are expected (such as `<|a` or `<a|`; we only want attributes for `<a |` or after).
            if (match &&
                path.position >= (match.index || 0) + match[0].length + tail.sourceSpan.start.offset) {
                return attributeCompletionsForElement(info, match[3]);
            }
        }
        return [];
    }
    var ExpressionVisitor = /** @class */ (function (_super) {
        tslib_1.__extends(ExpressionVisitor, _super);
        function ExpressionVisitor(info, position, getExpressionScope) {
            var _this = _super.call(this) || this;
            _this.info = info;
            _this.position = position;
            _this.getExpressionScope = getExpressionScope;
            _this.completions = new Map();
            return _this;
        }
        Object.defineProperty(ExpressionVisitor.prototype, "results", {
            get: function () {
                return Array.from(this.completions.values());
            },
            enumerable: false,
            configurable: true
        });
        ExpressionVisitor.prototype.visitDirectiveProperty = function (ast) {
            this.processExpressionCompletions(ast.value);
        };
        ExpressionVisitor.prototype.visitElementProperty = function (ast) {
            this.processExpressionCompletions(ast.value);
        };
        ExpressionVisitor.prototype.visitEvent = function (ast) {
            this.processExpressionCompletions(ast.handler);
        };
        ExpressionVisitor.prototype.visitElement = function () {
            // no-op for now
        };
        ExpressionVisitor.prototype.visitAttr = function (ast) {
            var binding = binding_utils_1.getBindingDescriptor(ast.name);
            if (binding && binding.kind === binding_utils_1.ATTR.KW_MICROSYNTAX) {
                // This a template binding given by micro syntax expression.
                // First, verify the attribute consists of some binding we can give completions for.
                // The sourceSpan of AttrAst points to the RHS of the attribute
                var templateKey = binding.name;
                var templateValue = ast.sourceSpan.toString();
                var templateUrl = ast.sourceSpan.start.file.url;
                // TODO(kyliau): We are unable to determine the absolute offset of the key
                // but it is okay here, because we are only looking at the RHS of the attr
                var absKeyOffset = 0;
                var absValueOffset = ast.sourceSpan.start.offset;
                var templateBindings = this.info.expressionParser.parseTemplateBindings(templateKey, templateValue, templateUrl, absKeyOffset, absValueOffset).templateBindings;
                // Find the nearest template binding to the position.
                var lastBindingEnd = templateBindings.length > 0 &&
                    templateBindings[templateBindings.length - 1].sourceSpan.end;
                var normalizedPositionToBinding_1 = lastBindingEnd && this.position > lastBindingEnd ? lastBindingEnd : this.position;
                var templateBinding = templateBindings.find(function (b) { return utils_1.inSpan(normalizedPositionToBinding_1, b.sourceSpan); });
                if (!templateBinding) {
                    return;
                }
                this.microSyntaxInAttributeValue(ast, templateBinding);
            }
            else {
                var expressionAst = this.info.expressionParser.parseBinding(ast.value, ast.sourceSpan.toString(), ast.sourceSpan.start.offset);
                this.processExpressionCompletions(expressionAst);
            }
        };
        ExpressionVisitor.prototype.visitReference = function (_ast, context) {
            var _this = this;
            context.directives.forEach(function (dir) {
                var exportAs = dir.directive.exportAs;
                if (exportAs) {
                    _this.completions.set(exportAs, { name: exportAs, kind: ng.CompletionKind.REFERENCE, sortText: exportAs });
                }
            });
        };
        ExpressionVisitor.prototype.visitBoundText = function (ast) {
            if (utils_1.inSpan(this.position, ast.value.sourceSpan)) {
                var completions = expressions_1.getExpressionCompletions(this.getExpressionScope(), ast.value, this.position, this.info.template);
                if (completions) {
                    this.addSymbolsToCompletions(completions);
                }
            }
        };
        ExpressionVisitor.prototype.processExpressionCompletions = function (value) {
            var symbols = expressions_1.getExpressionCompletions(this.getExpressionScope(), value, this.position, this.info.template);
            if (symbols) {
                this.addSymbolsToCompletions(symbols);
            }
        };
        ExpressionVisitor.prototype.addSymbolsToCompletions = function (symbols) {
            var e_4, _a;
            try {
                for (var symbols_1 = tslib_1.__values(symbols), symbols_1_1 = symbols_1.next(); !symbols_1_1.done; symbols_1_1 = symbols_1.next()) {
                    var s = symbols_1_1.value;
                    if (s.name.startsWith('__') || !s.public || this.completions.has(s.name)) {
                        continue;
                    }
                    // The pipe method should not include parentheses.
                    // e.g. {{ value_expression | slice : start [ : end ] }}
                    var shouldInsertParentheses = s.callable && s.kind !== ng.CompletionKind.PIPE;
                    this.completions.set(s.name, {
                        name: s.name,
                        kind: s.kind,
                        sortText: s.name,
                        insertText: shouldInsertParentheses ? s.name + "()" : s.name,
                    });
                }
            }
            catch (e_4_1) { e_4 = { error: e_4_1 }; }
            finally {
                try {
                    if (symbols_1_1 && !symbols_1_1.done && (_a = symbols_1.return)) _a.call(symbols_1);
                }
                finally { if (e_4) throw e_4.error; }
            }
        };
        /**
         * This method handles the completions of attribute values for directives that
         * support the microsyntax format. Examples are *ngFor and *ngIf.
         * These directives allows declaration of "let" variables, adds context-specific
         * symbols like $implicit, index, count, among other behaviors.
         * For a complete description of such format, see
         * https://angular.io/guide/structural-directives#the-asterisk--prefix
         *
         * @param attr descriptor for attribute name and value pair
         * @param binding template binding for the expression in the attribute
         */
        ExpressionVisitor.prototype.microSyntaxInAttributeValue = function (attr, binding) {
            var _a;
            var key = attr.name.substring(1); // remove leading asterisk
            // Find the selector - eg ngFor, ngIf, etc
            var selectorInfo = utils_1.getSelectors(this.info);
            var selector = selectorInfo.selectors.find(function (s) {
                // attributes are listed in (attribute, value) pairs
                for (var i = 0; i < s.attrs.length; i += 2) {
                    if (s.attrs[i] === key) {
                        return true;
                    }
                }
            });
            if (!selector) {
                return;
            }
            var valueRelativePosition = this.position - attr.sourceSpan.start.offset;
            if (binding instanceof compiler_1.VariableBinding) {
                // TODO(kyliau): With expression sourceSpan we shouldn't have to search
                // the attribute value string anymore. Just check if position is in the
                // expression source span.
                var equalLocation = attr.value.indexOf('=');
                if (equalLocation > 0 && valueRelativePosition > equalLocation) {
                    // We are after the '=' in a let clause. The valid values here are the members of the
                    // template reference's type parameter.
                    var directiveMetadata = selectorInfo.map.get(selector);
                    if (directiveMetadata) {
                        var contextTable = this.info.template.query.getTemplateContext(directiveMetadata.type.reference);
                        if (contextTable) {
                            // This adds symbols like $implicit, index, count, etc.
                            this.addSymbolsToCompletions(contextTable.values());
                            return;
                        }
                    }
                }
            }
            else if (binding instanceof compiler_1.ExpressionBinding) {
                if (utils_1.inSpan(this.position, (_a = binding.value) === null || _a === void 0 ? void 0 : _a.ast.sourceSpan)) {
                    this.processExpressionCompletions(binding.value.ast);
                    return;
                }
                else if (!binding.value && this.position > binding.key.span.end) {
                    // No expression is defined for the value of the key expression binding, but the cursor is
                    // in a location where the expression would be defined. This can happen in a case like
                    //   let i of |
                    //            ^-- cursor
                    // In this case, backfill the value to be an empty expression and retrieve completions.
                    this.processExpressionCompletions(new compiler_1.EmptyExpr(new compiler_1.ParseSpan(valueRelativePosition, valueRelativePosition), new compiler_1.AbsoluteSourceSpan(this.position, this.position)));
                    return;
                }
            }
        };
        return ExpressionVisitor;
    }(compiler_1.NullTemplateVisitor));
    /**
     * Return all Angular-specific attributes for the element with `elementName`.
     * @param info
     * @param elementName
     */
    function angularAttributes(info, elementName) {
        var e_5, _a, e_6, _b, e_7, _c, e_8, _d;
        var _e = utils_1.getSelectors(info), selectors = _e.selectors, selectorMap = _e.map;
        var templateRefs = new Set();
        var inputs = new Set();
        var outputs = new Set();
        var bananas = new Set();
        var others = new Set();
        try {
            for (var selectors_1 = tslib_1.__values(selectors), selectors_1_1 = selectors_1.next(); !selectors_1_1.done; selectors_1_1 = selectors_1.next()) {
                var selector = selectors_1_1.value;
                if (selector.element && selector.element !== elementName) {
                    continue;
                }
                var summary = selectorMap.get(selector);
                var hasTemplateRef = utils_1.isStructuralDirective(summary.type);
                // attributes are listed in (attribute, value) pairs
                for (var i = 0; i < selector.attrs.length; i += 2) {
                    var attr = selector.attrs[i];
                    if (hasTemplateRef) {
                        templateRefs.add(attr);
                    }
                    else {
                        others.add(attr);
                    }
                }
                try {
                    for (var _f = (e_6 = void 0, tslib_1.__values(Object.values(summary.inputs))), _g = _f.next(); !_g.done; _g = _f.next()) {
                        var input = _g.value;
                        inputs.add(input);
                    }
                }
                catch (e_6_1) { e_6 = { error: e_6_1 }; }
                finally {
                    try {
                        if (_g && !_g.done && (_b = _f.return)) _b.call(_f);
                    }
                    finally { if (e_6) throw e_6.error; }
                }
                try {
                    for (var _h = (e_7 = void 0, tslib_1.__values(Object.values(summary.outputs))), _j = _h.next(); !_j.done; _j = _h.next()) {
                        var output = _j.value;
                        outputs.add(output);
                    }
                }
                catch (e_7_1) { e_7 = { error: e_7_1 }; }
                finally {
                    try {
                        if (_j && !_j.done && (_c = _h.return)) _c.call(_h);
                    }
                    finally { if (e_7) throw e_7.error; }
                }
            }
        }
        catch (e_5_1) { e_5 = { error: e_5_1 }; }
        finally {
            try {
                if (selectors_1_1 && !selectors_1_1.done && (_a = selectors_1.return)) _a.call(selectors_1);
            }
            finally { if (e_5) throw e_5.error; }
        }
        try {
            for (var inputs_1 = tslib_1.__values(inputs), inputs_1_1 = inputs_1.next(); !inputs_1_1.done; inputs_1_1 = inputs_1.next()) {
                var name_4 = inputs_1_1.value;
                // Add banana-in-a-box syntax
                // https://angular.io/guide/template-syntax#two-way-binding-
                if (outputs.has(name_4 + "Change")) {
                    bananas.add(name_4);
                }
            }
        }
        catch (e_8_1) { e_8 = { error: e_8_1 }; }
        finally {
            try {
                if (inputs_1_1 && !inputs_1_1.done && (_d = inputs_1.return)) _d.call(inputs_1);
            }
            finally { if (e_8) throw e_8.error; }
        }
        return { templateRefs: templateRefs, inputs: inputs, outputs: outputs, bananas: bananas, others: others };
    }
});
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiY29tcGxldGlvbnMuanMiLCJzb3VyY2VSb290IjoiIiwic291cmNlcyI6WyIuLi8uLi8uLi8uLi8uLi8uLi9wYWNrYWdlcy9sYW5ndWFnZS1zZXJ2aWNlL3NyYy9jb21wbGV0aW9ucy50cyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiQUFBQTs7Ozs7O0dBTUc7Ozs7Ozs7Ozs7Ozs7O0lBRUgsOENBQThYO0lBQzlYLHFEQUEyRTtJQUUzRSw2RUFBMkQ7SUFDM0QsK0ZBQTREO0lBQzVELHlFQUF1RDtJQUN2RCxxRUFBb0Y7SUFDcEYsbUVBQTBDO0lBQzFDLHdEQUE4QjtJQUM5Qiw2REFBd0o7SUFFeEosSUFBTSxvQkFBb0IsR0FDdEIsSUFBSSxHQUFHLENBQUMsQ0FBQyxNQUFNLEVBQUUsUUFBUSxFQUFFLFVBQVUsRUFBRSxNQUFNLEVBQUUsTUFBTSxFQUFFLE9BQU8sRUFBRSxNQUFNLEVBQUUsTUFBTSxDQUFDLENBQUMsQ0FBQztJQUNyRixJQUFNLGFBQWEsR0FDZix3QkFBWSxFQUFFLENBQUMsTUFBTSxDQUFDLFVBQUEsSUFBSSxJQUFJLE9BQUEsQ0FBQyxvQkFBb0IsQ0FBQyxHQUFHLENBQUMsSUFBSSxDQUFDLEVBQS9CLENBQStCLENBQUMsQ0FBQyxHQUFHLENBQUMsVUFBQSxJQUFJO1FBQ3JFLE9BQU87WUFDTCxJQUFJLE1BQUE7WUFDSixJQUFJLEVBQUUsRUFBRSxDQUFDLGNBQWMsQ0FBQyxZQUFZO1lBQ3BDLFFBQVEsRUFBRSxJQUFJO1NBQ2YsQ0FBQztJQUNKLENBQUMsQ0FBQyxDQUFDO0lBQ1AsSUFBTSxnQkFBZ0IsR0FBc0M7UUFDMUQ7WUFDRSxJQUFJLEVBQUUsY0FBYztZQUNwQixJQUFJLEVBQUUsRUFBRSxDQUFDLGNBQWMsQ0FBQyxlQUFlO1lBQ3ZDLFFBQVEsRUFBRSxjQUFjO1NBQ3pCO1FBQ0Q7WUFDRSxJQUFJLEVBQUUsWUFBWTtZQUNsQixJQUFJLEVBQUUsRUFBRSxDQUFDLGNBQWMsQ0FBQyxlQUFlO1lBQ3ZDLFFBQVEsRUFBRSxZQUFZO1NB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T0FBTyxFQUFDLFlBQVksY0FBQSxFQUFFLE1BQU0sUUFBQSxFQUFFLE9BQU8sU0FBQSxFQUFFLE9BQU8sU0FBQSxFQUFFLE1BQU0sUUFBQSxFQUFDLENBQUM7SUFDMUQsQ0FBQyIsInNvdXJjZXNDb250ZW50IjpbIi8qKlxuICogQGxpY2Vuc2VcbiAqIENvcHlyaWdodCBHb29nbGUgTExDIEFsbCBSaWdodHMgUmVzZXJ2ZWQuXG4gKlxuICogVXNlIG9mIHRoaXMgc291cmNlIGNvZGUgaXMgZ292ZXJuZWQgYnkgYW4gTUlULXN0eWxlIGxpY2Vuc2UgdGhhdCBjYW4gYmVcbiAqIGZvdW5kIGluIHRoZSBMSUNFTlNFIGZpbGUgYXQgaHR0cHM6Ly9hbmd1bGFyLmlvL2xpY2Vuc2VcbiAqL1xuXG5pbXBvcnQge0Fic29sdXRlU291cmNlU3BhbiwgQVNULCBBc3RQYXRoLCBBdHRyQXN0LCBBdHRyaWJ1dGUsIEJvdW5kRGlyZWN0aXZlUHJvcGVydHlBc3QsIEJvdW5kRWxlbWVudFByb3BlcnR5QXN0LCBCb3VuZEV2ZW50QXN0LCBCb3VuZFRleHRBc3QsIEVsZW1lbnQsIEVsZW1lbnRBc3QsIEVtcHR5RXhwciwgRXhwcmVzc2lvbkJpbmRpbmcsIGdldEh0bWxUYWdEZWZpbml0aW9uLCBIdG1sQXN0UGF0aCwgTm9kZSBhcyBIdG1sQXN0LCBOdWxsVGVtcGxhdGVWaXNpdG9yLCBQYXJzZVNwYW4sIFJlZmVyZW5jZUFzdCwgVGFnQ29udGVudFR5cGUsIFRlbXBsYXRlQmluZGluZywgVGV4dCwgVmFyaWFibGVCaW5kaW5nLCBWaXNpdG9yfSBmcm9tICdAYW5ndWxhci9jb21waWxlcic7XG5pbXBvcnQgeyQkLCAkXywgaXNBc2NpaUxldHRlciwgaXNEaWdpdH0gZnJvbSAnQGFuZ3VsYXIvY29tcGlsZXIvc3JjL2NoYXJzJztcblxuaW1wb3J0IHtBVFRSLCBnZXRCaW5kaW5nRGVzY3JpcHRvcn0gZnJvbSAnLi9iaW5kaW5nX3V0aWxzJztcbmltcG9ydCB7Z2V0RXhwcmVzc2lvblNjb3BlfSBmcm9tICcuL2V4cHJlc3Npb25fZGlhZ25vc3RpY3MnO1xuaW1wb3J0IHtnZXRFeHByZXNzaW9uQ29tcGxldGlvbnN9IGZyb20gJy4vZXhwcmVzc2lvbnMnO1xuaW1wb3J0IHthdHRyaWJ1dGVOYW1lcywgZWxlbWVudE5hbWVzLCBldmVudE5hbWVzLCBwcm9wZXJ0eU5hbWVzfSBmcm9tICcuL2h0bWxfaW5mbyc7XG5pbXBvcnQge0lubGluZVRlbXBsYXRlfSBmcm9tICcuL3RlbXBsYXRlJztcbmltcG9ydCAqIGFzIG5nIGZyb20gJy4vdHlwZXMnO1xuaW1wb3J0IHtkaWFnbm9zdGljSW5mb0Zyb21UZW1wbGF0ZUluZm8sIGZpbmRUZW1wbGF0ZUFzdEF0LCBnZXRQYXRoVG9Ob2RlQXRQb3NpdGlvbiwgZ2V0U2VsZWN0b3JzLCBpblNwYW4sIGlzU3RydWN0dXJhbERpcmVjdGl2ZSwgc3Bhbk9mfSBmcm9tICcuL3V0aWxzJztcblxuY29uc3QgSElEREVOX0hUTUxfRUxFTUVOVFM6IFJlYWRvbmx5U2V0PHN0cmluZz4gPVxuICAgIG5ldyBTZXQoWydodG1sJywgJ3NjcmlwdCcsICdub3NjcmlwdCcsICdiYXNlJywgJ2JvZHknLCAndGl0bGUnLCAnaGVhZCcsICdsaW5rJ10pO1xuY29uc3QgSFRNTF9FTEVNRU5UUzogUmVhZG9ubHlBcnJheTxuZy5Db21wbGV0aW9uRW50cnk+ID1cbiAgICBlbGVtZW50TmFtZXMoKS5maWx0ZXIobmFtZSA9PiAhSElEREVOX0hUTUxfRUxFTUVOVFMuaGFzKG5hbWUpKS5tYXAobmFtZSA9PiB7XG4gICAgICByZXR1cm4ge1xuICAgICAgICBuYW1lLFxuICAgICAgICBraW5kOiBuZy5Db21wbGV0aW9uS2luZC5IVE1MX0VMRU1FTlQsXG4gICAgICAgIHNvcnRUZXh0OiBuYW1lLFxuICAgICAgfTtcbiAgICB9KTtcbmNvbnN0IEFOR1VMQVJfRUxFTUVOVFM6IFJlYWRvbmx5QXJyYXk8bmcuQ29tcGxldGlvbkVudHJ5PiA9IFtcbiAge1xuICAgIG5hbWU6ICduZy1jb250YWluZXInLFxuICAgIGtpbmQ6IG5nLkNvbXBsZXRpb25LaW5kLkFOR1VMQVJfRUxFTUVOVCxcbiAgICBzb3J0VGV4dDogJ25nLWNvbnRhaW5lcicsXG4gIH0sXG4gIHtcbiAgICBuYW1lOiAnbmctY29udGVudCcsXG4gICAga2luZDogbmcuQ29tcGxldGlvbktpbmQuQU5HVUxBUl9FTEVNRU5ULFxuICAgIHNvcnRUZXh0OiAnbmctY29udGVudCcsXG4gIH0sXG4gIHtcbiAgICBuYW1lOiAnbmctdGVtcGxhdGUnLFxuICAgIGtpbmQ6IG5nLkNvbXBsZXRpb25LaW5kLkFOR1VMQVJfRUxFTUVOVCxcbiAgICBzb3J0VGV4dDogJ25nLXRlbXBsYXRlJyxcbiAgfSxcbl07XG5cbmZ1bmN0aW9uIGlzSWRlbnRpZmllclBhcnQoY29kZTogbnVtYmVyKSB7XG4gIC8vIElkZW50aWZpZXJzIGNvbnNpc3Qgb2YgYWxwaGFudW1lcmljIGNoYXJhY3RlcnMsICdfJywgb3IgJyQnLlxuICByZXR1cm4gaXNBc2NpaUxldHRlcihjb2RlKSB8fCBpc0RpZ2l0KGNvZGUpIHx8IGNvZGUgPT0gJCQgfHwgY29kZSA9PSAkXztcbn1cblxuLyoqXG4gKiBHZXRzIHRoZSBzcGFuIG9mIHdvcmQgaW4gYSB0ZW1wbGF0ZSB0aGF0IHN1cnJvdW5kcyBgcG9zaXRpb25gLiBJZiB0aGVyZSBpcyBubyB3b3JkIGFyb3VuZFxuICogYHBvc2l0aW9uYCwgbm90aGluZyBpcyByZXR1cm5lZC5cbiAqL1xuZnVuY3Rpb24gZ2V0Qm91bmRlZFdvcmRTcGFuKFxuICAgIHRlbXBsYXRlSW5mbzogbmcuQXN0UmVzdWx0LCBwb3NpdGlvbjogbnVtYmVyLCBhc3Q6IEh0bWxBc3R8dW5kZWZpbmVkKTogdHMuVGV4dFNwYW58dW5kZWZpbmVkIHtcbiAgY29uc3Qge3RlbXBsYXRlfSA9IHRlbXBsYXRlSW5mbztcbiAgY29uc3QgdGVtcGxhdGVTcmMgPSB0ZW1wbGF0ZS5zb3VyY2U7XG5cbiAgaWYgKCF0ZW1wbGF0ZVNyYykgcmV0dXJuO1xuXG4gIGlmIChhc3QgaW5zdGFuY2VvZiBFbGVtZW50KSB7XG4gICAgLy8gVGhlIEhUTUwgdGFnIG1heSBpbmNsdWRlIGAtYCAoZS5nLiBgYXBwLXJvb3RgKSxcbiAgICAvLyBzbyB1c2UgdGhlIEh0bWxBc3QgdG8gZ2V0IHRoZSBzcGFuIGJlZm9yZSBheWF6aGFmaXogcmVmYWN0b3IgdGhlIGNvZGUuXG4gICAgcmV0dXJuIHtcbiAgICAgIHN0YXJ0OiB0ZW1wbGF0ZUluZm8udGVtcGxhdGUuc3Bhbi5zdGFydCArIGFzdC5zdGFydFNvdXJjZVNwYW4hLnN0YXJ0Lm9mZnNldCArIDEsXG4gICAgICBsZW5ndGg6IGFzdC5uYW1lLmxlbmd0aFxuICAgIH07XG4gIH1cblxuICAvLyBUT0RPKGF5YXpoYWZpeik6IEEgc29sdXRpb24gYmFzZWQgb24gd29yZCBleHBhbnNpb24gd2lsbCBhbHdheXMgYmUgZXhwZW5zaXZlIGNvbXBhcmVkIHRvIG9uZVxuICAvLyBiYXNlZCBvbiBBU1RzLiBXaGF0ZXZlciBwZW5hbHR5IHdlIGluY3VyIGlzIHByb2JhYmx5IG1hbmFnZWFibGUgZm9yIHNtYWxsLWxlbmd0aCAoaS5lLiB0aGVcbiAgLy8gbWFqb3JpdHkgb2YpIGlkZW50aWZpZXJzLCBidXQgdGhlIGN1cnJlbnQgc29sdXRpb24gaW52b2xlcyBhIG51bWJlciBvZiBicmFuY2hpbmdzIGFuZCB3ZSBjYW4ndFxuICAvLyBjb250cm9sIHBvdGVudGlhbGx5IHZlcnkgbG9uZyBpZGVudGlmaWVycy4gQ29uc2lkZXIgbW92aW5nIHRvIGFuIEFTVC1iYXNlZCBzb2x1dGlvbiBvbmNlXG4gIC8vIGV4aXN0aW5nIGRpZmZpY3VsdGllcyB3aXRoIEFTVCBzcGFucyBhcmUgbW9yZSBjbGVhcmx5IHJlc29sdmVkIChzZWUgIzMxODk4IGZvciBkaXNjdXNzaW9uIG9mXG4gIC8vIGtub3duIHByb2JsZW1zLCBhbmQgIzMzMDkxIGZvciBob3cgdGhleSBhZmZlY3QgdGV4dCByZXBsYWNlbWVudCkuXG4gIC8vXG4gIC8vIGB0ZW1wbGF0ZVBvc2l0aW9uYCByZXByZXNlbnRzIHRoZSByaWdodC1ib3VuZCBsb2NhdGlvbiBvZiBhIGN1cnNvciBpbiB0aGUgdGVtcGxhdGUuXG4gIC8vICAgIGtleS5lbnR8cnlcbiAgLy8gICAgICAgICAgIF4tLS0tIGN1cnNvciwgYXQgcG9zaXRpb24gYHJgIGlzIGF0LlxuICAvLyBBIGN1cnNvciBpcyBub3QgaXRzZWxmIGEgY2hhcmFjdGVyIGluIHRoZSB0ZW1wbGF0ZTsgaXQgaGFzIGEgbGVmdCAobG93ZXIpIGFuZCByaWdodCAodXBwZXIpXG4gIC8vIGluZGV4IGJvdW5kIHRoYXQgaHVncyB0aGUgY3Vyc29yIGl0c2VsZi5cbiAgbGV0IHRlbXBsYXRlUG9zaXRpb24gPSBwb3NpdGlvbiAtIHRlbXBsYXRlLnNwYW4uc3RhcnQ7XG4gIC8vIFRvIHBlcmZvcm0gd29yZCBleHBhbnNpb24sIHdlIHdhbnQgdG8gZGV0ZXJtaW5lIHRoZSBsZWZ0IGFuZCByaWdodCBpbmRpY2VzIHRoYXQgaHVnIHRoZSBjdXJzb3IuXG4gIC8vIFRoZXJlIGFyZSB0aHJlZSBjYXNlcyBoZXJlLlxuICBsZXQgbGVmdCwgcmlnaHQ7XG4gIGlmICh0ZW1wbGF0ZVBvc2l0aW9uID09PSAwKSB7XG4gICAgLy8gMS4gQ2FzZSBsaWtlXG4gICAgLy8gICAgICB8cmVzdCBvZiB0ZW1wbGF0ZVxuICAgIC8vICAgIHRoZSBjdXJzb3IgaXMgYXQgdGhlIHN0YXJ0IG9mIHRoZSB0ZW1wbGF0ZSwgaHVnZ2VkIG9ubHkgYnkgdGhlIHJpZ2h0IHNpZGUgKDAtaW5kZXgpLlxuICAgIGxlZnQgPSByaWdodCA9IDA7XG4gIH0gZWxzZSBpZiAodGVtcGxhdGVQb3NpdGlvbiA9PT0gdGVtcGxhdGVTcmMubGVuZ3RoKSB7XG4gICAgLy8gMi4gQ2FzZSBsaWtlXG4gICAgLy8gICAgICByZXN0IG9mIHRlbXBsYXRlfFxuICAgIC8vICAgIHRoZSBjdXJzb3IgaXMgYXQgdGhlIGVuZCBvZiB0aGUgdGVtcGxhdGUsIGh1Z2dlZCBvbmx5IGJ5IHRoZSBsZWZ0IHNpZGUgKGxhc3QtaW5kZXgpLlxuICAgIGxlZnQgPSByaWdodCA9IHRlbXBsYXRlU3JjLmxlbmd0aCAtIDE7XG4gIH0gZWxzZSB7XG4gICAgLy8gMy4gQ2FzZSBsaWtlXG4gICAgLy8gICAgICB3b3xyZFxuICAgIC8vICAgIHRoZXJlIGlzIGEgY2xlYXIgbGVmdCBhbmQgcmlnaHQgaW5kZXguXG4gICAgbGVmdCA9IHRlbXBsYXRlUG9zaXRpb24gLSAxO1xuICAgIHJpZ2h0ID0gdGVtcGxhdGVQb3NpdGlvbjtcbiAgfVxuXG4gIGlmICghaXNJZGVudGlmaWVyUGFydCh0ZW1wbGF0ZVNyYy5jaGFyQ29kZUF0KGxlZnQpKSAmJlxuICAgICAgIWlzSWRlbnRpZmllclBhcnQodGVtcGxhdGVTcmMuY2hhckNvZGVBdChyaWdodCkpKSB7XG4gICAgLy8gQ2FzZSBsaWtlXG4gICAgLy8gICAgICAgICAufC5cbiAgICAvLyBsZWZ0IC0tLV4gXi0tLSByaWdodFxuICAgIC8vIFRoZXJlIGlzIG5vIHdvcmQgaGVyZS5cbiAgICByZXR1cm47XG4gIH1cblxuICAvLyBFeHBhbmQgb24gdGhlIGxlZnQgYW5kIHJpZ2h0IHNpZGUgdW50aWwgYSB3b3JkIGJvdW5kYXJ5IGlzIGhpdC4gQmFjayB1cCBvbmUgZXhwYW5zaW9uIG9uIGJvdGhcbiAgLy8gc2lkZSB0byBzdGF5IGluc2lkZSB0aGUgd29yZC5cbiAgd2hpbGUgKGxlZnQgPj0gMCAmJiBpc0lkZW50aWZpZXJQYXJ0KHRlbXBsYXRlU3JjLmNoYXJDb2RlQXQobGVmdCkpKSAtLWxlZnQ7XG4gICsrbGVmdDtcbiAgd2hpbGUgKHJpZ2h0IDwgdGVtcGxhdGVTcmMubGVuZ3RoICYmIGlzSWRlbnRpZmllclBhcnQodGVtcGxhdGVTcmMuY2hhckNvZGVBdChyaWdodCkpKSArK3JpZ2h0O1xuICAtLXJpZ2h0O1xuXG4gIGNvbnN0IGFic29sdXRlU3RhcnRQb3NpdGlvbiA9IHBvc2l0aW9uIC0gKHRlbXBsYXRlUG9zaXRpb24gLSBsZWZ0KTtcbiAgY29uc3QgbGVuZ3RoID0gcmlnaHQgLSBsZWZ0ICsgMTtcbiAgcmV0dXJuIHtzdGFydDogYWJzb2x1dGVTdGFydFBvc2l0aW9uLCBsZW5ndGh9O1xufVxuXG5leHBvcnQgZnVuY3Rpb24gZ2V0VGVtcGxhdGVDb21wbGV0aW9ucyhcbiAgICB0ZW1wbGF0ZUluZm86IG5nLkFzdFJlc3VsdCwgcG9zaXRpb246IG51bWJlcik6IG5nLkNvbXBsZXRpb25FbnRyeVtdIHtcbiAgY29uc3Qge2h0bWxBc3QsIHRlbXBsYXRlfSA9IHRlbXBsYXRlSW5mbztcbiAgLy8gQ2FsY3VsYXRlIHRoZSBwb3NpdGlvbiByZWxhdGl2ZSB0byB0aGUgc3RhcnQgb2YgdGhlIHRlbXBsYXRlLiBUaGlzIGlzIG5lZWRlZFxuICAvLyBiZWNhdXNlIHNwYW5zIGluIEhUTUwgQVNUIGFyZSByZWxhdGl2ZS4gSW5saW5lIHRlbXBsYXRlIGhhcyBub24temVybyBzdGFydCBwb3NpdGlvbi5cbiAgY29uc3QgdGVtcGxhdGVQb3NpdGlvbiA9IHBvc2l0aW9uIC0gdGVtcGxhdGUuc3Bhbi5zdGFydDtcbiAgY29uc3QgaHRtbFBhdGg6IEh0bWxBc3RQYXRoID0gZ2V0UGF0aFRvTm9kZUF0UG9zaXRpb24oaHRtbEFzdCwgdGVtcGxhdGVQb3NpdGlvbik7XG4gIGNvbnN0IG1vc3RTcGVjaWZpYyA9IGh0bWxQYXRoLnRhaWw7XG4gIGNvbnN0IHZpc2l0b3IgPSBuZXcgSHRtbFZpc2l0b3IodGVtcGxhdGVJbmZvLCBodG1sUGF0aCk7XG4gIGNvbnN0IHJlc3VsdHM6IG5nLkNvbXBsZXRpb25FbnRyeVtdID0gbW9zdFNwZWNpZmljID9cbiAgICAgIG1vc3RTcGVjaWZpYy52aXNpdCh2aXNpdG9yLCBudWxsIC8qIGNvbnRleHQgKi8pIDpcbiAgICAgIGVsZW1lbnRDb21wbGV0aW9ucyh0ZW1wbGF0ZUluZm8pO1xuICBjb25zdCByZXBsYWNlbWVudFNwYW4gPSBnZXRCb3VuZGVkV29yZFNwYW4odGVtcGxhdGVJbmZvLCBwb3NpdGlvbiwgbW9zdFNwZWNpZmljKTtcbiAgcmV0dXJuIHJlc3VsdHMubWFwKGVudHJ5ID0+IHtcbiAgICByZXR1cm4ge1xuICAgICAgLi4uZW50cnksXG4gICAgICByZXBsYWNlbWVudFNwYW4sXG4gICAgfTtcbiAgfSk7XG59XG5cbmNsYXNzIEh0bWxWaXNpdG9yIGltcGxlbWVudHMgVmlzaXRvciB7XG4gIC8qKlxuICAgKiBQb3NpdGlvbiByZWxhdGl2ZSB0byB0aGUgc3RhcnQgb2YgdGhlIHRlbXBsYXRlLlxuICAgKi9cbiAgcHJpdmF0ZSByZWFkb25seSByZWxhdGl2ZVBvc2l0aW9uOiBudW1iZXI7XG4gIGNvbnN0cnVjdG9yKHByaXZhdGUgcmVhZG9ubHkgdGVtcGxhdGVJbmZvOiBuZy5Bc3RSZXN1bHQsIHByaXZhdGUgcmVhZG9ubHkgaHRtbFBhdGg6IEh0bWxBc3RQYXRoKSB7XG4gICAgdGhpcy5yZWxhdGl2ZVBvc2l0aW9uID0gaHRtbFBhdGgucG9zaXRpb247XG4gIH1cbiAgLy8gTm90ZSB0aGF0IGV2ZXJ5IHZpc2l0b3IgbWV0aG9kIG11c3QgZXhwbGljaXRseSBzcGVjaWZ5IHJldHVybiB0eXBlIGJlY2F1c2VcbiAgLy8gVmlzaXRvciByZXR1cm5zIGBhbnlgIGZvciBhbGwgbWV0aG9kcy5cbiAgdmlzaXRFbGVtZW50KGFzdDogRWxlbWVudCk6IG5nLkNvbXBsZXRpb25FbnRyeVtdIHtcbiAgICBjb25zdCBzdGFydFRhZ1NwYW4gPSBzcGFuT2YoYXN0LnNvdXJjZVNwYW4pO1xuICAgIGNvbnN0IHRhZ0xlbiA9IGFzdC5uYW1lLmxlbmd0aDtcbiAgICAvLyArIDEgZm9yIHRoZSBvcGVuaW5nIGFuZ2xlIGJyYWNrZXRcbiAgICBpZiAodGhpcy5yZWxhdGl2ZVBvc2l0aW9uIDw9IHN0YXJ0VGFnU3Bhbi5zdGFydCArIHRhZ0xlbiArIDEpIHtcbiAgICAgIC8vIElmIHdlIGFyZSBpbiB0aGUgdGFnIHRoZW4gcmV0dXJuIHRoZSBlbGVtZW50IGNvbXBsZXRpb25zLlxuICAgICAgcmV0dXJuIGVsZW1lbnRDb21wbGV0aW9ucyh0aGlzLnRlbXBsYXRlSW5mbyk7XG4gICAgfVxuICAgIGlmICh0aGlzLnJlbGF0aXZlUG9zaXRpb24gPCBzdGFydFRhZ1NwYW4uZW5kKSB7XG4gICAgICAvLyBXZSBhcmUgaW4gdGhlIGF0dHJpYnV0ZSBzZWN0aW9uIG9mIHRoZSBlbGVtZW50IChidXQgbm90IGluIGFuIGF0dHJpYnV0ZSkuXG4gICAgICAvLyBSZXR1cm4gdGhlIGF0dHJpYnV0ZSBjb21wbGV0aW9ucy5cbiAgICAgIHJldHVybiBhdHRyaWJ1dGVDb21wbGV0aW9uc0ZvckVsZW1lbnQodGhpcy50ZW1wbGF0ZUluZm8sIGFzdC5uYW1lKTtcbiAgICB9XG4gICAgcmV0dXJuIFtdO1xuICB9XG4gIHZpc2l0QXR0cmlidXRlKGFzdDogQXR0cmlidXRlKTogbmcuQ29tcGxldGlvbkVudHJ5W10ge1xuICAgIC8vIEFuIGF0dHJpYnV0ZSBjb25zaXN0cyBvZiB0d28gcGFydHMsIExIUz1cIlJIU1wiLlxuICAgIC8vIERldGVybWluZSBpZiBjb21wbGV0aW9ucyBhcmUgcmVxdWVzdGVkIGZvciBMSFMgb3IgUkhTXG4gICAgaWYgKGFzdC52YWx1ZVNwYW4gJiYgaW5TcGFuKHRoaXMucmVsYXRpdmVQb3NpdGlvbiwgc3Bhbk9mKGFzdC52YWx1ZVNwYW4pKSkge1xuICAgICAgLy8gUkhTIGNvbXBsZXRpb25cbiAgICAgIHJldHVybiBhdHRyaWJ1dGVWYWx1ZUNvbXBsZXRpb25zKHRoaXMudGVtcGxhdGVJbmZvLCB0aGlzLmh0bWxQYXRoKTtcbiAgICB9XG4gICAgLy8gTEhTIGNvbXBsZXRpb25cbiAgICByZXR1cm4gYXR0cmlidXRlQ29tcGxldGlvbnModGhpcy50ZW1wbGF0ZUluZm8sIHRoaXMuaHRtbFBhdGgpO1xuICB9XG4gIHZpc2l0VGV4dCgpOiBuZy5Db21wbGV0aW9uRW50cnlbXSB7XG4gICAgY29uc3QgdGVtcGxhdGVQYXRoID0gZmluZFRlbXBsYXRlQXN0QXQodGhpcy50ZW1wbGF0ZUluZm8udGVtcGxhdGVBc3QsIHRoaXMucmVsYXRpdmVQb3NpdGlvbik7XG4gICAgaWYgKHRlbXBsYXRlUGF0aC50YWlsIGluc3RhbmNlb2YgQm91bmRUZXh0QXN0KSB7XG4gICAgICAvLyBJZiB3ZSBrbm93IHRoYXQgdGhpcyBpcyBhbiBpbnRlcnBvbGF0aW9uIHRoZW4gZG8gbm90IHRyeSBvdGhlciBzY2VuYXJpb3MuXG4gICAgICBjb25zdCB2aXNpdG9yID0gbmV3IEV4cHJlc3Npb25WaXNpdG9yKFxuICAgICAgICAgIHRoaXMudGVtcGxhdGVJbmZvLCB0aGlzLnJlbGF0aXZlUG9zaXRpb24sXG4gICAgICAgICAgKCkgPT5cbiAgICAgICAgICAgICAgZ2V0RXhwcmVzc2lvblNjb3BlKGRpYWdub3N0aWNJbmZvRnJvbVRlbXBsYXRlSW5mbyh0aGlzLnRlbXBsYXRlSW5mbyksIHRlbXBsYXRlUGF0aCkpO1xuICAgICAgdGVtcGxhdGVQYXRoLnRhaWw/LnZpc2l0KHZpc2l0b3IsIG51bGwpO1xuICAgICAgcmV0dXJuIHZpc2l0b3IucmVzdWx0cztcbiAgICB9XG4gICAgLy8gVE9ETyhreWxpYXUpOiBOb3Qgc3VyZSBpZiB0aGlzIGNoZWNrIGlzIHJlYWxseSBuZWVkZWQgc2luY2Ugd2UgZG9uJ3QgaGF2ZVxuICAgIC8vIGFueSB0ZXN0IGNhc2VzIGZvciBpdC5cbiAgICBjb25zdCBlbGVtZW50ID0gdGhpcy5odG1sUGF0aC5maXJzdChFbGVtZW50KTtcbiAgICBpZiAoZWxlbWVudCAmJlxuICAgICAgICBnZXRIdG1sVGFnRGVmaW5pdGlvbihlbGVtZW50Lm5hbWUpLmNvbnRlbnRUeXBlICE9PSBUYWdDb250ZW50VHlwZS5QQVJTQUJMRV9EQVRBKSB7XG4gICAgICByZXR1cm4gW107XG4gICAgfVxuICAgIC8vIFRoaXMgaXMgdG8gYWNjb3VudCBmb3IgY2FzZXMgbGlrZSA8aDE+IDxhPiB0ZXh0IHwgPC9oMT4gd2hlcmUgdGhlXG4gICAgLy8gY2xvc2VzdCBlbGVtZW50IGhhcyBubyBjbG9zaW5nIHRhZyBhbmQgdGh1cyBpcyBjb25zaWRlcmVkIHBsYWluIHRleHQuXG4gICAgY29uc3QgcmVzdWx0cyA9IHZvaWRFbGVtZW50QXR0cmlidXRlQ29tcGxldGlvbnModGhpcy50ZW1wbGF0ZUluZm8sIHRoaXMuaHRtbFBhdGgpO1xuICAgIGlmIChyZXN1bHRzLmxlbmd0aCkge1xuICAgICAgcmV0dXJuIHJlc3VsdHM7XG4gICAgfVxuICAgIHJldHVybiBlbGVtZW50Q29tcGxldGlvbnModGhpcy50ZW1wbGF0ZUluZm8pO1xuICB9XG4gIHZpc2l0Q29tbWVudCgpOiBuZy5Db21wbGV0aW9uRW50cnlbXSB7XG4gICAgcmV0dXJuIFtdO1xuICB9XG4gIHZpc2l0RXhwYW5zaW9uKCk6IG5nLkNvbXBsZXRpb25FbnRyeVtdIHtcbiAgICByZXR1cm4gW107XG4gIH1cbiAgdmlzaXRFeHBhbnNpb25DYXNlKCk6IG5nLkNvbXBsZXRpb25FbnRyeVtdIHtcbiAgICByZXR1cm4gW107XG4gIH1cbn1cblxuZnVuY3Rpb24gYXR0cmlidXRlQ29tcGxldGlvbnMoaW5mbzogbmcuQXN0UmVzdWx0LCBwYXRoOiBBc3RQYXRoPEh0bWxBc3Q+KTogbmcuQ29tcGxldGlvbkVudHJ5W10ge1xuICBjb25zdCBhdHRyID0gcGF0aC50YWlsO1xuICBjb25zdCBlbGVtID0gcGF0aC5wYXJlbnRPZihhdHRyKTtcbiAgaWYgKCEoYXR0ciBpbnN0YW5jZW9mIEF0dHJpYnV0ZSkgfHwgIShlbGVtIGluc3RhbmNlb2YgRWxlbWVudCkpIHtcbiAgICByZXR1cm4gW107XG4gIH1cblxuICAvLyBUT0RPOiBDb25zaWRlciBwYXJzaW5nIHRoZSBhdHRyaW51dGUgbmFtZSB0byBhIHByb3BlciBBU1QgaW5zdGVhZCBvZlxuICAvLyBtYXRjaGluZyB1c2luZyByZWdleC4gVGhpcyBpcyBiZWNhdXNlIHRoZSByZWdleHAgd291bGQgaW5jb3JyZWN0bHkgaWRlbnRpZnlcbiAgLy8gYmluZCBwYXJ0cyBmb3IgY2FzZXMgbGlrZSBbKCl8XVxuICAvLyAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIF4gY3Vyc29yIGlzIGhlcmVcbiAgY29uc3QgYmluZGluZyA9IGdldEJpbmRpbmdEZXNjcmlwdG9yKGF0dHIubmFtZSk7XG4gIGlmICghYmluZGluZykge1xuICAgIC8vIFRoaXMgaXMgYSBub3JtYWwgSFRNTCBhdHRyaWJ1dGUsIG5vdCBhbiBBbmd1bGFyIGF0dHJpYnV0ZS5cbiAgICByZXR1cm4gYXR0cmlidXRlQ29tcGxldGlvbnNGb3JFbGVtZW50KGluZm8sIGVsZW0ubmFtZSk7XG4gIH1cblxuICBjb25zdCByZXN1bHRzOiBzdHJpbmdbXSA9IFtdO1xuICBjb25zdCBuZ0F0dHJzID0gYW5ndWxhckF0dHJpYnV0ZXMoaW5mbywgZWxlbS5uYW1lKTtcbiAgc3dpdGNoIChiaW5kaW5nLmtpbmQpIHtcbiAgICBjYXNlIEFUVFIuS1dfTUlDUk9TWU5UQVg6XG4gICAgICAvLyB0ZW1wbGF0ZSByZWZlcmVuY2UgYXR0cmlidXRlOiAqYXR0ck5hbWVcbiAgICAgIHJlc3VsdHMucHVzaCguLi5uZ0F0dHJzLnRlbXBsYXRlUmVmcyk7XG4gICAgICBicmVhaztcblxuICAgIGNhc2UgQVRUUi5LV19CSU5EOlxuICAgIGNhc2UgQVRUUi5JREVOVF9QUk9QRVJUWTpcbiAgICAgIC8vIHByb3BlcnR5IGJpbmRpbmcgdmlhIGJpbmQtIG9yIFtdXG4gICAgICByZXN1bHRzLnB1c2goLi4ucHJvcGVydHlOYW1lcyhlbGVtLm5hbWUpLCAuLi5uZ0F0dHJzLmlucHV0cyk7XG4gICAgICBicmVhaztcblxuICAgIGNhc2UgQVRUUi5LV19PTjpcbiAgICBjYXNlIEFUVFIuSURFTlRfRVZFTlQ6XG4gICAgICAvLyBldmVudCBiaW5kaW5nIHZpYSBvbi0gb3IgKClcbiAgICAgIHJlc3VsdHMucHVzaCguLi5ldmVudE5hbWVzKGVsZW0ubmFtZSksIC4uLm5nQXR0cnMub3V0cHV0cyk7XG4gICAgICBicmVhaztcblxuICAgIGNhc2UgQVRUUi5LV19CSU5ET046XG4gICAgY2FzZSBBVFRSLklERU5UX0JBTkFOQV9CT1g6XG4gICAgICAvLyBiYW5hbmEtaW4tYS1ib3ggYmluZGluZyB2aWEgYmluZG9uLSBvciBbKCldXG4gICAgICByZXN1bHRzLnB1c2goLi4ubmdBdHRycy5iYW5hbmFzKTtcbiAgICAgIGJyZWFrO1xuICB9XG5cbiAgcmV0dXJuIHJlc3VsdHMubWFwKG5hbWUgPT4ge1xuICAgIHJldHVybiB7XG4gICAgICBuYW1lLFxuICAgICAga2luZDogbmcuQ29tcGxldGlvbktpbmQuQVRUUklCVVRFLFxuICAgICAgc29ydFRleHQ6IG5hbWUsXG4gICAgfTtcbiAgfSk7XG59XG5cbmZ1bmN0aW9uIGF0dHJpYnV0ZUNvbXBsZXRpb25zRm9yRWxlbWVudChcbiAgICBpbmZvOiBuZy5Bc3RSZXN1bHQsIGVsZW1lbnROYW1lOiBzdHJpbmcpOiBuZy5Db21wbGV0aW9uRW50cnlbXSB7XG4gIGNvbnN0IHJlc3VsdHM6IG5nLkNvbXBsZXRpb25FbnRyeVtdID0gW107XG5cbiAgaWYgKGluZm8udGVtcGxhdGUgaW5zdGFuY2VvZiBJbmxpbmVUZW1wbGF0ZSkge1xuICAgIC8vIFByb3ZpZGUgSFRNTCBhdHRyaWJ1dGVzIGNvbXBsZXRpb24gb25seSBmb3IgaW5saW5lIHRlbXBsYXRlc1xuICAgIGZvciAoY29uc3QgbmFtZSBvZiBhdHRyaWJ1dGVOYW1lcyhlbGVtZW50TmFtZSkpIHtcbiAgICAgIHJlc3VsdHMucHVzaCh7XG4gICAgICAgIG5hbWUsXG4gICAgICAgIGtpbmQ6IG5nLkNvbXBsZXRpb25LaW5kLkhUTUxfQVRUUklCVVRFLFxuICAgICAgICBzb3J0VGV4dDogbmFtZSxcbiAgICAgIH0pO1xuICAgIH1cbiAgfVxuXG4gIC8vIEFkZCBBbmd1bGFyIGF0dHJpYnV0ZXNcbiAgY29uc3QgbmdBdHRycyA9IGFuZ3VsYXJBdHRyaWJ1dGVzKGluZm8sIGVsZW1lbnROYW1lKTtcbiAgZm9yIChjb25zdCBuYW1lIG9mIG5nQXR0cnMub3RoZXJzKSB7XG4gICAgcmVzdWx0cy5wdXNoKHtcbiAgICAgIG5hbWUsXG4gICAgICBraW5kOiBuZy5Db21wbGV0aW9uS2luZC5BVFRSSUJVVEUsXG4gICAgICBzb3J0VGV4dDogbmFtZSxcbiAgICB9KTtcbiAgfVxuXG4gIHJldHVybiByZXN1bHRzO1xufVxuXG4vKipcbiAqIFByb3ZpZGUgY29tcGxldGlvbnMgdG8gdGhlIFJIUyBvZiBhbiBhdHRyaWJ1dGUsIHdoaWNoIGlzIG9mIHRoZSBmb3JtXG4gKiBMSFM9XCJSSFNcIi4gVGhlIHRlbXBsYXRlIHBhdGggaXMgY29tcHV0ZWQgZnJvbSB0aGUgc3BlY2lmaWVkIGBpbmZvYCB3aGVyZWFzXG4gKiB0aGUgY29udGV4dCBpcyBkZXRlcm1pbmVkIGZyb20gdGhlIHNwZWNpZmllZCBgaHRtbFBhdGhgLlxuICogQHBhcmFtIGluZm8gT2JqZWN0IHRoYXQgY29udGFpbnMgdGhlIHRlbXBsYXRlIEFTVFxuICogQHBhcmFtIGh0bWxQYXRoIFBhdGggdG8gdGhlIEhUTUwgbm9kZVxuICovXG5mdW5jdGlvbiBhdHRyaWJ1dGVWYWx1ZUNvbXBsZXRpb25zKFxuICAgIGluZm86IG5nLkFzdFJlc3VsdCwgaHRtbFBhdGg6IEh0bWxBc3RQYXRoKTogbmcuQ29tcGxldGlvbkVudHJ5W10ge1xuICAvLyBGaW5kIHRoZSBjb3JyZXNwb25kaW5nIFRlbXBsYXRlIEFTVCBwYXRoLlxuICBjb25zdCB0ZW1wbGF0ZVBhdGggPSBmaW5kVGVtcGxhdGVBc3RBdChpbmZvLnRlbXBsYXRlQXN0LCBodG1sUGF0aC5wb3NpdGlvbik7XG4gIGNvbnN0IHZpc2l0b3IgPSBuZXcgRXhwcmVzc2lvblZpc2l0b3IoaW5mbywgaHRtbFBhdGgucG9zaXRpb24sICgpID0+IHtcbiAgICBjb25zdCBkaW5mbyA9IGRpYWdub3N0aWNJbmZvRnJvbVRlbXBsYXRlSW5mbyhpbmZvKTtcbiAgICByZXR1cm4gZ2V0RXhwcmVzc2lvblNjb3BlKGRpbmZvLCB0ZW1wbGF0ZVBhdGgpO1xuICB9KTtcbiAgaWYgKHRlbXBsYXRlUGF0aC50YWlsIGluc3RhbmNlb2YgQXR0ckFzdCB8fFxuICAgICAgdGVtcGxhdGVQYXRoLnRhaWwgaW5zdGFuY2VvZiBCb3VuZEVsZW1lbnRQcm9wZXJ0eUFzdCB8fFxuICAgICAgdGVtcGxhdGVQYXRoLnRhaWwgaW5zdGFuY2VvZiBCb3VuZEV2ZW50QXN0KSB7XG4gICAgdGVtcGxhdGVQYXRoLnRhaWwudmlzaXQodmlzaXRvciwgbnVsbCk7XG4gICAgcmV0dXJuIHZpc2l0b3IucmVzdWx0cztcbiAgfVxuICAvLyBJbiBvcmRlciB0byBwcm92aWRlIGFjY3VyYXRlIGF0dHJpYnV0ZSB2YWx1ZSBjb21wbGV0aW9uLCB3ZSBuZWVkIHRvIGtub3dcbiAgLy8gd2hhdCB0aGUgTEhTIGlzLCBhbmQgY29uc3RydWN0IHRoZSBwcm9wZXIgQVNUIGlmIGl0IGlzIG1pc3NpbmcuXG4gIGNvbnN0IGh0bWxBdHRyID0gaHRtbFBhdGgudGFpbCBhcyBBdHRyaWJ1dGU7XG4gIGNvbnN0IGJpbmRpbmcgPSBnZXRCaW5kaW5nRGVzY3JpcHRvcihodG1sQXR0ci5uYW1lKTtcbiAgaWYgKGJpbmRpbmcgJiYgYmluZGluZy5raW5kID09PSBBVFRSLktXX1JFRikge1xuICAgIGxldCByZWZBc3Q6IFJlZmVyZW5jZUFzdHx1bmRlZmluZWQ7XG4gICAgbGV0IGVsZW1Bc3Q6IEVsZW1lbnRBc3R8dW5kZWZpbmVkO1xuICAgIGlmICh0ZW1wbGF0ZVBhdGgudGFpbCBpbnN0YW5jZW9mIFJlZmVyZW5jZUFzdCkge1xuICAgICAgcmVmQXN0ID0gdGVtcGxhdGVQYXRoLnRhaWw7XG4gICAgICBjb25zdCBwYXJlbnQgPSB0ZW1wbGF0ZVBhdGgucGFyZW50T2YocmVmQXN0KTtcbiAgICAgIGlmIChwYXJlbnQgaW5zdGFuY2VvZiBFbGVtZW50QXN0KSB7XG4gICAgICAgIGVsZW1Bc3QgPSBwYXJlbnQ7XG4gICAgICB9XG4gICAgfSBlbHNlIGlmICh0ZW1wbGF0ZVBhdGgudGFpbCBpbnN0YW5jZW9mIEVsZW1lbnRBc3QpIHtcbiAgICAgIHJlZkFzdCA9IG5ldyBSZWZlcmVuY2VBc3QoaHRtbEF0dHIubmFtZSwgbnVsbCEsIGh0bWxBdHRyLnZhbHVlLCBodG1sQXR0ci52YWx1ZVNwYW4hKTtcbiAgICAgIGVsZW1Bc3QgPSB0ZW1wbGF0ZVBhdGgudGFpbDtcbiAgICB9XG4gICAgaWYgKHJlZkFzdCAmJiBlbGVtQXN0KSB7XG4gICAgICByZWZBc3QudmlzaXQodmlzaXRvciwgZWxlbUFzdCk7XG4gICAgfVxuICB9IGVsc2Uge1xuICAgIC8vIEh0bWxBc3QgY29udGFpbnMgdGhlIGBBdHRyaWJ1dGVgIG5vZGUsIGhvd2V2ZXIgdGhlIGNvcnJlc3BvbmRpbmcgYEF0dHJBc3RgXG4gICAgLy8gbm9kZSBpcyBtaXNzaW5nIGZyb20gdGhlIFRlbXBsYXRlQXN0LlxuICAgIGNvbnN0IGF0dHJBc3QgPSBuZXcgQXR0ckFzdChodG1sQXR0ci5uYW1lLCBodG1sQXR0ci52YWx1ZSwgaHRtbEF0dHIudmFsdWVTcGFuISk7XG4gICAgYXR0ckFzdC52aXNpdCh2aXNpdG9yLCBudWxsKTtcbiAgfVxuICByZXR1cm4gdmlzaXRvci5yZXN1bHRzO1xufVxuXG5mdW5jdGlvbiBlbGVtZW50Q29tcGxldGlvbnMoaW5mbzogbmcuQXN0UmVzdWx0KTogbmcuQ29tcGxldGlvbkVudHJ5W10ge1xuICBjb25zdCByZXN1bHRzOiBuZy5Db21wbGV0aW9uRW50cnlbXSA9IFsuLi5BTkdVTEFSX0VMRU1FTlRTXTtcblxuICBpZiAoaW5mby50ZW1wbGF0ZSBpbnN0YW5jZW9mIElubGluZVRlbXBsYXRlKSB7XG4gICAgLy8gUHJvdmlkZSBIVE1MIGVsZW1lbnRzIGNvbXBsZXRpb24gb25seSBmb3IgaW5saW5lIHRlbXBsYXRlc1xuICAgIHJlc3VsdHMucHVzaCguLi5IVE1MX0VMRU1FTlRTKTtcbiAgfVxuXG4gIC8vIENvbGxlY3QgdGhlIGVsZW1lbnRzIHJlZmVyZW5jZWQgYnkgdGhlIHNlbGVjdG9yc1xuICBjb25zdCBjb21wb25lbnRzID0gbmV3IFNldDxzdHJpbmc+KCk7XG4gIGZvciAoY29uc3Qgc2VsZWN0b3Igb2YgZ2V0U2VsZWN0b3JzKGluZm8pLnNlbGVjdG9ycykge1xuICAgIGNvbnN0IG5hbWUgPSBzZWxlY3Rvci5lbGVtZW50O1xuICAgIGlmIChuYW1lICYmICFjb21wb25lbnRzLmhhcyhuYW1lKSkge1xuICAgICAgY29tcG9uZW50cy5hZGQobmFtZSk7XG4gICAgICByZXN1bHRzLnB1c2goe1xuICAgICAgICBuYW1lLFxuICAgICAgICBraW5kOiBuZy5Db21wbGV0aW9uS2luZC5DT01QT05FTlQsXG4gICAgICAgIHNvcnRUZXh0OiBuYW1lLFxuICAgICAgfSk7XG4gICAgfVxuICB9XG5cbiAgcmV0dXJuIHJlc3VsdHM7XG59XG5cbi8vIFRoZXJlIGlzIGEgc3BlY2lhbCBjYXNlIG9mIEhUTUwgd2hlcmUgdGV4dCB0aGF0IGNvbnRhaW5zIGEgdW5jbG9zZWQgdGFnIGlzIHRyZWF0ZWQgYXNcbi8vIHRleHQuIEZvciBleGFwbGUgJzxoMT4gU29tZSA8YSB0ZXh0IDwvaDE+JyBwcm9kdWNlcyBhIHRleHQgbm9kZXMgaW5zaWRlIG9mIHRoZSBIMVxuLy8gZWxlbWVudCBcIlNvbWUgPGEgdGV4dFwiLiBXZSwgaG93ZXZlciwgd2FudCB0byB0cmVhdCB0aGlzIGFzIGlmIHRoZSB1c2VyIHdhcyByZXF1ZXN0aW5nXG4vLyB0aGUgYXR0cmlidXRlcyBvZiBhbiBcImFcIiBlbGVtZW50LCBub3QgcmVxdWVzdGluZyBjb21wbGV0aW9uIGluIHRoZSBhIHRleHQgZWxlbWVudC4gVGhpc1xuLy8gY29kZSBjaGVja3MgZm9yIHRoaXMgY2FzZSBhbmQgcmV0dXJucyBlbGVtZW50IGNvbXBsZXRpb25zIGlmIGl0IGlzIGRldGVjdGVkIG9yIHVuZGVmaW5lZFxuLy8gaWYgaXQgaXMgbm90LlxuZnVuY3Rpb24gdm9pZEVsZW1lbnRBdHRyaWJ1dGVDb21wbGV0aW9ucyhcbiAgICBpbmZvOiBuZy5Bc3RSZXN1bHQsIHBhdGg6IEFzdFBhdGg8SHRtbEFzdD4pOiBuZy5Db21wbGV0aW9uRW50cnlbXSB7XG4gIGNvbnN0IHRhaWwgPSBwYXRoLnRhaWw7XG4gIGlmICh0YWlsIGluc3RhbmNlb2YgVGV4dCkge1xuICAgIGNvbnN0IG1hdGNoID0gdGFpbC52YWx1ZS5tYXRjaCgvPChcXHcoXFx3fFxcZHwtKSo6KT8oXFx3KFxcd3xcXGR8LSkqKVxccy8pO1xuICAgIC8vIFRoZSBwb3NpdGlvbiBtdXN0IGJlIGFmdGVyIHRoZSBtYXRjaCwgb3RoZXJ3aXNlIHdlIGFyZSBzdGlsbCBpbiBhIHBsYWNlIHdoZXJlIGVsZW1lbnRzXG4gICAgLy8gYXJlIGV4cGVjdGVkIChzdWNoIGFzIGA8fGFgIG9yIGA8YXxgOyB3ZSBvbmx5IHdhbnQgYXR0cmlidXRlcyBmb3IgYDxhIHxgIG9yIGFmdGVyKS5cbiAgICBpZiAobWF0Y2ggJiZcbiAgICAgICAgcGF0aC5wb3NpdGlvbiA+PSAobWF0Y2guaW5kZXggfHwgMCkgKyBtYXRjaFswXS5sZW5ndGggKyB0YWlsLnNvdXJjZVNwYW4uc3RhcnQub2Zmc2V0KSB7XG4gICAgICByZXR1cm4gYXR0cmlidXRlQ29tcGxldGlvbnNGb3JFbGVtZW50KGluZm8sIG1hdGNoWzNdKTtcbiAgICB9XG4gIH1cbiAgcmV0dXJuIFtdO1xufVxuXG5jbGFzcyBFeHByZXNzaW9uVmlzaXRvciBleHRlbmRzIE51bGxUZW1wbGF0ZVZpc2l0b3Ige1xuICBwcml2YXRlIHJlYWRvbmx5IGNvbXBsZXRpb25zID0gbmV3IE1hcDxzdHJpbmcsIG5nLkNvbXBsZXRpb25FbnRyeT4oKTtcblxuICBjb25zdHJ1Y3RvcihcbiAgICAgIHByaXZhdGUgcmVhZG9ubHkgaW5mbzogbmcuQXN0UmVzdWx0LCBwcml2YXRlIHJlYWRvbmx5IHBvc2l0aW9uOiBudW1iZXIsXG4gICAgICBwcml2YXRlIHJlYWRvbmx5IGdldEV4cHJlc3Npb25TY29wZTogKCkgPT4gbmcuU3ltYm9sVGFibGUpIHtcbiAgICBzdXBlcigpO1xuICB9XG5cbiAgZ2V0IHJlc3VsdHMoKTogbmcuQ29tcGxldGlvbkVudHJ5W10ge1xuICAgIHJldHVybiBBcnJheS5mcm9tKHRoaXMuY29tcGxldGlvbnMudmFsdWVzKCkpO1xuICB9XG5cbiAgdmlzaXREaXJlY3RpdmVQcm9wZXJ0eShhc3Q6IEJvdW5kRGlyZWN0aXZlUHJvcGVydHlBc3QpOiB2b2lkIHtcbiAgICB0aGlzLnByb2Nlc3NFeHByZXNzaW9uQ29tcGxldGlvbnMoYXN0LnZhbHVlKTtcbiAgfVxuXG4gIHZpc2l0RWxlbWVudFByb3BlcnR5KGFzdDogQm91bmRFbGVtZW50UHJvcGVydHlBc3QpOiB2b2lkIHtcbiAgICB0aGlzLnByb2Nlc3NFeHByZXNzaW9uQ29tcGxldGlvbnMoYXN0LnZhbHVlKTtcbiAgfVxuXG4gIHZpc2l0RXZlbnQoYXN0OiBCb3VuZEV2ZW50QXN0KTogdm9pZCB7XG4gICAgdGhpcy5wcm9jZXNzRXhwcmVzc2lvbkNvbXBsZXRpb25zKGFzdC5oYW5kbGVyKTtcbiAgfVxuXG4gIHZpc2l0RWxlbWVudCgpOiB2b2lkIHtcbiAgICAvLyBuby1vcCBmb3Igbm93XG4gIH1cblxuICB2aXNpdEF0dHIoYXN0OiBBdHRyQXN0KSB7XG4gICAgY29uc3QgYmluZGluZyA9IGdldEJpbmRpbmdEZXNjcmlwdG9yKGFzdC5uYW1lKTtcbiAgICBpZiAoYmluZGluZyAmJiBiaW5kaW5nLmtpbmQgPT09IEFUVFIuS1dfTUlDUk9TWU5UQVgpIHtcbiAgICAgIC8vIFRoaXMgYSB0ZW1wbGF0ZSBiaW5kaW5nIGdpdmVuIGJ5IG1pY3JvIHN5bnRheCBleHByZXNzaW9uLlxuICAgICAgLy8gRmlyc3QsIHZlcmlmeSB0aGUgYXR0cmlidXRlIGNvbnNpc3RzIG9mIHNvbWUgYmluZGluZyB3ZSBjYW4gZ2l2ZSBjb21wbGV0aW9ucyBmb3IuXG4gICAgICAvLyBUaGUgc291cmNlU3BhbiBvZiBBdHRyQXN0IHBvaW50cyB0byB0aGUgUkhTIG9mIHRoZSBhdHRyaWJ1dGVcbiAgICAgIGNvbnN0IHRlbXBsYXRlS2V5ID0gYmluZGluZy5uYW1lO1xuICAgICAgY29uc3QgdGVtcGxhdGVWYWx1ZSA9IGFzdC5zb3VyY2VTcGFuLnRvU3RyaW5nKCk7XG4gICAgICBjb25zdCB0ZW1wbGF0ZVVybCA9IGFzdC5zb3VyY2VTcGFuLnN0YXJ0LmZpbGUudXJsO1xuICAgICAgLy8gVE9ETyhreWxpYXUpOiBXZSBhcmUgdW5hYmxlIHRvIGRldGVybWluZSB0aGUgYWJzb2x1dGUgb2Zmc2V0IG9mIHRoZSBrZXlcbiAgICAgIC8vIGJ1dCBpdCBpcyBva2F5IGhlcmUsIGJlY2F1c2Ugd2UgYXJlIG9ubHkgbG9va2luZyBhdCB0aGUgUkhTIG9mIHRoZSBhdHRyXG4gICAgICBjb25zdCBhYnNLZXlPZmZzZXQgPSAwO1xuICAgICAgY29uc3QgYWJzVmFsdWVPZmZzZXQgPSBhc3Quc291cmNlU3Bhbi5zdGFydC5vZmZzZXQ7XG4gICAgICBjb25zdCB7dGVtcGxhdGVCaW5kaW5nc30gPSB0aGlzLmluZm8uZXhwcmVzc2lvblBhcnNlci5wYXJzZVRlbXBsYXRlQmluZGluZ3MoXG4gICAgICAgICAgdGVtcGxhdGVLZXksIHRlbXBsYXRlVmFsdWUsIHRlbXBsYXRlVXJsLCBhYnNLZXlPZmZzZXQsIGFic1ZhbHVlT2Zmc2V0KTtcbiAgICAgIC8vIEZpbmQgdGhlIG5lYXJlc3QgdGVtcGxhdGUgYmluZGluZyB0byB0aGUgcG9zaXRpb24uXG4gICAgICBjb25zdCBsYXN0QmluZGluZ0VuZCA9IHRlbXBsYXRlQmluZGluZ3MubGVuZ3RoID4gMCAmJlxuICAgICAgICAgIHRlbXBsYXRlQmluZGluZ3NbdGVtcGxhdGVCaW5kaW5ncy5sZW5ndGggLSAxXS5zb3VyY2VTcGFuLmVuZDtcbiAgICAgIGNvbnN0IG5vcm1hbGl6ZWRQb3NpdGlvblRvQmluZGluZyA9XG4gICAgICAgICAgbGFzdEJpbmRpbmdFbmQgJiYgdGhpcy5wb3NpdGlvbiA+IGxhc3RCaW5kaW5nRW5kID8gbGFzdEJpbmRpbmdFbmQgOiB0aGlzLnBvc2l0aW9uO1xuICAgICAgY29uc3QgdGVtcGxhdGVCaW5kaW5nID1cbiAgICAgICAgICB0ZW1wbGF0ZUJpbmRpbmdzLmZpbmQoYiA9PiBpblNwYW4obm9ybWFsaXplZFBvc2l0aW9uVG9CaW5kaW5nLCBiLnNvdXJjZVNwYW4pKTtcblxuICAgICAgaWYgKCF0ZW1wbGF0ZUJpbmRpbmcpIHtcbiAgICAgICAgcmV0dXJuO1xuICAgICAgfVxuXG4gICAgICB0aGlzLm1pY3JvU3ludGF4SW5BdHRyaWJ1dGVWYWx1ZShhc3QsIHRlbXBsYXRlQmluZGluZyk7XG4gICAgfSBlbHNlIHtcbiAgICAgIGNvbnN0IGV4cHJlc3Npb25Bc3QgPSB0aGlzLmluZm8uZXhwcmVzc2lvblBhcnNlci5wYXJzZUJpbmRpbmcoXG4gICAgICAgICAgYXN0LnZhbHVlLCBhc3Quc291cmNlU3Bhbi50b1N0cmluZygpLCBhc3Quc291cmNlU3Bhbi5zdGFydC5vZmZzZXQpO1xuICAgICAgdGhpcy5wcm9jZXNzRXhwcmVzc2lvbkNvbXBsZXRpb25zKGV4cHJlc3Npb25Bc3QpO1xuICAgIH1cbiAgfVxuXG4gIHZpc2l0UmVmZXJlbmNlKF9hc3Q6IFJlZmVyZW5jZUFzdCwgY29udGV4dDogRWxlbWVudEFzdCkge1xuICAgIGNvbnRleHQuZGlyZWN0aXZlcy5mb3JFYWNoKGRpciA9PiB7XG4gICAgICBjb25zdCB7ZXhwb3J0QXN9ID0gZGlyLmRpcmVjdGl2ZTtcbiAgICAgIGlmIChleHBvcnRBcykge1xuICAgICAgICB0aGlzLmNvbXBsZXRpb25zLnNldChcbiAgICAgICAgICAgIGV4cG9ydEFzLCB7bmFtZTogZXhwb3J0QXMsIGtpbmQ6IG5nLkNvbXBsZXRpb25LaW5kLlJFRkVSRU5DRSwgc29ydFRleHQ6IGV4cG9ydEFzfSk7XG4gICAgICB9XG4gICAgfSk7XG4gIH1cblxuICB2aXNpdEJvdW5kVGV4dChhc3Q6IEJvdW5kVGV4dEFzdCkge1xuICAgIGlmIChpblNwYW4odGhpcy5wb3NpdGlvbiwgYXN0LnZhbHVlLnNvdXJjZVNwYW4pKSB7XG4gICAgICBjb25zdCBjb21wbGV0aW9ucyA9IGdldEV4cHJlc3Npb25Db21wbGV0aW9ucyhcbiAgICAgICAgICB0aGlzLmdldEV4cHJlc3Npb25TY29wZSgpLCBhc3QudmFsdWUsIHRoaXMucG9zaXRpb24sIHRoaXMuaW5mby50ZW1wbGF0ZSk7XG4gICAgICBpZiAoY29tcGxldGlvbnMpIHtcbiAgICAgICAgdGhpcy5hZGRTeW1ib2xzVG9Db21wbGV0aW9ucyhjb21wbGV0aW9ucyk7XG4gICAgICB9XG4gICAgfVxuICB9XG5cbiAgcHJpdmF0ZSBwcm9jZXNzRXhwcmVzc2lvbkNvbXBsZXRpb25zKHZhbHVlOiBBU1QpIHtcbiAgICBjb25zdCBzeW1ib2xzID0gZ2V0RXhwcmVzc2lvbkNvbXBsZXRpb25zKFxuICAgICAgICB0aGlzLmdldEV4cHJlc3Npb25TY29wZSgpLCB2YWx1ZSwgdGhpcy5wb3NpdGlvbiwgdGhpcy5pbmZvLnRlbXBsYXRlKTtcbiAgICBpZiAoc3ltYm9scykge1xuICAgICAgdGhpcy5hZGRTeW1ib2xzVG9Db21wbGV0aW9ucyhzeW1ib2xzKTtcbiAgICB9XG4gIH1cblxuICBwcml2YXRlIGFkZFN5bWJvbHNUb0NvbXBsZXRpb25zKHN5bWJvbHM6IG5nLlN5bWJvbFtdKSB7XG4gICAgZm9yIChjb25zdCBzIG9mIHN5bWJvbHMpIHtcbiAgICAgIGlmIChzLm5hbWUuc3RhcnRzV2l0aCgnX18nKSB8fCAhcy5wdWJsaWMgfHwgdGhpcy5jb21wbGV0aW9ucy5oYXMocy5uYW1lKSkge1xuICAgICAgICBjb250aW51ZTtcbiAgICAgIH1cblxuICAgICAgLy8gVGhlIHBpcGUgbWV0aG9kIHNob3VsZCBub3QgaW5jbHVkZSBwYXJlbnRoZXNlcy5cbiAgICAgIC8vIGUuZy4ge3sgdmFsdWVfZXhwcmVzc2lvbiB8IHNsaWNlIDogc3RhcnQgWyA6IGVuZCBdIH19XG4gICAgICBjb25zdCBzaG91bGRJbnNlcnRQYXJlbnRoZXNlcyA9IHMuY2FsbGFibGUgJiYgcy5raW5kICE9PSBuZy5Db21wbGV0aW9uS2luZC5QSVBFO1xuICAgICAgdGhpcy5jb21wbGV0aW9ucy5zZXQocy5uYW1lLCB7XG4gICAgICAgIG5hbWU6IHMubmFtZSxcbiAgICAgICAga2luZDogcy5raW5kIGFzIG5nLkNvbXBsZXRpb25LaW5kLFxuICAgICAgICBzb3J0VGV4dDogcy5uYW1lLFxuICAgICAgICBpbnNlcnRUZXh0OiBzaG91bGRJbnNlcnRQYXJlbnRoZXNlcyA/IGAke3MubmFtZX0oKWAgOiBzLm5hbWUsXG4gICAgICB9KTtcbiAgICB9XG4gIH1cblxuICAvKipcbiAgICogVGhpcyBtZXRob2QgaGFuZGxlcyB0aGUgY29tcGxldGlvbnMgb2YgYXR0cmlidXRlIHZhbHVlcyBmb3IgZGlyZWN0aXZlcyB0aGF0XG4gICAqIHN1cHBvcnQgdGhlIG1pY3Jvc3ludGF4IGZvcm1hdC4gRXhhbXBsZXMgYXJlICpuZ0ZvciBhbmQgKm5nSWYuXG4gICAqIFRoZXNlIGRpcmVjdGl2ZXMgYWxsb3dzIGRlY2xhcmF0aW9uIG9mIFwibGV0XCIgdmFyaWFibGVzLCBhZGRzIGNvbnRleHQtc3BlY2lmaWNcbiAgICogc3ltYm9scyBsaWtlICRpbXBsaWNpdCwgaW5kZXgsIGNvdW50LCBhbW9uZyBvdGhlciBiZWhhdmlvcnMuXG4gICAqIEZvciBhIGNvbXBsZXRlIGRlc2NyaXB0aW9uIG9mIHN1Y2ggZm9ybWF0LCBzZWVcbiAgICogaHR0cHM6Ly9hbmd1bGFyLmlvL2d1aWRlL3N0cnVjdHVyYWwtZGlyZWN0aXZlcyN0aGUtYXN0ZXJpc2stLXByZWZpeFxuICAgKlxuICAgKiBAcGFyYW0gYXR0ciBkZXNjcmlwdG9yIGZvciBhdHRyaWJ1dGUgbmFtZSBhbmQgdmFsdWUgcGFpclxuICAgKiBAcGFyYW0gYmluZGluZyB0ZW1wbGF0ZSBiaW5kaW5nIGZvciB0aGUgZXhwcmVzc2lvbiBpbiB0aGUgYXR0cmlidXRlXG4gICAqL1xuICBwcml2YXRlIG1pY3JvU3ludGF4SW5BdHRyaWJ1dGVWYWx1ZShhdHRyOiBBdHRyQXN0LCBiaW5kaW5nOiBUZW1wbGF0ZUJpbmRpbmcpIHtcbiAgICBjb25zdCBrZXkgPSBhdHRyLm5hbWUuc3Vic3RyaW5nKDEpOyAgLy8gcmVtb3ZlIGxlYWRpbmcgYXN0ZXJpc2tcblxuICAgIC8vIEZpbmQgdGhlIHNlbGVjdG9yIC0gZWcgbmdGb3IsIG5nSWYsIGV0Y1xuICAgIGNvbnN0IHNlbGVjdG9ySW5mbyA9IGdldFNlbGVjdG9ycyh0aGlzLmluZm8pO1xuICAgIGNvbnN0IHNlbGVjdG9yID0gc2VsZWN0b3JJbmZvLnNlbGVjdG9ycy5maW5kKHMgPT4ge1xuICAgICAgLy8gYXR0cmlidXRlcyBhcmUgbGlzdGVkIGluIChhdHRyaWJ1dGUsIHZhbHVlKSBwYWlyc1xuICAgICAgZm9yIChsZXQgaSA9IDA7IGkgPCBzLmF0dHJzLmxlbmd0aDsgaSArPSAyKSB7XG4gICAgICAgIGlmIChzLmF0dHJzW2ldID09PSBrZXkpIHtcbiAgICAgICAgICByZXR1cm4gdHJ1ZTtcbiAgICAgICAgfVxuICAgICAgfVxuICAgIH0pO1xuXG4gICAgaWYgKCFzZWxlY3Rvcikge1xuICAgICAgcmV0dXJuO1xuICAgIH1cblxuICAgIGNvbnN0IHZhbHVlUmVsYXRpdmVQb3NpdGlvbiA9IHRoaXMucG9zaXRpb24gLSBhdHRyLnNvdXJjZVNwYW4uc3RhcnQub2Zmc2V0O1xuXG4gICAgaWYgKGJpbmRpbmcgaW5zdGFuY2VvZiBWYXJpYWJsZUJpbmRpbmcpIHtcbiAgICAgIC8vIFRPRE8oa3lsaWF1KTogV2l0aCBleHByZXNzaW9uIHNvdXJjZVNwYW4gd2Ugc2hvdWxkbid0IGhhdmUgdG8gc2VhcmNoXG4gICAgICAvLyB0aGUgYXR0cmlidXRlIHZhbHVlIHN0cmluZyBhbnltb3JlLiBKdXN0IGNoZWNrIGlmIHBvc2l0aW9uIGlzIGluIHRoZVxuICAgICAgLy8gZXhwcmVzc2lvbiBzb3VyY2Ugc3Bhbi5cbiAgICAgIGNvbnN0IGVxdWFsTG9jYXRpb24gPSBhdHRyLnZhbHVlLmluZGV4T2YoJz0nKTtcbiAgICAgIGlmIChlcXVhbExvY2F0aW9uID4gMCAmJiB2YWx1ZVJlbGF0aXZlUG9zaXRpb24gPiBlcXVhbExvY2F0aW9uKSB7XG4gICAgICAgIC8vIFdlIGFyZSBhZnRlciB0aGUgJz0nIGluIGEgbGV0IGNsYXVzZS4gVGhlIHZhbGlkIHZhbHVlcyBoZXJlIGFyZSB0aGUgbWVtYmVycyBvZiB0aGVcbiAgICAgICAgLy8gdGVtcGxhdGUgcmVmZXJlbmNlJ3MgdHlwZSBwYXJhbWV0ZXIuXG4gICAgICAgIGNvbnN0IGRpcmVjdGl2ZU1ldGFkYXRhID0gc2VsZWN0b3JJbmZvLm1hcC5nZXQoc2VsZWN0b3IpO1xuICAgICAgICBpZiAoZGlyZWN0aXZlTWV0YWRhdGEpIHtcbiAgICAgICAgICBjb25zdCBjb250ZXh0VGFibGUgPVxuICAgICAgICAgICAgICB0aGlzLmluZm8udGVtcGxhdGUucXVlcnkuZ2V0VGVtcGxhdGVDb250ZXh0KGRpcmVjdGl2ZU1ldGFkYXRhLnR5cGUucmVmZXJlbmNlKTtcbiAgICAgICAgICBpZiAoY29udGV4dFRhYmxlKSB7XG4gICAgICAgICAgICAvLyBUaGlzIGFkZHMgc3ltYm9scyBsaWtlICRpbXBsaWNpdCwgaW5kZXgsIGNvdW50LCBldGMuXG4gICAgICAgICAgICB0aGlzLmFkZFN5bWJvbHNUb0NvbXBsZXRpb25zKGNvbnRleHRUYWJsZS52YWx1ZXMoKSk7XG4gICAgICAgICAgICByZXR1cm47XG4gICAgICAgICAgfVxuICAgICAgICB9XG4gICAgICB9XG4gICAgfSBlbHNlIGlmIChiaW5kaW5nIGluc3RhbmNlb2YgRXhwcmVzc2lvbkJpbmRpbmcpIHtcbiAgICAgIGlmIChpblNwYW4odGhpcy5wb3NpdGlvbiwgYmluZGluZy52YWx1ZT8uYXN0LnNvdXJjZVNwYW4pKSB7XG4gICAgICAgIHRoaXMucHJvY2Vzc0V4cHJlc3Npb25Db21wbGV0aW9ucyhiaW5kaW5nLnZhbHVlIS5hc3QpO1xuICAgICAgICByZXR1cm47XG4gICAgICB9IGVsc2UgaWYgKCFiaW5kaW5nLnZhbHVlICYmIHRoaXMucG9zaXRpb24gPiBiaW5kaW5nLmtleS5zcGFuLmVuZCkge1xuICAgICAgICAvLyBObyBleHByZXNzaW9uIGlzIGRlZmluZWQgZm9yIHRoZSB2YWx1ZSBvZiB0aGUga2V5IGV4cHJlc3Npb24gYmluZGluZywgYnV0IHRoZSBjdXJzb3IgaXNcbiAgICAgICAgLy8gaW4gYSBsb2NhdGlvbiB3aGVyZSB0aGUgZXhwcmVzc2lvbiB3b3VsZCBiZSBkZWZpbmVkLiBUaGlzIGNhbiBoYXBwZW4gaW4gYSBjYXNlIGxpa2VcbiAgICAgICAgLy8gICBsZXQgaSBvZiB8XG4gICAgICAgIC8vICAgICAgICAgICAgXi0tIGN1cnNvclxuICAgICAgICAvLyBJbiB0aGlzIGNhc2UsIGJhY2tmaWxsIHRoZSB2YWx1ZSB0byBiZSBhbiBlbXB0eSBleHByZXNzaW9uIGFuZCByZXRyaWV2ZSBjb21wbGV0aW9ucy5cbiAgICAgICAgdGhpcy5wcm9jZXNzRXhwcmVzc2lvbkNvbXBsZXRpb25zKG5ldyBFbXB0eUV4cHIoXG4gICAgICAgICAgICBuZXcgUGFyc2VTcGFuKHZhbHVlUmVsYXRpdmVQb3NpdGlvbiwgdmFsdWVSZWxhdGl2ZVBvc2l0aW9uKSxcbiAgICAgICAgICAgIG5ldyBBYnNvbHV0ZVNvdXJjZVNwYW4odGhpcy5wb3NpdGlvbiwgdGhpcy5wb3NpdGlvbikpKTtcbiAgICAgICAgcmV0dXJuO1xuICAgICAgfVxuICAgIH1cbiAgfVxufVxuXG5pbnRlcmZhY2UgQW5ndWxhckF0dHJpYnV0ZXMge1xuICAvKipcbiAgICogQXR0cmlidXRlcyB0aGF0IHN1cHBvcnQgdGhlICogc3ludGF4LiBTZWUgaHR0cHM6Ly9hbmd1bGFyLmlvL2FwaS9jb3JlL1RlbXBsYXRlUmVmXG4gICAqL1xuICB0ZW1wbGF0ZVJlZnM6IFNldDxzdHJpbmc+O1xuICAvKipcbiAgICogQXR0cmlidXRlcyB3aXRoIHRoZSBASW5wdXQgYW5ub3RhdGlvbi5cbiAgICovXG4gIGlucHV0czogU2V0PHN0cmluZz47XG4gIC8qKlxuICAgKiBBdHRyaWJ1dGVzIHdpdGggdGhlIEBPdXRwdXQgYW5ub3RhdGlvbi5cbiAgICovXG4gIG91dHB1dHM6IFNldDxzdHJpbmc+O1xuICAvKipcbiAgICogQXR0cmlidXRlcyB0aGF0IHN1cHBvcnQgdGhlIFsoKV0gb3IgYmluZG9uLSBzeW50YXguXG4gICAqL1xuICBiYW5hbmFzOiBTZXQ8c3RyaW5nPjtcbiAgLyoqXG4gICAqIEdlbmVyYWwgYXR0cmlidXRlcyB0aGF0IG1hdGNoIHRoZSBzcGVjaWZpZWQgZWxlbWVudC5cbiAgICovXG4gIG90aGVyczogU2V0PHN0cmluZz47XG59XG5cbi8qKlxuICogUmV0dXJuIGFsbCBBbmd1bGFyLXNwZWNpZmljIGF0dHJpYnV0ZXMgZm9yIHRoZSBlbGVtZW50IHdpdGggYGVsZW1lbnROYW1lYC5cbiAqIEBwYXJhbSBpbmZvXG4gKiBAcGFyYW0gZWxlbWVudE5hbWVcbiAqL1xuZnVuY3Rpb24gYW5ndWxhckF0dHJpYnV0ZXMoaW5mbzogbmcuQXN0UmVzdWx0LCBlbGVtZW50TmFtZTogc3RyaW5nKTogQW5ndWxhckF0dHJpYnV0ZXMge1xuICBjb25zdCB7c2VsZWN0b3JzLCBtYXA6IHNlbGVjdG9yTWFwfSA9IGdldFNlbGVjdG9ycyhpbmZvKTtcbiAgY29uc3QgdGVtcGxhdGVSZWZzID0gbmV3IFNldDxzdHJpbmc+KCk7XG4gIGNvbnN0IGlucHV0cyA9IG5ldyBTZXQ8c3RyaW5nPigpO1xuICBjb25zdCBvdXRwdXRzID0gbmV3IFNldDxzdHJpbmc+KCk7XG4gIGNvbnN0IGJhbmFuYXMgPSBuZXcgU2V0PHN0cmluZz4oKTtcbiAgY29uc3Qgb3RoZXJzID0gbmV3IFNldDxzdHJpbmc+KCk7XG4gIGZvciAoY29uc3Qgc2VsZWN0b3Igb2Ygc2VsZWN0b3JzKSB7XG4gICAgaWYgKHNlbGVjdG9yLmVsZW1lbnQgJiYgc2VsZWN0b3IuZWxlbWVudCAhPT0gZWxlbWVudE5hbWUpIHtcbiAgICAgIGNvbnRpbnVlO1xuICAgIH1cbiAgICBjb25zdCBzdW1tYXJ5ID0gc2VsZWN0b3JNYXAuZ2V0KHNlbGVjdG9yKSE7XG4gICAgY29uc3QgaGFzVGVtcGxhdGVSZWYgPSBpc1N0cnVjdHVyYWxEaXJlY3RpdmUoc3VtbWFyeS50eXBlKTtcbiAgICAvLyBhdHRyaWJ1dGVzIGFyZSBsaXN0ZWQgaW4gKGF0dHJpYnV0ZSwgdmFsdWUpIHBhaXJzXG4gICAgZm9yIChsZXQgaSA9IDA7IGkgPCBzZWxlY3Rvci5hdHRycy5sZW5ndGg7IGkgKz0gMikge1xuICAgICAgY29uc3QgYXR0ciA9IHNlbGVjdG9yLmF0dHJzW2ldO1xuICAgICAgaWYgKGhhc1RlbXBsYXRlUmVmKSB7XG4gICAgICAgIHRlbXBsYXRlUmVmcy5hZGQoYXR0cik7XG4gICAgICB9IGVsc2Uge1xuICAgICAgICBvdGhlcnMuYWRkKGF0dHIpO1xuICAgICAgfVxuICAgIH1cbiAgICBmb3IgKGNvbnN0IGlucHV0IG9mIE9iamVjdC52YWx1ZXMoc3VtbWFyeS5pbnB1dHMpKSB7XG4gICAgICBpbnB1dHMuYWRkKGlucHV0KTtcbiAgICB9XG4gICAgZm9yIChjb25zdCBvdXRwdXQgb2YgT2JqZWN0LnZhbHVlcyhzdW1tYXJ5Lm91dHB1dHMpKSB7XG4gICAgICBvdXRwdXRzLmFkZChvdXRwdXQpO1xuICAgIH1cbiAgfVxuICBmb3IgKGNvbnN0IG5hbWUgb2YgaW5wdXRzKSB7XG4gICAgLy8gQWRkIGJhbmFuYS1pbi1hLWJveCBzeW50YXhcbiAgICAvLyBodHRwczovL2FuZ3VsYXIuaW8vZ3VpZGUvdGVtcGxhdGUtc3ludGF4I3R3by13YXktYmluZGluZy1cbiAgICBpZiAob3V0cHV0cy5oYXMoYCR7bmFtZX1DaGFuZ2VgKSkge1xuICAgICAgYmFuYW5hcy5hZGQobmFtZSk7XG4gICAgfVxuICB9XG4gIHJldHVybiB7dGVtcGxhdGVSZWZzLCBpbnB1dHMsIG91dHB1dHMsIGJhbmFuYXMsIG90aGVyc307XG59XG4iXX0=