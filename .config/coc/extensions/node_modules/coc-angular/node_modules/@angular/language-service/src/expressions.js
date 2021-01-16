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
        define("@angular/language-service/src/expressions", ["require", "exports", "tslib", "@angular/compiler", "@angular/language-service/src/expression_type", "@angular/language-service/src/types", "@angular/language-service/src/utils"], factory);
    }
})(function (require, exports) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    exports.getExpressionSymbol = exports.getExpressionCompletions = void 0;
    var tslib_1 = require("tslib");
    var compiler_1 = require("@angular/compiler");
    var expression_type_1 = require("@angular/language-service/src/expression_type");
    var types_1 = require("@angular/language-service/src/types");
    var utils_1 = require("@angular/language-service/src/utils");
    function findAstAt(ast, position, excludeEmpty) {
        if (excludeEmpty === void 0) { excludeEmpty = false; }
        var path = [];
        var visitor = new /** @class */ (function (_super) {
            tslib_1.__extends(class_1, _super);
            function class_1() {
                return _super !== null && _super.apply(this, arguments) || this;
            }
            class_1.prototype.visit = function (ast) {
                if ((!excludeEmpty || ast.sourceSpan.start < ast.sourceSpan.end) &&
                    utils_1.inSpan(position, ast.sourceSpan)) {
                    var isNotNarrower = path.length && !utils_1.isNarrower(ast.span, path[path.length - 1].span);
                    if (!isNotNarrower) {
                        path.push(ast);
                    }
                    ast.visit(this);
                }
            };
            return class_1;
        }(compiler_1.RecursiveAstVisitor));
        // We never care about the ASTWithSource node and its visit() method calls its ast's visit so
        // the visit() method above would never see it.
        if (ast instanceof compiler_1.ASTWithSource) {
            ast = ast.ast;
        }
        // `Interpolation` is useless here except the `expressions` of it.
        if (ast instanceof compiler_1.Interpolation) {
            ast = ast.expressions.filter(function (_ast) { return utils_1.inSpan(position, _ast.sourceSpan); })[0];
        }
        if (ast) {
            visitor.visit(ast);
        }
        return new compiler_1.AstPath(path, position);
    }
    function getExpressionCompletions(scope, ast, position, templateInfo) {
        var path = findAstAt(ast, position);
        if (path.empty)
            return undefined;
        var tail = path.tail;
        var result = scope;
        function getType(ast) {
            return new expression_type_1.AstType(scope, templateInfo.query, {}, templateInfo.source).getType(ast);
        }
        // If the completion request is in a not in a pipe or property access then the global scope
        // (that is the scope of the implicit receiver) is the right scope as the user is typing the
        // beginning of an expression.
        tail.visit({
            visitBinary: function (_ast) { },
            visitChain: function (_ast) { },
            visitConditional: function (_ast) { },
            visitFunctionCall: function (_ast) { },
            visitImplicitReceiver: function (_ast) { },
            visitInterpolation: function (_ast) {
                result = undefined;
            },
            visitKeyedRead: function (_ast) { },
            visitKeyedWrite: function (_ast) { },
            visitLiteralArray: function (_ast) { },
            visitLiteralMap: function (_ast) { },
            visitLiteralPrimitive: function (ast) {
                // The type `LiteralPrimitive` include the `ERROR`, and it's wrapped as `string`.
                // packages/compiler/src/template_parser/binding_parser.ts#L308
                // So exclude the `ERROR` here.
                if (typeof ast.value === 'string' &&
                    ast.value ===
                        templateInfo.source.slice(ast.sourceSpan.start + 1, ast.sourceSpan.end - 1)) {
                    result = undefined;
                }
            },
            visitMethodCall: function (_ast) { },
            visitPipe: function (ast) {
                if (position >= ast.exp.span.end &&
                    (!ast.args || !ast.args.length || position < ast.args[0].span.start)) {
                    // We are in a position a pipe name is expected.
                    result = templateInfo.query.getPipes();
                }
            },
            visitPrefixNot: function (_ast) { },
            visitNonNullAssert: function (_ast) { },
            visitPropertyRead: function (ast) {
                var receiverType = getType(ast.receiver);
                result = receiverType ? receiverType.members() : scope;
            },
            visitPropertyWrite: function (ast) {
                var receiverType = getType(ast.receiver);
                result = receiverType ? receiverType.members() : scope;
            },
            visitQuote: function (_ast) {
                // For a quote, return the members of any (if there are any).
                result = templateInfo.query.getBuiltinType(types_1.BuiltinType.Any).members();
            },
            visitSafeMethodCall: function (ast) {
                var receiverType = getType(ast.receiver);
                result = receiverType ? receiverType.members() : scope;
            },
            visitSafePropertyRead: function (ast) {
                var receiverType = getType(ast.receiver);
                result = receiverType ? receiverType.members() : scope;
            },
        });
        return result && result.values();
    }
    exports.getExpressionCompletions = getExpressionCompletions;
    /**
     * Retrieves the expression symbol at a particular position in a template.
     *
     * @param scope symbols in scope of the template
     * @param ast template AST
     * @param position absolute location in template to retrieve symbol at
     * @param query type symbol query for the template scope
     */
    function getExpressionSymbol(scope, ast, position, templateInfo) {
        var path = findAstAt(ast, position, /* excludeEmpty */ true);
        if (path.empty)
            return undefined;
        var tail = path.tail;
        function getType(ast) {
            return new expression_type_1.AstType(scope, templateInfo.query, {}, templateInfo.source).getType(ast);
        }
        function spanFromName(ast) {
            // `nameSpan` is an absolute span, but the span expected by the result of this method is
            // relative to the start of the expression.
            // TODO(ayazhafiz): migrate to only using absolute spans
            var offset = ast.sourceSpan.start - ast.span.start;
            return {
                start: ast.nameSpan.start - offset,
                end: ast.nameSpan.end - offset,
            };
        }
        var symbol = undefined;
        var span = undefined;
        // If the completion request is in a not in a pipe or property access then the global scope
        // (that is the scope of the implicit receiver) is the right scope as the user is typing the
        // beginning of an expression.
        tail.visit({
            visitBinary: function (_ast) { },
            visitChain: function (_ast) { },
            visitConditional: function (_ast) { },
            visitFunctionCall: function (_ast) { },
            visitImplicitReceiver: function (_ast) { },
            visitInterpolation: function (_ast) { },
            visitKeyedRead: function (_ast) { },
            visitKeyedWrite: function (_ast) { },
            visitLiteralArray: function (_ast) { },
            visitLiteralMap: function (_ast) { },
            visitLiteralPrimitive: function (_ast) { },
            visitMethodCall: function (ast) {
                var receiverType = getType(ast.receiver);
                symbol = receiverType && receiverType.members().get(ast.name);
                span = spanFromName(ast);
            },
            visitPipe: function (ast) {
                if (utils_1.inSpan(position, ast.nameSpan, /* exclusive */ true)) {
                    // We are in a position a pipe name is expected.
                    var pipes = templateInfo.query.getPipes();
                    symbol = pipes.get(ast.name);
                    span = spanFromName(ast);
                }
            },
            visitPrefixNot: function (_ast) { },
            visitNonNullAssert: function (_ast) { },
            visitPropertyRead: function (ast) {
                var receiverType = getType(ast.receiver);
                symbol = receiverType && receiverType.members().get(ast.name);
                span = spanFromName(ast);
            },
            visitPropertyWrite: function (ast) {
                var receiverType = getType(ast.receiver);
                symbol = receiverType && receiverType.members().get(ast.name);
                span = spanFromName(ast);
            },
            visitQuote: function (_ast) { },
            visitSafeMethodCall: function (ast) {
                var receiverType = getType(ast.receiver);
                symbol = receiverType && receiverType.members().get(ast.name);
                span = spanFromName(ast);
            },
            visitSafePropertyRead: function (ast) {
                var receiverType = getType(ast.receiver);
                symbol = receiverType && receiverType.members().get(ast.name);
                span = spanFromName(ast);
            },
        });
        if (symbol && span) {
            return { symbol: symbol, span: span };
        }
    }
    exports.getExpressionSymbol = getExpressionSymbol;
});
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiZXhwcmVzc2lvbnMuanMiLCJzb3VyY2VSb290IjoiIiwic291cmNlcyI6WyIuLi8uLi8uLi8uLi8uLi8uLi9wYWNrYWdlcy9sYW5ndWFnZS1zZXJ2aWNlL3NyYy9leHByZXNzaW9ucy50cyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiQUFBQTs7Ozs7O0dBTUc7Ozs7Ozs7Ozs7Ozs7O0lBRUgsOENBQThIO0lBRTlILGlGQUEwQztJQUMxQyw2REFBK0U7SUFDL0UsNkRBQTJDO0lBSTNDLFNBQVMsU0FBUyxDQUFDLEdBQVEsRUFBRSxRQUFnQixFQUFFLFlBQTZCO1FBQTdCLDZCQUFBLEVBQUEsb0JBQTZCO1FBQzFFLElBQU0sSUFBSSxHQUFVLEVBQUUsQ0FBQztRQUN2QixJQUFNLE9BQU8sR0FBRztZQUFrQixtQ0FBbUI7WUFBakM7O1lBV3BCLENBQUM7WUFWQyx1QkFBSyxHQUFMLFVBQU0sR0FBUTtnQkFDWixJQUFJLENBQUMsQ0FBQyxZQUFZLElBQUksR0FBRyxDQUFDLFVBQVUsQ0FBQyxLQUFLLEdBQUcsR0FBRyxDQUFDLFVBQVUsQ0FBQyxHQUFHLENBQUM7b0JBQzVELGNBQU0sQ0FBQyxRQUFRLEVBQUUsR0FBRyxDQUFDLFVBQVUsQ0FBQyxFQUFFO29CQUNwQyxJQUFNLGFBQWEsR0FBRyxJQUFJLENBQUMsTUFBTSxJQUFJLENBQUMsa0JBQVUsQ0FBQyxHQUFHLENBQUMsSUFBSSxFQUFFLElBQUksQ0FBQyxJQUFJLENBQUMsTUFBTSxHQUFHLENBQUMsQ0FBQyxDQUFDLElBQUksQ0FBQyxDQUFDO29CQUN2RixJQUFJLENBQUMsYUFBYSxFQUFFO3dCQUNsQixJQUFJLENBQUMsSUFBSSxDQUFDLEdBQUcsQ0FBQyxDQUFDO3FCQUNoQjtvQkFDRCxHQUFHLENBQUMsS0FBSyxDQUFDLElBQUksQ0FBQyxDQUFDO2lCQUNqQjtZQUNILENBQUM7WUFDSCxjQUFDO1FBQUQsQ0FBQyxBQVhtQixDQUFjLDhCQUFtQixFQVdwRCxDQUFDO1FBRUYsNkZBQTZGO1FBQzdGLCtDQUEr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LENBQUMsR0FBRyxDQUFDLENBQUM7WUFDM0IsQ0FBQztTQUNGLENBQUMsQ0FBQztRQUVILElBQUksTUFBTSxJQUFJLElBQUksRUFBRTtZQUNsQixPQUFPLEVBQUMsTUFBTSxRQUFBLEVBQUUsSUFBSSxNQUFBLEVBQUMsQ0FBQztTQUN2QjtJQUNILENBQUM7SUFqRkQsa0RBaUZDIiwic291cmNlc0NvbnRlbnQiOlsiLyoqXG4gKiBAbGljZW5zZVxuICogQ29weXJpZ2h0IEdvb2dsZSBMTEMgQWxsIFJpZ2h0cyBSZXNlcnZlZC5cbiAqXG4gKiBVc2Ugb2YgdGhpcyBzb3VyY2UgY29kZSBpcyBnb3Zlcm5lZCBieSBhbiBNSVQtc3R5bGUgbGljZW5zZSB0aGF0IGNhbiBiZVxuICogZm91bmQgaW4gdGhlIExJQ0VOU0UgZmlsZSBhdCBodHRwczovL2FuZ3VsYXIuaW8vbGljZW5zZVxuICovXG5cbmltcG9ydCB7QVNULCBBc3RQYXRoIGFzIEFzdFBhdGhCYXNlLCBBU1RXaXRoTmFtZSwgQVNUV2l0aFNvdXJjZSwgSW50ZXJwb2xhdGlvbiwgUmVjdXJzaXZlQXN0VmlzaXRvcn0gZnJvbSAnQGFuZ3VsYXIvY29tcGlsZXInO1xuXG5pbXBvcnQge0FzdFR5cGV9IGZyb20gJy4vZXhwcmVzc2lvbl90eXBlJztcbmltcG9ydCB7QnVpbHRpblR5cGUsIFNwYW4sIFN5bWJvbCwgU3ltYm9sVGFibGUsIFRlbXBsYXRlU291cmNlfSBmcm9tICcuL3R5cGVzJztcbmltcG9ydCB7aW5TcGFuLCBpc05hcnJvd2VyfSBmcm9tICcuL3V0aWxzJztcblxudHlwZSBBc3RQYXRoID0gQXN0UGF0aEJhc2U8QVNUPjtcblxuZnVuY3Rpb24gZmluZEFzdEF0KGFzdDogQVNULCBwb3NpdGlvbjogbnVtYmVyLCBleGNsdWRlRW1wdHk6IGJvb2xlYW4gPSBmYWxzZSk6IEFzdFBhdGgge1xuICBjb25zdCBwYXRoOiBBU1RbXSA9IFtdO1xuICBjb25zdCB2aXNpdG9yID0gbmV3IGNsYXNzIGV4dGVuZHMgUmVjdXJzaXZlQXN0VmlzaXRvciB7XG4gICAgdmlzaXQoYXN0OiBBU1QpIHtcbiAgICAgIGlmICgoIWV4Y2x1ZGVFbXB0eSB8fCBhc3Quc291cmNlU3Bhbi5zdGFydCA8IGFzdC5zb3VyY2VTcGFuLmVuZCkgJiZcbiAgICAgICAgICBpblNwYW4ocG9zaXRpb24sIGFzdC5zb3VyY2VTcGFuKSkge1xuICAgICAgICBjb25zdCBpc05vdE5hcnJvd2VyID0gcGF0aC5sZW5ndGggJiYgIWlzTmFycm93ZXIoYXN0LnNwYW4sIHBhdGhbcGF0aC5sZW5ndGggLSAxXS5zcGFuKTtcbiAgICAgICAgaWYgKCFpc05vdE5hcnJvd2VyKSB7XG4gICAgICAgICAgcGF0aC5wdXNoKGFzdCk7XG4gICAgICAgIH1cbiAgICAgICAgYXN0LnZpc2l0KHRoaXMpO1xuICAgICAgfVxuICAgIH1cbiAgfTtcblxuICAvLyBXZSBuZXZlciBjYXJlIGFib3V0IHRoZSBBU1RXaXRoU291cmNlIG5vZGUgYW5kIGl0cyB2aXNpdCgpIG1ldGhvZCBjYWxscyBpdHMgYXN0J3MgdmlzaXQgc29cbiAgLy8gdGhlIHZpc2l0KCkgbWV0aG9kIGFib3ZlIHdvdWxkIG5ldmVyIHNlZSBpdC5cbiAgaWYgKGFzdCBpbnN0YW5jZW9mIEFTVFdpdGhTb3VyY2UpIHtcbiAgICBhc3QgPSBhc3QuYXN0O1xuICB9XG5cbiAgLy8gYEludGVycG9sYXRpb25gIGlzIHVzZWxlc3MgaGVyZSBleGNlcHQgdGhlIGBleHByZXNzaW9uc2Agb2YgaXQuXG4gIGlmIChhc3QgaW5zdGFuY2VvZiBJbnRlcnBvbGF0aW9uKSB7XG4gICAgYXN0ID0gYXN0LmV4cHJlc3Npb25zLmZpbHRlcigoX2FzdDogQVNUKSA9PiBpblNwYW4ocG9zaXRpb24sIF9hc3Quc291cmNlU3BhbikpWzBdO1xuICB9XG5cbiAgaWYgKGFzdCkge1xuICAgIHZpc2l0b3IudmlzaXQoYXN0KTtcbiAgfVxuXG4gIHJldHVybiBuZXcgQXN0UGF0aEJhc2U8QVNUPihwYXRoLCBwb3NpdGlvbik7XG59XG5cbmV4cG9ydCBmdW5jdGlvbiBnZXRFeHByZXNzaW9uQ29tcGxldGlvbnMoXG4gICAgc2NvcGU6IFN5bWJvbFRhYmxlLCBhc3Q6IEFTVCwgcG9zaXRpb246IG51bWJlciwgdGVtcGxhdGVJbmZvOiBUZW1wbGF0ZVNvdXJjZSk6IFN5bWJvbFtdfFxuICAgIHVuZGVmaW5lZCB7XG4gIGNvbnN0IHBhdGggPSBmaW5kQXN0QXQoYXN0LCBwb3NpdGlvbik7XG4gIGlmIChwYXRoLmVtcHR5KSByZXR1cm4gdW5kZWZpbmVkO1xuICBjb25zdCB0YWlsID0gcGF0aC50YWlsITtcbiAgbGV0IHJlc3VsdDogU3ltYm9sVGFibGV8dW5kZWZpbmVkID0gc2NvcGU7XG5cbiAgZnVuY3Rpb24gZ2V0VHlwZShhc3Q6IEFTVCk6IFN5bWJvbCB7XG4gICAgcmV0dXJuIG5ldyBBc3RUeXBlKHNjb3BlLCB0ZW1wbGF0ZUluZm8ucXVlcnksIHt9LCB0ZW1wbGF0ZUluZm8uc291cmNlKS5nZXRUeXBlKGFzdCk7XG4gIH1cblxuICAvLyBJZiB0aGUgY29tcGxldGlvbiByZXF1ZXN0IGlzIGluIGEgbm90IGluIGEgcGlwZSBvciBwcm9wZXJ0eSBhY2Nlc3MgdGhlbiB0aGUgZ2xvYmFsIHNjb3BlXG4gIC8vICh0aGF0IGlzIHRoZSBzY29wZSBvZiB0aGUgaW1wbGljaXQgcmVjZWl2ZXIpIGlzIHRoZSByaWdodCBzY29wZSBhcyB0aGUgdXNlciBpcyB0eXBpbmcgdGhlXG4gIC8vIGJlZ2lubmluZyBvZiBhbiBleHByZXNzaW9uLlxuICB0YWlsLnZpc2l0KHtcbiAgICB2aXNpdEJpbmFyeShfYXN0KSB7fSxcbiAgICB2aXNpdENoYWluKF9hc3QpIHt9LFxuICAgIHZpc2l0Q29uZGl0aW9uYWwoX2FzdCkge30sXG4gICAgdmlzaXRGdW5jdGlvbkNhbGwoX2FzdCkge30sXG4gICAgdmlzaXRJbXBsaWNpdFJlY2VpdmVyKF9hc3QpIHt9LFxuICAgIHZpc2l0SW50ZXJwb2xhdGlvbihfYXN0KSB7XG4gICAgICByZXN1bHQgPSB1bmRlZmluZWQ7XG4gICAgfSxcbiAgICB2aXNpdEtleWVkUmVhZChfYXN0KSB7fSxcbiAgICB2aXNpdEtleWVkV3JpdGUoX2FzdCkge30sXG4gICAgdmlzaXRMaXRlcmFsQXJyYXkoX2FzdCkge30sXG4gICAgdmlzaXRMaXRlcmFsTWFwKF9hc3QpIHt9LFxuICAgIHZpc2l0TGl0ZXJhbFByaW1pdGl2ZShhc3QpIHtcbiAgICAgIC8vIFRoZSB0eXBlIGBMaXRlcmFsUHJpbWl0aXZlYCBpbmNsdWRlIHRoZSBgRVJST1JgLCBhbmQgaXQncyB3cmFwcGVkIGFzIGBzdHJpbmdgLlxuICAgICAgLy8gcGFja2FnZXMvY29tcGlsZXIvc3JjL3RlbXBsYXRlX3BhcnNlci9iaW5kaW5nX3BhcnNlci50cyNMMzA4XG4gICAgICAvLyBTbyBleGNsdWRlIHRoZSBgRVJST1JgIGhlcmUuXG4gICAgICBpZiAodHlwZW9mIGFzdC52YWx1ZSA9PT0gJ3N0cmluZycgJiZcbiAgICAgICAgICBhc3QudmFsdWUgPT09XG4gICAgICAgICAgICAgIHRlbXBsYXRlSW5mby5zb3VyY2Uuc2xpY2UoYXN0LnNvdXJjZVNwYW4uc3RhcnQgKyAxLCBhc3Quc291cmNlU3Bhbi5lbmQgLSAxKSkge1xuICAgICAgICByZXN1bHQgPSB1bmRlZmluZWQ7XG4gICAgICB9XG4gICAgfSxcbiAgICB2aXNpdE1ldGhvZENhbGwoX2FzdCkge30sXG4gICAgdmlzaXRQaXBlKGFzdCkge1xuICAgICAgaWYgKHBvc2l0aW9uID49IGFzdC5leHAuc3Bhbi5lbmQgJiZcbiAgICAgICAgICAoIWFzdC5hcmdzIHx8ICFhc3QuYXJncy5sZW5ndGggfHwgcG9zaXRpb24gPCAoPEFTVD5hc3QuYXJnc1swXSkuc3Bhbi5zdGFydCkpIHtcbiAgICAgICAgLy8gV2UgYXJlIGluIGEgcG9zaXRpb24gYSBwaXBlIG5hbWUgaXMgZXhwZWN0ZWQuXG4gICAgICAgIHJlc3VsdCA9IHRlbXBsYXRlSW5mby5xdWVyeS5nZXRQaXBlcygpO1xuICAgICAgfVxuICAgIH0sXG4gICAgdmlzaXRQcmVmaXhOb3QoX2FzdCkge30sXG4gICAgdmlzaXROb25OdWxsQXNzZXJ0KF9hc3QpIHt9LFxuICAgIHZpc2l0UHJvcGVydHlSZWFkKGFzdCkge1xuICAgICAgY29uc3QgcmVjZWl2ZXJUeXBlID0gZ2V0VHlwZShhc3QucmVjZWl2ZXIpO1xuICAgICAgcmVzdWx0ID0gcmVjZWl2ZXJUeXBlID8gcmVjZWl2ZXJUeXBlLm1lbWJlcnMoKSA6IHNjb3BlO1xuICAgIH0sXG4gICAgdmlzaXRQcm9wZXJ0eVdyaXRlKGFzdCkge1xuICAgICAgY29uc3QgcmVjZWl2ZXJUeXBlID0gZ2V0VHlwZShhc3QucmVjZWl2ZXIpO1xuICAgICAgcmVzdWx0ID0gcmVjZWl2ZXJUeXBlID8gcmVjZWl2ZXJUeXBlLm1lbWJlcnMoKSA6IHNjb3BlO1xuICAgIH0sXG4gICAgdmlzaXRRdW90ZShfYXN0KSB7XG4gICAgICAvLyBGb3IgYSBxdW90ZSwgcmV0dXJuIHRoZSBtZW1iZXJzIG9mIGFueSAoaWYgdGhlcmUgYXJlIGFueSkuXG4gICAgICByZXN1bHQgPSB0ZW1wbGF0ZUluZm8ucXVlcnkuZ2V0QnVpbHRpblR5cGUoQnVpbHRpblR5cGUuQW55KS5tZW1iZXJzKCk7XG4gICAgfSxcbiAgICB2aXNpdFNhZmVNZXRob2RDYWxsKGFzdCkge1xuICAgICAgY29uc3QgcmVjZWl2ZXJUeXBlID0gZ2V0VHlwZShhc3QucmVjZWl2ZXIpO1xuICAgICAgcmVzdWx0ID0gcmVjZWl2ZXJUeXBlID8gcmVjZWl2ZXJUeXBlLm1lbWJlcnMoKSA6IHNjb3BlO1xuICAgIH0sXG4gICAgdmlzaXRTYWZlUHJvcGVydHlSZWFkKGFzdCkge1xuICAgICAgY29uc3QgcmVjZWl2ZXJUeXBlID0gZ2V0VHlwZShhc3QucmVjZWl2ZXIpO1xuICAgICAgcmVzdWx0ID0gcmVjZWl2ZXJUeXBlID8gcmVjZWl2ZXJUeXBlLm1lbWJlcnMoKSA6IHNjb3BlO1xuICAgIH0sXG4gIH0pO1xuXG4gIHJldHVybiByZXN1bHQgJiYgcmVzdWx0LnZhbHVlcygpO1xufVxuXG4vKipcbiAqIFJldHJpZXZlcyB0aGUgZXhwcmVzc2lvbiBzeW1ib2wgYXQgYSBwYXJ0aWN1bGFyIHBvc2l0aW9uIGluIGEgdGVtcGxhdGUuXG4gKlxuICogQHBhcmFtIHNjb3BlIHN5bWJvbHMgaW4gc2NvcGUgb2YgdGhlIHRlbXBsYXRlXG4gKiBAcGFyYW0gYXN0IHRlbXBsYXRlIEFTVFxuICogQHBhcmFtIHBvc2l0aW9uIGFic29sdXRlIGxvY2F0aW9uIGluIHRlbXBsYXRlIHRvIHJldHJpZXZlIHN5bWJvbCBhdFxuICogQHBhcmFtIHF1ZXJ5IHR5cGUgc3ltYm9sIHF1ZXJ5IGZvciB0aGUgdGVtcGxhdGUgc2NvcGVcbiAqL1xuZXhwb3J0IGZ1bmN0aW9uIGdldEV4cHJlc3Npb25TeW1ib2woXG4gICAgc2NvcGU6IFN5bWJvbFRhYmxlLCBhc3Q6IEFTVCwgcG9zaXRpb246IG51bWJlcixcbiAgICB0ZW1wbGF0ZUluZm86IFRlbXBsYXRlU291cmNlKToge3N5bWJvbDogU3ltYm9sLCBzcGFuOiBTcGFufXx1bmRlZmluZWQge1xuICBjb25zdCBwYXRoID0gZmluZEFzdEF0KGFzdCwgcG9zaXRpb24sIC8qIGV4Y2x1ZGVFbXB0eSAqLyB0cnVlKTtcbiAgaWYgKHBhdGguZW1wdHkpIHJldHVybiB1bmRlZmluZWQ7XG4gIGNvbnN0IHRhaWwgPSBwYXRoLnRhaWwhO1xuXG4gIGZ1bmN0aW9uIGdldFR5cGUoYXN0OiBBU1QpOiBTeW1ib2wge1xuICAgIHJldHVybiBuZXcgQXN0VHlwZShzY29wZSwgdGVtcGxhdGVJbmZvLnF1ZXJ5LCB7fSwgdGVtcGxhdGVJbmZvLnNvdXJjZSkuZ2V0VHlwZShhc3QpO1xuICB9XG5cbiAgZnVuY3Rpb24gc3BhbkZyb21OYW1lKGFzdDogQVNUV2l0aE5hbWUpOiBTcGFuIHtcbiAgICAvLyBgbmFtZVNwYW5gIGlzIGFuIGFic29sdXRlIHNwYW4sIGJ1dCB0aGUgc3BhbiBleHBlY3RlZCBieSB0aGUgcmVzdWx0IG9mIHRoaXMgbWV0aG9kIGlzXG4gICAgLy8gcmVsYXRpdmUgdG8gdGhlIHN0YXJ0IG9mIHRoZSBleHByZXNzaW9uLlxuICAgIC8vIFRPRE8oYXlhemhhZml6KTogbWlncmF0ZSB0byBvbmx5IHVzaW5nIGFic29sdXRlIHNwYW5zXG4gICAgY29uc3Qgb2Zmc2V0ID0gYXN0LnNvdXJjZVNwYW4uc3RhcnQgLSBhc3Quc3Bhbi5zdGFydDtcbiAgICByZXR1cm4ge1xuICAgICAgc3RhcnQ6IGFzdC5uYW1lU3Bhbi5zdGFydCAtIG9mZnNldCxcbiAgICAgIGVuZDogYXN0Lm5hbWVTcGFuLmVuZCAtIG9mZnNldCxcbiAgICB9O1xuICB9XG5cbiAgbGV0IHN5bWJvbDogU3ltYm9sfHVuZGVmaW5lZCA9IHVuZGVmaW5lZDtcbiAgbGV0IHNwYW46IFNwYW58dW5kZWZpbmVkID0gdW5kZWZpbmVkO1xuXG4gIC8vIElmIHRoZSBjb21wbGV0aW9uIHJlcXVlc3QgaXMgaW4gYSBub3QgaW4gYSBwaXBlIG9yIHByb3BlcnR5IGFjY2VzcyB0aGVuIHRoZSBnbG9iYWwgc2NvcGVcbiAgLy8gKHRoYXQgaXMgdGhlIHNjb3BlIG9mIHRoZSBpbXBsaWNpdCByZWNlaXZlcikgaXMgdGhlIHJpZ2h0IHNjb3BlIGFzIHRoZSB1c2VyIGlzIHR5cGluZyB0aGVcbiAgLy8gYmVnaW5uaW5nIG9mIGFuIGV4cHJlc3Npb24uXG4gIHRhaWwudmlzaXQoe1xuICAgIHZpc2l0QmluYXJ5KF9hc3QpIHt9LFxuICAgIHZpc2l0Q2hhaW4oX2FzdCkge30sXG4gICAgdmlzaXRDb25kaXRpb25hbChfYXN0KSB7fSxcbiAgICB2aXNpdEZ1bmN0aW9uQ2FsbChfYXN0KSB7fSxcbiAgICB2aXNpdEltcGxpY2l0UmVjZWl2ZXIoX2FzdCkge30sXG4gICAgdmlzaXRJbnRlcnBvbGF0aW9uKF9hc3QpIHt9LFxuICAgIHZpc2l0S2V5ZWRSZWFkKF9hc3QpIHt9LFxuICAgIHZpc2l0S2V5ZWRXcml0ZShfYXN0KSB7fSxcbiAgICB2aXNpdExpdGVyYWxBcnJheShfYXN0KSB7fSxcbiAgICB2aXNpdExpdGVyYWxNYXAoX2FzdCkge30sXG4gICAgdmlzaXRMaXRlcmFsUHJpbWl0aXZlKF9hc3QpIHt9LFxuICAgIHZpc2l0TWV0aG9kQ2FsbChhc3QpIHtcbiAgICAgIGNvbnN0IHJlY2VpdmVyVHlwZSA9IGdldFR5cGUoYXN0LnJlY2VpdmVyKTtcbiAgICAgIHN5bWJvbCA9IHJlY2VpdmVyVHlwZSAmJiByZWNlaXZlclR5cGUubWVtYmVycygpLmdldChhc3QubmFtZSk7XG4gICAgICBzcGFuID0gc3BhbkZyb21OYW1lKGFzdCk7XG4gICAgfSxcbiAgICB2aXNpdFBpcGUoYXN0KSB7XG4gICAgICBpZiAoaW5TcGFuKHBvc2l0aW9uLCBhc3QubmFtZVNwYW4sIC8qIGV4Y2x1c2l2ZSAqLyB0cnVlKSkge1xuICAgICAgICAvLyBXZSBhcmUgaW4gYSBwb3NpdGlvbiBhIHBpcGUgbmFtZSBpcyBleHBlY3RlZC5cbiAgICAgICAgY29uc3QgcGlwZXMgPSB0ZW1wbGF0ZUluZm8ucXVlcnkuZ2V0UGlwZXMoKTtcbiAgICAgICAgc3ltYm9sID0gcGlwZXMuZ2V0KGFzdC5uYW1lKTtcbiAgICAgICAgc3BhbiA9IHNwYW5Gcm9tTmFtZShhc3QpO1xuICAgICAgfVxuICAgIH0sXG4gICAgdmlzaXRQcmVmaXhOb3QoX2FzdCkge30sXG4gICAgdmlzaXROb25OdWxsQXNzZXJ0KF9hc3QpIHt9LFxuICAgIHZpc2l0UHJvcGVydHlSZWFkKGFzdCkge1xuICAgICAgY29uc3QgcmVjZWl2ZXJUeXBlID0gZ2V0VHlwZShhc3QucmVjZWl2ZXIpO1xuICAgICAgc3ltYm9sID0gcmVjZWl2ZXJUeXBlICYmIHJlY2VpdmVyVHlwZS5tZW1iZXJzKCkuZ2V0KGFzdC5uYW1lKTtcbiAgICAgIHNwYW4gPSBzcGFuRnJvbU5hbWUoYXN0KTtcbiAgICB9LFxuICAgIHZpc2l0UHJvcGVydHlXcml0ZShhc3QpIHtcbiAgICAgIGNvbnN0IHJlY2VpdmVyVHlwZSA9IGdldFR5cGUoYXN0LnJlY2VpdmVyKTtcbiAgICAgIHN5bWJvbCA9IHJlY2VpdmVyVHlwZSAmJiByZWNlaXZlclR5cGUubWVtYmVycygpLmdldChhc3QubmFtZSk7XG4gICAgICBzcGFuID0gc3BhbkZyb21OYW1lKGFzdCk7XG4gICAgfSxcbiAgICB2aXNpdFF1b3RlKF9hc3QpIHt9LFxuICAgIHZpc2l0U2FmZU1ldGhvZENhbGwoYXN0KSB7XG4gICAgICBjb25zdCByZWNlaXZlclR5cGUgPSBnZXRUeXBlKGFzdC5yZWNlaXZlcik7XG4gICAgICBzeW1ib2wgPSByZWNlaXZlclR5cGUgJiYgcmVjZWl2ZXJUeXBlLm1lbWJlcnMoKS5nZXQoYXN0Lm5hbWUpO1xuICAgICAgc3BhbiA9IHNwYW5Gcm9tTmFtZShhc3QpO1xuICAgIH0sXG4gICAgdmlzaXRTYWZlUHJvcGVydHlSZWFkKGFzdCkge1xuICAgICAgY29uc3QgcmVjZWl2ZXJUeXBlID0gZ2V0VHlwZShhc3QucmVjZWl2ZXIpO1xuICAgICAgc3ltYm9sID0gcmVjZWl2ZXJUeXBlICYmIHJlY2VpdmVyVHlwZS5tZW1iZXJzKCkuZ2V0KGFzdC5uYW1lKTtcbiAgICAgIHNwYW4gPSBzcGFuRnJvbU5hbWUoYXN0KTtcbiAgICB9LFxuICB9KTtcblxuICBpZiAoc3ltYm9sICYmIHNwYW4pIHtcbiAgICByZXR1cm4ge3N5bWJvbCwgc3Bhbn07XG4gIH1cbn1cbiJdfQ==