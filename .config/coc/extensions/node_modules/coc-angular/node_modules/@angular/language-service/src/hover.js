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
        define("@angular/language-service/src/hover", ["require", "exports", "tslib", "typescript", "@angular/language-service/src/locate_symbol", "@angular/language-service/src/utils"], factory);
    }
})(function (require, exports) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    exports.getTsHover = exports.getTemplateHover = void 0;
    var tslib_1 = require("tslib");
    var ts = require("typescript");
    var locate_symbol_1 = require("@angular/language-service/src/locate_symbol");
    var utils_1 = require("@angular/language-service/src/utils");
    // Reverse mappings of enum would generate strings
    var SYMBOL_SPACE = ts.SymbolDisplayPartKind[ts.SymbolDisplayPartKind.space];
    var SYMBOL_PUNC = ts.SymbolDisplayPartKind[ts.SymbolDisplayPartKind.punctuation];
    var SYMBOL_TEXT = ts.SymbolDisplayPartKind[ts.SymbolDisplayPartKind.text];
    var SYMBOL_INTERFACE = ts.SymbolDisplayPartKind[ts.SymbolDisplayPartKind.interfaceName];
    /**
     * Traverse the template AST and look for the symbol located at `position`, then
     * return the corresponding quick info.
     * @param info template AST
     * @param position location of the symbol
     * @param analyzedModules all NgModules in the program.
     */
    function getTemplateHover(info, position, analyzedModules) {
        var _a, _b;
        var symbolInfo = locate_symbol_1.locateSymbols(info, position)[0];
        if (!symbolInfo) {
            return;
        }
        var symbol = symbolInfo.symbol, span = symbolInfo.span, staticSymbol = symbolInfo.staticSymbol;
        // The container is either the symbol's container (for example, 'AppComponent'
        // is the container of the symbol 'title' in its template) or the NgModule
        // that the directive belongs to (the container of AppComponent is AppModule).
        var containerName = (_a = symbol.container) === null || _a === void 0 ? void 0 : _a.name;
        if (!containerName && staticSymbol) {
            // If there is a static symbol then the target is a directive.
            var ngModule = analyzedModules.ngModuleByPipeOrDirective.get(staticSymbol);
            containerName = ngModule === null || ngModule === void 0 ? void 0 : ngModule.type.reference.name;
        }
        return createQuickInfo(symbol.name, symbol.kind, span, containerName, (_b = symbol.type) === null || _b === void 0 ? void 0 : _b.name, symbol.documentation);
    }
    exports.getTemplateHover = getTemplateHover;
    /**
     * Get quick info for Angular semantic entities in TypeScript files, like Directives.
     * @param position location of the symbol in the source file
     * @param declarations All Directive-like declarations in the source file.
     * @param analyzedModules all NgModules in the program.
     */
    function getTsHover(position, declarations, analyzedModules) {
        var e_1, _a;
        try {
            for (var declarations_1 = tslib_1.__values(declarations), declarations_1_1 = declarations_1.next(); !declarations_1_1.done; declarations_1_1 = declarations_1.next()) {
                var _b = declarations_1_1.value, declarationSpan = _b.declarationSpan, metadata = _b.metadata;
                if (utils_1.inSpan(position, declarationSpan)) {
                    var staticSymbol = metadata.type.reference;
                    var directiveName = staticSymbol.name;
                    var kind = metadata.isComponent ? 'component' : 'directive';
                    var textSpan = ts.createTextSpanFromBounds(declarationSpan.start, declarationSpan.end);
                    var ngModule = analyzedModules.ngModuleByPipeOrDirective.get(staticSymbol);
                    var moduleName = ngModule === null || ngModule === void 0 ? void 0 : ngModule.type.reference.name;
                    return createQuickInfo(directiveName, kind, textSpan, moduleName, ts.ScriptElementKind.classElement);
                }
            }
        }
        catch (e_1_1) { e_1 = { error: e_1_1 }; }
        finally {
            try {
                if (declarations_1_1 && !declarations_1_1.done && (_a = declarations_1.return)) _a.call(declarations_1);
            }
            finally { if (e_1) throw e_1.error; }
        }
    }
    exports.getTsHover = getTsHover;
    /**
     * Construct a QuickInfo object taking into account its container and type.
     * @param name Name of the QuickInfo target
     * @param kind component, directive, pipe, etc.
     * @param textSpan span of the target
     * @param containerName either the Symbol's container or the NgModule that contains the directive
     * @param type user-friendly name of the type
     * @param documentation docstring or comment
     */
    function createQuickInfo(name, kind, textSpan, containerName, type, documentation) {
        var containerDisplayParts = containerName ?
            [
                { text: containerName, kind: SYMBOL_INTERFACE },
                { text: '.', kind: SYMBOL_PUNC },
            ] :
            [];
        var typeDisplayParts = type ?
            [
                { text: ':', kind: SYMBOL_PUNC },
                { text: ' ', kind: SYMBOL_SPACE },
                { text: type, kind: SYMBOL_INTERFACE },
            ] :
            [];
        return {
            kind: kind,
            kindModifiers: ts.ScriptElementKindModifier.none,
            textSpan: textSpan,
            displayParts: tslib_1.__spread([
                { text: '(', kind: SYMBOL_PUNC },
                { text: kind, kind: SYMBOL_TEXT },
                { text: ')', kind: SYMBOL_PUNC },
                { text: ' ', kind: SYMBOL_SPACE }
            ], containerDisplayParts, [
                { text: name, kind: SYMBOL_INTERFACE }
            ], typeDisplayParts),
            documentation: documentation,
        };
    }
});
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiaG92ZXIuanMiLCJzb3VyY2VSb290IjoiIiwic291cmNlcyI6WyIuLi8uLi8uLi8uLi8uLi8uLi9wYWNrYWdlcy9sYW5ndWFnZS1zZXJ2aWNlL3NyYy9ob3Zlci50cyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiQUFBQTs7Ozs7O0dBTUc7Ozs7Ozs7Ozs7Ozs7O0lBR0gsK0JBQWlDO0lBQ2pDLDZFQUE4QztJQUU5Qyw2REFBK0I7SUFFL0Isa0RBQWtEO0lBQ2xELElBQU0sWUFBWSxHQUFHLEVBQUUsQ0FBQyxxQkFBcUIsQ0FBQyxFQUFFLENBQUMscUJBQXFCLENBQUMsS0FBSyxDQUFDLENBQUM7SUFDOUUsSUFBTSxXQUFXLEdBQUcsRUFBRSxDQUFDLHFCQUFxQixDQUFDLEVBQUUsQ0FBQyxxQkFBcUIsQ0FBQyxXQUFXLENBQUMsQ0FBQztJQUNuRixJQUFNLFdBQVcsR0FBRyxFQUFFLENBQUMscUJBQXFCLENBQUMsRUFBRSxDQUFDLHFCQUFxQixDQUFDLElBQUksQ0FBQyxDQUFDO0lBQzVFLElBQU0sZ0JBQWdCLEdBQUcsRUFBRSxDQUFDLHFCQUFxQixDQUFDLEVBQUUsQ0FBQyxxQkFBcUIsQ0FBQyxhQUFhLENBQUMsQ0FBQztJQUUxRjs7Ozs7O09BTUc7SUFDSCxTQUFnQixnQkFBZ0IsQ0FDNUIsSUFBa0IsRUFBRSxRQUFnQixFQUFFLGVBQWtDOztRQUUxRSxJQUFNLFVBQVUsR0FBRyw2QkFBYSxDQUFDLElBQUksRUFBRSxRQUFRLENBQUMsQ0FBQyxDQUFDLENBQUMsQ0FBQztRQUNwRCxJQUFJLENBQUMsVUFBVSxFQUFFO1lBQ2YsT0FBTztTQUNSO1FBQ00sSUFBQSxNQUFNLEdBQXdCLFVBQVUsT0FBbEMsRUFBRSxJQUFJLEdBQWtCLFVBQVUsS0FBNUIsRUFBRSxZQUFZLEdBQUksVUFBVSxhQUFkLENBQWU7UUFFaEQsOEVBQThFO1FBQzlFLDBFQUEwRTtRQUMxRSw4RUFBOEU7UUFDOUUsSUFBSSxhQUFhLFNBQXFCLE1BQU0sQ0FBQyxTQUFTLDBDQUFFLElBQUks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LEVBQUM7Z0JBQzlCLEVBQUMsSUFBSSxFQUFFLElBQUksRUFBRSxJQUFJLEVBQUUsV0FBVyxFQUFDO2dCQUMvQixFQUFDLElBQUksRUFBRSxHQUFHLEVBQUUsSUFBSSxFQUFFLFdBQVcsRUFBQztnQkFDOUIsRUFBQyxJQUFJLEVBQUUsR0FBRyxFQUFFLElBQUksRUFBRSxZQUFZLEVBQUM7ZUFDNUIscUJBQXFCO2dCQUN4QixFQUFDLElBQUksRUFBRSxJQUFJLEVBQUUsSUFBSSxFQUFFLGdCQUFnQixFQUFDO2VBQ2pDLGdCQUFnQixDQUNwQjtZQUNELGFBQWEsZUFBQTtTQUNkLENBQUM7SUFDSixDQUFDIiwic291cmNlc0NvbnRlbnQiOlsiLyoqXG4gKiBAbGljZW5zZVxuICogQ29weXJpZ2h0IEdvb2dsZSBMTEMgQWxsIFJpZ2h0cyBSZXNlcnZlZC5cbiAqXG4gKiBVc2Ugb2YgdGhpcyBzb3VyY2UgY29kZSBpcyBnb3Zlcm5lZCBieSBhbiBNSVQtc3R5bGUgbGljZW5zZSB0aGF0IGNhbiBiZVxuICogZm91bmQgaW4gdGhlIExJQ0VOU0UgZmlsZSBhdCBodHRwczovL2FuZ3VsYXIuaW8vbGljZW5zZVxuICovXG5cbmltcG9ydCB7TmdBbmFseXplZE1vZHVsZXN9IGZyb20gJ0Bhbmd1bGFyL2NvbXBpbGVyJztcbmltcG9ydCAqIGFzIHRzIGZyb20gJ3R5cGVzY3JpcHQnO1xuaW1wb3J0IHtsb2NhdGVTeW1ib2xzfSBmcm9tICcuL2xvY2F0ZV9zeW1ib2wnO1xuaW1wb3J0ICogYXMgbmcgZnJvbSAnLi90eXBlcyc7XG5pbXBvcnQge2luU3Bhbn0gZnJvbSAnLi91dGlscyc7XG5cbi8vIFJldmVyc2UgbWFwcGluZ3Mgb2YgZW51bSB3b3VsZCBnZW5lcmF0ZSBzdHJpbmdzXG5jb25zdCBTWU1CT0xfU1BBQ0UgPSB0cy5TeW1ib2xEaXNwbGF5UGFydEtpbmRbdHMuU3ltYm9sRGlzcGxheVBhcnRLaW5kLnNwYWNlXTtcbmNvbnN0IFNZTUJPTF9QVU5DID0gdHMuU3ltYm9sRGlzcGxheVBhcnRLaW5kW3RzLlN5bWJvbERpc3BsYXlQYXJ0S2luZC5wdW5jdHVhdGlvbl07XG5jb25zdCBTWU1CT0xfVEVYVCA9IHRzLlN5bWJvbERpc3BsYXlQYXJ0S2luZFt0cy5TeW1ib2xEaXNwbGF5UGFydEtpbmQudGV4dF07XG5jb25zdCBTWU1CT0xfSU5URVJGQUNFID0gdHMuU3ltYm9sRGlzcGxheVBhcnRLaW5kW3RzLlN5bWJvbERpc3BsYXlQYXJ0S2luZC5pbnRlcmZhY2VOYW1lXTtcblxuLyoqXG4gKiBUcmF2ZXJzZSB0aGUgdGVtcGxhdGUgQVNUIGFuZCBsb29rIGZvciB0aGUgc3ltYm9sIGxvY2F0ZWQgYXQgYHBvc2l0aW9uYCwgdGhlblxuICogcmV0dXJuIHRoZSBjb3JyZXNwb25kaW5nIHF1aWNrIGluZm8uXG4gKiBAcGFyYW0gaW5mbyB0ZW1wbGF0ZSBBU1RcbiAqIEBwYXJhbSBwb3NpdGlvbiBsb2NhdGlvbiBvZiB0aGUgc3ltYm9sXG4gKiBAcGFyYW0gYW5hbHl6ZWRNb2R1bGVzIGFsbCBOZ01vZHVsZXMgaW4gdGhlIHByb2dyYW0uXG4gKi9cbmV4cG9ydCBmdW5jdGlvbiBnZXRUZW1wbGF0ZUhvdmVyKFxuICAgIGluZm86IG5nLkFzdFJlc3VsdCwgcG9zaXRpb246IG51bWJlciwgYW5hbHl6ZWRNb2R1bGVzOiBOZ0FuYWx5emVkTW9kdWxlcyk6IHRzLlF1aWNrSW5mb3xcbiAgICB1bmRlZmluZWQge1xuICBjb25zdCBzeW1ib2xJbmZvID0gbG9jYXRlU3ltYm9scyhpbmZvLCBwb3NpdGlvbilbMF07XG4gIGlmICghc3ltYm9sSW5mbykge1xuICAgIHJldHVybjtcbiAgfVxuICBjb25zdCB7c3ltYm9sLCBzcGFuLCBzdGF0aWNTeW1ib2x9ID0gc3ltYm9sSW5mbztcblxuICAvLyBUaGUgY29udGFpbmVyIGlzIGVpdGhlciB0aGUgc3ltYm9sJ3MgY29udGFpbmVyIChmb3IgZXhhbXBsZSwgJ0FwcENvbXBvbmVudCdcbiAgLy8gaXMgdGhlIGNvbnRhaW5lciBvZiB0aGUgc3ltYm9sICd0aXRsZScgaW4gaXRzIHRlbXBsYXRlKSBvciB0aGUgTmdNb2R1bGVcbiAgLy8gdGhhdCB0aGUgZGlyZWN0aXZlIGJlbG9uZ3MgdG8gKHRoZSBjb250YWluZXIgb2YgQXBwQ29tcG9uZW50IGlzIEFwcE1vZHVsZSkuXG4gIGxldCBjb250YWluZXJOYW1lOiBzdHJpbmd8dW5kZWZpbmVkID0gc3ltYm9sLmNvbnRhaW5lcj8ubmFtZTtcbiAgaWYgKCFjb250YWluZXJOYW1lICYmIHN0YXRpY1N5bWJvbCkge1xuICAgIC8vIElmIHRoZXJlIGlzIGEgc3RhdGljIHN5bWJvbCB0aGVuIHRoZSB0YXJnZXQgaXMgYSBkaXJlY3RpdmUuXG4gICAgY29uc3QgbmdNb2R1bGUgPSBhbmFseXplZE1vZHVsZXMubmdNb2R1bGVCeVBpcGVPckRpcmVjdGl2ZS5nZXQoc3RhdGljU3ltYm9sKTtcbiAgICBjb250YWluZXJOYW1lID0gbmdNb2R1bGU/LnR5cGUucmVmZXJlbmNlLm5hbWU7XG4gIH1cblxuICByZXR1cm4gY3JlYXRlUXVpY2tJbmZvKFxuICAgICAgc3ltYm9sLm5hbWUsIHN5bWJvbC5raW5kLCBzcGFuLCBjb250YWluZXJOYW1lLCBzeW1ib2wudHlwZT8ubmFtZSwgc3ltYm9sLmRvY3VtZW50YXRpb24pO1xufVxuXG4vKipcbiAqIEdldCBxdWljayBpbmZvIGZvciBBbmd1bGFyIHNlbWFudGljIGVudGl0aWVzIGluIFR5cGVTY3JpcHQgZmlsZXMsIGxpa2UgRGlyZWN0aXZlcy5cbiAqIEBwYXJhbSBwb3NpdGlvbiBsb2NhdGlvbiBvZiB0aGUgc3ltYm9sIGluIHRoZSBzb3VyY2UgZmlsZVxuICogQHBhcmFtIGRlY2xhcmF0aW9ucyBBbGwgRGlyZWN0aXZlLWxpa2UgZGVjbGFyYXRpb25zIGluIHRoZSBzb3VyY2UgZmlsZS5cbiAqIEBwYXJhbSBhbmFseXplZE1vZHVsZXMgYWxsIE5nTW9kdWxlcyBpbiB0aGUgcHJvZ3JhbS5cbiAqL1xuZXhwb3J0IGZ1bmN0aW9uIGdldFRzSG92ZXIoXG4gICAgcG9zaXRpb246IG51bWJlciwgZGVjbGFyYXRpb25zOiBuZy5EZWNsYXJhdGlvbltdLFxuICAgIGFuYWx5emVkTW9kdWxlczogTmdBbmFseXplZE1vZHVsZXMpOiB0cy5RdWlja0luZm98dW5kZWZpbmVkIHtcbiAgZm9yIChjb25zdCB7ZGVjbGFyYXRpb25TcGFuLCBtZXRhZGF0YX0gb2YgZGVjbGFyYXRpb25zKSB7XG4gICAgaWYgKGluU3Bhbihwb3NpdGlvbiwgZGVjbGFyYXRpb25TcGFuKSkge1xuICAgICAgY29uc3Qgc3RhdGljU3ltYm9sOiBuZy5TdGF0aWNTeW1ib2wgPSBtZXRhZGF0YS50eXBlLnJlZmVyZW5jZTtcbiAgICAgIGNvbnN0IGRpcmVjdGl2ZU5hbWUgPSBzdGF0aWNTeW1ib2wubmFtZTtcbiAgICAgIGNvbnN0IGtpbmQgPSBtZXRhZGF0YS5pc0NvbXBvbmVudCA/ICdjb21wb25lbnQnIDogJ2RpcmVjdGl2ZSc7XG4gICAgICBjb25zdCB0ZXh0U3BhbiA9IHRzLmNyZWF0ZVRleHRTcGFuRnJvbUJvdW5kcyhkZWNsYXJhdGlvblNwYW4uc3RhcnQsIGRlY2xhcmF0aW9uU3Bhbi5lbmQpO1xuICAgICAgY29uc3QgbmdNb2R1bGUgPSBhbmFseXplZE1vZHVsZXMubmdNb2R1bGVCeVBpcGVPckRpcmVjdGl2ZS5nZXQoc3RhdGljU3ltYm9sKTtcbiAgICAgIGNvbnN0IG1vZHVsZU5hbWUgPSBuZ01vZHVsZT8udHlwZS5yZWZlcmVuY2UubmFtZTtcbiAgICAgIHJldHVybiBjcmVhdGVRdWlja0luZm8oXG4gICAgICAgICAgZGlyZWN0aXZlTmFtZSwga2luZCwgdGV4dFNwYW4sIG1vZHVsZU5hbWUsIHRzLlNjcmlwdEVsZW1lbnRLaW5kLmNsYXNzRWxlbWVudCk7XG4gICAgfVxuICB9XG59XG5cbi8qKlxuICogQ29uc3RydWN0IGEgUXVpY2tJbmZvIG9iamVjdCB0YWtpbmcgaW50byBhY2NvdW50IGl0cyBjb250YWluZXIgYW5kIHR5cGUuXG4gKiBAcGFyYW0gbmFtZSBOYW1lIG9mIHRoZSBRdWlja0luZm8gdGFyZ2V0XG4gKiBAcGFyYW0ga2luZCBjb21wb25lbnQsIGRpcmVjdGl2ZSwgcGlwZSwgZXRjLlxuICogQHBhcmFtIHRleHRTcGFuIHNwYW4gb2YgdGhlIHRhcmdldFxuICogQHBhcmFtIGNvbnRhaW5lck5hbWUgZWl0aGVyIHRoZSBTeW1ib2wncyBjb250YWluZXIgb3IgdGhlIE5nTW9kdWxlIHRoYXQgY29udGFpbnMgdGhlIGRpcmVjdGl2ZVxuICogQHBhcmFtIHR5cGUgdXNlci1mcmllbmRseSBuYW1lIG9mIHRoZSB0eXBlXG4gKiBAcGFyYW0gZG9jdW1lbnRhdGlvbiBkb2NzdHJpbmcgb3IgY29tbWVudFxuICovXG5mdW5jdGlvbiBjcmVhdGVRdWlja0luZm8oXG4gICAgbmFtZTogc3RyaW5nLCBraW5kOiBzdHJpbmcsIHRleHRTcGFuOiB0cy5UZXh0U3BhbiwgY29udGFpbmVyTmFtZT86IHN0cmluZywgdHlwZT86IHN0cmluZyxcbiAgICBkb2N1bWVudGF0aW9uPzogdHMuU3ltYm9sRGlzcGxheVBhcnRbXSk6IHRzLlF1aWNrSW5mbyB7XG4gIGNvbnN0IGNvbnRhaW5lckRpc3BsYXlQYXJ0cyA9IGNvbnRhaW5lck5hbWUgP1xuICAgICAgW1xuICAgICAgICB7dGV4dDogY29udGFpbmVyTmFtZSwga2luZDogU1lNQk9MX0lOVEVSRkFDRX0sXG4gICAgICAgIHt0ZXh0OiAnLicsIGtpbmQ6IFNZTUJPTF9QVU5DfSxcbiAgICAgIF0gOlxuICAgICAgW107XG5cbiAgY29uc3QgdHlwZURpc3BsYXlQYXJ0cyA9IHR5cGUgP1xuICAgICAgW1xuICAgICAgICB7dGV4dDogJzonLCBraW5kOiBTWU1CT0xfUFVOQ30sXG4gICAgICAgIHt0ZXh0OiAnICcsIGtpbmQ6IFNZTUJPTF9TUEFDRX0sXG4gICAgICAgIHt0ZXh0OiB0eXBlLCBraW5kOiBTWU1CT0xfSU5URVJGQUNFfSxcbiAgICAgIF0gOlxuICAgICAgW107XG5cbiAgcmV0dXJuIHtcbiAgICBraW5kOiBraW5kIGFzIHRzLlNjcmlwdEVsZW1lbnRLaW5kLFxuICAgIGtpbmRNb2RpZmllcnM6IHRzLlNjcmlwdEVsZW1lbnRLaW5kTW9kaWZpZXIubm9uZSxcbiAgICB0ZXh0U3BhbjogdGV4dFNwYW4sXG4gICAgZGlzcGxheVBhcnRzOiBbXG4gICAgICB7dGV4dDogJygnLCBraW5kOiBTWU1CT0xfUFVOQ30sXG4gICAgICB7dGV4dDoga2luZCwga2luZDogU1lNQk9MX1RFWFR9LFxuICAgICAge3RleHQ6ICcpJywga2luZDogU1lNQk9MX1BVTkN9LFxuICAgICAge3RleHQ6ICcgJywga2luZDogU1lNQk9MX1NQQUNFfSxcbiAgICAgIC4uLmNvbnRhaW5lckRpc3BsYXlQYXJ0cyxcbiAgICAgIHt0ZXh0OiBuYW1lLCBraW5kOiBTWU1CT0xfSU5URVJGQUNFfSxcbiAgICAgIC4uLnR5cGVEaXNwbGF5UGFydHMsXG4gICAgXSxcbiAgICBkb2N1bWVudGF0aW9uLFxuICB9O1xufVxuIl19