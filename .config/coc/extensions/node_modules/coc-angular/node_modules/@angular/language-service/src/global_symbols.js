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
        define("@angular/language-service/src/global_symbols", ["require", "exports", "@angular/language-service/src/types"], factory);
    }
})(function (require, exports) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    exports.createGlobalSymbolTable = exports.EMPTY_SYMBOL_TABLE = void 0;
    var ng = require("@angular/language-service/src/types");
    exports.EMPTY_SYMBOL_TABLE = {
        size: 0,
        get: function () { return undefined; },
        has: function () { return false; },
        values: function () { return []; },
    };
    /**
     * A factory function that returns a symbol table that contains all global symbols
     * available in an interpolation scope in a template.
     * This function creates the table the first time it is called, and return a cached
     * value for all subsequent calls.
     */
    exports.createGlobalSymbolTable = (function () {
        var GLOBAL_SYMBOL_TABLE;
        return function (query) {
            if (GLOBAL_SYMBOL_TABLE) {
                return GLOBAL_SYMBOL_TABLE;
            }
            GLOBAL_SYMBOL_TABLE = query.createSymbolTable([
                // The `$any()` method casts the type of an expression to `any`.
                // https://angular.io/guide/template-syntax#the-any-type-cast-function
                {
                    name: '$any',
                    kind: 'method',
                    type: {
                        name: '$any',
                        kind: 'method',
                        type: undefined,
                        language: 'typescript',
                        container: undefined,
                        public: true,
                        callable: true,
                        definition: undefined,
                        nullable: false,
                        documentation: [{
                                kind: 'text',
                                text: 'function to cast an expression to the `any` type',
                            }],
                        members: function () { return exports.EMPTY_SYMBOL_TABLE; },
                        signatures: function () { return []; },
                        selectSignature: function (args) {
                            if (args.length !== 1) {
                                return;
                            }
                            return {
                                arguments: exports.EMPTY_SYMBOL_TABLE,
                                result: query.getBuiltinType(ng.BuiltinType.Any),
                            };
                        },
                        indexed: function () { return undefined; },
                        typeArguments: function () { return undefined; },
                    },
                },
            ]);
            return GLOBAL_SYMBOL_TABLE;
        };
    })();
});
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiZ2xvYmFsX3N5bWJvbHMuanMiLCJzb3VyY2VSb290IjoiIiwic291cmNlcyI6WyIuLi8uLi8uLi8uLi8uLi8uLi9wYWNrYWdlcy9sYW5ndWFnZS1zZXJ2aWNlL3NyYy9nbG9iYWxfc3ltYm9scy50cyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiQUFBQTs7Ozs7O0dBTUc7Ozs7Ozs7Ozs7Ozs7SUFFSCx3REFBbUM7SUFFdEIsUUFBQSxrQkFBa0IsR0FBNkI7UUFDMUQsSUFBSSxFQUFFLENBQUM7UUFDUCxHQUFHLEVBQUUsY0FBTSxPQUFBLFNBQVMsRUFBVCxDQUFTO1FBQ3BCLEdBQUcsRUFBRSxjQUFNLE9BQUEsS0FBSyxFQUFMLENBQUs7UUFDaEIsTUFBTSxFQUFFLGNBQU0sT0FBQSxFQUFFLEVBQUYsQ0FBRTtLQUNqQixDQUFDO0lBRUY7Ozs7O09BS0c7SUFDVSxRQUFBLHVCQUF1QixHQUE4QyxDQUFDO1FBQ2pGLElBQUksbUJBQTZDLENBQUM7UUFDbEQsT0FBTyxVQUFTLEtBQXFCO1lBQ25DLElBQUksbUJBQW1CLEVBQUU7Z0JBQ3ZCLE9BQU8sbUJBQW1CLENBQUM7YUFDNUI7WUFDRCxtQkFBbUIsR0FBRyxLQUFLLENBQUMsaUJBQWlCLENBQUM7Z0JBQzVDLGdFQUFnRTtnQkFDaEUsc0VBQXNFO2dCQUN0RTtvQkFDRSxJQUFJLEVBQUUsTUFBTTtvQkFDWixJQUFJLEVBQUUsUUFBUTtvQkFDZCxJQUFJLEVBQUU7d0JBQ0osSUFBSSxFQUFFLE1BQU07d0JBQ1osSUFBSSxFQUFFLFFBQVE7d0JBQ2QsSUFBSSxFQUFFLFNBQVM7d0JBQ2YsUUFBUSxFQUFFLFlBQVk7d0JBQ3RCLFNBQVMsRUFBRSxTQUFTO3dCQUNwQixNQUFNLEVBQUUsSUFBSTt3QkFDWixRQUFRLEVBQUUsSUFBSTt3QkFDZCxVQUFVLEVBQUUsU0FBUzt3QkFDckIsUUFBUSxFQUFFLEtBQUs7d0JBQ2YsYUFBYSxFQUFFLENBQUM7Z0NBQ2QsSUFBSSxFQUFFLE1BQU07Z0NBQ1osSUFBSSxFQUFFLGtEQUFrRDs2QkFDekQsQ0FBQzt3QkFDRixPQUFPLEVBQUUsY0FBTSxPQUFBLDBCQUFrQixFQUFsQixDQUFrQjt3QkFDakMsVUFBVSxFQUFFLGNBQU0sT0FBQSxFQUFFLEVBQUYsQ0FBRTt3QkFDcEIsZUFBZSxFQUFmLFVBQWdCLElBQWlCOzRCQUMvQixJQUFJLElBQUksQ0FBQyxNQUFNLEtBQUssQ0FBQyxFQUFFO2dDQUNyQixPQUFPOzZCQUNSOzRCQUNELE9BQU87Z0NBQ0wsU0FBUyxFQUFFLDBCQUFrQjtnQ0FDN0IsTUFBTSxFQUFFLEtBQUssQ0FBQyxjQUFjLENBQUMsRUFBRSxDQUFDLFdBQVcsQ0FBQyxHQUFHLENBQUM7NkJBQ2pELENBQUM7d0JBQ0osQ0FBQzt3QkFDRCxPQUFPLEVBQUUsY0FBTSxPQUFBLFNBQVMsRUFBVCxDQUFTO3dCQUN4QixhQUFhLEVBQUUsY0FBTSxPQUFBLFNBQVMsRUFBVCxDQUFTO3FCQUMvQjtpQkFDRjthQUNGLENBQUMsQ0FBQztZQUNILE9BQU8sbUJBQW1CLENBQUM7UUFDN0IsQ0FBQyxDQUFDO0lBQ0osQ0FBQyxDQUFDLEVBQUUsQ0FBQyIsInNvdXJjZXNDb250ZW50IjpbIi8qKlxuICogQGxpY2Vuc2VcbiAqIENvcHlyaWdodCBHb29nbGUgTExDIEFsbCBSaWdodHMgUmVzZXJ2ZWQuXG4gKlxuICogVXNlIG9mIHRoaXMgc291cmNlIGNvZGUgaXMgZ292ZXJuZWQgYnkgYW4gTUlULXN0eWxlIGxpY2Vuc2UgdGhhdCBjYW4gYmVcbiAqIGZvdW5kIGluIHRoZSBMSUNFTlNFIGZpbGUgYXQgaHR0cHM6Ly9hbmd1bGFyLmlvL2xpY2Vuc2VcbiAqL1xuXG5pbXBvcnQgKiBhcyBuZyBmcm9tICcuLi9zcmMvdHlwZXMnO1xuXG5leHBvcnQgY29uc3QgRU1QVFlfU1lNQk9MX1RBQkxFOiBSZWFkb25seTxuZy5TeW1ib2xUYWJsZT4gPSB7XG4gIHNpemU6IDAsXG4gIGdldDogKCkgPT4gdW5kZWZpbmVkLFxuICBoYXM6ICgpID0+IGZhbHNlLFxuICB2YWx1ZXM6ICgpID0+IFtdLFxufTtcblxuLyoqXG4gKiBBIGZhY3RvcnkgZnVuY3Rpb24gdGhhdCByZXR1cm5zIGEgc3ltYm9sIHRhYmxlIHRoYXQgY29udGFpbnMgYWxsIGdsb2JhbCBzeW1ib2xzXG4gKiBhdmFpbGFibGUgaW4gYW4gaW50ZXJwb2xhdGlvbiBzY29wZSBpbiBhIHRlbXBsYXRlLlxuICogVGhpcyBmdW5jdGlvbiBjcmVhdGVzIHRoZSB0YWJsZSB0aGUgZmlyc3QgdGltZSBpdCBpcyBjYWxsZWQsIGFuZCByZXR1cm4gYSBjYWNoZWRcbiAqIHZhbHVlIGZvciBhbGwgc3Vic2VxdWVudCBjYWxscy5cbiAqL1xuZXhwb3J0IGNvbnN0IGNyZWF0ZUdsb2JhbFN5bWJvbFRhYmxlOiAocXVlcnk6IG5nLlN5bWJvbFF1ZXJ5KSA9PiBuZy5TeW1ib2xUYWJsZSA9IChmdW5jdGlvbigpIHtcbiAgbGV0IEdMT0JBTF9TWU1CT0xfVEFCTEU6IG5nLlN5bWJvbFRhYmxlfHVuZGVmaW5lZDtcbiAgcmV0dXJuIGZ1bmN0aW9uKHF1ZXJ5OiBuZy5TeW1ib2xRdWVyeSkge1xuICAgIGlmIChHTE9CQUxfU1lNQk9MX1RBQkxFKSB7XG4gICAgICByZXR1cm4gR0xPQkFMX1NZTUJPTF9UQUJMRTtcbiAgICB9XG4gICAgR0xPQkFMX1NZTUJPTF9UQUJMRSA9IHF1ZXJ5LmNyZWF0ZVN5bWJvbFRhYmxlKFtcbiAgICAgIC8vIFRoZSBgJGFueSgpYCBtZXRob2QgY2FzdHMgdGhlIHR5cGUgb2YgYW4gZXhwcmVzc2lvbiB0byBgYW55YC5cbiAgICAgIC8vIGh0dHBzOi8vYW5ndWxhci5pby9ndWlkZS90ZW1wbGF0ZS1zeW50YXgjdGhlLWFueS10eXBlLWNhc3QtZnVuY3Rpb25cbiAgICAgIHtcbiAgICAgICAgbmFtZTogJyRhbnknLFxuICAgICAgICBraW5kOiAnbWV0aG9kJyxcbiAgICAgICAgdHlwZToge1xuICAgICAgICAgIG5hbWU6ICckYW55JyxcbiAgICAgICAgICBraW5kOiAnbWV0aG9kJyxcbiAgICAgICAgICB0eXBlOiB1bmRlZmluZWQsXG4gICAgICAgICAgbGFuZ3VhZ2U6ICd0eXBlc2NyaXB0JyxcbiAgICAgICAgICBjb250YWluZXI6IHVuZGVmaW5lZCxcbiAgICAgICAgICBwdWJsaWM6IHRydWUsXG4gICAgICAgICAgY2FsbGFibGU6IHRydWUsXG4gICAgICAgICAgZGVmaW5pdGlvbjogdW5kZWZpbmVkLFxuICAgICAgICAgIG51bGxhYmxlOiBmYWxzZSxcbiAgICAgICAgICBkb2N1bWVudGF0aW9uOiBbe1xuICAgICAgICAgICAga2luZDogJ3RleHQnLFxuICAgICAgICAgICAgdGV4dDogJ2Z1bmN0aW9uIHRvIGNhc3QgYW4gZXhwcmVzc2lvbiB0byB0aGUgYGFueWAgdHlwZScsXG4gICAgICAgICAgfV0sXG4gICAgICAgICAgbWVtYmVyczogKCkgPT4gRU1QVFlfU1lNQk9MX1RBQkxFLFxuICAgICAgICAgIHNpZ25hdHVyZXM6ICgpID0+IFtdLFxuICAgICAgICAgIHNlbGVjdFNpZ25hdHVyZShhcmdzOiBuZy5TeW1ib2xbXSkge1xuICAgICAgICAgICAgaWYgKGFyZ3MubGVuZ3RoICE9PSAxKSB7XG4gICAgICAgICAgICAgIHJldHVybjtcbiAgICAgICAgICAgIH1cbiAgICAgICAgICAgIHJldHVybiB7XG4gICAgICAgICAgICAgIGFyZ3VtZW50czogRU1QVFlfU1lNQk9MX1RBQkxFLCAgLy8gbm90IHVzZWRcbiAgICAgICAgICAgICAgcmVzdWx0OiBxdWVyeS5nZXRCdWlsdGluVHlwZShuZy5CdWlsdGluVHlwZS5BbnkpLFxuICAgICAgICAgICAgfTtcbiAgICAgICAgICB9LFxuICAgICAgICAgIGluZGV4ZWQ6ICgpID0+IHVuZGVmaW5lZCxcbiAgICAgICAgICB0eXBlQXJndW1lbnRzOiAoKSA9PiB1bmRlZmluZWQsXG4gICAgICAgIH0sXG4gICAgICB9LFxuICAgIF0pO1xuICAgIHJldHVybiBHTE9CQUxfU1lNQk9MX1RBQkxFO1xuICB9O1xufSkoKTtcbiJdfQ==