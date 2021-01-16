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
        define("@angular/language-service/src/typescript_symbols", ["require", "exports", "tslib", "path", "typescript", "@angular/language-service/src/symbols"], factory);
    }
})(function (require, exports) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    exports.getPipesTable = exports.getClassMembersFromDeclaration = exports.getClassMembers = exports.getSymbolQuery = void 0;
    var tslib_1 = require("tslib");
    var path = require("path");
    var ts = require("typescript");
    var symbols_1 = require("@angular/language-service/src/symbols");
    // In TypeScript 2.1 these flags moved
    // These helpers work for both 2.0 and 2.1.
    var isPrivate = ts.ModifierFlags ?
        (function (node) {
            return !!(ts.getCombinedModifierFlags(node) & ts.ModifierFlags.Private);
        }) :
        (function (node) { return !!(node.flags & ts.NodeFlags.Private); });
    var isReferenceType = ts.ObjectFlags ?
        (function (type) {
            return !!(type.flags & ts.TypeFlags.Object &&
                type.objectFlags & ts.ObjectFlags.Reference);
        }) :
        (function (type) { return !!(type.flags & ts.TypeFlags.Reference); });
    function getSymbolQuery(program, checker, source, fetchPipes) {
        return new TypeScriptSymbolQuery(program, checker, source, fetchPipes);
    }
    exports.getSymbolQuery = getSymbolQuery;
    function getClassMembers(program, checker, staticSymbol) {
        var declaration = getClassFromStaticSymbol(program, staticSymbol);
        if (declaration) {
            var type = checker.getTypeAtLocation(declaration);
            var node = program.getSourceFile(staticSymbol.filePath);
            if (node) {
                return new TypeWrapper(type, { node: node, program: program, checker: checker }).members();
            }
        }
    }
    exports.getClassMembers = getClassMembers;
    function getClassMembersFromDeclaration(program, checker, source, declaration) {
        var type = checker.getTypeAtLocation(declaration);
        return new TypeWrapper(type, { node: source, program: program, checker: checker }).members();
    }
    exports.getClassMembersFromDeclaration = getClassMembersFromDeclaration;
    function getPipesTable(source, program, checker, pipes) {
        return new PipesTable(pipes, { program: program, checker: checker, node: source });
    }
    exports.getPipesTable = getPipesTable;
    function getClassFromStaticSymbol(program, type) {
        var source = program.getSourceFile(type.filePath);
        if (source) {
            return ts.forEachChild(source, function (child) {
                if (child.kind === ts.SyntaxKind.ClassDeclaration) {
                    var classDeclaration = child;
                    if (classDeclaration.name != null && classDeclaration.name.text === type.name) {
                        return classDeclaration;
                    }
                }
            });
        }
        return undefined;
    }
    var TypeScriptSymbolQuery = /** @class */ (function () {
        function TypeScriptSymbolQuery(program, checker, source, fetchPipes) {
            this.program = program;
            this.checker = checker;
            this.source = source;
            this.fetchPipes = fetchPipes;
            this.typeCache = new Map();
        }
        TypeScriptSymbolQuery.prototype.getTypeKind = function (symbol) {
            var type = symbol instanceof TypeWrapper ? symbol.tsType : undefined;
            return typeKindOf(type);
        };
        TypeScriptSymbolQuery.prototype.getBuiltinType = function (kind) {
            var result = this.typeCache.get(kind);
            if (!result) {
                var type = getTsTypeFromBuiltinType(kind, {
                    checker: this.checker,
                    node: this.source,
                    program: this.program,
                });
                result =
                    new TypeWrapper(type, { program: this.program, checker: this.checker, node: this.source });
                this.typeCache.set(kind, result);
            }
            return result;
        };
        TypeScriptSymbolQuery.prototype.getTypeUnion = function () {
            var types = [];
            for (var _i = 0; _i < arguments.length; _i++) {
                types[_i] = arguments[_i];
            }
            // No API exists so return any if the types are not all the same type.
            var result = undefined;
            if (types.length) {
                result = types[0];
                for (var i = 1; i < types.length; i++) {
                    if (types[i] != result) {
                        result = undefined;
                        break;
                    }
                }
            }
            return result || this.getBuiltinType(symbols_1.BuiltinType.Any);
        };
        TypeScriptSymbolQuery.prototype.getArrayType = function (_type) {
            return this.getBuiltinType(symbols_1.BuiltinType.Any);
        };
        TypeScriptSymbolQuery.prototype.getElementType = function (type) {
            if (type instanceof TypeWrapper) {
                var ty = type.tsType;
                var tyArgs = type.typeArguments();
                // TODO(ayazhafiz): Track https://github.com/microsoft/TypeScript/issues/37711 to expose
                // `isArrayLikeType` as a public method.
                if (!this.checker.isArrayLikeType(ty) || (tyArgs === null || tyArgs === void 0 ? void 0 : tyArgs.length) !== 1)
                    return;
                return tyArgs[0];
            }
        };
        TypeScriptSymbolQuery.prototype.getNonNullableType = function (symbol) {
            if (symbol instanceof TypeWrapper && (typeof this.checker.getNonNullableType == 'function')) {
                var tsType = symbol.tsType;
                var nonNullableType = this.checker.getNonNullableType(tsType);
                if (nonNullableType != tsType) {
                    return new TypeWrapper(nonNullableType, symbol.context);
                }
                else if (nonNullableType == tsType) {
                    return symbol;
                }
            }
            return this.getBuiltinType(symbols_1.BuiltinType.Any);
        };
        TypeScriptSymbolQuery.prototype.getPipes = function () {
            var result = this.pipesCache;
            if (!result) {
                result = this.pipesCache = this.fetchPipes();
            }
            return result;
        };
        TypeScriptSymbolQuery.prototype.getTemplateContext = function (type) {
            var context = { node: this.source, program: this.program, checker: this.checker };
            var typeSymbol = findClassSymbolInContext(type, context);
            if (typeSymbol) {
                var contextType = this.getTemplateRefContextType(typeSymbol, context);
                if (contextType)
                    return contextType.members();
            }
        };
        TypeScriptSymbolQuery.prototype.getTypeSymbol = function (type) {
            var context = { node: this.source, program: this.program, checker: this.checker };
            var typeSymbol = findClassSymbolInContext(type, context);
            return typeSymbol && new SymbolWrapper(typeSymbol, context);
        };
        TypeScriptSymbolQuery.prototype.createSymbolTable = function (symbols) {
            var result = new MapSymbolTable();
            result.addAll(symbols.map(function (s) { return new DeclaredSymbol(s); }));
            return result;
        };
        TypeScriptSymbolQuery.prototype.mergeSymbolTable = function (symbolTables) {
            var e_1, _a;
            var result = new MapSymbolTable();
            try {
                for (var symbolTables_1 = tslib_1.__values(symbolTables), symbolTables_1_1 = symbolTables_1.next(); !symbolTables_1_1.done; symbolTables_1_1 = symbolTables_1.next()) {
                    var symbolTable = symbolTables_1_1.value;
                    result.addAll(symbolTable.values());
                }
            }
            catch (e_1_1) { e_1 = { error: e_1_1 }; }
            finally {
                try {
                    if (symbolTables_1_1 && !symbolTables_1_1.done && (_a = symbolTables_1.return)) _a.call(symbolTables_1);
                }
                finally { if (e_1) throw e_1.error; }
            }
            return result;
        };
        TypeScriptSymbolQuery.prototype.getSpanAt = function (line, column) {
            return spanAt(this.source, line, column);
        };
        TypeScriptSymbolQuery.prototype.getTemplateRefContextType = function (typeSymbol, context) {
            var e_2, _a;
            var type = this.checker.getTypeOfSymbolAtLocation(typeSymbol, this.source);
            var constructor = type.symbol && type.symbol.members &&
                getFromSymbolTable(type.symbol.members, '__constructor');
            if (constructor) {
                var constructorDeclaration = constructor.declarations[0];
                try {
                    for (var _b = tslib_1.__values(constructorDeclaration.parameters), _c = _b.next(); !_c.done; _c = _b.next()) {
                        var parameter = _c.value;
                        var type_1 = this.checker.getTypeAtLocation(parameter.type);
                        if (type_1.symbol.name == 'TemplateRef' && isReferenceType(type_1)) {
                            var typeWrapper = new TypeWrapper(type_1, context);
                            var typeArguments = typeWrapper.typeArguments();
                            if (typeArguments && typeArguments.length === 1) {
                                return typeArguments[0];
                            }
                        }
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
        };
        return TypeScriptSymbolQuery;
    }());
    function typeCallable(type) {
        var signatures = type.getCallSignatures();
        return signatures && signatures.length != 0;
    }
    function signaturesOf(type, context) {
        return type.getCallSignatures().map(function (s) { return new SignatureWrapper(s, context); });
    }
    function selectSignature(type, context, types) {
        // TODO: Do a better job of selecting the right signature. TypeScript does not currently support a
        // Type Relationship API (see https://github.com/angular/vscode-ng-language-service/issues/143).
        // Consider creating a TypeCheckBlock host in the language service that may also act as a
        // scratchpad for type comparisons.
        var signatures = type.getCallSignatures();
        var passedInTypes = types.map(function (type) {
            if (type instanceof TypeWrapper) {
                return type.tsType;
            }
        });
        // Try to select a matching signature in which all parameter types match.
        // Note that this is just a best-effort approach, because we're checking for
        // strict type equality rather than compatibility.
        // For example, if the signature contains a ReadonlyArray<number> and the
        // passed parameter type is an Array<number>, this will fail.
        function allParameterTypesMatch(signature) {
            var tc = context.checker;
            return signature.getParameters().every(function (parameter, i) {
                var type = tc.getTypeOfSymbolAtLocation(parameter, parameter.valueDeclaration);
                return type === passedInTypes[i];
            });
        }
        var exactMatch = signatures.find(allParameterTypesMatch);
        if (exactMatch) {
            return new SignatureWrapper(exactMatch, context);
        }
        // If not, fallback to a naive selection
        return signatures.length ? new SignatureWrapper(signatures[0], context) : undefined;
    }
    var TypeWrapper = /** @class */ (function () {
        function TypeWrapper(tsType, context) {
            this.tsType = tsType;
            this.context = context;
            this.kind = 'type';
            this.language = 'typescript';
            this.type = undefined;
            this.container = undefined;
            this.public = true;
            if (!tsType) {
                throw Error('Internal: null type');
            }
        }
        Object.defineProperty(TypeWrapper.prototype, "name", {
            get: function () {
                return this.context.checker.typeToString(this.tsType);
            },
            enumerable: false,
            configurable: true
        });
        Object.defineProperty(TypeWrapper.prototype, "callable", {
            get: function () {
                return typeCallable(this.tsType);
            },
            enumerable: false,
            configurable: true
        });
        Object.defineProperty(TypeWrapper.prototype, "nullable", {
            get: function () {
                return this.context.checker.getNonNullableType(this.tsType) != this.tsType;
            },
            enumerable: false,
            configurable: true
        });
        Object.defineProperty(TypeWrapper.prototype, "documentation", {
            get: function () {
                var symbol = this.tsType.getSymbol();
                if (!symbol) {
                    return [];
                }
                return symbol.getDocumentationComment(this.context.checker);
            },
            enumerable: false,
            configurable: true
        });
        Object.defineProperty(TypeWrapper.prototype, "definition", {
            get: function () {
                var symbol = this.tsType.getSymbol();
                return symbol ? definitionFromTsSymbol(symbol) : undefined;
            },
            enumerable: false,
            configurable: true
        });
        TypeWrapper.prototype.members = function () {
            // Should call getApparentProperties() instead of getProperties() because
            // the former includes properties on the base class whereas the latter does
            // not. This provides properties like .bind(), .call(), .apply(), etc for
            // functions.
            return new SymbolTableWrapper(this.tsType.getApparentProperties(), this.context, this.tsType);
        };
        TypeWrapper.prototype.signatures = function () {
            return signaturesOf(this.tsType, this.context);
        };
        TypeWrapper.prototype.selectSignature = function (types) {
            return selectSignature(this.tsType, this.context, types);
        };
        TypeWrapper.prototype.indexed = function (type, value) {
            if (!(type instanceof TypeWrapper))
                return;
            var typeKind = typeKindOf(type.tsType);
            switch (typeKind) {
                case symbols_1.BuiltinType.Number:
                    var nType = this.tsType.getNumberIndexType();
                    if (nType) {
                        // get the right tuple type by value, like 'var t: [number, string];'
                        if (nType.isUnion()) {
                            // return undefined if array index out of bound.
                            return nType.types[value] && new TypeWrapper(nType.types[value], this.context);
                        }
                        return new TypeWrapper(nType, this.context);
                    }
                    return undefined;
                case symbols_1.BuiltinType.String:
                    var sType = this.tsType.getStringIndexType();
                    return sType && new TypeWrapper(sType, this.context);
            }
        };
        TypeWrapper.prototype.typeArguments = function () {
            var _this = this;
            if (!isReferenceType(this.tsType))
                return;
            var typeReference = this.tsType;
            var typeArguments;
            typeArguments = this.context.checker.getTypeArguments(typeReference);
            if (!typeArguments)
                return undefined;
            return typeArguments.map(function (ta) { return new TypeWrapper(ta, _this.context); });
        };
        return TypeWrapper;
    }());
    // If stringIndexType a primitive type(e.g. 'string'), the Symbol is undefined;
    // and in AstType.resolvePropertyRead method, the Symbol.type should get the right type.
    var StringIndexTypeWrapper = /** @class */ (function (_super) {
        tslib_1.__extends(StringIndexTypeWrapper, _super);
        function StringIndexTypeWrapper() {
            var _this = _super !== null && _super.apply(this, arguments) || this;
            _this.type = new TypeWrapper(_this.tsType, _this.context);
            return _this;
        }
        return StringIndexTypeWrapper;
    }(TypeWrapper));
    var SymbolWrapper = /** @class */ (function () {
        function SymbolWrapper(symbol, 
        /** TypeScript type context of the symbol. */
        context, 
        /**
         * Type of the TypeScript symbol, if known. If not provided, the type of the symbol
         * will be determined dynamically; see `SymbolWrapper#tsType`.
         */
        _tsType) {
            this.context = context;
            this._tsType = _tsType;
            this.nullable = false;
            this.language = 'typescript';
            this.symbol = symbol && context && (symbol.flags & ts.SymbolFlags.Alias) ?
                context.checker.getAliasedSymbol(symbol) :
                symbol;
        }
        Object.defineProperty(SymbolWrapper.prototype, "name", {
            get: function () {
                return this.symbol.name;
            },
            enumerable: false,
            configurable: true
        });
        Object.defineProperty(SymbolWrapper.prototype, "kind", {
            get: function () {
                return this.callable ? 'method' : 'property';
            },
            enumerable: false,
            configurable: true
        });
        Object.defineProperty(SymbolWrapper.prototype, "type", {
            get: function () {
                return new TypeWrapper(this.tsType, this.context);
            },
            enumerable: false,
            configurable: true
        });
        Object.defineProperty(SymbolWrapper.prototype, "container", {
            get: function () {
                return getContainerOf(this.symbol, this.context);
            },
            enumerable: false,
            configurable: true
        });
        Object.defineProperty(SymbolWrapper.prototype, "public", {
            get: function () {
                // Symbols that are not explicitly made private are public.
                return !isSymbolPrivate(this.symbol);
            },
            enumerable: false,
            configurable: true
        });
        Object.defineProperty(SymbolWrapper.prototype, "callable", {
            get: function () {
                return typeCallable(this.tsType);
            },
            enumerable: false,
            configurable: true
        });
        Object.defineProperty(SymbolWrapper.prototype, "definition", {
            get: function () {
                return definitionFromTsSymbol(this.symbol);
            },
            enumerable: false,
            configurable: true
        });
        Object.defineProperty(SymbolWrapper.prototype, "documentation", {
            get: function () {
                return this.symbol.getDocumentationComment(this.context.checker);
            },
            enumerable: false,
            configurable: true
        });
        SymbolWrapper.prototype.members = function () {
            if (!this._members) {
                if ((this.symbol.flags & (ts.SymbolFlags.Class | ts.SymbolFlags.Interface)) != 0) {
                    var declaredType = this.context.checker.getDeclaredTypeOfSymbol(this.symbol);
                    var typeWrapper = new TypeWrapper(declaredType, this.context);
                    this._members = typeWrapper.members();
                }
                else {
                    this._members = new SymbolTableWrapper(this.symbol.members, this.context, this.tsType);
                }
            }
            return this._members;
        };
        SymbolWrapper.prototype.signatures = function () {
            return signaturesOf(this.tsType, this.context);
        };
        SymbolWrapper.prototype.selectSignature = function (types) {
            return selectSignature(this.tsType, this.context, types);
        };
        SymbolWrapper.prototype.indexed = function (_argument) {
            return undefined;
        };
        SymbolWrapper.prototype.typeArguments = function () {
            return this.type.typeArguments();
        };
        Object.defineProperty(SymbolWrapper.prototype, "tsType", {
            get: function () {
                var type = this._tsType;
                if (!type) {
                    type = this._tsType =
                        this.context.checker.getTypeOfSymbolAtLocation(this.symbol, this.context.node);
                }
                return type;
            },
            enumerable: false,
            configurable: true
        });
        return SymbolWrapper;
    }());
    var DeclaredSymbol = /** @class */ (function () {
        function DeclaredSymbol(declaration) {
            this.declaration = declaration;
            this.language = 'ng-template';
            this.nullable = false;
            this.public = true;
        }
        Object.defineProperty(DeclaredSymbol.prototype, "name", {
            get: function () {
                return this.declaration.name;
            },
            enumerable: false,
            configurable: true
        });
        Object.defineProperty(DeclaredSymbol.prototype, "kind", {
            get: function () {
                return this.declaration.kind;
            },
            enumerable: false,
            configurable: true
        });
        Object.defineProperty(DeclaredSymbol.prototype, "container", {
            get: function () {
                return undefined;
            },
            enumerable: false,
            configurable: true
        });
        Object.defineProperty(DeclaredSymbol.prototype, "type", {
            get: function () {
                return this.declaration.type;
            },
            enumerable: false,
            configurable: true
        });
        Object.defineProperty(DeclaredSymbol.prototype, "callable", {
            get: function () {
                return this.type.callable;
            },
            enumerable: false,
            configurable: true
        });
        Object.defineProperty(DeclaredSymbol.prototype, "definition", {
            get: function () {
                return this.declaration.definition;
            },
            enumerable: false,
            configurable: true
        });
        Object.defineProperty(DeclaredSymbol.prototype, "documentation", {
            get: function () {
                return this.declaration.type.documentation;
            },
            enumerable: false,
            configurable: true
        });
        DeclaredSymbol.prototype.members = function () {
            return this.type.members();
        };
        DeclaredSymbol.prototype.signatures = function () {
            return this.type.signatures();
        };
        DeclaredSymbol.prototype.selectSignature = function (types) {
            return this.type.selectSignature(types);
        };
        DeclaredSymbol.prototype.typeArguments = function () {
            return this.type.typeArguments();
        };
        DeclaredSymbol.prototype.indexed = function (_argument) {
            return undefined;
        };
        return DeclaredSymbol;
    }());
    var SignatureWrapper = /** @class */ (function () {
        function SignatureWrapper(signature, context) {
            this.signature = signature;
            this.context = context;
        }
        Object.defineProperty(SignatureWrapper.prototype, "arguments", {
            get: function () {
                return new SymbolTableWrapper(this.signature.getParameters(), this.context);
            },
            enumerable: false,
            configurable: true
        });
        Object.defineProperty(SignatureWrapper.prototype, "result", {
            get: function () {
                return new TypeWrapper(this.signature.getReturnType(), this.context);
            },
            enumerable: false,
            configurable: true
        });
        return SignatureWrapper;
    }());
    var SignatureResultOverride = /** @class */ (function () {
        function SignatureResultOverride(signature, resultType) {
            this.signature = signature;
            this.resultType = resultType;
        }
        Object.defineProperty(SignatureResultOverride.prototype, "arguments", {
            get: function () {
                return this.signature.arguments;
            },
            enumerable: false,
            configurable: true
        });
        Object.defineProperty(SignatureResultOverride.prototype, "result", {
            get: function () {
                return this.resultType;
            },
            enumerable: false,
            configurable: true
        });
        return SignatureResultOverride;
    }());
    function toSymbolTableFactory(symbols) {
        var e_3, _a;
        // ∀ Typescript version >= 2.2, `SymbolTable` is implemented as an ES6 `Map`
        var result = new Map();
        try {
            for (var symbols_2 = tslib_1.__values(symbols), symbols_2_1 = symbols_2.next(); !symbols_2_1.done; symbols_2_1 = symbols_2.next()) {
                var symbol = symbols_2_1.value;
                result.set(symbol.name, symbol);
            }
        }
        catch (e_3_1) { e_3 = { error: e_3_1 }; }
        finally {
            try {
                if (symbols_2_1 && !symbols_2_1.done && (_a = symbols_2.return)) _a.call(symbols_2);
            }
            finally { if (e_3) throw e_3.error; }
        }
        return result;
    }
    function toSymbols(symbolTable) {
        if (!symbolTable)
            return [];
        var table = symbolTable;
        if (typeof table.values === 'function') {
            return Array.from(table.values());
        }
        var result = [];
        var own = typeof table.hasOwnProperty === 'function' ?
            function (name) { return table.hasOwnProperty(name); } :
            function (name) { return !!table[name]; };
        for (var name_1 in table) {
            if (own(name_1)) {
                result.push(table[name_1]);
            }
        }
        return result;
    }
    var SymbolTableWrapper = /** @class */ (function () {
        /**
         * Creates a queryable table of symbols belonging to a TypeScript entity.
         * @param symbols symbols to query belonging to the entity
         * @param context program context
         * @param type original TypeScript type of entity owning the symbols, if known
         */
        function SymbolTableWrapper(symbols, context, type) {
            this.context = context;
            symbols = symbols || [];
            if (Array.isArray(symbols)) {
                this.symbols = symbols;
                this.symbolTable = toSymbolTableFactory(symbols);
            }
            else {
                this.symbols = toSymbols(symbols);
                this.symbolTable = symbols;
            }
            if (type) {
                this.stringIndexType = type.getStringIndexType();
            }
        }
        Object.defineProperty(SymbolTableWrapper.prototype, "size", {
            get: function () {
                return this.symbols.length;
            },
            enumerable: false,
            configurable: true
        });
        SymbolTableWrapper.prototype.get = function (key) {
            var symbol = getFromSymbolTable(this.symbolTable, key);
            if (symbol) {
                return new SymbolWrapper(symbol, this.context);
            }
            if (this.stringIndexType) {
                // If the key does not exist as an explicit symbol on the type, it may be accessing a string
                // index signature using dot notation:
                //
                //   const obj<T>: { [key: string]: T };
                //   obj.stringIndex // equivalent to obj['stringIndex'];
                //
                // In this case, return the type indexed by an arbitrary string key.
                return new StringIndexTypeWrapper(this.stringIndexType, this.context);
            }
            return undefined;
        };
        SymbolTableWrapper.prototype.has = function (key) {
            var table = this.symbolTable;
            return ((typeof table.has === 'function') ? table.has(key) : table[key] != null) ||
                this.stringIndexType !== undefined;
        };
        SymbolTableWrapper.prototype.values = function () {
            var _this = this;
            return this.symbols.map(function (s) { return new SymbolWrapper(s, _this.context); });
        };
        return SymbolTableWrapper;
    }());
    var MapSymbolTable = /** @class */ (function () {
        function MapSymbolTable() {
            this.map = new Map();
            this._values = [];
        }
        Object.defineProperty(MapSymbolTable.prototype, "size", {
            get: function () {
                return this.map.size;
            },
            enumerable: false,
            configurable: true
        });
        MapSymbolTable.prototype.get = function (key) {
            return this.map.get(key);
        };
        MapSymbolTable.prototype.add = function (symbol) {
            if (this.map.has(symbol.name)) {
                var previous = this.map.get(symbol.name);
                this._values[this._values.indexOf(previous)] = symbol;
            }
            this.map.set(symbol.name, symbol);
            this._values.push(symbol);
        };
        MapSymbolTable.prototype.addAll = function (symbols) {
            var e_4, _a;
            try {
                for (var symbols_3 = tslib_1.__values(symbols), symbols_3_1 = symbols_3.next(); !symbols_3_1.done; symbols_3_1 = symbols_3.next()) {
                    var symbol = symbols_3_1.value;
                    this.add(symbol);
                }
            }
            catch (e_4_1) { e_4 = { error: e_4_1 }; }
            finally {
                try {
                    if (symbols_3_1 && !symbols_3_1.done && (_a = symbols_3.return)) _a.call(symbols_3);
                }
                finally { if (e_4) throw e_4.error; }
            }
        };
        MapSymbolTable.prototype.has = function (key) {
            return this.map.has(key);
        };
        MapSymbolTable.prototype.values = function () {
            // Switch to this.map.values once iterables are supported by the target language.
            return this._values;
        };
        return MapSymbolTable;
    }());
    var PipesTable = /** @class */ (function () {
        function PipesTable(pipes, context) {
            this.pipes = pipes;
            this.context = context;
        }
        Object.defineProperty(PipesTable.prototype, "size", {
            get: function () {
                return this.pipes.length;
            },
            enumerable: false,
            configurable: true
        });
        PipesTable.prototype.get = function (key) {
            var pipe = this.pipes.find(function (pipe) { return pipe.name == key; });
            if (pipe) {
                return new PipeSymbol(pipe, this.context);
            }
        };
        PipesTable.prototype.has = function (key) {
            return this.pipes.find(function (pipe) { return pipe.name == key; }) != null;
        };
        PipesTable.prototype.values = function () {
            var _this = this;
            return this.pipes.map(function (pipe) { return new PipeSymbol(pipe, _this.context); });
        };
        return PipesTable;
    }());
    // This matches .d.ts files that look like ".../<package-name>/<package-name>.d.ts",
    var INDEX_PATTERN = /[\\/]([^\\/]+)[\\/]\1\.d\.ts$/;
    var PipeSymbol = /** @class */ (function () {
        function PipeSymbol(pipe, context) {
            this.pipe = pipe;
            this.context = context;
            this.kind = 'pipe';
            this.language = 'typescript';
            this.container = undefined;
            this.callable = true;
            this.nullable = false;
            this.public = true;
        }
        Object.defineProperty(PipeSymbol.prototype, "name", {
            get: function () {
                return this.pipe.name;
            },
            enumerable: false,
            configurable: true
        });
        Object.defineProperty(PipeSymbol.prototype, "type", {
            get: function () {
                return new TypeWrapper(this.tsType, this.context);
            },
            enumerable: false,
            configurable: true
        });
        Object.defineProperty(PipeSymbol.prototype, "definition", {
            get: function () {
                var symbol = this.tsType.getSymbol();
                return symbol ? definitionFromTsSymbol(symbol) : undefined;
            },
            enumerable: false,
            configurable: true
        });
        Object.defineProperty(PipeSymbol.prototype, "documentation", {
            get: function () {
                var symbol = this.tsType.getSymbol();
                if (!symbol) {
                    return [];
                }
                return symbol.getDocumentationComment(this.context.checker);
            },
            enumerable: false,
            configurable: true
        });
        PipeSymbol.prototype.members = function () {
            return EmptyTable.instance;
        };
        PipeSymbol.prototype.signatures = function () {
            return signaturesOf(this.tsType, this.context);
        };
        PipeSymbol.prototype.selectSignature = function (types) {
            var signature = selectSignature(this.tsType, this.context, types);
            if (types.length > 0) {
                var parameterType = types[0];
                var resultType = undefined;
                switch (this.name) {
                    case 'async':
                        // Get type argument of 'Observable', 'Promise', or 'EventEmitter'.
                        var tArgs = parameterType.typeArguments();
                        if (tArgs && tArgs.length === 1) {
                            resultType = tArgs[0];
                        }
                        break;
                    case 'slice':
                        resultType = parameterType;
                        break;
                }
                if (resultType) {
                    signature = new SignatureResultOverride(signature, resultType);
                }
            }
            return signature;
        };
        PipeSymbol.prototype.indexed = function (_argument) {
            return undefined;
        };
        PipeSymbol.prototype.typeArguments = function () {
            return this.type.typeArguments();
        };
        Object.defineProperty(PipeSymbol.prototype, "tsType", {
            get: function () {
                var type = this._tsType;
                if (!type) {
                    var classSymbol = this.findClassSymbol(this.pipe.type.reference);
                    if (classSymbol) {
                        type = this._tsType = this.findTransformMethodType(classSymbol);
                    }
                    if (!type) {
                        type = this._tsType = getTsTypeFromBuiltinType(symbols_1.BuiltinType.Any, this.context);
                    }
                }
                return type;
            },
            enumerable: false,
            configurable: true
        });
        PipeSymbol.prototype.findClassSymbol = function (type) {
            return findClassSymbolInContext(type, this.context);
        };
        PipeSymbol.prototype.findTransformMethodType = function (classSymbol) {
            var classType = this.context.checker.getDeclaredTypeOfSymbol(classSymbol);
            if (classType) {
                var transform = classType.getProperty('transform');
                if (transform) {
                    return this.context.checker.getTypeOfSymbolAtLocation(transform, this.context.node);
                }
            }
        };
        return PipeSymbol;
    }());
    function findClassSymbolInContext(type, context) {
        var sourceFile = context.program.getSourceFile(type.filePath);
        if (!sourceFile) {
            // This handles a case where an <packageName>/index.d.ts and a <packageName>/<packageName>.d.ts
            // are in the same directory. If we are looking for <packageName>/<packageName> and didn't
            // find it, look for <packageName>/index.d.ts as the program might have found that instead.
            var p = type.filePath;
            var m = p.match(INDEX_PATTERN);
            if (m) {
                var indexVersion = path.join(path.dirname(p), 'index.d.ts');
                sourceFile = context.program.getSourceFile(indexVersion);
            }
        }
        if (sourceFile) {
            var moduleSymbol = sourceFile.module || sourceFile.symbol;
            var exports_1 = context.checker.getExportsOfModule(moduleSymbol);
            return (exports_1 || []).find(function (symbol) { return symbol.name == type.name; });
        }
    }
    var EmptyTable = /** @class */ (function () {
        function EmptyTable() {
            this.size = 0;
        }
        EmptyTable.prototype.get = function (_key) {
            return undefined;
        };
        EmptyTable.prototype.has = function (_key) {
            return false;
        };
        EmptyTable.prototype.values = function () {
            return [];
        };
        EmptyTable.instance = new EmptyTable();
        return EmptyTable;
    }());
    function isSymbolPrivate(s) {
        return !!s.valueDeclaration && isPrivate(s.valueDeclaration);
    }
    function getTsTypeFromBuiltinType(builtinType, ctx) {
        var syntaxKind;
        switch (builtinType) {
            case symbols_1.BuiltinType.Any:
                syntaxKind = ts.SyntaxKind.AnyKeyword;
                break;
            case symbols_1.BuiltinType.Boolean:
                syntaxKind = ts.SyntaxKind.BooleanKeyword;
                break;
            case symbols_1.BuiltinType.Null:
                syntaxKind = ts.SyntaxKind.NullKeyword;
                break;
            case symbols_1.BuiltinType.Number:
                syntaxKind = ts.SyntaxKind.NumberKeyword;
                break;
            case symbols_1.BuiltinType.String:
                syntaxKind = ts.SyntaxKind.StringKeyword;
                break;
            case symbols_1.BuiltinType.Undefined:
                syntaxKind = ts.SyntaxKind.UndefinedKeyword;
                break;
            default:
                throw new Error("Internal error, unhandled literal kind " + builtinType + ":" + symbols_1.BuiltinType[builtinType]);
        }
        var node = ts.createNode(syntaxKind);
        node.parent = ts.createEmptyStatement();
        return ctx.checker.getTypeAtLocation(node);
    }
    function spanAt(sourceFile, line, column) {
        if (line != null && column != null) {
            var position_1 = ts.getPositionOfLineAndCharacter(sourceFile, line, column);
            var findChild = function findChild(node) {
                if (node.kind > ts.SyntaxKind.LastToken && node.pos <= position_1 && node.end > position_1) {
                    var betterNode = ts.forEachChild(node, findChild);
                    return betterNode || node;
                }
            };
            var node = ts.forEachChild(sourceFile, findChild);
            if (node) {
                return { start: node.getStart(), end: node.getEnd() };
            }
        }
    }
    function definitionFromTsSymbol(symbol) {
        var declarations = symbol.declarations;
        if (declarations) {
            return declarations.map(function (declaration) {
                var sourceFile = declaration.getSourceFile();
                return {
                    fileName: sourceFile.fileName,
                    span: { start: declaration.getStart(), end: declaration.getEnd() }
                };
            });
        }
    }
    function parentDeclarationOf(node) {
        while (node) {
            switch (node.kind) {
                case ts.SyntaxKind.ClassDeclaration:
                case ts.SyntaxKind.InterfaceDeclaration:
                    return node;
                case ts.SyntaxKind.SourceFile:
                    return undefined;
            }
            node = node.parent;
        }
    }
    function getContainerOf(symbol, context) {
        var e_5, _a;
        if (symbol.getFlags() & ts.SymbolFlags.ClassMember && symbol.declarations) {
            try {
                for (var _b = tslib_1.__values(symbol.declarations), _c = _b.next(); !_c.done; _c = _b.next()) {
                    var declaration = _c.value;
                    var parent_1 = parentDeclarationOf(declaration);
                    if (parent_1) {
                        var type = context.checker.getTypeAtLocation(parent_1);
                        if (type) {
                            return new TypeWrapper(type, context);
                        }
                    }
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
    }
    function typeKindOf(type) {
        var e_6, _a;
        if (type) {
            if (type.flags & ts.TypeFlags.Any) {
                return symbols_1.BuiltinType.Any;
            }
            else if (type.flags & (ts.TypeFlags.String | ts.TypeFlags.StringLike | ts.TypeFlags.StringLiteral)) {
                return symbols_1.BuiltinType.String;
            }
            else if (type.flags & (ts.TypeFlags.Number | ts.TypeFlags.NumberLike)) {
                return symbols_1.BuiltinType.Number;
            }
            else if (type.flags & ts.TypeFlags.Object) {
                return symbols_1.BuiltinType.Object;
            }
            else if (type.flags & (ts.TypeFlags.Undefined)) {
                return symbols_1.BuiltinType.Undefined;
            }
            else if (type.flags & (ts.TypeFlags.Null)) {
                return symbols_1.BuiltinType.Null;
            }
            else if (type.flags & ts.TypeFlags.Union) {
                var unionType = type;
                if (unionType.types.length === 0)
                    return symbols_1.BuiltinType.Other;
                var ty = 0;
                try {
                    for (var _b = tslib_1.__values(unionType.types), _c = _b.next(); !_c.done; _c = _b.next()) {
                        var subType = _c.value;
                        ty |= typeKindOf(subType);
                    }
                }
                catch (e_6_1) { e_6 = { error: e_6_1 }; }
                finally {
                    try {
                        if (_c && !_c.done && (_a = _b.return)) _a.call(_b);
                    }
                    finally { if (e_6) throw e_6.error; }
                }
                return ty;
            }
            else if (type.flags & ts.TypeFlags.TypeParameter) {
                return symbols_1.BuiltinType.Unbound;
            }
        }
        return symbols_1.BuiltinType.Other;
    }
    function getFromSymbolTable(symbolTable, key) {
        var table = symbolTable;
        var symbol;
        if (typeof table.get === 'function') {
            // TS 2.2 uses a Map
            symbol = table.get(key);
        }
        else {
            // TS pre-2.2 uses an object
            symbol = table[key];
        }
        return symbol;
    }
});
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoidHlwZXNjcmlwdF9zeW1ib2xzLmpzIiwic291cmNlUm9vdCI6IiIsInNvdXJjZXMiOlsiLi4vLi4vLi4vLi4vLi4vLi4vcGFja2FnZXMvbGFuZ3VhZ2Utc2VydmljZS9zcmMvdHlwZXNjcmlwdF9zeW1ib2xzLnRzIl0sIm5hbWVzIjpbXSwibWFwcGluZ3MiOiJBQUFBOzs7Ozs7R0FNRzs7Ozs7Ozs7Ozs7Ozs7SUFHSCwyQkFBNkI7SUFDN0IsK0JBQWlDO0lBRWpDLGlFQUF5STtJQUV6SSxzQ0FBc0M7SUFDdEMsMkNBQTJDO0lBQzNDLElBQU0sU0FBUyxHQUFJLEVBQVUsQ0FBQyxhQUFhLENBQUMsQ0FBQztRQUN6QyxDQUFDLFVBQUMsSUFBYTtZQUNWLE9BQUEsQ0FBQyxDQUFDLENBQUUsRUFBVSxDQUFDLHdCQUF3QixDQUFDLElBQUksQ0FBQyxHQUFJLEVBQVUsQ0FBQyxhQUFhLENBQUMsT0FBTyxDQUFDO1FBQWxGLENBQWtGLENBQUMsQ0FBQyxDQUFDO1FBQzFGLENBQUMsVUFBQyxJQUFhLElBQUssT0FBQSxDQUFDLENBQUMsQ0FBQyxJQUFJLENBQUMsS0FBSyxHQUFJLEVBQVUsQ0FBQyxTQUFTLENBQUMsT0FBTyxDQUFDLEVBQTlDLENBQThDLENBQUMsQ0FBQztJQUV4RSxJQUFNLGVBQWUsR0FBSSxFQUFVLENBQUMsV0FBVyxDQUFDLENBQUM7UUFDN0MsQ0FBQyxVQUFDLElBQWE7WUFDVixPQUFBLENBQUMsQ0FBQyxDQUFDLElBQUksQ0FBQyxLQUFLLEdBQUksRUFBVSxDQUFDLFNBQVMsQ0FBQyxNQUFNO2dCQUN4QyxJQUFZLENBQUMsV0FBVyxHQUFJLEVBQVUsQ0FBQyxXQUFXLENBQUMsU0FBUyxDQUFDO1FBRGpFLENBQ2lFLENBQUMsQ0FBQyxDQUFDO1FBQ3pFLENBQUMsVUFBQyxJQUFhLElBQUssT0FBQSxDQUFDLENBQUMsQ0FBQyxJQUFJLENBQUMsS0FBSyxHQUFJLEVBQVUsQ0FBQyxTQUFTLENBQUMsU0FBUyxDQUFDLEVBQWhELENBQWdELENBQUMsQ0FB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QixFQUFFLEdBQVc7UUFDbEUsSUFBTSxLQUFLLEdBQUcsV0FBa0IsQ0FBQztRQUNqQyxJQUFJLE1BQTJCLENBQUM7UUFFaEMsSUFBSSxPQUFPLEtBQUssQ0FBQyxHQUFHLEtBQUssVUFBVSxFQUFFO1lBQ25DLG9CQUFvQjtZQUNwQixNQUFNLEdBQUcsS0FBSyxDQUFDLEdBQUcsQ0FBQyxHQUFHLENBQUMsQ0FBQztTQUN6QjthQUFNO1lBQ0wsNEJBQTRCO1lBQzVCLE1BQU0sR0FBRyxLQUFLLENBQUMsR0FBRyxDQUFDLENBQUM7U0FDckI7UUFFRCxPQUFPLE1BQU0sQ0FBQztJQUNoQixDQUFDIiwic291cmNlc0NvbnRlbnQiOlsiLyoqXG4gKiBAbGljZW5zZVxuICogQ29weXJpZ2h0IEdvb2dsZSBMTEMgQWxsIFJpZ2h0cyBSZXNlcnZlZC5cbiAqXG4gKiBVc2Ugb2YgdGhpcyBzb3VyY2UgY29kZSBpcyBnb3Zlcm5lZCBieSBhbiBNSVQtc3R5bGUgbGljZW5zZSB0aGF0IGNhbiBiZVxuICogZm91bmQgaW4gdGhlIExJQ0VOU0UgZmlsZSBhdCBodHRwczovL2FuZ3VsYXIuaW8vbGljZW5zZVxuICovXG5cbmltcG9ydCB7Q29tcGlsZVBpcGVTdW1tYXJ5LCBTdGF0aWNTeW1ib2x9IGZyb20gJ0Bhbmd1bGFyL2NvbXBpbGVyJztcbmltcG9ydCAqIGFzIHBhdGggZnJvbSAncGF0aCc7XG5pbXBvcnQgKiBhcyB0cyBmcm9tICd0eXBlc2NyaXB0JztcblxuaW1wb3J0IHtCdWlsdGluVHlwZSwgRGVjbGFyYXRpb25LaW5kLCBEZWZpbml0aW9uLCBTaWduYXR1cmUsIFNwYW4sIFN5bWJvbCwgU3ltYm9sRGVjbGFyYXRpb24sIFN5bWJvbFF1ZXJ5LCBTeW1ib2xUYWJsZX0gZnJvbSAnLi9zeW1ib2xzJztcblxuLy8gSW4gVHlwZVNjcmlwdCAyLjEgdGhlc2UgZmxhZ3MgbW92ZWRcbi8vIFRoZXNlIGhlbHBlcnMgd29yayBmb3IgYm90aCAyLjAgYW5kIDIuMS5cbmNvbnN0IGlzUHJpdmF0ZSA9ICh0cyBhcyBhbnkpLk1vZGlmaWVyRmxhZ3MgP1xuICAgICgobm9kZTogdHMuTm9kZSkgPT5cbiAgICAgICAgICEhKCh0cyBhcyBhbnkpLmdldENvbWJpbmVkTW9kaWZpZXJGbGFncyhub2RlKSAmICh0cyBhcyBhbnkpLk1vZGlmaWVyRmxhZ3MuUHJpdmF0ZSkpIDpcbiAgICAoKG5vZGU6IHRzLk5vZGUpID0+ICEhKG5vZGUuZmxhZ3MgJiAodHMgYXMgYW55KS5Ob2RlRmxhZ3MuUHJpdmF0ZSkpO1xuXG5jb25zdCBpc1JlZmVyZW5jZVR5cGUgPSAodHMgYXMgYW55KS5PYmplY3RGbGFncyA/XG4gICAgKCh0eXBlOiB0cy5UeXBlKSA9PlxuICAgICAgICAgISEodHlwZS5mbGFncyAmICh0cyBhcyBhbnkpLlR5cGVGbGFncy5PYmplY3QgJiZcbiAgICAgICAgICAgICh0eXBlIGFzIGFueSkub2JqZWN0RmxhZ3MgJiAodHMgYXMgYW55KS5PYmplY3RGbGFncy5SZWZlcmVuY2UpKSA6XG4gICAgKCh0eXBlOiB0cy5UeXBlKSA9PiAhISh0eXBlLmZsYWdzICYgKHRzIGFzIGFueSkuVHlwZUZsYWdzLlJlZmVyZW5jZSkpO1xuXG5pbnRlcmZhY2UgVHlwZUNvbnRleHQge1xuICBub2RlOiB0cy5Ob2RlO1xuICBwcm9ncmFtOiB0cy5Qcm9ncmFtO1xuICBjaGVja2VyOiB0cy5UeXBlQ2hlY2tlcjtcbn1cblxuZXhwb3J0IGZ1bmN0aW9uIGdldFN5bWJvbFF1ZXJ5KFxuICAgIHByb2dyYW06IHRzLlByb2dyYW0sIGNoZWNrZXI6IHRzLlR5cGVDaGVja2VyLCBzb3VyY2U6IHRzLlNvdXJjZUZpbGUsXG4gICAgZmV0Y2hQaXBlczogKCkgPT4gU3ltYm9sVGFibGUpOiBTeW1ib2xRdWVyeSB7XG4gIHJldHVybiBuZXcgVHlwZVNjcmlwdFN5bWJvbFF1ZXJ5KHByb2dyYW0sIGNoZWNrZXIsIHNvdXJjZSwgZmV0Y2hQaXBlcyk7XG59XG5cbmV4cG9ydCBmdW5jdGlvbiBnZXRDbGFzc01lbWJlcnMoXG4gICAgcHJvZ3JhbTogdHMuUHJvZ3JhbSwgY2hlY2tlcjogdHMuVHlwZUNoZWNrZXIsIHN0YXRpY1N5bWJvbDogU3RhdGljU3ltYm9sKTogU3ltYm9sVGFibGV8XG4gICAgdW5kZWZpbmVkIHtcbiAgY29uc3QgZGVjbGFyYXRpb24gPSBnZXRDbGFzc0Zyb21TdGF0aWNTeW1ib2wocHJvZ3JhbSwgc3RhdGljU3ltYm9sKTtcbiAgaWYgKGRlY2xhcmF0aW9uKSB7XG4gICAgY29uc3QgdHlwZSA9IGNoZWNrZXIuZ2V0VHlwZUF0TG9jYXRpb24oZGVjbGFyYXRpb24pO1xuICAgIGNvbnN0IG5vZGUgPSBwcm9ncmFtLmdldFNvdXJjZUZpbGUoc3RhdGljU3ltYm9sLmZpbGVQYXRoKTtcbiAgICBpZiAobm9kZSkge1xuICAgICAgcmV0dXJuIG5ldyBUeXBlV3JhcHBlcih0eXBlLCB7bm9kZSwgcHJvZ3JhbSwgY2hlY2tlcn0pLm1lbWJlcnMoKTtcbiAgICB9XG4gIH1cbn1cblxuZXhwb3J0IGZ1bmN0aW9uIGdldENsYXNzTWVtYmVyc0Zyb21EZWNsYXJhdGlvbihcbiAgICBwcm9ncmFtOiB0cy5Qcm9ncmFtLCBjaGVja2VyOiB0cy5UeXBlQ2hlY2tlciwgc291cmNlOiB0cy5Tb3VyY2VGaWxlLFxuICAgIGRlY2xhcmF0aW9uOiB0cy5DbGFzc0RlY2xhcmF0aW9uKSB7XG4gIGNvbnN0IHR5cGUgPSBjaGVja2VyLmdldFR5cGVBdExvY2F0aW9uKGRlY2xhcmF0aW9uKTtcbiAgcmV0dXJuIG5ldyBUeXBlV3JhcHBlcih0eXBlLCB7bm9kZTogc291cmNlLCBwcm9ncmFtLCBjaGVja2VyfSkubWVtYmVycygpO1xufVxuXG5leHBvcnQgZnVuY3Rpb24gZ2V0UGlwZXNUYWJsZShcbiAgICBzb3VyY2U6IHRzLlNvdXJjZUZpbGUsIHByb2dyYW06IHRzLlByb2dyYW0sIGNoZWNrZXI6IHRzLlR5cGVDaGVja2VyLFxuICAgIHBpcGVzOiBDb21waWxlUGlwZVN1bW1hcnlbXSk6IFN5bWJvbFRhYmxlIHtcbiAgcmV0dXJuIG5ldyBQaXBlc1RhYmxlKHBpcGVzLCB7cHJvZ3JhbSwgY2hlY2tlciwgbm9kZTogc291cmNlfSk7XG59XG5cbmZ1bmN0aW9uIGdldENsYXNzRnJvbVN0YXRpY1N5bWJvbChwcm9ncmFtOiB0cy5Qcm9ncmFtLCB0eXBlOiBTdGF0aWNTeW1ib2wpOiB0cy5DbGFzc0RlY2xhcmF0aW9ufFxuICAgIHVuZGVmaW5lZCB7XG4gIGNvbnN0IHNvdXJjZSA9IHByb2dyYW0uZ2V0U291cmNlRmlsZSh0eXBlLmZpbGVQYXRoKTtcbiAgaWYgKHNvdXJjZSkge1xuICAgIHJldHVybiB0cy5mb3JFYWNoQ2hpbGQoc291cmNlLCBjaGlsZCA9PiB7XG4gICAgICBpZiAoY2hpbGQua2luZCA9PT0gdHMuU3ludGF4S2luZC5DbGFzc0RlY2xhcmF0aW9uKSB7XG4gICAgICAgIGNvbnN0IGNsYXNzRGVjbGFyYXRpb24gPSBjaGlsZCBhcyB0cy5DbGFzc0RlY2xhcmF0aW9uO1xuICAgICAgICBpZiAoY2xhc3NEZWNsYXJhdGlvbi5uYW1lICE9IG51bGwgJiYgY2xhc3NEZWNsYXJhdGlvbi5uYW1lLnRleHQgPT09IHR5cGUubmFtZSkge1xuICAgICAgICAgIHJldHVybiBjbGFzc0RlY2xhcmF0aW9uO1xuICAgICAgICB9XG4gICAgICB9XG4gICAgfSkgYXMgKHRzLkNsYXNzRGVjbGFyYXRpb24gfCB1bmRlZmluZWQpO1xuICB9XG5cbiAgcmV0dXJuIHVuZGVmaW5lZDtcbn1cblxuY2xhc3MgVHlwZVNjcmlwdFN5bWJvbFF1ZXJ5IGltcGxlbWVudHMgU3ltYm9sUXVlcnkge1xuICBwcml2YXRlIHR5cGVDYWNoZSA9IG5ldyBNYXA8QnVpbHRpblR5cGUsIFN5bWJvbD4oKTtcbiAgcHJpdmF0ZSBwaXBlc0NhY2hlOiBTeW1ib2xUYWJsZXx1bmRlZmluZWQ7XG5cbiAgY29uc3RydWN0b3IoXG4gICAgICBwcml2YXRlIHByb2dyYW06IHRzLlByb2dyYW0sIHByaXZhdGUgY2hlY2tlcjogdHMuVHlwZUNoZWNrZXIsIHByaXZhdGUgc291cmNlOiB0cy5Tb3VyY2VGaWxlLFxuICAgICAgcHJpdmF0ZSBmZXRjaFBpcGVzOiAoKSA9PiBTeW1ib2xUYWJsZSkge31cblxuICBnZXRUeXBlS2luZChzeW1ib2w6IFN5bWJvbCk6IEJ1aWx0aW5UeXBlIHtcbiAgICBjb25zdCB0eXBlID0gc3ltYm9sIGluc3RhbmNlb2YgVHlwZVdyYXBwZXIgPyBzeW1ib2wudHNUeXBlIDogdW5kZWZpbmVkO1xuICAgIHJldHVybiB0eXBlS2luZE9mKHR5cGUpO1xuICB9XG5cbiAgZ2V0QnVpbHRpblR5cGUoa2luZDogQnVpbHRpblR5cGUpOiBTeW1ib2wge1xuICAgIGxldCByZXN1bHQgPSB0aGlzLnR5cGVDYWNoZS5nZXQoa2luZCk7XG4gICAgaWYgKCFyZXN1bHQpIHtcbiAgICAgIGNvbnN0IHR5cGUgPSBnZXRUc1R5cGVGcm9tQnVpbHRpblR5cGUoa2luZCwge1xuICAgICAgICBjaGVja2VyOiB0aGlzLmNoZWNrZXIsXG4gICAgICAgIG5vZGU6IHRoaXMuc291cmNlLFxuICAgICAgICBwcm9ncmFtOiB0aGlzLnByb2dyYW0sXG4gICAgICB9KTtcbiAgICAgIHJlc3VsdCA9XG4gICAgICAgICAgbmV3IFR5cGVXcmFwcGVyKHR5cGUsIHtwcm9ncmFtOiB0aGlzLnByb2dyYW0sIGNoZWNrZXI6IHRoaXMuY2hlY2tlciwgbm9kZTogdGhpcy5zb3VyY2V9KTtcbiAgICAgIHRoaXMudHlwZUNhY2hlLnNldChraW5kLCByZXN1bHQpO1xuICAgIH1cbiAgICByZXR1cm4gcmVzdWx0O1xuICB9XG5cbiAgZ2V0VHlwZVVuaW9uKC4uLnR5cGVzOiBTeW1ib2xbXSk6IFN5bWJvbCB7XG4gICAgLy8gTm8gQVBJIGV4aXN0cyBzbyByZXR1cm4gYW55IGlmIHRoZSB0eXBlcyBhcmUgbm90IGFsbCB0aGUgc2FtZSB0eXBlLlxuICAgIGxldCByZXN1bHQ6IFN5bWJvbHx1bmRlZmluZWQgPSB1bmRlZmluZWQ7XG4gICAgaWYgKHR5cGVzLmxlbmd0aCkge1xuICAgICAgcmVzdWx0ID0gdHlwZXNbMF07XG4gICAgICBmb3IgKGxldCBpID0gMTsgaSA8IHR5cGVzLmxlbmd0aDsgaSsrKSB7XG4gICAgICAgIGlmICh0eXBlc1tpXSAhPSByZXN1bHQpIHtcbiAgICAgICAgICByZXN1bHQgPSB1bmRlZmluZWQ7XG4gICAgICAgICAgYnJlYWs7XG4gICAgICAgIH1cbiAgICAgIH1cbiAgICB9XG4gICAgcmV0dXJuIHJlc3VsdCB8fCB0aGlzLmdldEJ1aWx0aW5UeXBlKEJ1aWx0aW5UeXBlLkFueSk7XG4gIH1cblxuICBnZXRBcnJheVR5cGUoX3R5cGU6IFN5bWJvbCk6IFN5bWJvbCB7XG4gICAgcmV0dXJuIHRoaXMuZ2V0QnVpbHRpblR5cGUoQnVpbHRpblR5cGUuQW55KTtcbiAgfVxuXG4gIGdldEVsZW1lbnRUeXBlKHR5cGU6IFN5bWJvbCk6IFN5bWJvbHx1bmRlZmluZWQge1xuICAgIGlmICh0eXBlIGluc3RhbmNlb2YgVHlwZVdyYXBwZXIpIHtcbiAgICAgIGNvbnN0IHR5ID0gdHlwZS50c1R5cGU7XG4gICAgICBjb25zdCB0eUFyZ3MgPSB0eXBlLnR5cGVBcmd1bWVudHMoKTtcbiAgICAgIC8vIFRPRE8oYXlhemhhZml6KTogVHJhY2sgaHR0cHM6Ly9naXRodWIuY29tL21pY3Jvc29mdC9UeXBlU2NyaXB0L2lzc3Vlcy8zNzcxMSB0byBleHBvc2VcbiAgICAgIC8vIGBpc0FycmF5TGlrZVR5cGVgIGFzIGEgcHVibGljIG1ldGhvZC5cbiAgICAgIGlmICghKHRoaXMuY2hlY2tlciBhcyBhbnkpLmlzQXJyYXlMaWtlVHlwZSh0eSkgfHwgdHlBcmdzPy5sZW5ndGggIT09IDEpIHJldHVybjtcbiAgICAgIHJldHVybiB0eUFyZ3NbMF07XG4gICAgfVxuICB9XG5cbiAgZ2V0Tm9uTnVsbGFibGVUeXBlKHN5bWJvbDogU3ltYm9sKTogU3ltYm9sIHtcbiAgICBpZiAoc3ltYm9sIGluc3RhbmNlb2YgVHlwZVdyYXBwZXIgJiYgKHR5cGVvZiB0aGlzLmNoZWNrZXIuZ2V0Tm9uTnVsbGFibGVUeXBlID09ICdmdW5jdGlvbicpKSB7XG4gICAgICBjb25zdCB0c1R5cGUgPSBzeW1ib2wudHNUeXBlO1xuICAgICAgY29uc3Qgbm9uTnVsbGFibGVUeXBlID0gdGhpcy5jaGVja2VyLmdldE5vbk51bGxhYmxlVHlwZSh0c1R5cGUpO1xuICAgICAgaWYgKG5vbk51bGxhYmxlVHlwZSAhPSB0c1R5cGUpIHtcbiAgICAgICAgcmV0dXJuIG5ldyBUeXBlV3JhcHBlcihub25OdWxsYWJsZVR5cGUsIHN5bWJvbC5jb250ZXh0KTtcbiAgICAgIH0gZWxzZSBpZiAobm9uTnVsbGFibGVUeXBlID09IHRzVHlwZSkge1xuICAgICAgICByZXR1cm4gc3ltYm9sO1xuICAgICAgfVxuICAgIH1cbiAgICByZXR1cm4gdGhpcy5nZXRCdWlsdGluVHlwZShCdWlsdGluVHlwZS5BbnkpO1xuICB9XG5cbiAgZ2V0UGlwZXMoKTogU3ltYm9sVGFibGUge1xuICAgIGxldCByZXN1bHQgPSB0aGlzLnBpcGVzQ2FjaGU7XG4gICAgaWYgKCFyZXN1bHQpIHtcbiAgICAgIHJlc3VsdCA9IHRoaXMucGlwZXNDYWNoZSA9IHRoaXMuZmV0Y2hQaXBlcygpO1xuICAgIH1cbiAgICByZXR1cm4gcmVzdWx0O1xuICB9XG5cbiAgZ2V0VGVtcGxhdGVDb250ZXh0KHR5cGU6IFN0YXRpY1N5bWJvbCk6IFN5bWJvbFRhYmxlfHVuZGVmaW5lZCB7XG4gICAgY29uc3QgY29udGV4dDogVHlwZUNvbnRleHQgPSB7bm9kZTogdGhpcy5zb3VyY2UsIHByb2dyYW06IHRoaXMucHJvZ3JhbSwgY2hlY2tlcjogdGhpcy5jaGVja2VyfTtcbiAgICBjb25zdCB0eXBlU3ltYm9sID0gZmluZENsYXNzU3ltYm9sSW5Db250ZXh0KHR5cGUsIGNvbnRleHQpO1xuICAgIGlmICh0eXBlU3ltYm9sKSB7XG4gICAgICBjb25zdCBjb250ZXh0VHlwZSA9IHRoaXMuZ2V0VGVtcGxhdGVSZWZDb250ZXh0VHlwZSh0eXBlU3ltYm9sLCBjb250ZXh0KTtcbiAgICAgIGlmIChjb250ZXh0VHlwZSkgcmV0dXJuIGNvbnRleHRUeXBlLm1lbWJlcnMoKTtcbiAgICB9XG4gIH1cblxuICBnZXRUeXBlU3ltYm9sKHR5cGU6IFN0YXRpY1N5bWJvbCk6IFN5bWJvbHx1bmRlZmluZWQge1xuICAgIGNvbnN0IGNvbnRleHQ6IFR5cGVDb250ZXh0ID0ge25vZGU6IHRoaXMuc291cmNlLCBwcm9ncmFtOiB0aGlzLnByb2dyYW0sIGNoZWNrZXI6IHRoaXMuY2hlY2tlcn07XG4gICAgY29uc3QgdHlwZVN5bWJvbCA9IGZpbmRDbGFzc1N5bWJvbEluQ29udGV4dCh0eXBlLCBjb250ZXh0KTtcbiAgICByZXR1cm4gdHlwZVN5bWJvbCAmJiBuZXcgU3ltYm9sV3JhcHBlcih0eXBlU3ltYm9sLCBjb250ZXh0KTtcbiAgfVxuXG4gIGNyZWF0ZVN5bWJvbFRhYmxlKHN5bWJvbHM6IFN5bWJvbERlY2xhcmF0aW9uW10pOiBTeW1ib2xUYWJsZSB7XG4gICAgY29uc3QgcmVzdWx0ID0gbmV3IE1hcFN5bWJvbFRhYmxlKCk7XG4gICAgcmVzdWx0LmFkZEFsbChzeW1ib2xzLm1hcChzID0+IG5ldyBEZWNsYXJlZFN5bWJvbChzKSkpO1xuICAgIHJldHVybiByZXN1bHQ7XG4gIH1cblxuICBtZXJnZVN5bWJvbFRhYmxlKHN5bWJvbFRhYmxlczogU3ltYm9sVGFibGVbXSk6IFN5bWJvbFRhYmxlIHtcbiAgICBjb25zdCByZXN1bHQgPSBuZXcgTWFwU3ltYm9sVGFibGUoKTtcbiAgICBmb3IgKGNvbnN0IHN5bWJvbFRhYmxlIG9mIHN5bWJvbFRhYmxlcykge1xuICAgICAgcmVzdWx0LmFkZEFsbChzeW1ib2xUYWJsZS52YWx1ZXMoKSk7XG4gICAgfVxuICAgIHJldHVybiByZXN1bHQ7XG4gIH1cblxuICBnZXRTcGFuQXQobGluZTogbnVtYmVyLCBjb2x1bW46IG51bWJlcik6IFNwYW58dW5kZWZpbmVkIHtcbiAgICByZXR1cm4gc3BhbkF0KHRoaXMuc291cmNlLCBsaW5lLCBjb2x1bW4pO1xuICB9XG5cbiAgcHJpdmF0ZSBnZXRUZW1wbGF0ZVJlZkNvbnRleHRUeXBlKHR5cGVTeW1ib2w6IHRzLlN5bWJvbCwgY29udGV4dDogVHlwZUNvbnRleHQpOiBTeW1ib2x8dW5kZWZpbmVkIHtcbiAgICBjb25zdCB0eXBlID0gdGhpcy5jaGVja2VyLmdldFR5cGVPZlN5bWJvbEF0TG9jYXRpb24odHlwZVN5bWJvbCwgdGhpcy5zb3VyY2UpO1xuICAgIGNvbnN0IGNvbnN0cnVjdG9yID0gdHlwZS5zeW1ib2wgJiYgdHlwZS5zeW1ib2wubWVtYmVycyAmJlxuICAgICAgICBnZXRGcm9tU3ltYm9sVGFibGUodHlwZS5zeW1ib2wubWVtYmVycyEsICdfX2NvbnN0cnVjdG9yJyk7XG5cbiAgICBpZiAoY29uc3RydWN0b3IpIHtcbiAgICAgIGNvbnN0IGNvbnN0cnVjdG9yRGVjbGFyYXRpb24gPSBjb25zdHJ1Y3Rvci5kZWNsYXJhdGlvbnMhWzBdIGFzIHRzLkNvbnN0cnVjdG9yVHlwZU5vZGU7XG4gICAgICBmb3IgKGNvbnN0IHBhcmFtZXRlciBvZiBjb25zdHJ1Y3RvckRlY2xhcmF0aW9uLnBhcmFtZXRlcnMpIHtcbiAgICAgICAgY29uc3QgdHlwZSA9IHRoaXMuY2hlY2tlci5nZXRUeXBlQXRMb2NhdGlvbihwYXJhbWV0ZXIudHlwZSEpO1xuICAgICAgICBpZiAodHlwZS5zeW1ib2whLm5hbWUgPT0gJ1RlbXBsYXRlUmVmJyAmJiBpc1JlZmVyZW5jZVR5cGUodHlwZSkpIHtcbiAgICAgICAgICBjb25zdCB0eXBlV3JhcHBlciA9IG5ldyBUeXBlV3JhcHBlcih0eXBlLCBjb250ZXh0KTtcbiAgICAgICAgICBjb25zdCB0eXBlQXJndW1lbnRzID0gdHlwZVdyYXBwZXIudHlwZUFyZ3VtZW50cygpO1xuICAgICAgICAgIGlmICh0eXBlQXJndW1lbnRzICYmIHR5cGVBcmd1bWVudHMubGVuZ3RoID09PSAxKSB7XG4gICAgICAgICAgICByZXR1cm4gdHlwZUFyZ3VtZW50c1swXTtcbiAgICAgICAgICB9XG4gICAgICAgIH1cbiAgICAgIH1cbiAgICB9XG4gIH1cbn1cblxuZnVuY3Rpb24gdHlwZUNhbGxhYmxlKHR5cGU6IHRzLlR5cGUpOiBib29sZWFuIHtcbiAgY29uc3Qgc2lnbmF0dXJlcyA9IHR5cGUuZ2V0Q2FsbFNpZ25hdHVyZXMoKTtcbiAgcmV0dXJuIHNpZ25hdHVyZXMgJiYgc2lnbmF0dXJlcy5sZW5ndGggIT0gMDtcbn1cblxuZnVuY3Rpb24gc2lnbmF0dXJlc09mKHR5cGU6IHRzLlR5cGUsIGNvbnRleHQ6IFR5cGVDb250ZXh0KTogU2lnbmF0dXJlW10ge1xuICByZXR1cm4gdHlwZS5nZXRDYWxsU2lnbmF0dXJlcygpLm1hcChzID0+IG5ldyBTaWduYXR1cmVXcmFwcGVyKHMsIGNvbnRleHQpKTtcbn1cblxuZnVuY3Rpb24gc2VsZWN0U2lnbmF0dXJlKHR5cGU6IHRzLlR5cGUsIGNvbnRleHQ6IFR5cGVDb250ZXh0LCB0eXBlczogU3ltYm9sW10pOiBTaWduYXR1cmV8XG4gICAgdW5kZWZpbmVkIHtcbiAgLy8gVE9ETzogRG8gYSBiZXR0ZXIgam9iIG9mIHNlbGVjdGluZyB0aGUgcmlnaHQgc2lnbmF0dXJlLiBUeXBlU2NyaXB0IGRvZXMgbm90IGN1cnJlbnRseSBzdXBwb3J0IGFcbiAgLy8gVHlwZSBSZWxhdGlvbnNoaXAgQVBJIChzZWUgaHR0cHM6Ly9naXRodWIuY29tL2FuZ3VsYXIvdnNjb2RlLW5nLWxhbmd1YWdlLXNlcnZpY2UvaXNzdWVzLzE0MykuXG4gIC8vIENvbnNpZGVyIGNyZWF0aW5nIGEgVHlwZUNoZWNrQmxvY2sgaG9zdCBpbiB0aGUgbGFuZ3VhZ2Ugc2VydmljZSB0aGF0IG1heSBhbHNvIGFjdCBhcyBhXG4gIC8vIHNjcmF0Y2hwYWQgZm9yIHR5cGUgY29tcGFyaXNvbnMuXG4gIGNvbnN0IHNpZ25hdHVyZXMgPSB0eXBlLmdldENhbGxTaWduYXR1cmVzKCk7XG4gIGNvbnN0IHBhc3NlZEluVHlwZXM6IEFycmF5PHRzLlR5cGV8dW5kZWZpbmVkPiA9IHR5cGVzLm1hcCh0eXBlID0+IHtcbiAgICBpZiAodHlwZSBpbnN0YW5jZW9mIFR5cGVXcmFwcGVyKSB7XG4gICAgICByZXR1cm4gdHlwZS50c1R5cGU7XG4gICAgfVxuICB9KTtcbiAgLy8gVHJ5IHRvIHNlbGVjdCBhIG1hdGNoaW5nIHNpZ25hdHVyZSBpbiB3aGljaCBhbGwgcGFyYW1ldGVyIHR5cGVzIG1hdGNoLlxuICAvLyBOb3RlIHRoYXQgdGhpcyBpcyBqdXN0IGEgYmVzdC1lZmZvcnQgYXBwcm9hY2gsIGJlY2F1c2Ugd2UncmUgY2hlY2tpbmcgZm9yXG4gIC8vIHN0cmljdCB0eXBlIGVxdWFsaXR5IHJhdGhlciB0aGFuIGNvbXBhdGliaWxpdHkuXG4gIC8vIEZvciBleGFtcGxlLCBpZiB0aGUgc2lnbmF0dXJlIGNvbnRhaW5zIGEgUmVhZG9ubHlBcnJheTxudW1iZXI+IGFuZCB0aGVcbiAgLy8gcGFzc2VkIHBhcmFtZXRlciB0eXBlIGlzIGFuIEFycmF5PG51bWJlcj4sIHRoaXMgd2lsbCBmYWlsLlxuICBmdW5jdGlvbiBhbGxQYXJhbWV0ZXJUeXBlc01hdGNoKHNpZ25hdHVyZTogdHMuU2lnbmF0dXJlKSB7XG4gICAgY29uc3QgdGMgPSBjb250ZXh0LmNoZWNrZXI7XG4gICAgcmV0dXJuIHNpZ25hdHVyZS5nZXRQYXJhbWV0ZXJzKCkuZXZlcnkoKHBhcmFtZXRlcjogdHMuU3ltYm9sLCBpOiBudW1iZXIpID0+IHtcbiAgICAgIGNvbnN0IHR5cGUgPSB0Yy5nZXRUeXBlT2ZTeW1ib2xBdExvY2F0aW9uKHBhcmFtZXRlciwgcGFyYW1ldGVyLnZhbHVlRGVjbGFyYXRpb24pO1xuICAgICAgcmV0dXJuIHR5cGUgPT09IHBhc3NlZEluVHlwZXNbaV07XG4gICAgfSk7XG4gIH1cbiAgY29uc3QgZXhhY3RNYXRjaCA9IHNpZ25hdHVyZXMuZmluZChhbGxQYXJhbWV0ZXJUeXBlc01hdGNoKTtcbiAgaWYgKGV4YWN0TWF0Y2gpIHtcbiAgICByZXR1cm4gbmV3IFNpZ25hdHVyZVdyYXBwZXIoZXhhY3RNYXRjaCwgY29udGV4dCk7XG4gIH1cbiAgLy8gSWYgbm90LCBmYWxsYmFjayB0byBhIG5haXZlIHNlbGVjdGlvblxuICByZXR1cm4gc2lnbmF0dXJlcy5sZW5ndGggPyBuZXcgU2lnbmF0dXJlV3JhcHBlcihzaWduYXR1cmVzWzBdLCBjb250ZXh0KSA6IHVuZGVmaW5lZDtcbn1cblxuY2xhc3MgVHlwZVdyYXBwZXIgaW1wbGVtZW50cyBTeW1ib2wge1xuICBjb25zdHJ1Y3RvcihwdWJsaWMgdHNUeXBlOiB0cy5UeXBlLCBwdWJsaWMgY29udGV4dDogVHlwZUNvbnRleHQpIHtcbiAgICBpZiAoIXRzVHlwZSkge1xuICAgICAgdGhyb3cgRXJyb3IoJ0ludGVybmFsOiBudWxsIHR5cGUnKTtcbiAgICB9XG4gIH1cblxuICBnZXQgbmFtZSgpOiBzdHJpbmcge1xuICAgIHJldHVybiB0aGlzLmNvbnRleHQuY2hlY2tlci50eXBlVG9TdHJpbmcodGhpcy50c1R5cGUpO1xuICB9XG5cbiAgcHVibGljIHJlYWRvbmx5IGtpbmQ6IERlY2xhcmF0aW9uS2luZCA9ICd0eXBlJztcblxuICBwdWJsaWMgcmVhZG9ubHkgbGFuZ3VhZ2U6IHN0cmluZyA9ICd0eXBlc2NyaXB0JztcblxuICBwdWJsaWMgcmVhZG9ubHkgdHlwZTogU3ltYm9sfHVuZGVmaW5lZCA9IHVuZGVmaW5lZDtcblxuICBwdWJsaWMgcmVhZG9ubHkgY29udGFpbmVyOiBTeW1ib2x8dW5kZWZpbmVkID0gdW5kZWZpbmVkO1xuXG4gIHB1YmxpYyByZWFkb25seSBwdWJsaWM6IGJvb2xlYW4gPSB0cnVlO1xuXG4gIGdldCBjYWxsYWJsZSgpOiBib29sZWFuIHtcbiAgICByZXR1cm4gdHlwZUNhbGxhYmxlKHRoaXMudHNUeXBlKTtcbiAgfVxuXG4gIGdldCBudWxsYWJsZSgpOiBib29sZWFuIHtcbiAgICByZXR1cm4gdGhpcy5jb250ZXh0LmNoZWNrZXIuZ2V0Tm9uTnVsbGFibGVUeXBlKHRoaXMudHNUeXBlKSAhPSB0aGlzLnRzVHlwZTtcbiAgfVxuXG4gIGdldCBkb2N1bWVudGF0aW9uKCk6IHRzLlN5bWJvbERpc3BsYXlQYXJ0W10ge1xuICAgIGNvbnN0IHN5bWJvbCA9IHRoaXMudHNUeXBlLmdldFN5bWJvbCgpO1xuICAgIGlmICghc3ltYm9sKSB7XG4gICAgICByZXR1cm4gW107XG4gICAgfVxuICAgIHJldHVybiBzeW1ib2wuZ2V0RG9jdW1lbnRhdGlvbkNvbW1lbnQodGhpcy5jb250ZXh0LmNoZWNrZXIpO1xuICB9XG5cbiAgZ2V0IGRlZmluaXRpb24oKTogRGVmaW5pdGlvbnx1bmRlZmluZWQge1xuICAgIGNvbnN0IHN5bWJvbCA9IHRoaXMudHNUeXBlLmdldFN5bWJvbCgpO1xuICAgIHJldHVybiBzeW1ib2wgPyBkZWZpbml0aW9uRnJvbVRzU3ltYm9sKHN5bWJvbCkgOiB1bmRlZmluZWQ7XG4gIH1cblxuICBtZW1iZXJzKCk6IFN5bWJvbFRhYmxlIHtcbiAgICAvLyBTaG91bGQgY2FsbCBnZXRBcHBhcmVudFByb3BlcnRpZXMoKSBpbnN0ZWFkIG9mIGdldFByb3BlcnRpZXMoKSBiZWNhdXNlXG4gICAgLy8gdGhlIGZvcm1lciBpbmNsdWRlcyBwcm9wZXJ0aWVzIG9uIHRoZSBiYXNlIGNsYXNzIHdoZXJlYXMgdGhlIGxhdHRlciBkb2VzXG4gICAgLy8gbm90LiBUaGlzIHByb3ZpZGVzIHByb3BlcnRpZXMgbGlrZSAuYmluZCgpLCAuY2FsbCgpLCAuYXBwbHkoKSwgZXRjIGZvclxuICAgIC8vIGZ1bmN0aW9ucy5cbiAgICByZXR1cm4gbmV3IFN5bWJvbFRhYmxlV3JhcHBlcih0aGlzLnRzVHlwZS5nZXRBcHBhcmVudFByb3BlcnRpZXMoKSwgdGhpcy5jb250ZXh0LCB0aGlzLnRzVHlwZSk7XG4gIH1cblxuICBzaWduYXR1cmVzKCk6IFNpZ25hdHVyZVtdIHtcbiAgICByZXR1cm4gc2lnbmF0dXJlc09mKHRoaXMudHNUeXBlLCB0aGlzLmNvbnRleHQpO1xuICB9XG5cbiAgc2VsZWN0U2lnbmF0dXJlKHR5cGVzOiBTeW1ib2xbXSk6IFNpZ25hdHVyZXx1bmRlZmluZWQge1xuICAgIHJldHVybiBzZWxlY3RTaWduYXR1cmUodGhpcy50c1R5cGUsIHRoaXMuY29udGV4dCwgdHlwZXMpO1xuICB9XG5cbiAgaW5kZXhlZCh0eXBlOiBTeW1ib2wsIHZhbHVlOiBhbnkpOiBTeW1ib2x8dW5kZWZpbmVkIHtcbiAgICBpZiAoISh0eXBlIGluc3RhbmNlb2YgVHlwZVdyYXBwZXIpKSByZXR1cm47XG5cbiAgICBjb25zdCB0eXBlS2luZCA9IHR5cGVLaW5kT2YodHlwZS50c1R5cGUpO1xuICAgIHN3aXRjaCAodHlwZUtpbmQpIHtcbiAgICAgIGNhc2UgQnVpbHRpblR5cGUuTnVtYmVyOlxuICAgICAgICBjb25zdCBuVHlwZSA9IHRoaXMudHNUeXBlLmdldE51bWJlckluZGV4VHlwZSgpO1xuICAgICAgICBpZiAoblR5cGUpIHtcbiAgICAgICAgICAvLyBnZXQgdGhlIHJpZ2h0IHR1cGxlIHR5cGUgYnkgdmFsdWUsIGxpa2UgJ3ZhciB0OiBbbnVtYmVyLCBzdHJpbmddOydcbiAgICAgICAgICBpZiAoblR5cGUuaXNVbmlvbigpKSB7XG4gICAgICAgICAgICAvLyByZXR1cm4gdW5kZWZpbmVkIGlmIGFycmF5IGluZGV4IG91dCBvZiBib3VuZC5cbiAgICAgICAgICAgIHJldHVybiBuVHlwZS50eXBlc1t2YWx1ZV0gJiYgbmV3IFR5cGVXcmFwcGVyKG5UeXBlLnR5cGVzW3ZhbHVlXSwgdGhpcy5jb250ZXh0KTtcbiAgICAgICAgICB9XG4gICAgICAgICAgcmV0dXJuIG5ldyBUeXBlV3JhcHBlcihuVHlwZSwgdGhpcy5jb250ZXh0KTtcbiAgICAgICAgfVxuICAgICAgICByZXR1cm4gdW5kZWZpbmVkO1xuICAgICAgY2FzZSBCdWlsdGluVHlwZS5TdHJpbmc6XG4gICAgICAgIGNvbnN0IHNUeXBlID0gdGhpcy50c1R5cGUuZ2V0U3RyaW5nSW5kZXhUeXBlKCk7XG4gICAgICAgIHJldHVybiBzVHlwZSAmJiBuZXcgVHlwZVdyYXBwZXIoc1R5cGUsIHRoaXMuY29udGV4dCk7XG4gICAgfVxuICB9XG5cbiAgdHlwZUFyZ3VtZW50cygpOiBTeW1ib2xbXXx1bmRlZmluZWQge1xuICAgIGlmICghaXNSZWZlcmVuY2VUeXBlKHRoaXMudHNUeXBlKSkgcmV0dXJuO1xuXG4gICAgY29uc3QgdHlwZVJlZmVyZW5jZSA9ICh0aGlzLnRzVHlwZSBhcyB0cy5UeXBlUmVmZXJlbmNlKTtcbiAgICBsZXQgdHlwZUFyZ3VtZW50czogUmVhZG9ubHlBcnJheTx0cy5UeXBlPnx1bmRlZmluZWQ7XG4gICAgdHlwZUFyZ3VtZW50cyA9IHRoaXMuY29udGV4dC5jaGVja2VyLmdldFR5cGVBcmd1bWVudHModHlwZVJlZmVyZW5jZSk7XG4gICAgaWYgKCF0eXBlQXJndW1lbnRzKSByZXR1cm4gdW5kZWZpbmVkO1xuICAgIHJldHVybiB0eXBlQXJndW1lbnRzLm1hcCh0YSA9PiBuZXcgVHlwZVdyYXBwZXIodGEsIHRoaXMuY29udGV4dCkpO1xuICB9XG59XG5cbi8vIElmIHN0cmluZ0luZGV4VHlwZSBhIHByaW1pdGl2ZSB0eXBlKGUuZy4gJ3N0cmluZycpLCB0aGUgU3ltYm9sIGlzIHVuZGVmaW5lZDtcbi8vIGFuZCBpbiBBc3RUeXBlLnJlc29sdmVQcm9wZXJ0eVJlYWQgbWV0aG9kLCB0aGUgU3ltYm9sLnR5cGUgc2hvdWxkIGdldCB0aGUgcmlnaHQgdHlwZS5cbmNsYXNzIFN0cmluZ0luZGV4VHlwZVdyYXBwZXIgZXh0ZW5kcyBUeXBlV3JhcHBlciB7XG4gIHB1YmxpYyByZWFkb25seSB0eXBlID0gbmV3IFR5cGVXcmFwcGVyKHRoaXMudHNUeXBlLCB0aGlzLmNvbnRleHQpO1xufVxuXG5jbGFzcyBTeW1ib2xXcmFwcGVyIGltcGxlbWVudHMgU3ltYm9sIHtcbiAgcHJpdmF0ZSBzeW1ib2w6IHRzLlN5bWJvbDtcbiAgcHJpdmF0ZSBfbWVtYmVycz86IFN5bWJvbFRhYmxlO1xuXG4gIHB1YmxpYyByZWFkb25seSBudWxsYWJsZTogYm9vbGVhbiA9IGZhbHNlO1xuICBwdWJsaWMgcmVhZG9ubHkgbGFuZ3VhZ2U6IHN0cmluZyA9ICd0eXBlc2NyaXB0JztcblxuICBjb25zdHJ1Y3RvcihcbiAgICAgIHN5bWJvbDogdHMuU3ltYm9sLFxuICAgICAgLyoqIFR5cGVTY3JpcHQgdHlwZSBjb250ZXh0IG9mIHRoZSBzeW1ib2wuICovXG4gICAgICBwcml2YXRlIGNvbnRleHQ6IFR5cGVDb250ZXh0LFxuICAgICAgLyoqXG4gICAgICAgKiBUeXBlIG9mIHRoZSBUeXBlU2NyaXB0IHN5bWJvbCwgaWYga25vd24uIElmIG5vdCBwcm92aWRlZCwgdGhlIHR5cGUgb2YgdGhlIHN5bWJvbFxuICAgICAgICogd2lsbCBiZSBkZXRlcm1pbmVkIGR5bmFtaWNhbGx5OyBzZWUgYFN5bWJvbFdyYXBwZXIjdHNUeXBlYC5cbiAgICAgICAqL1xuICAgICAgcHJpdmF0ZSBfdHNUeXBlPzogdHMuVHlwZSkge1xuICAgIHRoaXMuc3ltYm9sID0gc3ltYm9sICYmIGNvbnRleHQgJiYgKHN5bWJvbC5mbGFncyAmIHRzLlN5bWJvbEZsYWdzLkFsaWFzKSA/XG4gICAgICAgIGNvbnRleHQuY2hlY2tlci5nZXRBbGlhc2VkU3ltYm9sKHN5bWJvbCkgOlxuICAgICAgICBzeW1ib2w7XG4gIH1cblxuICBnZXQgbmFtZSgpOiBzdHJpbmcge1xuICAgIHJldHVybiB0aGlzLnN5bWJvbC5uYW1lO1xuICB9XG5cbiAgZ2V0IGtpbmQoKTogRGVjbGFyYXRpb25LaW5kIHtcbiAgICByZXR1cm4gdGhpcy5jYWxsYWJsZSA/ICdtZXRob2QnIDogJ3Byb3BlcnR5JztcbiAgfVxuXG4gIGdldCB0eXBlKCk6IFR5cGVXcmFwcGVyIHtcbiAgICByZXR1cm4gbmV3IFR5cGVXcmFwcGVyKHRoaXMudHNUeXBlLCB0aGlzLmNvbnRleHQpO1xuICB9XG5cbiAgZ2V0IGNvbnRhaW5lcigpOiBTeW1ib2x8dW5kZWZpbmVkIHtcbiAgICByZXR1cm4gZ2V0Q29udGFpbmVyT2YodGhpcy5zeW1ib2wsIHRoaXMuY29udGV4dCk7XG4gIH1cblxuICBnZXQgcHVibGljKCk6IGJvb2xlYW4ge1xuICAgIC8vIFN5bWJvbHMgdGhhdCBhcmUgbm90IGV4cGxpY2l0bHkgbWFkZSBwcml2YXRlIGFyZSBwdWJsaWMuXG4gICAgcmV0dXJuICFpc1N5bWJvbFByaXZhdGUodGhpcy5zeW1ib2wpO1xuICB9XG5cbiAgZ2V0IGNhbGxhYmxlKCk6IGJvb2xlYW4ge1xuICAgIHJldHVybiB0eXBlQ2FsbGFibGUodGhpcy50c1R5cGUpO1xuICB9XG5cbiAgZ2V0IGRlZmluaXRpb24oKTogRGVmaW5pdGlvbiB7XG4gICAgcmV0dXJuIGRlZmluaXRpb25Gcm9tVHNTeW1ib2wodGhpcy5zeW1ib2wpO1xuICB9XG5cbiAgZ2V0IGRvY3VtZW50YXRpb24oKTogdHMuU3ltYm9sRGlzcGxheVBhcnRbXSB7XG4gICAgcmV0dXJuIHRoaXMuc3ltYm9sLmdldERvY3VtZW50YXRpb25Db21tZW50KHRoaXMuY29udGV4dC5jaGVja2VyKTtcbiAgfVxuXG4gIG1lbWJlcnMoKTogU3ltYm9sVGFibGUge1xuICAgIGlmICghdGhpcy5fbWVtYmVycykge1xuICAgICAgaWYgKCh0aGlzLnN5bWJvbC5mbGFncyAmICh0cy5TeW1ib2xGbGFncy5DbGFzcyB8IHRzLlN5bWJvbEZsYWdzLkludGVyZmFjZSkpICE9IDApIHtcbiAgICAgICAgY29uc3QgZGVjbGFyZWRUeXBlID0gdGhpcy5jb250ZXh0LmNoZWNrZXIuZ2V0RGVjbGFyZWRUeXBlT2ZTeW1ib2wodGhpcy5zeW1ib2wpO1xuICAgICAgICBjb25zdCB0eXBlV3JhcHBlciA9IG5ldyBUeXBlV3JhcHBlcihkZWNsYXJlZFR5cGUsIHRoaXMuY29udGV4dCk7XG4gICAgICAgIHRoaXMuX21lbWJlcnMgPSB0eXBlV3JhcHBlci5tZW1iZXJzKCk7XG4gICAgICB9IGVsc2Uge1xuICAgICAgICB0aGlzLl9tZW1iZXJzID0gbmV3IFN5bWJvbFRhYmxlV3JhcHBlcih0aGlzLnN5bWJvbC5tZW1iZXJzISwgdGhpcy5jb250ZXh0LCB0aGlzLnRzVHlwZSk7XG4gICAgICB9XG4gICAgfVxuICAgIHJldHVybiB0aGlzLl9tZW1iZXJzO1xuICB9XG5cbiAgc2lnbmF0dXJlcygpOiBTaWduYXR1cmVbXSB7XG4gICAgcmV0dXJuIHNpZ25hdHVyZXNPZih0aGlzLnRzVHlwZSwgdGhpcy5jb250ZXh0KTtcbiAgfVxuXG4gIHNlbGVjdFNpZ25hdHVyZSh0eXBlczogU3ltYm9sW10pOiBTaWduYXR1cmV8dW5kZWZpbmVkIHtcbiAgICByZXR1cm4gc2VsZWN0U2lnbmF0dXJlKHRoaXMudHNUeXBlLCB0aGlzLmNvbnRleHQsIHR5cGVzKTtcbiAgfVxuXG4gIGluZGV4ZWQoX2FyZ3VtZW50OiBTeW1ib2wpOiBTeW1ib2x8dW5kZWZpbmVkIHtcbiAgICByZXR1cm4gdW5kZWZpbmVkO1xuICB9XG5cbiAgdHlwZUFyZ3VtZW50cygpOiBTeW1ib2xbXXx1bmRlZmluZWQge1xuICAgIHJldHVybiB0aGlzLnR5cGUudHlwZUFyZ3VtZW50cygpO1xuICB9XG5cbiAgcHJpdmF0ZSBnZXQgdHNUeXBlKCk6IHRzLlR5cGUge1xuICAgIGxldCB0eXBlID0gdGhpcy5fdHNUeXBlO1xuICAgIGlmICghdHlwZSkge1xuICAgICAgdHlwZSA9IHRoaXMuX3RzVHlwZSA9XG4gICAgICAgICAgdGhpcy5jb250ZXh0LmNoZWNrZXIuZ2V0VHlwZU9mU3ltYm9sQXRMb2NhdGlvbih0aGlzLnN5bWJvbCwgdGhpcy5jb250ZXh0Lm5vZGUpO1xuICAgIH1cbiAgICByZXR1cm4gdHlwZTtcbiAgfVxufVxuXG5jbGFzcyBEZWNsYXJlZFN5bWJvbCBpbXBsZW1lbnRzIFN5bWJvbCB7XG4gIHB1YmxpYyByZWFkb25seSBsYW5ndWFnZTogc3RyaW5nID0gJ25nLXRlbXBsYXRlJztcblxuICBwdWJsaWMgcmVhZG9ubHkgbnVsbGFibGU6IGJvb2xlYW4gPSBmYWxzZTtcblxuICBwdWJsaWMgcmVhZG9ubHkgcHVibGljOiBib29sZWFuID0gdHJ1ZTtcblxuICBjb25zdHJ1Y3Rvcihwcml2YXRlIGRlY2xhcmF0aW9uOiBTeW1ib2xEZWNsYXJhdGlvbikge31cblxuICBnZXQgbmFtZSgpIHtcbiAgICByZXR1cm4gdGhpcy5kZWNsYXJhdGlvbi5uYW1lO1xuICB9XG5cbiAgZ2V0IGtpbmQoKSB7XG4gICAgcmV0dXJuIHRoaXMuZGVjbGFyYXRpb24ua2luZDtcbiAgfVxuXG4gIGdldCBjb250YWluZXIoKTogU3ltYm9sfHVuZGVmaW5lZCB7XG4gICAgcmV0dXJuIHVuZGVmaW5lZDtcbiAgfVxuXG4gIGdldCB0eXBlKCk6IFN5bWJvbCB7XG4gICAgcmV0dXJuIHRoaXMuZGVjbGFyYXRpb24udHlwZTtcbiAgfVxuXG4gIGdldCBjYWxsYWJsZSgpOiBib29sZWFuIHtcbiAgICByZXR1cm4gdGhpcy50eXBlLmNhbGxhYmxlO1xuICB9XG5cbiAgZ2V0IGRlZmluaXRpb24oKTogRGVmaW5pdGlvbiB7XG4gICAgcmV0dXJuIHRoaXMuZGVjbGFyYXRpb24uZGVmaW5pdGlvbjtcbiAgfVxuXG4gIGdldCBkb2N1bWVudGF0aW9uKCk6IHRzLlN5bWJvbERpc3BsYXlQYXJ0W10ge1xuICAgIHJldHVybiB0aGlzLmRlY2xhcmF0aW9uLnR5cGUuZG9jdW1lbnRhdGlvbjtcbiAgfVxuXG4gIG1lbWJlcnMoKTogU3ltYm9sVGFibGUge1xuICAgIHJldHVybiB0aGlzLnR5cGUubWVtYmVycygpO1xuICB9XG5cbiAgc2lnbmF0dXJlcygpOiBTaWduYXR1cmVbXSB7XG4gICAgcmV0dXJuIHRoaXMudHlwZS5zaWduYXR1cmVzKCk7XG4gIH1cblxuICBzZWxlY3RTaWduYXR1cmUodHlwZXM6IFN5bWJvbFtdKTogU2lnbmF0dXJlfHVuZGVmaW5lZCB7XG4gICAgcmV0dXJuIHRoaXMudHlwZS5zZWxlY3RTaWduYXR1cmUodHlwZXMpO1xuICB9XG5cbiAgdHlwZUFyZ3VtZW50cygpOiBTeW1ib2xbXXx1bmRlZmluZWQge1xuICAgIHJldHVybiB0aGlzLnR5cGUudHlwZUFyZ3VtZW50cygpO1xuICB9XG5cbiAgaW5kZXhlZChfYXJndW1lbnQ6IFN5bWJvbCk6IFN5bWJvbHx1bmRlZmluZWQge1xuICAgIHJldHVybiB1bmRlZmluZWQ7XG4gIH1cbn1cblxuY2xhc3MgU2lnbmF0dXJlV3JhcHBlciBpbXBsZW1lbnRzIFNpZ25hdHVyZSB7XG4gIGNvbnN0cnVjdG9yKHByaXZhdGUgc2lnbmF0dXJlOiB0cy5TaWduYXR1cmUsIHByaXZhdGUgY29udGV4dDogVHlwZUNvbnRleHQpIHt9XG5cbiAgZ2V0IGFyZ3VtZW50cygpOiBTeW1ib2xUYWJsZSB7XG4gICAgcmV0dXJuIG5ldyBTeW1ib2xUYWJsZVdyYXBwZXIodGhpcy5zaWduYXR1cmUuZ2V0UGFyYW1ldGVycygpLCB0aGlzLmNvbnRleHQpO1xuICB9XG5cbiAgZ2V0IHJlc3VsdCgpOiBTeW1ib2wge1xuICAgIHJldHVybiBuZXcgVHlwZVdyYXBwZXIodGhpcy5zaWduYXR1cmUuZ2V0UmV0dXJuVHlwZSgpLCB0aGlzLmNvbnRleHQpO1xuICB9XG59XG5cbmNsYXNzIFNpZ25hdHVyZVJlc3VsdE92ZXJyaWRlIGltcGxlbWVudHMgU2lnbmF0dXJlIHtcbiAgY29uc3RydWN0b3IocHJpdmF0ZSBzaWduYXR1cmU6IFNpZ25hdHVyZSwgcHJpdmF0ZSByZXN1bHRUeXBlOiBTeW1ib2wpIHt9XG5cbiAgZ2V0IGFyZ3VtZW50cygpOiBTeW1ib2xUYWJsZSB7XG4gICAgcmV0dXJuIHRoaXMuc2lnbmF0dXJlLmFyZ3VtZW50cztcbiAgfVxuXG4gIGdldCByZXN1bHQoKTogU3ltYm9sIHtcbiAgICByZXR1cm4gdGhpcy5yZXN1bHRUeXBlO1xuICB9XG59XG5cbmZ1bmN0aW9uIHRvU3ltYm9sVGFibGVGYWN0b3J5KHN5bWJvbHM6IHRzLlN5bWJvbFtdKTogdHMuU3ltYm9sVGFibGUge1xuICAvLyDiiIAgVHlwZXNjcmlwdCB2ZXJzaW9uID49IDIuMiwgYFN5bWJvbFRhYmxlYCBpcyBpbXBsZW1lbnRlZCBhcyBhbiBFUzYgYE1hcGBcbiAgY29uc3QgcmVzdWx0ID0gbmV3IE1hcDxzdHJpbmcsIHRzLlN5bWJvbD4oKTtcbiAgZm9yIChjb25zdCBzeW1ib2wgb2Ygc3ltYm9scykge1xuICAgIHJlc3VsdC5zZXQoc3ltYm9sLm5hbWUsIHN5bWJvbCk7XG4gIH1cblxuICByZXR1cm4gcmVzdWx0IGFzIHRzLlN5bWJvbFRhYmxlO1xufVxuXG5mdW5jdGlvbiB0b1N5bWJvbHMoc3ltYm9sVGFibGU6IHRzLlN5bWJvbFRhYmxlfHVuZGVmaW5lZCk6IHRzLlN5bWJvbFtdIHtcbiAgaWYgKCFzeW1ib2xUYWJsZSkgcmV0dXJuIFtdO1xuXG4gIGNvbnN0IHRhYmxlID0gc3ltYm9sVGFibGUgYXMgYW55O1xuXG4gIGlmICh0eXBlb2YgdGFibGUudmFsdWVzID09PSAnZnVuY3Rpb24nKSB7XG4gICAgcmV0dXJuIEFycmF5LmZyb20odGFibGUudmFsdWVzKCkpIGFzIHRzLlN5bWJvbFtdO1xuICB9XG5cbiAgY29uc3QgcmVzdWx0OiB0cy5TeW1ib2xbXSA9IFtdO1xuXG4gIGNvbnN0IG93biA9IHR5cGVvZiB0YWJsZS5oYXNPd25Qcm9wZXJ0eSA9PT0gJ2Z1bmN0aW9uJyA/XG4gICAgICAobmFtZTogc3RyaW5nKSA9PiB0YWJsZS5oYXNPd25Qcm9wZXJ0eShuYW1lKSA6XG4gICAgICAobmFtZTogc3RyaW5nKSA9PiAhIXRhYmxlW25hbWVdO1xuXG4gIGZvciAoY29uc3QgbmFtZSBpbiB0YWJsZSkge1xuICAgIGlmIChvd24obmFtZSkpIHtcbiAgICAgIHJlc3VsdC5wdXNoKHRhYmxlW25hbWVdKTtcbiAgICB9XG4gIH1cbiAgcmV0dXJuIHJlc3VsdDtcbn1cblxuY2xhc3MgU3ltYm9sVGFibGVXcmFwcGVyIGltcGxlbWVudHMgU3ltYm9sVGFibGUge1xuICBwcml2YXRlIHN5bWJvbHM6IHRzLlN5bWJvbFtdO1xuICBwcml2YXRlIHN5bWJvbFRhYmxlOiB0cy5TeW1ib2xUYWJsZTtcbiAgcHJpdmF0ZSBzdHJpbmdJbmRleFR5cGU/OiB0cy5UeXBlO1xuXG4gIC8qKlxuICAgKiBDcmVhdGVzIGEgcXVlcnlhYmxlIHRhYmxlIG9mIHN5bWJvbHMgYmVsb25naW5nIHRvIGEgVHlwZVNjcmlwdCBlbnRpdHkuXG4gICAqIEBwYXJhbSBzeW1ib2xzIHN5bWJvbHMgdG8gcXVlcnkgYmVsb25naW5nIHRvIHRoZSBlbnRpdHlcbiAgICogQHBhcmFtIGNvbnRleHQgcHJvZ3JhbSBjb250ZXh0XG4gICAqIEBwYXJhbSB0eXBlIG9yaWdpbmFsIFR5cGVTY3JpcHQgdHlwZSBvZiBlbnRpdHkgb3duaW5nIHRoZSBzeW1ib2xzLCBpZiBrbm93blxuICAgKi9cbiAgY29uc3RydWN0b3Ioc3ltYm9sczogdHMuU3ltYm9sVGFibGV8dHMuU3ltYm9sW10sIHByaXZhdGUgY29udGV4dDogVHlwZUNvbnRleHQsIHR5cGU/OiB0cy5UeXBlKSB7XG4gICAgc3ltYm9scyA9IHN5bWJvbHMgfHwgW107XG5cbiAgICBpZiAoQXJyYXkuaXNBcnJheShzeW1ib2xzKSkge1xuICAgICAgdGhpcy5zeW1ib2xzID0gc3ltYm9scztcbiAgICAgIHRoaXMuc3ltYm9sVGFibGUgPSB0b1N5bWJvbFRhYmxlRmFjdG9yeShzeW1ib2xzKTtcbiAgICB9IGVsc2Uge1xuICAgICAgdGhpcy5zeW1ib2xzID0gdG9TeW1ib2xzKHN5bWJvbHMpO1xuICAgICAgdGhpcy5zeW1ib2xUYWJsZSA9IHN5bWJvbHM7XG4gICAgfVxuXG4gICAgaWYgKHR5cGUpIHtcbiAgICAgIHRoaXMuc3RyaW5nSW5kZXhUeXBlID0gdHlwZS5nZXRTdHJpbmdJbmRleFR5cGUoKTtcbiAgICB9XG4gIH1cblxuICBnZXQgc2l6ZSgpOiBudW1iZXIge1xuICAgIHJldHVybiB0aGlzLnN5bWJvbHMubGVuZ3RoO1xuICB9XG5cbiAgZ2V0KGtleTogc3RyaW5nKTogU3ltYm9sfHVuZGVmaW5lZCB7XG4gICAgY29uc3Qgc3ltYm9sID0gZ2V0RnJvbVN5bWJvbFRhYmxlKHRoaXMuc3ltYm9sVGFibGUsIGtleSk7XG4gICAgaWYgKHN5bWJvbCkge1xuICAgICAgcmV0dXJuIG5ldyBTeW1ib2xXcmFwcGVyKHN5bWJvbCwgdGhpcy5jb250ZXh0KTtcbiAgICB9XG5cbiAgICBpZiAodGhpcy5zdHJpbmdJbmRleFR5cGUpIHtcbiAgICAgIC8vIElmIHRoZSBrZXkgZG9lcyBub3QgZXhpc3QgYXMgYW4gZXhwbGljaXQgc3ltYm9sIG9uIHRoZSB0eXBlLCBpdCBtYXkgYmUgYWNjZXNzaW5nIGEgc3RyaW5nXG4gICAgICAvLyBpbmRleCBzaWduYXR1cmUgdXNpbmcgZG90IG5vdGF0aW9uOlxuICAgICAgLy9cbiAgICAgIC8vICAgY29uc3Qgb2JqPFQ+OiB7IFtrZXk6IHN0cmluZ106IFQgfTtcbiAgICAgIC8vICAgb2JqLnN0cmluZ0luZGV4IC8vIGVxdWl2YWxlbnQgdG8gb2JqWydzdHJpbmdJbmRleCddO1xuICAgICAgLy9cbiAgICAgIC8vIEluIHRoaXMgY2FzZSwgcmV0dXJuIHRoZSB0eXBlIGluZGV4ZWQgYnkgYW4gYXJiaXRyYXJ5IHN0cmluZyBrZXkuXG4gICAgICByZXR1cm4gbmV3IFN0cmluZ0luZGV4VHlwZVdyYXBwZXIodGhpcy5zdHJpbmdJbmRleFR5cGUsIHRoaXMuY29udGV4dCk7XG4gICAgfVxuXG4gICAgcmV0dXJuIHVuZGVmaW5lZDtcbiAgfVxuXG4gIGhhcyhrZXk6IHN0cmluZyk6IGJvb2xlYW4ge1xuICAgIGNvbnN0IHRhYmxlOiBhbnkgPSB0aGlzLnN5bWJvbFRhYmxlO1xuICAgIHJldHVybiAoKHR5cGVvZiB0YWJsZS5oYXMgPT09ICdmdW5jdGlvbicpID8gdGFibGUuaGFzKGtleSkgOiB0YWJsZVtrZXldICE9IG51bGwpIHx8XG4gICAgICAgIHRoaXMuc3RyaW5nSW5kZXhUeXBlICE9PSB1bmRlZmluZWQ7XG4gIH1cblxuICB2YWx1ZXMoKTogU3ltYm9sW10ge1xuICAgIHJldHVybiB0aGlzLnN5bWJvbHMubWFwKHMgPT4gbmV3IFN5bWJvbFdyYXBwZXIocywgdGhpcy5jb250ZXh0KSk7XG4gIH1cbn1cblxuY2xhc3MgTWFwU3ltYm9sVGFibGUgaW1wbGVtZW50cyBTeW1ib2xUYWJsZSB7XG4gIHByaXZhdGUgbWFwID0gbmV3IE1hcDxzdHJpbmcsIFN5bWJvbD4oKTtcbiAgcHJpdmF0ZSBfdmFsdWVzOiBTeW1ib2xbXSA9IFtdO1xuXG4gIGdldCBzaXplKCk6IG51bWJlciB7XG4gICAgcmV0dXJuIHRoaXMubWFwLnNpemU7XG4gIH1cblxuICBnZXQoa2V5OiBzdHJpbmcpOiBTeW1ib2x8dW5kZWZpbmVkIHtcbiAgICByZXR1cm4gdGhpcy5tYXAuZ2V0KGtleSk7XG4gIH1cblxuICBhZGQoc3ltYm9sOiBTeW1ib2wpIHtcbiAgICBpZiAodGhpcy5tYXAuaGFzKHN5bWJvbC5uYW1lKSkge1xuICAgICAgY29uc3QgcHJldmlvdXMgPSB0aGlzLm1hcC5nZXQoc3ltYm9sLm5hbWUpITtcbiAgICAgIHRoaXMuX3ZhbHVlc1t0aGlzLl92YWx1ZXMuaW5kZXhPZihwcmV2aW91cyldID0gc3ltYm9sO1xuICAgIH1cbiAgICB0aGlzLm1hcC5zZXQoc3ltYm9sLm5hbWUsIHN5bWJvbCk7XG4gICAgdGhpcy5fdmFsdWVzLnB1c2goc3ltYm9sKTtcbiAgfVxuXG4gIGFkZEFsbChzeW1ib2xzOiBTeW1ib2xbXSkge1xuICAgIGZvciAoY29uc3Qgc3ltYm9sIG9mIHN5bWJvbHMpIHtcbiAgICAgIHRoaXMuYWRkKHN5bWJvbCk7XG4gICAgfVxuICB9XG5cbiAgaGFzKGtleTogc3RyaW5nKTogYm9vbGVhbiB7XG4gICAgcmV0dXJuIHRoaXMubWFwLmhhcyhrZXkpO1xuICB9XG5cbiAgdmFsdWVzKCk6IFN5bWJvbFtdIHtcbiAgICAvLyBTd2l0Y2ggdG8gdGhpcy5tYXAudmFsdWVzIG9uY2UgaXRlcmFibGVzIGFyZSBzdXBwb3J0ZWQgYnkgdGhlIHRhcmdldCBsYW5ndWFnZS5cbiAgICByZXR1cm4gdGhpcy5fdmFsdWVzO1xuICB9XG59XG5cbmNsYXNzIFBpcGVzVGFibGUgaW1wbGVtZW50cyBTeW1ib2xUYWJsZSB7XG4gIGNvbnN0cnVjdG9yKHByaXZhdGUgcGlwZXM6IENvbXBpbGVQaXBlU3VtbWFyeVtdLCBwcml2YXRlIGNvbnRleHQ6IFR5cGVDb250ZXh0KSB7fVxuXG4gIGdldCBzaXplKCkge1xuICAgIHJldHVybiB0aGlzLnBpcGVzLmxlbmd0aDtcbiAgfVxuXG4gIGdldChrZXk6IHN0cmluZyk6IFN5bWJvbHx1bmRlZmluZWQge1xuICAgIGNvbnN0IHBpcGUgPSB0aGlzLnBpcGVzLmZpbmQocGlwZSA9PiBwaXBlLm5hbWUgPT0ga2V5KTtcbiAgICBpZiAocGlwZSkge1xuICAgICAgcmV0dXJuIG5ldyBQaXBlU3ltYm9sKHBpcGUsIHRoaXMuY29udGV4dCk7XG4gICAgfVxuICB9XG5cbiAgaGFzKGtleTogc3RyaW5nKTogYm9vbGVhbiB7XG4gICAgcmV0dXJuIHRoaXMucGlwZXMuZmluZChwaXBlID0+IHBpcGUubmFtZSA9PSBrZXkpICE9IG51bGw7XG4gIH1cblxuICB2YWx1ZXMoKTogU3ltYm9sW10ge1xuICAgIHJldHVybiB0aGlzLnBpcGVzLm1hcChwaXBlID0+IG5ldyBQaXBlU3ltYm9sKHBpcGUsIHRoaXMuY29udGV4dCkpO1xuICB9XG59XG5cbi8vIFRoaXMgbWF0Y2hlcyAuZC50cyBmaWxlcyB0aGF0IGxvb2sgbGlrZSBcIi4uLi88cGFja2FnZS1uYW1lPi88cGFja2FnZS1uYW1lPi5kLnRzXCIsXG5jb25zdCBJTkRFWF9QQVRURVJOID0gL1tcXFxcL10oW15cXFxcL10rKVtcXFxcL11cXDFcXC5kXFwudHMkLztcblxuY2xhc3MgUGlwZVN5bWJvbCBpbXBsZW1lbnRzIFN5bWJvbCB7XG4gIHByaXZhdGUgX3RzVHlwZTogdHMuVHlwZXx1bmRlZmluZWQ7XG4gIHB1YmxpYyByZWFkb25seSBraW5kOiBEZWNsYXJhdGlvbktpbmQgPSAncGlwZSc7XG4gIHB1YmxpYyByZWFkb25seSBsYW5ndWFnZTogc3RyaW5nID0gJ3R5cGVzY3JpcHQnO1xuICBwdWJsaWMgcmVhZG9ubHkgY29udGFpbmVyOiBTeW1ib2x8dW5kZWZpbmVkID0gdW5kZWZpbmVkO1xuICBwdWJsaWMgcmVhZG9ubHkgY2FsbGFibGU6IGJvb2xlYW4gPSB0cnVlO1xuICBwdWJsaWMgcmVhZG9ubHkgbnVsbGFibGU6IGJvb2xlYW4gPSBmYWxzZTtcbiAgcHVibGljIHJlYWRvbmx5IHB1YmxpYzogYm9vbGVhbiA9IHRydWU7XG5cbiAgY29uc3RydWN0b3IocHJpdmF0ZSBwaXBlOiBDb21waWxlUGlwZVN1bW1hcnksIHByaXZhdGUgY29udGV4dDogVHlwZUNvbnRleHQpIHt9XG5cbiAgZ2V0IG5hbWUoKTogc3RyaW5nIHtcbiAgICByZXR1cm4gdGhpcy5waXBlLm5hbWU7XG4gIH1cblxuICBnZXQgdHlwZSgpOiBUeXBlV3JhcHBlciB7XG4gICAgcmV0dXJuIG5ldyBUeXBlV3JhcHBlcih0aGlzLnRzVHlwZSwgdGhpcy5jb250ZXh0KTtcbiAgfVxuXG4gIGdldCBkZWZpbml0aW9uKCk6IERlZmluaXRpb258dW5kZWZpbmVkIHtcbiAgICBjb25zdCBzeW1ib2wgPSB0aGlzLnRzVHlwZS5nZXRTeW1ib2woKTtcbiAgICByZXR1cm4gc3ltYm9sID8gZGVmaW5pdGlvbkZyb21Uc1N5bWJvbChzeW1ib2wpIDogdW5kZWZpbmVkO1xuICB9XG5cbiAgZ2V0IGRvY3VtZW50YXRpb24oKTogdHMuU3ltYm9sRGlzcGxheVBhcnRbXSB7XG4gICAgY29uc3Qgc3ltYm9sID0gdGhpcy50c1R5cGUuZ2V0U3ltYm9sKCk7XG4gICAgaWYgKCFzeW1ib2wpIHtcbiAgICAgIHJldHVybiBbXTtcbiAgICB9XG4gICAgcmV0dXJuIHN5bWJvbC5nZXREb2N1bWVudGF0aW9uQ29tbWVudCh0aGlzLmNvbnRleHQuY2hlY2tlcik7XG4gIH1cblxuICBtZW1iZXJzKCk6IFN5bWJvbFRhYmxlIHtcbiAgICByZXR1cm4gRW1wdHlUYWJsZS5pbnN0YW5jZTtcbiAgfVxuXG4gIHNpZ25hdHVyZXMoKTogU2lnbmF0dXJlW10ge1xuICAgIHJldHVybiBzaWduYXR1cmVzT2YodGhpcy50c1R5cGUsIHRoaXMuY29udGV4dCk7XG4gIH1cblxuICBzZWxlY3RTaWduYXR1cmUodHlwZXM6IFN5bWJvbFtdKTogU2lnbmF0dXJlfHVuZGVmaW5lZCB7XG4gICAgbGV0IHNpZ25hdHVyZSA9IHNlbGVjdFNpZ25hdHVyZSh0aGlzLnRzVHlwZSwgdGhpcy5jb250ZXh0LCB0eXBlcykhO1xuICAgIGlmICh0eXBlcy5sZW5ndGggPiAwKSB7XG4gICAgICBjb25zdCBwYXJhbWV0ZXJUeXBlID0gdHlwZXNbMF07XG4gICAgICBsZXQgcmVzdWx0VHlwZTogU3ltYm9sfHVuZGVmaW5lZCA9IHVuZGVmaW5lZDtcbiAgICAgIHN3aXRjaCAodGhpcy5uYW1lKSB7XG4gICAgICAgIGNhc2UgJ2FzeW5jJzpcbiAgICAgICAgICAvLyBHZXQgdHlwZSBhcmd1bWVudCBvZiAnT2JzZXJ2YWJsZScsICdQcm9taXNlJywgb3IgJ0V2ZW50RW1pdHRlcicuXG4gICAgICAgICAgY29uc3QgdEFyZ3MgPSBwYXJhbWV0ZXJUeXBlLnR5cGVBcmd1bWVudHMoKTtcbiAgICAgICAgICBpZiAodEFyZ3MgJiYgdEFyZ3MubGVuZ3RoID09PSAxKSB7XG4gICAgICAgICAgICByZXN1bHRUeXBlID0gdEFyZ3NbMF07XG4gICAgICAgICAgfVxuICAgICAgICAgIGJyZWFrO1xuICAgICAgICBjYXNlICdzbGljZSc6XG4gICAgICAgICAgcmVzdWx0VHlwZSA9IHBhcmFtZXRlclR5cGU7XG4gICAgICAgICAgYnJlYWs7XG4gICAgICB9XG4gICAgICBpZiAocmVzdWx0VHlwZSkge1xuICAgICAgICBzaWduYXR1cmUgPSBuZXcgU2lnbmF0dXJlUmVzdWx0T3ZlcnJpZGUoc2lnbmF0dXJlLCByZXN1bHRUeXBlKTtcbiAgICAgIH1cbiAgICB9XG4gICAgcmV0dXJuIHNpZ25hdHVyZTtcbiAgfVxuXG4gIGluZGV4ZWQoX2FyZ3VtZW50OiBTeW1ib2wpOiBTeW1ib2x8dW5kZWZpbmVkIHtcbiAgICByZXR1cm4gdW5kZWZpbmVkO1xuICB9XG5cbiAgdHlwZUFyZ3VtZW50cygpOiBTeW1ib2xbXXx1bmRlZmluZWQge1xuICAgIHJldHVybiB0aGlzLnR5cGUudHlwZUFyZ3VtZW50cygpO1xuICB9XG5cbiAgcHJpdmF0ZSBnZXQgdHNUeXBlKCk6IHRzLlR5cGUge1xuICAgIGxldCB0eXBlID0gdGhpcy5fdHNUeXBlO1xuICAgIGlmICghdHlwZSkge1xuICAgICAgY29uc3QgY2xhc3NTeW1ib2wgPSB0aGlzLmZpbmRDbGFzc1N5bWJvbCh0aGlzLnBpcGUudHlwZS5yZWZlcmVuY2UpO1xuICAgICAgaWYgKGNsYXNzU3ltYm9sKSB7XG4gICAgICAgIHR5cGUgPSB0aGlzLl90c1R5cGUgPSB0aGlzLmZpbmRUcmFuc2Zvcm1NZXRob2RUeXBlKGNsYXNzU3ltYm9sKSE7XG4gICAgICB9XG4gICAgICBpZiAoIXR5cGUpIHtcbiAgICAgICAgdHlwZSA9IHRoaXMuX3RzVHlwZSA9IGdldFRzVHlwZUZyb21CdWlsdGluVHlwZShCdWlsdGluVHlwZS5BbnksIHRoaXMuY29udGV4dCk7XG4gICAgICB9XG4gICAgfVxuICAgIHJldHVybiB0eXBlO1xuICB9XG5cbiAgcHJpdmF0ZSBmaW5kQ2xhc3NTeW1ib2wodHlwZTogU3RhdGljU3ltYm9sKTogdHMuU3ltYm9sfHVuZGVmaW5lZCB7XG4gICAgcmV0dXJuIGZpbmRDbGFzc1N5bWJvbEluQ29udGV4dCh0eXBlLCB0aGlzLmNvbnRleHQpO1xuICB9XG5cbiAgcHJpdmF0ZSBmaW5kVHJhbnNmb3JtTWV0aG9kVHlwZShjbGFzc1N5bWJvbDogdHMuU3ltYm9sKTogdHMuVHlwZXx1bmRlZmluZWQge1xuICAgIGNvbnN0IGNsYXNzVHlwZSA9IHRoaXMuY29udGV4dC5jaGVja2VyLmdldERlY2xhcmVkVHlwZU9mU3ltYm9sKGNsYXNzU3ltYm9sKTtcbiAgICBpZiAoY2xhc3NUeXBlKSB7XG4gICAgICBjb25zdCB0cmFuc2Zvcm0gPSBjbGFzc1R5cGUuZ2V0UHJvcGVydHkoJ3RyYW5zZm9ybScpO1xuICAgICAgaWYgKHRyYW5zZm9ybSkge1xuICAgICAgICByZXR1cm4gdGhpcy5jb250ZXh0LmNoZWNrZXIuZ2V0VHlwZU9mU3ltYm9sQXRMb2NhdGlvbih0cmFuc2Zvcm0sIHRoaXMuY29udGV4dC5ub2RlKTtcbiAgICAgIH1cbiAgICB9XG4gIH1cbn1cblxuZnVuY3Rpb24gZmluZENsYXNzU3ltYm9sSW5Db250ZXh0KHR5cGU6IFN0YXRpY1N5bWJvbCwgY29udGV4dDogVHlwZUNvbnRleHQpOiB0cy5TeW1ib2x8dW5kZWZpbmVkIHtcbiAgbGV0IHNvdXJjZUZpbGUgPSBjb250ZXh0LnByb2dyYW0uZ2V0U291cmNlRmlsZSh0eXBlLmZpbGVQYXRoKTtcbiAgaWYgKCFzb3VyY2VGaWxlKSB7XG4gICAgLy8gVGhpcyBoYW5kbGVzIGEgY2FzZSB3aGVyZSBhbiA8cGFja2FnZU5hbWU+L2luZGV4LmQudHMgYW5kIGEgPHBhY2thZ2VOYW1lPi88cGFja2FnZU5hbWU+LmQudHNcbiAgICAvLyBhcmUgaW4gdGhlIHNhbWUgZGlyZWN0b3J5LiBJZiB3ZSBhcmUgbG9va2luZyBmb3IgPHBhY2thZ2VOYW1lPi88cGFja2FnZU5hbWU+IGFuZCBkaWRuJ3RcbiAgICAvLyBmaW5kIGl0LCBsb29rIGZvciA8cGFja2FnZU5hbWU+L2luZGV4LmQudHMgYXMgdGhlIHByb2dyYW0gbWlnaHQgaGF2ZSBmb3VuZCB0aGF0IGluc3RlYWQuXG4gICAgY29uc3QgcCA9IHR5cGUuZmlsZVBhdGg7XG4gICAgY29uc3QgbSA9IHAubWF0Y2goSU5ERVhfUEFUVEVSTik7XG4gICAgaWYgKG0pIHtcbiAgICAgIGNvbnN0IGluZGV4VmVyc2lvbiA9IHBhdGguam9pbihwYXRoLmRpcm5hbWUocCksICdpbmRleC5kLnRzJyk7XG4gICAgICBzb3VyY2VGaWxlID0gY29udGV4dC5wcm9ncmFtLmdldFNvdXJjZUZpbGUoaW5kZXhWZXJzaW9uKTtcbiAgICB9XG4gIH1cbiAgaWYgKHNvdXJjZUZpbGUpIHtcbiAgICBjb25zdCBtb2R1bGVTeW1ib2wgPSAoc291cmNlRmlsZSBhcyBhbnkpLm1vZHVsZSB8fCAoc291cmNlRmlsZSBhcyBhbnkpLnN5bWJvbDtcbiAgICBjb25zdCBleHBvcnRzID0gY29udGV4dC5jaGVja2VyLmdldEV4cG9ydHNPZk1vZHVsZShtb2R1bGVTeW1ib2wpO1xuICAgIHJldHVybiAoZXhwb3J0cyB8fCBbXSkuZmluZChzeW1ib2wgPT4gc3ltYm9sLm5hbWUgPT0gdHlwZS5uYW1lKTtcbiAgfVxufVxuXG5jbGFzcyBFbXB0eVRhYmxlIGltcGxlbWVudHMgU3ltYm9sVGFibGUge1xuICBwdWJsaWMgcmVhZG9ubHkgc2l6ZTogbnVtYmVyID0gMDtcbiAgZ2V0KF9rZXk6IHN0cmluZyk6IFN5bWJvbHx1bmRlZmluZWQge1xuICAgIHJldHVybiB1bmRlZmluZWQ7XG4gIH1cbiAgaGFzKF9rZXk6IHN0cmluZyk6IGJvb2xlYW4ge1xuICAgIHJldHVybiBmYWxzZTtcbiAgfVxuICB2YWx1ZXMoKTogU3ltYm9sW10ge1xuICAgIHJldHVybiBbXTtcbiAgfVxuICBzdGF0aWMgaW5zdGFuY2UgPSBuZXcgRW1wdHlUYWJsZSgpO1xufVxuXG5mdW5jdGlvbiBpc1N5bWJvbFByaXZhdGUoczogdHMuU3ltYm9sKTogYm9vbGVhbiB7XG4gIHJldHVybiAhIXMudmFsdWVEZWNsYXJhdGlvbiAmJiBpc1ByaXZhdGUocy52YWx1ZURlY2xhcmF0aW9uKTtcbn1cblxuZnVuY3Rpb24gZ2V0VHNUeXBlRnJvbUJ1aWx0aW5UeXBlKGJ1aWx0aW5UeXBlOiBCdWlsdGluVHlwZSwgY3R4OiBUeXBlQ29udGV4dCk6IHRzLlR5cGUge1xuICBsZXQgc3ludGF4S2luZDogdHMuU3ludGF4S2luZDtcbiAgc3dpdGNoIChidWlsdGluVHlwZSkge1xuICAgIGNhc2UgQnVpbHRpblR5cGUuQW55OlxuICAgICAgc3ludGF4S2luZCA9IHRzLlN5bnRheEtpbmQuQW55S2V5d29yZDtcbiAgICAgIGJyZWFrO1xuICAgIGNhc2UgQnVpbHRpblR5cGUuQm9vbGVhbjpcbiAgICAgIHN5bnRheEtpbmQgPSB0cy5TeW50YXhLaW5kLkJvb2xlYW5LZXl3b3JkO1xuICAgICAgYnJlYWs7XG4gICAgY2FzZSBCdWlsdGluVHlwZS5OdWxsOlxuICAgICAgc3ludGF4S2luZCA9IHRzLlN5bnRheEtpbmQuTnVsbEtleXdvcmQ7XG4gICAgICBicmVhaztcbiAgICBjYXNlIEJ1aWx0aW5UeXBlLk51bWJlcjpcbiAgICAgIHN5bnRheEtpbmQgPSB0cy5TeW50YXhLaW5kLk51bWJlcktleXdvcmQ7XG4gICAgICBicmVhaztcbiAgICBjYXNlIEJ1aWx0aW5UeXBlLlN0cmluZzpcbiAgICAgIHN5bnRheEtpbmQgPSB0cy5TeW50YXhLaW5kLlN0cmluZ0tleXdvcmQ7XG4gICAgICBicmVhaztcbiAgICBjYXNlIEJ1aWx0aW5UeXBlLlVuZGVmaW5lZDpcbiAgICAgIHN5bnRheEtpbmQgPSB0cy5TeW50YXhLaW5kLlVuZGVmaW5lZEtleXdvcmQ7XG4gICAgICBicmVhaztcbiAgICBkZWZhdWx0OlxuICAgICAgdGhyb3cgbmV3IEVycm9yKFxuICAgICAgICAgIGBJbnRlcm5hbCBlcnJvciwgdW5oYW5kbGVkIGxpdGVyYWwga2luZCAke2J1aWx0aW5UeXBlfToke0J1aWx0aW5UeXBlW2J1aWx0aW5UeXBlXX1gKTtcbiAgfVxuICBjb25zdCBub2RlID0gdHMuY3JlYXRlTm9kZShzeW50YXhLaW5kKTtcbiAgbm9kZS5wYXJlbnQgPSB0cy5jcmVhdGVFbXB0eVN0YXRlbWVudCgpO1xuICByZXR1cm4gY3R4LmNoZWNrZXIuZ2V0VHlwZUF0TG9jYXRpb24obm9kZSk7XG59XG5cbmZ1bmN0aW9uIHNwYW5BdChzb3VyY2VGaWxlOiB0cy5Tb3VyY2VGaWxlLCBsaW5lOiBudW1iZXIsIGNvbHVtbjogbnVtYmVyKTogU3Bhbnx1bmRlZmluZWQge1xuICBpZiAobGluZSAhPSBudWxsICYmIGNvbHVtbiAhPSBudWxsKSB7XG4gICAgY29uc3QgcG9zaXRpb24gPSB0cy5nZXRQb3NpdGlvbk9mTGluZUFuZENoYXJhY3Rlcihzb3VyY2VGaWxlLCBsaW5lLCBjb2x1bW4pO1xuICAgIGNvbnN0IGZpbmRDaGlsZCA9IGZ1bmN0aW9uIGZpbmRDaGlsZChub2RlOiB0cy5Ob2RlKTogdHMuTm9kZXx1bmRlZmluZWQge1xuICAgICAgaWYgKG5vZGUua2luZCA+IHRzLlN5bnRheEtpbmQuTGFzdFRva2VuICYmIG5vZGUucG9zIDw9IHBvc2l0aW9uICYmIG5vZGUuZW5kID4gcG9zaXRpb24pIHtcbiAgICAgICAgY29uc3QgYmV0dGVyTm9kZSA9IHRzLmZvckVhY2hDaGlsZChub2RlLCBmaW5kQ2hpbGQpO1xuICAgICAgICByZXR1cm4gYmV0dGVyTm9kZSB8fCBub2RlO1xuICAgICAgfVxuICAgIH07XG5cbiAgICBjb25zdCBub2RlID0gdHMuZm9yRWFjaENoaWxkKHNvdXJjZUZpbGUsIGZpbmRDaGlsZCk7XG4gICAgaWYgKG5vZGUpIHtcbiAgICAgIHJldHVybiB7c3RhcnQ6IG5vZGUuZ2V0U3RhcnQoKSwgZW5kOiBub2RlLmdldEVuZCgpfTtcbiAgICB9XG4gIH1cbn1cblxuZnVuY3Rpb24gZGVmaW5pdGlvbkZyb21Uc1N5bWJvbChzeW1ib2w6IHRzLlN5bWJvbCk6IERlZmluaXRpb24ge1xuICBjb25zdCBkZWNsYXJhdGlvbnMgPSBzeW1ib2wuZGVjbGFyYXRpb25zO1xuICBpZiAoZGVjbGFyYXRpb25zKSB7XG4gICAgcmV0dXJuIGRlY2xhcmF0aW9ucy5tYXAoZGVjbGFyYXRpb24gPT4ge1xuICAgICAgY29uc3Qgc291cmNlRmlsZSA9IGRlY2xhcmF0aW9uLmdldFNvdXJjZUZpbGUoKTtcbiAgICAgIHJldHVybiB7XG4gICAgICAgIGZpbGVOYW1lOiBzb3VyY2VGaWxlLmZpbGVOYW1lLFxuICAgICAgICBzcGFuOiB7c3RhcnQ6IGRlY2xhcmF0aW9uLmdldFN0YXJ0KCksIGVuZDogZGVjbGFyYXRpb24uZ2V0RW5kKCl9XG4gICAgICB9O1xuICAgIH0pO1xuICB9XG59XG5cbmZ1bmN0aW9uIHBhcmVudERlY2xhcmF0aW9uT2Yobm9kZTogdHMuTm9kZSk6IHRzLk5vZGV8dW5kZWZpbmVkIHtcbiAgd2hpbGUgKG5vZGUpIHtcbiAgICBzd2l0Y2ggKG5vZGUua2luZCkge1xuICAgICAgY2FzZSB0cy5TeW50YXhLaW5kLkNsYXNzRGVjbGFyYXRpb246XG4gICAgICBjYXNlIHRzLlN5bnRheEtpbmQuSW50ZXJmYWNlRGVjbGFyYXRpb246XG4gICAgICAgIHJldHVybiBub2RlO1xuICAgICAgY2FzZSB0cy5TeW50YXhLaW5kLlNvdXJjZUZpbGU6XG4gICAgICAgIHJldHVybiB1bmRlZmluZWQ7XG4gICAgfVxuICAgIG5vZGUgPSBub2RlLnBhcmVudCE7XG4gIH1cbn1cblxuZnVuY3Rpb24gZ2V0Q29udGFpbmVyT2Yoc3ltYm9sOiB0cy5TeW1ib2wsIGNvbnRleHQ6IFR5cGVDb250ZXh0KTogU3ltYm9sfHVuZGVmaW5lZCB7XG4gIGlmIChzeW1ib2wuZ2V0RmxhZ3MoKSAmIHRzLlN5bWJvbEZsYWdzLkNsYXNzTWVtYmVyICYmIHN5bWJvbC5kZWNsYXJhdGlvbnMpIHtcbiAgICBmb3IgKGNvbnN0IGRlY2xhcmF0aW9uIG9mIHN5bWJvbC5kZWNsYXJhdGlvbnMpIHtcbiAgICAgIGNvbnN0IHBhcmVudCA9IHBhcmVudERlY2xhcmF0aW9uT2YoZGVjbGFyYXRpb24pO1xuICAgICAgaWYgKHBhcmVudCkge1xuICAgICAgICBjb25zdCB0eXBlID0gY29udGV4dC5jaGVja2VyLmdldFR5cGVBdExvY2F0aW9uKHBhcmVudCk7XG4gICAgICAgIGlmICh0eXBlKSB7XG4gICAgICAgICAgcmV0dXJuIG5ldyBUeXBlV3JhcHBlcih0eXBlLCBjb250ZXh0KTtcbiAgICAgICAgfVxuICAgICAgfVxuICAgIH1cbiAgfVxufVxuXG5mdW5jdGlvbiB0eXBlS2luZE9mKHR5cGU6IHRzLlR5cGV8dW5kZWZpbmVkKTogQnVpbHRpblR5cGUge1xuICBpZiAodHlwZSkge1xuICAgIGlmICh0eXBlLmZsYWdzICYgdHMuVHlwZUZsYWdzLkFueSkge1xuICAgICAgcmV0dXJuIEJ1aWx0aW5UeXBlLkFueTtcbiAgICB9IGVsc2UgaWYgKFxuICAgICAgICB0eXBlLmZsYWdzICYgKHRzLlR5cGVGbGFncy5TdHJpbmcgfCB0cy5UeXBlRmxhZ3MuU3RyaW5nTGlrZSB8IHRzLlR5cGVGbGFncy5TdHJpbmdMaXRlcmFsKSkge1xuICAgICAgcmV0dXJuIEJ1aWx0aW5UeXBlLlN0cmluZztcbiAgICB9IGVsc2UgaWYgKHR5cGUuZmxhZ3MgJiAodHMuVHlwZUZsYWdzLk51bWJlciB8IHRzLlR5cGVGbGFncy5OdW1iZXJMaWtlKSkge1xuICAgICAgcmV0dXJuIEJ1aWx0aW5UeXBlLk51bWJlcjtcbiAgICB9IGVsc2UgaWYgKHR5cGUuZmxhZ3MgJiB0cy5UeXBlRmxhZ3MuT2JqZWN0KSB7XG4gICAgICByZXR1cm4gQnVpbHRpblR5cGUuT2JqZWN0O1xuICAgIH0gZWxzZSBpZiAodHlwZS5mbGFncyAmICh0cy5UeXBlRmxhZ3MuVW5kZWZpbmVkKSkge1xuICAgICAgcmV0dXJuIEJ1aWx0aW5UeXBlLlVuZGVmaW5lZDtcbiAgICB9IGVsc2UgaWYgKHR5cGUuZmxhZ3MgJiAodHMuVHlwZUZsYWdzLk51bGwpKSB7XG4gICAgICByZXR1cm4gQnVpbHRpblR5cGUuTnVsbDtcbiAgICB9IGVsc2UgaWYgKHR5cGUuZmxhZ3MgJiB0cy5UeXBlRmxhZ3MuVW5pb24pIHtcbiAgICAgIGNvbnN0IHVuaW9uVHlwZSA9IHR5cGUgYXMgdHMuVW5pb25UeXBlO1xuICAgICAgaWYgKHVuaW9uVHlwZS50eXBlcy5sZW5ndGggPT09IDApIHJldHVybiBCdWlsdGluVHlwZS5PdGhlcjtcbiAgICAgIGxldCB0eTogQnVpbHRpblR5cGUgPSAwO1xuICAgICAgZm9yIChjb25zdCBzdWJUeXBlIG9mIHVuaW9uVHlwZS50eXBlcykge1xuICAgICAgICB0eSB8PSB0eXBlS2luZE9mKHN1YlR5cGUpO1xuICAgICAgfVxuICAgICAgcmV0dXJuIHR5O1xuICAgIH0gZWxzZSBpZiAodHlwZS5mbGFncyAmIHRzLlR5cGVGbGFncy5UeXBlUGFyYW1ldGVyKSB7XG4gICAgICByZXR1cm4gQnVpbHRpblR5cGUuVW5ib3VuZDtcbiAgICB9XG4gIH1cbiAgcmV0dXJuIEJ1aWx0aW5UeXBlLk90aGVyO1xufVxuXG5mdW5jdGlvbiBnZXRGcm9tU3ltYm9sVGFibGUoc3ltYm9sVGFibGU6IHRzLlN5bWJvbFRhYmxlLCBrZXk6IHN0cmluZyk6IHRzLlN5bWJvbHx1bmRlZmluZWQge1xuICBjb25zdCB0YWJsZSA9IHN5bWJvbFRhYmxlIGFzIGFueTtcbiAgbGV0IHN5bWJvbDogdHMuU3ltYm9sfHVuZGVmaW5lZDtcblxuICBpZiAodHlwZW9mIHRhYmxlLmdldCA9PT0gJ2Z1bmN0aW9uJykge1xuICAgIC8vIFRTIDIuMiB1c2VzIGEgTWFwXG4gICAgc3ltYm9sID0gdGFibGUuZ2V0KGtleSk7XG4gIH0gZWxzZSB7XG4gICAgLy8gVFMgcHJlLTIuMiB1c2VzIGFuIG9iamVjdFxuICAgIHN5bWJvbCA9IHRhYmxlW2tleV07XG4gIH1cblxuICByZXR1cm4gc3ltYm9sO1xufVxuIl19