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
        define("@angular/language-service/language-service", ["require", "exports", "tslib", "@angular/language-service/src/language_service", "@angular/language-service/src/ts_plugin", "@angular/language-service/src/typescript_host"], factory);
    }
})(function (require, exports) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    var tslib_1 = require("tslib");
    /// <reference types="node" />
    /**
     * @module
     * @description
     * Entry point for all public APIs of the language service package.
     */
    var language_service_1 = require("@angular/language-service/src/language_service");
    Object.defineProperty(exports, "createLanguageService", { enumerable: true, get: function () { return language_service_1.createLanguageService; } });
    tslib_1.__exportStar(require("@angular/language-service/src/ts_plugin"), exports);
    var typescript_host_1 = require("@angular/language-service/src/typescript_host");
    Object.defineProperty(exports, "TypeScriptServiceHost", { enumerable: true, get: function () { return typescript_host_1.TypeScriptServiceHost; } });
    Object.defineProperty(exports, "createLanguageServiceFromTypescript", { enumerable: true, get: function () { return typescript_host_1.createLanguageServiceFromTypescript; } });
});
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoibGFuZ3VhZ2Utc2VydmljZS5qcyIsInNvdXJjZVJvb3QiOiIiLCJzb3VyY2VzIjpbIi4uLy4uLy4uLy4uLy4uL3BhY2thZ2VzL2xhbmd1YWdlLXNlcnZpY2UvbGFuZ3VhZ2Utc2VydmljZS50cyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiQUFBQTs7Ozs7O0dBTUc7Ozs7Ozs7Ozs7Ozs7SUFFSCw4QkFBOEI7SUFFOUI7Ozs7T0FJRztJQUNILG1GQUE2RDtJQUFyRCx5SEFBQSxxQkFBcUIsT0FBQTtJQUM3QixrRkFBZ0M7SUFFaEMsaUZBQWlHO0lBQXpGLHdIQUFBLHFCQUFxQixPQUFBO0lBQUUsc0lBQUEsbUNBQW1DLE9BQUEiLCJzb3VyY2VzQ29udGVudCI6WyIvKipcbiAqIEBsaWNlbnNlXG4gKiBDb3B5cmlnaHQgR29vZ2xlIExMQyBBbGwgUmlnaHRzIFJlc2VydmVkLlxuICpcbiAqIFVzZSBvZiB0aGlzIHNvdXJjZSBjb2RlIGlzIGdvdmVybmVkIGJ5IGFuIE1JVC1zdHlsZSBsaWNlbnNlIHRoYXQgY2FuIGJlXG4gKiBmb3VuZCBpbiB0aGUgTElDRU5TRSBmaWxlIGF0IGh0dHBzOi8vYW5ndWxhci5pby9saWNlbnNlXG4gKi9cblxuLy8vIDxyZWZlcmVuY2UgdHlwZXM9XCJub2RlXCIgLz5cblxuLyoqXG4gKiBAbW9kdWxlXG4gKiBAZGVzY3JpcHRpb25cbiAqIEVudHJ5IHBvaW50IGZvciBhbGwgcHVibGljIEFQSXMgb2YgdGhlIGxhbmd1YWdlIHNlcnZpY2UgcGFja2FnZS5cbiAqL1xuZXhwb3J0IHtjcmVhdGVMYW5ndWFnZVNlcnZpY2V9IGZyb20gJy4vc3JjL2xhbmd1YWdlX3NlcnZpY2UnO1xuZXhwb3J0ICogZnJvbSAnLi9zcmMvdHNfcGx1Z2luJztcbmV4cG9ydCB7RGVjbGFyYXRpb24sIERlZmluaXRpb24sIERpYWdub3N0aWMsIExhbmd1YWdlU2VydmljZSwgTGFuZ3VhZ2VTZXJ2aWNlSG9zdCwgU3BhbiwgVGVtcGxhdGVTb3VyY2V9IGZyb20gJy4vc3JjL3R5cGVzJztcbmV4cG9ydCB7VHlwZVNjcmlwdFNlcnZpY2VIb3N0LCBjcmVhdGVMYW5ndWFnZVNlcnZpY2VGcm9tVHlwZXNjcmlwdH0gZnJvbSAnLi9zcmMvdHlwZXNjcmlwdF9ob3N0JztcbiJdfQ==