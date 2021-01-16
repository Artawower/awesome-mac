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
        define("@angular/language-service/src/ts_plugin", ["require", "exports", "tslib", "@angular/language-service/src/language_service", "@angular/language-service/src/typescript_host"], factory);
    }
})(function (require, exports) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    exports.create = exports.getExternalFiles = void 0;
    var tslib_1 = require("tslib");
    var language_service_1 = require("@angular/language-service/src/language_service");
    var typescript_host_1 = require("@angular/language-service/src/typescript_host");
    // Use a WeakMap to keep track of Project to Host mapping so that when Project
    // is deleted Host could be garbage collected.
    var PROJECT_MAP = new WeakMap();
    /**
     * This function is called by tsserver to retrieve the external (non-TS) files
     * that should belong to the specified `project`. For Angular, these files are
     * external templates. This is called once when the project is loaded, then
     * every time when the program is updated.
     * @param project Project for which external files should be retrieved.
     */
    function getExternalFiles(project) {
        if (!project.hasRoots()) {
            // During project initialization where there is no root files yet we should
            // not do any work.
            return [];
        }
        var ngLsHost = PROJECT_MAP.get(project);
        ngLsHost === null || ngLsHost === void 0 ? void 0 : ngLsHost.getAnalyzedModules();
        return (ngLsHost === null || ngLsHost === void 0 ? void 0 : ngLsHost.getExternalTemplates()) || [];
    }
    exports.getExternalFiles = getExternalFiles;
    function create(info) {
        var tsLS = info.languageService, tsLSHost = info.languageServiceHost, config = info.config, project = info.project;
        // This plugin could operate under two different modes:
        // 1. TS + Angular
        //    Plugin augments TS language service to provide additional Angular
        //    information. This only works with inline templates and is meant to be
        //    used as a local plugin (configured via tsconfig.json)
        // 2. Angular only
        //    Plugin only provides information on Angular templates, no TS info at all.
        //    This effectively disables native TS features and is meant for internal
        //    use only.
        var angularOnly = config ? config.angularOnly === true : false;
        var ngLSHost = new typescript_host_1.TypeScriptServiceHost(tsLSHost, tsLS);
        var ngLS = language_service_1.createLanguageService(ngLSHost);
        PROJECT_MAP.set(project, ngLSHost);
        function getCompletionsAtPosition(fileName, position, options) {
            if (!angularOnly) {
                var results = tsLS.getCompletionsAtPosition(fileName, position, options);
                if (results && results.entries.length) {
                    // If TS could answer the query, then return results immediately.
                    return results;
                }
            }
            return ngLS.getCompletionsAtPosition(fileName, position, options);
        }
        function getQuickInfoAtPosition(fileName, position) {
            if (!angularOnly) {
                var result = tsLS.getQuickInfoAtPosition(fileName, position);
                if (result) {
                    // If TS could answer the query, then return results immediately.
                    return result;
                }
            }
            return ngLS.getQuickInfoAtPosition(fileName, position);
        }
        function getSemanticDiagnostics(fileName) {
            var results = [];
            if (!angularOnly) {
                results.push.apply(results, tslib_1.__spread(tsLS.getSemanticDiagnostics(fileName)));
            }
            // For semantic diagnostics we need to combine both TS + Angular results
            results.push.apply(results, tslib_1.__spread(ngLS.getSemanticDiagnostics(fileName)));
            return results;
        }
        function getDefinitionAtPosition(fileName, position) {
            if (!angularOnly) {
                var results = tsLS.getDefinitionAtPosition(fileName, position);
                if (results) {
                    // If TS could answer the query, then return results immediately.
                    return results;
                }
            }
            var result = ngLS.getDefinitionAndBoundSpan(fileName, position);
            if (!result || !result.definitions || !result.definitions.length) {
                return;
            }
            return result.definitions;
        }
        function getDefinitionAndBoundSpan(fileName, position) {
            if (!angularOnly) {
                var result = tsLS.getDefinitionAndBoundSpan(fileName, position);
                if (result) {
                    // If TS could answer the query, then return results immediately.
                    return result;
                }
            }
            return ngLS.getDefinitionAndBoundSpan(fileName, position);
        }
        var proxy = Object.assign(
        // First clone the original TS language service
        {}, tsLS, 
        // Then override the methods supported by Angular language service
        {
            getCompletionsAtPosition: getCompletionsAtPosition,
            getQuickInfoAtPosition: getQuickInfoAtPosition,
            getSemanticDiagnostics: getSemanticDiagnostics,
            getDefinitionAtPosition: getDefinitionAtPosition,
            getDefinitionAndBoundSpan: getDefinitionAndBoundSpan,
        });
        return proxy;
    }
    exports.create = create;
});
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoidHNfcGx1Z2luLmpzIiwic291cmNlUm9vdCI6IiIsInNvdXJjZXMiOlsiLi4vLi4vLi4vLi4vLi4vLi4vcGFja2FnZXMvbGFuZ3VhZ2Utc2VydmljZS9zcmMvdHNfcGx1Z2luLnRzIl0sIm5hbWVzIjpbXSwibWFwcGluZ3MiOiJBQUFBOzs7Ozs7R0FNRzs7Ozs7Ozs7Ozs7Ozs7SUFJSCxtRkFBeUQ7SUFDekQsaUZBQXdEO0lBRXhELDhFQUE4RTtJQUM5RSw4Q0FBOEM7SUFDOUMsSUFBTSxXQUFXLEdBQUcsSUFBSSxPQUFPLEVBQTZDLENBQUM7SUFFN0U7Ozs7OztPQU1HO0lBQ0gsU0FBZ0IsZ0JBQWdCLENBQUMsT0FBMkI7UUFDMUQsSUFBSSxDQUFDLE9BQU8sQ0FBQyxRQUFRLEVBQUUsRUFBRTtZQUN2QiwyRUFBMkU7WUFDM0UsbUJBQW1CO1lBQ25CLE9BQU8sRUFBRSxDQUFDO1NBQ1g7UUFDRCxJQUFNLFFBQVEsR0FBRyxXQUFXLENBQUMsR0FBRyxDQUFDLE9BQU8sQ0FBQyxDQUFDO1FBQzFDLFFBQVEsYUFBUixRQUFRLHVCQUFSLFFBQVEsQ0FBRSxrQkFBa0IsR0FBRztRQUMvQixPQUFPLENBQUEsUUFBUSxhQUFSLFFBQVEsdUJBQVIsUUFBUSxDQUFFLG9CQUFvQixPQUFNLEVBQUUsQ0FBQztJQUNoRCxDQUFDO0lBVEQsNENBU0M7SUFFRCxTQUFnQixNQUFNLENBQUMsSUFBaUM7UUFDL0MsSUFBaUIsSUFBSSxHQUFvRCxJQUFJLGdCQUF4RCxFQUF1QixRQUFRLEdBQXFCLElBQUksb0JBQXpCLEVBQUUsTUFBTSxHQUFhLElBQUksT0FBakIsRUFBRSxPQUFPLEdBQUksSUFBSSxRQUFSLENBQVM7UUFDckYsdURBQXVEO1FBQ3ZELGtCQUFrQjtRQUNsQix1RUFBdUU7UUFDdkUsMkVBQTJFO1FBQzNFLDJEQUEyRDtRQUMzRCxrQkFBa0I7UUFDbEIsK0VBQStFO1FBQy9FLDRFQUE0RTtRQUM1RSxlQUFlO1FBQ2YsSUFBTSxXQUFXLEdBQUcsTUFBTSxDQUFDLENBQUMsQ0FBQyxNQUFNLENBQUMsV0FBVyxLQUFLLElBQUksQ0FBQyxDQUFDLENBQUMsS0FBSyxDQUFDO1FBQ2pFLElBQU0sUUFBUSxHQUFHLElBQUksdUNBQXFCLENBQUMsUUFBUSxFQUFFLElBQUksQ0FBQyxDQUFDO1FBQzNELElBQU0sSUFBSSxHQUFHLHdDQUFxQixDQUFDLFFBQVEsQ0FBQyxDQUFDO1FBQzdDLFdBQVcsQ0FBQyxHQUFHLENBQUMsT0FBTyxFQUFFLFFBQVEsQ0FBQyxDQUFDO1FBRW5DLFNBQVMsd0JBQXdCLENBQzdCLFFBQWdCLEVBQUUsUUFBZ0IsRUFBRSxPQUFzRDtZQUM1RixJQUFJLENBQUMsV0FBVyxFQUFFO2dCQUNoQixJQUFNLE9BQU8sR0FBRyxJQUFJLENBQUMsd0JBQXdCLENBQUMsUUFBUSxFQUFFLFFBQVEsRUFBRSxPQUFPLENBQUMsQ0FBQztnQkFDM0UsSUFBSSxPQUFPLElBQUksT0FBTyxDQUFDLE9BQU8sQ0FBQyxNQUFNLEVBQUU7b0JBQ3JDLGlFQUFpRTtvQkFDakUsT0FBTyxPQUFPLENBQUM7aUJBQ2hCO2FBQ0Y7WUFDRCxPQUFPLElBQUksQ0FBQyx3QkFBd0IsQ0FBQyxRQUFRLEVBQUUsUUFBUSxFQUFFLE9BQU8sQ0FBQyxDQUFDO1FBQ3BFLENBQUM7UUFFRCxTQUFTLHNCQUFzQixDQUFDLFFBQWdCLEVBQUUsUUFBZ0I7WUFDaEUsSUFBSSxDQUFDLFdBQVcsRUFBRTtnQkFDaEIsSUFBTSxNQUFNLEdBQUcsSUFBSSxDQUFDLHNCQUFzQixDQUFDLFFBQVEsRUFBRSxRQUFRLENBQUMsQ0FBQztnQkFDL0QsSUFBSSxNQUFNLEVBQUU7b0JBQ1YsaUVBQWlFO29CQUNqRSxPQUFPLE1BQU0sQ0FBQztpQkFDZjthQUNGO1lBQ0QsT0FBTyxJQUFJLENBQUMsc0JBQXNCLENBQUMsUUFBUSxFQUFFLFFBQVEsQ0FBQyxDQUFDO1FBQ3pELENBQUM7UUFFRCxTQUFTLHNCQUFzQixDQUFDLFFBQWdCO1lBQzlDLElBQU0sT0FBTyxHQUFxQixFQUFFLENBQUM7WUFDckMsSUFBSSxDQUFDLFdBQVcsRUFBRTtnQkFDaEIsT0FBTyxDQUFDLElBQUksT0FBWixPQUFPLG1CQUFTLElBQUksQ0FBQyxzQkFBc0IsQ0FBQyxRQUFRLENBQUMsR0FBRTthQUN4RDtZQUNELHdFQUF3RTtZQUN4RSxPQUFPLENBQUMsSUFBSSxPQUFaLE9BQU8sbUJBQVMsSUFBSSxDQUFDLHNCQUFzQixDQUFDLFFBQVEsQ0FBQyxHQUFFO1lBQ3ZELE9BQU8sT0FBTyxDQUFDO1FBQ2pCLENBQUM7UUFFRCxTQUFTLHVCQUF1QixDQUM1QixRQUFnQixFQUFFLFFBQWdCO1lBQ3BDLElBQUksQ0FBQyxXQUFXLEVBQUU7Z0JBQ2hCLElBQU0sT0FBTyxHQUFHLElBQUksQ0FBQyx1QkFBdUIsQ0FBQyxRQUFRLEVBQUUsUUFBUSxDQUFDLENBQUM7Z0JBQ2pFLElBQUksT0FBTyxFQUFFO29CQUNYLGlFQUFpRTtvQkFDakUsT0FBTyxPQUFPLENBQUM7aUJBQ2hCO2FBQ0Y7WUFDRCxJQUFNLE1BQU0sR0FBRyxJQUFJLENBQUMseUJBQXlCLENBQUMsUUFBUSxFQUFFLFFBQVEsQ0FBQyxDQUFDO1lBQ2xFLElBQUksQ0FBQyxNQUFNLElBQUksQ0FBQyxNQUFNLENBQUMsV0FBVyxJQUFJLENBQUMsTUFBTSxDQUFDLFdBQVcsQ0FBQyxNQUFNLEVBQUU7Z0JBQ2hFLE9BQU87YUFDUjtZQUNELE9BQU8sTUFBTSxDQUFDLFdBQVcsQ0FBQztRQUM1QixDQUFDO1FBRUQsU0FBUyx5QkFBeUIsQ0FDOUIsUUFBZ0IsRUFBRSxRQUFnQjtZQUNwQyxJQUFJLENBQUMsV0FBVyxFQUFFO2dCQUNoQixJQUFNLE1BQU0sR0FBRyxJQUFJLENBQUMseUJBQXlCLENBQUMsUUFBUSxFQUFFLFFBQVEsQ0FBQyxDQUFDO2dCQUNsRSxJQUFJLE1BQU0sRUFBRTtvQkFDVixpRUFBaUU7b0JBQ2pFLE9BQU8sTUFBTSxDQUFDO2lCQUNmO2FBQ0Y7WUFDRCxPQUFPLElBQUksQ0FBQyx5QkFBeUIsQ0FBQyxRQUFRLEVBQUUsUUFBUSxDQUFDLENBQUM7UUFDNUQsQ0FBQztRQUVELElBQU0sS0FBSyxHQUF3QixNQUFNLENBQUMsTUFBTTtRQUM1QywrQ0FBK0M7UUFDL0MsRUFBRSxFQUFFLElBQUk7UUFDUixrRUFBa0U7UUFDbEU7WUFDRSx3QkFBd0IsMEJBQUE7WUFDeEIsc0JBQXNCLHdCQUFBO1lBQ3RCLHNCQUFzQix3QkFBQTtZQUN0Qix1QkFBdUIseUJBQUE7WUFDdkIseUJBQXlCLDJCQUFBO1NBQzFCLENBQUMsQ0FBQztRQUNQLE9BQU8sS0FBSyxDQUFDO0lBQ2YsQ0FBQztJQXpGRCx3QkF5RkMiLCJzb3VyY2VzQ29udGVudCI6WyIvKipcbiAqIEBsaWNlbnNlXG4gKiBDb3B5cmlnaHQgR29vZ2xlIExMQyBBbGwgUmlnaHRzIFJlc2VydmVkLlxuICpcbiAqIFVzZSBvZiB0aGlzIHNvdXJjZSBjb2RlIGlzIGdvdmVybmVkIGJ5IGFuIE1JVC1zdHlsZSBsaWNlbnNlIHRoYXQgY2FuIGJlXG4gKiBmb3VuZCBpbiB0aGUgTElDRU5TRSBmaWxlIGF0IGh0dHBzOi8vYW5ndWxhci5pby9saWNlbnNlXG4gKi9cblxuaW1wb3J0ICogYXMgdHNzIGZyb20gJ3R5cGVzY3JpcHQvbGliL3Rzc2VydmVybGlicmFyeSc7XG5cbmltcG9ydCB7Y3JlYXRlTGFuZ3VhZ2VTZXJ2aWNlfSBmcm9tICcuL2xhbmd1YWdlX3NlcnZpY2UnO1xuaW1wb3J0IHtUeXBlU2NyaXB0U2VydmljZUhvc3R9IGZyb20gJy4vdHlwZXNjcmlwdF9ob3N0JztcblxuLy8gVXNlIGEgV2Vha01hcCB0byBrZWVwIHRyYWNrIG9mIFByb2plY3QgdG8gSG9zdCBtYXBwaW5nIHNvIHRoYXQgd2hlbiBQcm9qZWN0XG4vLyBpcyBkZWxldGVkIEhvc3QgY291bGQgYmUgZ2FyYmFnZSBjb2xsZWN0ZWQuXG5jb25zdCBQUk9KRUNUX01BUCA9IG5ldyBXZWFrTWFwPHRzcy5zZXJ2ZXIuUHJvamVjdCwgVHlwZVNjcmlwdFNlcnZpY2VIb3N0PigpO1xuXG4vKipcbiAqIFRoaXMgZnVuY3Rpb24gaXMgY2FsbGVkIGJ5IHRzc2VydmVyIHRvIHJldHJpZXZlIHRoZSBleHRlcm5hbCAobm9uLVRTKSBmaWxlc1xuICogdGhhdCBzaG91bGQgYmVsb25nIHRvIHRoZSBzcGVjaWZpZWQgYHByb2plY3RgLiBGb3IgQW5ndWxhciwgdGhlc2UgZmlsZXMgYXJlXG4gKiBleHRlcm5hbCB0ZW1wbGF0ZXMuIFRoaXMgaXMgY2FsbGVkIG9uY2Ugd2hlbiB0aGUgcHJvamVjdCBpcyBsb2FkZWQsIHRoZW5cbiAqIGV2ZXJ5IHRpbWUgd2hlbiB0aGUgcHJvZ3JhbSBpcyB1cGRhdGVkLlxuICogQHBhcmFtIHByb2plY3QgUHJvamVjdCBmb3Igd2hpY2ggZXh0ZXJuYWwgZmlsZXMgc2hvdWxkIGJlIHJldHJpZXZlZC5cbiAqL1xuZXhwb3J0IGZ1bmN0aW9uIGdldEV4dGVybmFsRmlsZXMocHJvamVjdDogdHNzLnNlcnZlci5Qcm9qZWN0KTogc3RyaW5nW10ge1xuICBpZiAoIXByb2plY3QuaGFzUm9vdHMoKSkge1xuICAgIC8vIER1cmluZyBwcm9qZWN0IGluaXRpYWxpemF0aW9uIHdoZXJlIHRoZXJlIGlzIG5vIHJvb3QgZmlsZXMgeWV0IHdlIHNob3VsZFxuICAgIC8vIG5vdCBkbyBhbnkgd29yay5cbiAgICByZXR1cm4gW107XG4gIH1cbiAgY29uc3QgbmdMc0hvc3QgPSBQUk9KRUNUX01BUC5nZXQocHJvamVjdCk7XG4gIG5nTHNIb3N0Py5nZXRBbmFseXplZE1vZHVsZXMoKTtcbiAgcmV0dXJuIG5nTHNIb3N0Py5nZXRFeHRlcm5hbFRlbXBsYXRlcygpIHx8IFtdO1xufVxuXG5leHBvcnQgZnVuY3Rpb24gY3JlYXRlKGluZm86IHRzcy5zZXJ2ZXIuUGx1Z2luQ3JlYXRlSW5mbyk6IHRzcy5MYW5ndWFnZVNlcnZpY2Uge1xuICBjb25zdCB7bGFuZ3VhZ2VTZXJ2aWNlOiB0c0xTLCBsYW5ndWFnZVNlcnZpY2VIb3N0OiB0c0xTSG9zdCwgY29uZmlnLCBwcm9qZWN0fSA9IGluZm87XG4gIC8vIFRoaXMgcGx1Z2luIGNvdWxkIG9wZXJhdGUgdW5kZXIgdHdvIGRpZmZlcmVudCBtb2RlczpcbiAgLy8gMS4gVFMgKyBBbmd1bGFyXG4gIC8vICAgIFBsdWdpbiBhdWdtZW50cyBUUyBsYW5ndWFnZSBzZXJ2aWNlIHRvIHByb3ZpZGUgYWRkaXRpb25hbCBBbmd1bGFyXG4gIC8vICAgIGluZm9ybWF0aW9uLiBUaGlzIG9ubHkgd29ya3Mgd2l0aCBpbmxpbmUgdGVtcGxhdGVzIGFuZCBpcyBtZWFudCB0byBiZVxuICAvLyAgICB1c2VkIGFzIGEgbG9jYWwgcGx1Z2luIChjb25maWd1cmVkIHZpYSB0c2NvbmZpZy5qc29uKVxuICAvLyAyLiBBbmd1bGFyIG9ubHlcbiAgLy8gICAgUGx1Z2luIG9ubHkgcHJvdmlkZXMgaW5mb3JtYXRpb24gb24gQW5ndWxhciB0ZW1wbGF0ZXMsIG5vIFRTIGluZm8gYXQgYWxsLlxuICAvLyAgICBUaGlzIGVmZmVjdGl2ZWx5IGRpc2FibGVzIG5hdGl2ZSBUUyBmZWF0dXJlcyBhbmQgaXMgbWVhbnQgZm9yIGludGVybmFsXG4gIC8vICAgIHVzZSBvbmx5LlxuICBjb25zdCBhbmd1bGFyT25seSA9IGNvbmZpZyA/IGNvbmZpZy5hbmd1bGFyT25seSA9PT0gdHJ1ZSA6IGZhbHNlO1xuICBjb25zdCBuZ0xTSG9zdCA9IG5ldyBUeXBlU2NyaXB0U2VydmljZUhvc3QodHNMU0hvc3QsIHRzTFMpO1xuICBjb25zdCBuZ0xTID0gY3JlYXRlTGFuZ3VhZ2VTZXJ2aWNlKG5nTFNIb3N0KTtcbiAgUFJPSkVDVF9NQVAuc2V0KHByb2plY3QsIG5nTFNIb3N0KTtcblxuICBmdW5jdGlvbiBnZXRDb21wbGV0aW9uc0F0UG9zaXRpb24oXG4gICAgICBmaWxlTmFtZTogc3RyaW5nLCBwb3NpdGlvbjogbnVtYmVyLCBvcHRpb25zOiB0c3MuR2V0Q29tcGxldGlvbnNBdFBvc2l0aW9uT3B0aW9uc3x1bmRlZmluZWQpIHtcbiAgICBpZiAoIWFuZ3VsYXJPbmx5KSB7XG4gICAgICBjb25zdCByZXN1bHRzID0gdHNMUy5nZXRDb21wbGV0aW9uc0F0UG9zaXRpb24oZmlsZU5hbWUsIHBvc2l0aW9uLCBvcHRpb25zKTtcbiAgICAgIGlmIChyZXN1bHRzICYmIHJlc3VsdHMuZW50cmllcy5sZW5ndGgpIHtcbiAgICAgICAgLy8gSWYgVFMgY291bGQgYW5zd2VyIHRoZSBxdWVyeSwgdGhlbiByZXR1cm4gcmVzdWx0cyBpbW1lZGlhdGVseS5cbiAgICAgICAgcmV0dXJuIHJlc3VsdHM7XG4gICAgICB9XG4gICAgfVxuICAgIHJldHVybiBuZ0xTLmdldENvbXBsZXRpb25zQXRQb3NpdGlvbihmaWxlTmFtZSwgcG9zaXRpb24sIG9wdGlvbnMpO1xuICB9XG5cbiAgZnVuY3Rpb24gZ2V0UXVpY2tJbmZvQXRQb3NpdGlvbihmaWxlTmFtZTogc3RyaW5nLCBwb3NpdGlvbjogbnVtYmVyKTogdHNzLlF1aWNrSW5mb3x1bmRlZmluZWQge1xuICAgIGlmICghYW5ndWxhck9ubHkpIHtcbiAgICAgIGNvbnN0IHJlc3VsdCA9IHRzTFMuZ2V0UXVpY2tJbmZvQXRQb3NpdGlvbihmaWxlTmFtZSwgcG9zaXRpb24pO1xuICAgICAgaWYgKHJlc3VsdCkge1xuICAgICAgICAvLyBJZiBUUyBjb3VsZCBhbnN3ZXIgdGhlIHF1ZXJ5LCB0aGVuIHJldHVybiByZXN1bHRzIGltbWVkaWF0ZWx5LlxuICAgICAgICByZXR1cm4gcmVzdWx0O1xuICAgICAgfVxuICAgIH1cbiAgICByZXR1cm4gbmdMUy5nZXRRdWlja0luZm9BdFBvc2l0aW9uKGZpbGVOYW1lLCBwb3NpdGlvbik7XG4gIH1cblxuICBmdW5jdGlvbiBnZXRTZW1hbnRpY0RpYWdub3N0aWNzKGZpbGVOYW1lOiBzdHJpbmcpOiB0c3MuRGlhZ25vc3RpY1tdIHtcbiAgICBjb25zdCByZXN1bHRzOiB0c3MuRGlhZ25vc3RpY1tdID0gW107XG4gICAgaWYgKCFhbmd1bGFyT25seSkge1xuICAgICAgcmVzdWx0cy5wdXNoKC4uLnRzTFMuZ2V0U2VtYW50aWNEaWFnbm9zdGljcyhmaWxlTmFtZSkpO1xuICAgIH1cbiAgICAvLyBGb3Igc2VtYW50aWMgZGlhZ25vc3RpY3Mgd2UgbmVlZCB0byBjb21iaW5lIGJvdGggVFMgKyBBbmd1bGFyIHJlc3VsdHNcbiAgICByZXN1bHRzLnB1c2goLi4ubmdMUy5nZXRTZW1hbnRpY0RpYWdub3N0aWNzKGZpbGVOYW1lKSk7XG4gICAgcmV0dXJuIHJlc3VsdHM7XG4gIH1cblxuICBmdW5jdGlvbiBnZXREZWZpbml0aW9uQXRQb3NpdGlvbihcbiAgICAgIGZpbGVOYW1lOiBzdHJpbmcsIHBvc2l0aW9uOiBudW1iZXIpOiBSZWFkb25seUFycmF5PHRzcy5EZWZpbml0aW9uSW5mbz58dW5kZWZpbmVkIHtcbiAgICBpZiAoIWFuZ3VsYXJPbmx5KSB7XG4gICAgICBjb25zdCByZXN1bHRzID0gdHNMUy5nZXREZWZpbml0aW9uQXRQb3NpdGlvbihmaWxlTmFtZSwgcG9zaXRpb24pO1xuICAgICAgaWYgKHJlc3VsdHMpIHtcbiAgICAgICAgLy8gSWYgVFMgY291bGQgYW5zd2VyIHRoZSBxdWVyeSwgdGhlbiByZXR1cm4gcmVzdWx0cyBpbW1lZGlhdGVseS5cbiAgICAgICAgcmV0dXJuIHJlc3VsdHM7XG4gICAgICB9XG4gICAgfVxuICAgIGNvbnN0IHJlc3VsdCA9IG5nTFMuZ2V0RGVmaW5pdGlvbkFuZEJvdW5kU3BhbihmaWxlTmFtZSwgcG9zaXRpb24pO1xuICAgIGlmICghcmVzdWx0IHx8ICFyZXN1bHQuZGVmaW5pdGlvbnMgfHwgIXJlc3VsdC5kZWZpbml0aW9ucy5sZW5ndGgpIHtcbiAgICAgIHJldHVybjtcbiAgICB9XG4gICAgcmV0dXJuIHJlc3VsdC5kZWZpbml0aW9ucztcbiAgfVxuXG4gIGZ1bmN0aW9uIGdldERlZmluaXRpb25BbmRCb3VuZFNwYW4oXG4gICAgICBmaWxlTmFtZTogc3RyaW5nLCBwb3NpdGlvbjogbnVtYmVyKTogdHNzLkRlZmluaXRpb25JbmZvQW5kQm91bmRTcGFufHVuZGVmaW5lZCB7XG4gICAgaWYgKCFhbmd1bGFyT25seSkge1xuICAgICAgY29uc3QgcmVzdWx0ID0gdHNMUy5nZXREZWZpbml0aW9uQW5kQm91bmRTcGFuKGZpbGVOYW1lLCBwb3NpdGlvbik7XG4gICAgICBpZiAocmVzdWx0KSB7XG4gICAgICAgIC8vIElmIFRTIGNvdWxkIGFuc3dlciB0aGUgcXVlcnksIHRoZW4gcmV0dXJuIHJlc3VsdHMgaW1tZWRpYXRlbHkuXG4gICAgICAgIHJldHVybiByZXN1bHQ7XG4gICAgICB9XG4gICAgfVxuICAgIHJldHVybiBuZ0xTLmdldERlZmluaXRpb25BbmRCb3VuZFNwYW4oZmlsZU5hbWUsIHBvc2l0aW9uKTtcbiAgfVxuXG4gIGNvbnN0IHByb3h5OiB0c3MuTGFuZ3VhZ2VTZXJ2aWNlID0gT2JqZWN0LmFzc2lnbihcbiAgICAgIC8vIEZpcnN0IGNsb25lIHRoZSBvcmlnaW5hbCBUUyBsYW5ndWFnZSBzZXJ2aWNlXG4gICAgICB7fSwgdHNMUyxcbiAgICAgIC8vIFRoZW4gb3ZlcnJpZGUgdGhlIG1ldGhvZHMgc3VwcG9ydGVkIGJ5IEFuZ3VsYXIgbGFuZ3VhZ2Ugc2VydmljZVxuICAgICAge1xuICAgICAgICBnZXRDb21wbGV0aW9uc0F0UG9zaXRpb24sXG4gICAgICAgIGdldFF1aWNrSW5mb0F0UG9zaXRpb24sXG4gICAgICAgIGdldFNlbWFudGljRGlhZ25vc3RpY3MsXG4gICAgICAgIGdldERlZmluaXRpb25BdFBvc2l0aW9uLFxuICAgICAgICBnZXREZWZpbml0aW9uQW5kQm91bmRTcGFuLFxuICAgICAgfSk7XG4gIHJldHVybiBwcm94eTtcbn1cbiJdfQ==