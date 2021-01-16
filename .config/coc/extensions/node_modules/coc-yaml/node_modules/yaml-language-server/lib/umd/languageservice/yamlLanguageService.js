/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Red Hat, Inc. All rights reserved.
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *  Licensed under the MIT License. See License.txt in the project root for license information.
 *--------------------------------------------------------------------------------------------*/
(function (factory) {
    if (typeof module === "object" && typeof module.exports === "object") {
        var v = factory(require, exports);
        if (v !== undefined) module.exports = v;
    }
    else if (typeof define === "function" && define.amd) {
        define(["require", "exports", "./services/yamlSchemaService", "./services/documentSymbols", "./services/yamlCompletion", "./services/yamlHover", "./services/yamlValidation", "./services/yamlFormatter", "./services/yamlDefinition"], factory);
    }
})(function (require, exports) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    const yamlSchemaService_1 = require("./services/yamlSchemaService");
    const documentSymbols_1 = require("./services/documentSymbols");
    const yamlCompletion_1 = require("./services/yamlCompletion");
    const yamlHover_1 = require("./services/yamlHover");
    const yamlValidation_1 = require("./services/yamlValidation");
    const yamlFormatter_1 = require("./services/yamlFormatter");
    const yamlDefinition_1 = require("./services/yamlDefinition");
    function getLanguageService(schemaRequestService, workspaceContext, contributions, promiseConstructor) {
        const promise = promiseConstructor || Promise;
        const schemaService = new yamlSchemaService_1.YAMLSchemaService(schemaRequestService, workspaceContext);
        const completer = new yamlCompletion_1.YAMLCompletion(schemaService, contributions, promise);
        const hover = new yamlHover_1.YAMLHover(schemaService, promise);
        const yamlDocumentSymbols = new documentSymbols_1.YAMLDocumentSymbols(schemaService);
        const yamlValidation = new yamlValidation_1.YAMLValidation(schemaService, promise);
        const formatter = new yamlFormatter_1.YAMLFormatter();
        return {
            configure: (settings) => {
                schemaService.clearExternalSchemas();
                if (settings.schemas) {
                    settings.schemas.forEach((settings) => {
                        schemaService.registerExternalSchema(settings.uri, settings.fileMatch, settings.schema);
                    });
                }
                yamlValidation.configure(settings);
                hover.configure(settings);
                const customTagsSetting = settings && settings['customTags'] ? settings['customTags'] : [];
                completer.configure(settings, customTagsSetting);
                formatter.configure(settings);
            },
            registerCustomSchemaProvider: (schemaProvider) => {
                schemaService.registerCustomSchemaProvider(schemaProvider);
            },
            findDefinition: yamlDefinition_1.findDefinition,
            doComplete: completer.doComplete.bind(completer),
            doResolve: completer.doResolve.bind(completer),
            doValidation: yamlValidation.doValidation.bind(yamlValidation),
            doHover: hover.doHover.bind(hover),
            findDocumentSymbols: yamlDocumentSymbols.findDocumentSymbols.bind(yamlDocumentSymbols),
            findDocumentSymbols2: yamlDocumentSymbols.findHierarchicalDocumentSymbols.bind(yamlDocumentSymbols),
            resetSchema: (uri) => {
                return schemaService.onResourceChange(uri);
            },
            doFormat: formatter.format.bind(formatter),
            addSchema: (schemaID, schema) => {
                return schemaService.saveSchema(schemaID, schema);
            },
            deleteSchema: (schemaID) => {
                return schemaService.deleteSchema(schemaID);
            },
            modifySchemaContent: (schemaAdditions) => {
                return schemaService.addContent(schemaAdditions);
            },
            deleteSchemaContent: (schemaDeletions) => {
                return schemaService.deleteContent(schemaDeletions);
            },
        };
    }
    exports.getLanguageService = getLanguageService;
});
//# sourceMappingURL=yamlLanguageService.js.map