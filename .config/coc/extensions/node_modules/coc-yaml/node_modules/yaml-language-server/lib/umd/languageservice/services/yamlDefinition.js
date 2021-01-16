(function (factory) {
    if (typeof module === "object" && typeof module.exports === "object") {
        var v = factory(require, exports);
        if (v !== undefined) module.exports = v;
    }
    else if (typeof define === "function" && define.amd) {
        define(["require", "exports", "../parser/yamlParser07", "../utils/arrUtils", "vscode-json-languageservice/lib/umd/services/jsonDefinition"], factory);
    }
})(function (require, exports) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    const yamlParser07_1 = require("../parser/yamlParser07");
    const arrUtils_1 = require("../utils/arrUtils");
    const jsonDefinition_1 = require("vscode-json-languageservice/lib/umd/services/jsonDefinition");
    function findDefinition(document, position) {
        const doc = yamlParser07_1.parse(document.getText());
        const offset = document.offsetAt(position);
        const currentDoc = arrUtils_1.matchOffsetToDocument(offset, doc);
        if (currentDoc === null) {
            return Promise.resolve([]);
        }
        const currentDocIndex = doc.documents.indexOf(currentDoc);
        currentDoc.currentDocIndex = currentDocIndex;
        return jsonDefinition_1.findDefinition(document, position, currentDoc);
    }
    exports.findDefinition = findDefinition;
});
//# sourceMappingURL=yamlDefinition.js.map