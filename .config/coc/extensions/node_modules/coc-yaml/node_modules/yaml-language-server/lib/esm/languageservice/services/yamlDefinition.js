import { parse as parseYAML } from '../parser/yamlParser07';
import { matchOffsetToDocument } from '../utils/arrUtils';
import { findDefinition as JSONFindDefinition } from 'vscode-json-languageservice/lib/umd/services/jsonDefinition';
export function findDefinition(document, position) {
    const doc = parseYAML(document.getText());
    const offset = document.offsetAt(position);
    const currentDoc = matchOffsetToDocument(offset, doc);
    if (currentDoc === null) {
        return Promise.resolve([]);
    }
    const currentDocIndex = doc.documents.indexOf(currentDoc);
    currentDoc.currentDocIndex = currentDocIndex;
    return JSONFindDefinition(document, position, currentDoc);
}
//# sourceMappingURL=yamlDefinition.js.map