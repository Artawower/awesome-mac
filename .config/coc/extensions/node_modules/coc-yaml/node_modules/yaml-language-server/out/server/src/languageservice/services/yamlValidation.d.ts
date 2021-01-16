import { Diagnostic, TextDocument } from 'vscode-languageserver-types';
import { PromiseConstructor, LanguageSettings } from '../yamlLanguageService';
import { YAMLSchemaService } from './yamlSchemaService';
import { YAMLDocDiagnostic } from '../utils/parseUtils';
/**
 * Convert a YAMLDocDiagnostic to a language server Diagnostic
 * @param yamlDiag A YAMLDocDiagnostic from the parser
 * @param textDocument TextDocument from the language server client
 */
export declare const yamlDiagToLSDiag: (yamlDiag: YAMLDocDiagnostic, textDocument: TextDocument) => Diagnostic;
export declare class YAMLValidation {
    private promise;
    private validationEnabled;
    private customTags;
    private jsonValidation;
    private MATCHES_MULTIPLE;
    constructor(schemaService: YAMLSchemaService, promiseConstructor: PromiseConstructor);
    configure(settings: LanguageSettings): void;
    doValidation(textDocument: TextDocument, isKubernetes?: boolean): Promise<Diagnostic[]>;
}
