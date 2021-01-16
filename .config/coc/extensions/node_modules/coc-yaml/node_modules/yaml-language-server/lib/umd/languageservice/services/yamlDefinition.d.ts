import { TextDocument, Position, DefinitionLink } from 'vscode-languageserver-types';
export declare function findDefinition(document: TextDocument, position: Position): Thenable<DefinitionLink[]>;
