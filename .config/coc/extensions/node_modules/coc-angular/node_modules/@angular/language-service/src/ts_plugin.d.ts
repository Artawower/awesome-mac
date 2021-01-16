/**
 * @license
 * Copyright Google LLC All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */
/// <amd-module name="@angular/language-service/src/ts_plugin" />
import * as tss from 'typescript/lib/tsserverlibrary';
/**
 * This function is called by tsserver to retrieve the external (non-TS) files
 * that should belong to the specified `project`. For Angular, these files are
 * external templates. This is called once when the project is loaded, then
 * every time when the program is updated.
 * @param project Project for which external files should be retrieved.
 */
export declare function getExternalFiles(project: tss.server.Project): string[];
export declare function create(info: tss.server.PluginCreateInfo): tss.LanguageService;
