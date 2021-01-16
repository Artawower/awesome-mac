"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Red Hat. All rights reserved.
 *  Licensed under the MIT License. See License.txt in the project root for license information.
 *--------------------------------------------------------------------------------------------*/
const testHelper_1 = require("./utils/testHelper");
const assert = require("assert");
const serviceSetup_1 = require("./utils/serviceSetup");
const languageService = testHelper_1.configureLanguageService(new serviceSetup_1.ServiceSetup().languageSettings);
suite('FindDefintion Tests', () => {
    describe('Jump to defintion', function () {
        function findDefinitions(content, position) {
            const testTextDocument = testHelper_1.setupTextDocument(content);
            return languageService.findDefinition(testTextDocument, testTextDocument.positionAt(position));
        }
        it('Find source defintion', (done) => {
            const content = "definitions:\n  link:\n    type: string\ntype: object\nproperties:\n  uri:\n    $ref: '#/definitions/link'\n";
            const definitions = findDefinitions(content, content.lastIndexOf('/li'));
            definitions
                .then(function (results) {
                assert.equal(results.length, 1);
                assert.deepEqual(results[0].originSelectionRange, {
                    start: {
                        line: 6,
                        character: 10,
                    },
                    end: {
                        line: 6,
                        character: 30,
                    },
                });
                assert.deepEqual(results[0].targetRange, {
                    start: {
                        line: 2,
                        character: 4,
                    },
                    end: {
                        line: 2,
                        character: 16,
                    },
                });
            })
                .then(done, done);
        });
    });
});
//# sourceMappingURL=findDefintion.test.js.map