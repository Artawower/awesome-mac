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
        define("@angular/language-service/src/ts_utils", ["require", "exports", "tslib", "typescript/lib/tsserverlibrary"], factory);
    }
})(function (require, exports) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    exports.findPropertyValueOfType = exports.getDirectiveClassLike = exports.getClassDeclFromDecoratorProp = exports.getPropertyAssignmentFromValue = exports.findTightestNode = void 0;
    var tslib_1 = require("tslib");
    var ts = require("typescript/lib/tsserverlibrary");
    /**
     * Return the node that most tightly encompass the specified `position`.
     * @param node
     * @param position
     */
    function findTightestNode(node, position) {
        if (node.getStart() <= position && position < node.getEnd()) {
            return node.forEachChild(function (c) { return findTightestNode(c, position); }) || node;
        }
    }
    exports.findTightestNode = findTightestNode;
    /**
     * Returns a property assignment from the assignment value if the property name
     * matches the specified `key`, or `undefined` if there is no match.
     */
    function getPropertyAssignmentFromValue(value, key) {
        var propAssignment = value.parent;
        if (!propAssignment || !ts.isPropertyAssignment(propAssignment) ||
            propAssignment.name.getText() !== key) {
            return;
        }
        return propAssignment;
    }
    exports.getPropertyAssignmentFromValue = getPropertyAssignmentFromValue;
    /**
     * Given a decorator property assignment, return the ClassDeclaration node that corresponds to the
     * directive class the property applies to.
     * If the property assignment is not on a class decorator, no declaration is returned.
     *
     * For example,
     *
     * @Component({
     *   template: '<div></div>'
     *   ^^^^^^^^^^^^^^^^^^^^^^^---- property assignment
     * })
     * class AppComponent {}
     *           ^---- class declaration node
     *
     * @param propAsgn property assignment
     */
    function getClassDeclFromDecoratorProp(propAsgnNode) {
        if (!propAsgnNode.parent || !ts.isObjectLiteralExpression(propAsgnNode.parent)) {
            return;
        }
        var objLitExprNode = propAsgnNode.parent;
        if (!objLitExprNode.parent || !ts.isCallExpression(objLitExprNode.parent)) {
            return;
        }
        var callExprNode = objLitExprNode.parent;
        if (!callExprNode.parent || !ts.isDecorator(callExprNode.parent)) {
            return;
        }
        var decorator = callExprNode.parent;
        if (!decorator.parent || !ts.isClassDeclaration(decorator.parent)) {
            return;
        }
        var classDeclNode = decorator.parent;
        return classDeclNode;
    }
    exports.getClassDeclFromDecoratorProp = getClassDeclFromDecoratorProp;
    /**
     * Return metadata about `node` if it looks like an Angular directive class.
     * In this case, potential matches are `@NgModule`, `@Component`, `@Directive`,
     * `@Pipe`, etc.
     * These class declarations all share some common attributes, namely their
     * decorator takes exactly one parameter and the parameter must be an object
     * literal.
     *
     * For example,
     *     v---------- `decoratorId`
     * @NgModule({           <
     *   declarations: [],   < classDecl
     * })                    <
     * class AppModule {}    <
     *          ^----- `classId`
     *
     * @param node Potential node that represents an Angular directive.
     */
    function getDirectiveClassLike(node) {
        var e_1, _a;
        if (!ts.isClassDeclaration(node) || !node.name || !node.decorators) {
            return;
        }
        try {
            for (var _b = tslib_1.__values(node.decorators), _c = _b.next(); !_c.done; _c = _b.next()) {
                var d = _c.value;
                var expr = d.expression;
                if (!ts.isCallExpression(expr) || expr.arguments.length !== 1 ||
                    !ts.isIdentifier(expr.expression)) {
                    continue;
                }
                var arg = expr.arguments[0];
                if (ts.isObjectLiteralExpression(arg)) {
                    return {
                        decoratorId: expr.expression,
                        classId: node.name,
                    };
                }
            }
        }
        catch (e_1_1) { e_1 = { error: e_1_1 }; }
        finally {
            try {
                if (_c && !_c.done && (_a = _b.return)) _a.call(_b);
            }
            finally { if (e_1) throw e_1.error; }
        }
    }
    exports.getDirectiveClassLike = getDirectiveClassLike;
    /**
     * Finds the value of a property assignment that is nested in a TypeScript node and is of a certain
     * type T.
     *
     * @param startNode node to start searching for nested property assignment from
     * @param propName property assignment name
     * @param predicate function to verify that a node is of type T.
     * @return node property assignment value of type T, or undefined if none is found
     */
    function findPropertyValueOfType(startNode, propName, predicate) {
        if (ts.isPropertyAssignment(startNode) && startNode.name.getText() === propName) {
            var initializer = startNode.initializer;
            if (predicate(initializer))
                return initializer;
        }
        return startNode.forEachChild(function (c) { return findPropertyValueOfType(c, propName, predicate); });
    }
    exports.findPropertyValueOfType = findPropertyValueOfType;
});
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoidHNfdXRpbHMuanMiLCJzb3VyY2VSb290IjoiIiwic291cmNlcyI6WyIuLi8uLi8uLi8uLi8uLi8uLi9wYWNrYWdlcy9sYW5ndWFnZS1zZXJ2aWNlL3NyYy90c191dGlscy50cyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiQUFBQTs7Ozs7O0dBTUc7Ozs7Ozs7Ozs7Ozs7O0lBRUgsbURBQXFEO0lBRXJEOzs7O09BSUc7SUFDSCxTQUFnQixnQkFBZ0IsQ0FBQyxJQUFhLEVBQUUsUUFBZ0I7UUFDOUQsSUFBSSxJQUFJLENBQUMsUUFBUSxFQUFFLElBQUksUUFBUSxJQUFJLFFBQVEsR0FBRyxJQUFJLENBQUMsTUFBTSxFQUFFLEVBQUU7WUFDM0QsT0FBTyxJQUFJLENBQUMsWUFBWSxDQUFDLFVBQUEsQ0FBQyxJQUFJLE9BQUEsZ0JBQWdCLENBQUMsQ0FBQyxFQUFFLFFBQVEsQ0FBQyxFQUE3QixDQUE2QixDQUFDLElBQUksSUFBSSxDQUFDO1NBQ3RFO0lBQ0gsQ0FBQztJQUpELDRDQUlDO0lBRUQ7OztPQUdHO0lBQ0gsU0FBZ0IsOEJBQThCLENBQUMsS0FBYyxFQUFFLEdBQVc7UUFFeEUsSUFBTSxjQUFjLEdBQUcsS0FBSyxDQUFDLE1BQU0sQ0FBQztRQUNwQyxJQUFJLENBQUMsY0FBYyxJQUFJLENBQUMsRUFBRSxDQUFDLG9CQUFvQixDQUFDLGNBQWMsQ0FBQztZQUMzRCxjQUFjLENBQUMsSUFBSSxDQUFDLE9BQU8sRUFBRSxLQUFLLEdBQUcsRUFBRTtZQUN6QyxPQUFPO1NBQ1I7UUFDRCxPQUFPLGNBQWMsQ0FBQztJQUN4QixDQUFDO0lBUkQsd0VBUUM7SUFFRDs7Ozs7Ozs7Ozs7Ozs7O09BZUc7SUFDSCxTQUFnQiw2QkFBNkIsQ0FBQyxZQUFtQztRQUUvRSxJQUFJLENBQUMsWUFBWSxDQUFDLE1BQU0sSUFBSSxDQUFDLEVBQUUsQ0FBQyx5QkFBeUIsQ0FBQyxZQUFZLENBQUMsTUFBTSxDQUFDLEVBQUU7WUFDOUUsT0FBTztTQUNSO1FBQ0QsSUFBTSxjQUFjLEdBQUcsWUFBWSxDQUFDLE1BQU0sQ0FBQztRQUMzQyxJQUFJLENBQUMsY0FBYyxDQUFDLE1BQU0sSUFBSSxDQUFDLEVBQUUsQ0FBQyxnQkFBZ0IsQ0FBQyxjQUFjLENBQUMsTUFBTSxDQUFDLEVBQUU7WUFDekUsT0FBTztTQUNSO1FBQ0QsSUFBTSxZQUFZLEdBQUcsY0FBYyxDQUFDLE1BQU0sQ0FBQztRQUMzQyxJQUFJLENBQUMsWUFBWSxDQUFDLE1BQU0sSUFBSSxDQUFDLEVBQUUsQ0FBQyxXQUFXLENBQUMsWUFBWSxDQUFDLE1BQU0sQ0FBQyxFQUFFO1lBQ2hFLE9BQU87U0FDUjtRQUNELElBQU0sU0FBUyxHQUFHLFlBQVksQ0FBQyxNQUFNLENBQUM7UUFDdEMsSUFBSSxDQUFDLFNBQVMsQ0FBQyxNQUFNLElBQUksQ0FBQyxFQUFFLENBQUMsa0JBQWtCLENBQUMsU0FBUyxDQUFDLE1BQU0sQ0FBQyxFQUFFO1lBQ2pFLE9BQU87U0FDUjtRQUNELElBQU0sYUFBYSxHQUFHLFNBQVMsQ0FBQyxNQUFNLENBQUM7UUFDdkMsT0FBTyxhQUFhLENBQUM7SUFDdkIsQ0FBQztJQW5CRCxzRUFtQkM7SUFPRDs7Ozs7Ozs7Ozs7Ozs7Ozs7T0FpQkc7SUFDSCxTQUFnQixxQkFBcUIsQ0FBQyxJQUFhOztRQUNqRCxJQUFJLENBQUMsRUFBRSxDQUFDLGtCQUFrQixDQUFDLElBQUksQ0FBQyxJQUFJLENBQUMsSUFBSSxDQUFDLElBQUksSUFBSSxDQUFDLElBQUksQ0FBQyxVQUFVLEVBQUU7WUFDbEUsT0FBTztTQUNSOztZQUNELEtBQWdCLElBQUEsS0FBQSxpQkFBQSxJQUFJLENBQUMsVUFBVSxDQUFBLGdCQUFBLDRCQUFFO2dCQUE1QixJQUFNLENBQUMsV0FBQTtnQkFDVixJQUFNLElBQUksR0FBRyxDQUFDLENBQUMsVUFBVSxDQUFDO2dCQUMxQixJQUFJLENBQUMsRUFBRSxDQUFDLGdCQUFnQixDQUFDLElBQUksQ0FBQyxJQUFJLElBQUksQ0FBQyxTQUFTLENBQUMsTUFBTSxLQUFLLENBQUM7b0JBQ3pELENBQUMsRUFBRSxDQUFDLFlBQVksQ0FBQyxJQUFJLENBQUMsVUFBVSxDQUFDLEVBQUU7b0JBQ3JDLFNBQVM7aUJBQ1Y7Z0JBQ0QsSUFBTSxHQUFHLEdBQUcsSUFBSSxDQUFDLFNBQVMsQ0FBQyxDQUFDLENBQUMsQ0FBQztnQkFDOUIsSUFBSSxFQUFFLENBQUMseUJBQXlCLENBQUMsR0FBRyxDQUFDLEVBQUU7b0JBQ3JDLE9BQU87d0JBQ0wsV0FBVyxFQUFFLElBQUksQ0FBQyxVQUFVO3dCQUM1QixPQUFPLEVBQUUsSUFBSSxDQUFDLElBQUk7cUJBQ25CLENBQUM7aUJBQ0g7YUFDRjs7Ozs7Ozs7O0lBQ0gsQ0FBQztJQWxCRCxzREFrQkM7SUFFRDs7Ozs7Ozs7T0FRRztJQUNILFNBQWdCLHVCQUF1QixDQUNuQyxTQUFrQixFQUFFLFFBQWdCLEVBQUUsU0FBdUM7UUFDL0UsSUFBSSxFQUFFLENBQUMsb0JBQW9CLENBQUMsU0FBUyxDQUFDLElBQUksU0FBUyxDQUFDLElBQUksQ0FBQyxPQUFPLEVBQUUsS0FBSyxRQUFRLEVBQUU7WUFDeEUsSUFBQSxXQUFXLEdBQUksU0FBUyxZQUFiLENBQWM7WUFDaEMsSUFBSSxTQUFTLENBQUMsV0FBVyxDQUFDO2dCQUFFLE9BQU8sV0FBVyxDQUFDO1NBQ2hEO1FBQ0QsT0FBTyxTQUFTLENBQUMsWUFBWSxDQUFDLFVBQUEsQ0FBQyxJQUFJLE9BQUEsdUJBQXVCLENBQUMsQ0FBQyxFQUFFLFFBQVEsRUFBRSxTQUFTLENBQUMsRUFBL0MsQ0FBK0MsQ0FBQyxDQUFDO0lBQ3RGLENBQUM7SUFQRCwwREFPQyIsInNvdXJjZXNDb250ZW50IjpbIi8qKlxuICogQGxpY2Vuc2VcbiAqIENvcHlyaWdodCBHb29nbGUgTExDIEFsbCBSaWdodHMgUmVzZXJ2ZWQuXG4gKlxuICogVXNlIG9mIHRoaXMgc291cmNlIGNvZGUgaXMgZ292ZXJuZWQgYnkgYW4gTUlULXN0eWxlIGxpY2Vuc2UgdGhhdCBjYW4gYmVcbiAqIGZvdW5kIGluIHRoZSBMSUNFTlNFIGZpbGUgYXQgaHR0cHM6Ly9hbmd1bGFyLmlvL2xpY2Vuc2VcbiAqL1xuXG5pbXBvcnQgKiBhcyB0cyBmcm9tICd0eXBlc2NyaXB0L2xpYi90c3NlcnZlcmxpYnJhcnknO1xuXG4vKipcbiAqIFJldHVybiB0aGUgbm9kZSB0aGF0IG1vc3QgdGlnaHRseSBlbmNvbXBhc3MgdGhlIHNwZWNpZmllZCBgcG9zaXRpb25gLlxuICogQHBhcmFtIG5vZGVcbiAqIEBwYXJhbSBwb3NpdGlvblxuICovXG5leHBvcnQgZnVuY3Rpb24gZmluZFRpZ2h0ZXN0Tm9kZShub2RlOiB0cy5Ob2RlLCBwb3NpdGlvbjogbnVtYmVyKTogdHMuTm9kZXx1bmRlZmluZWQge1xuICBpZiAobm9kZS5nZXRTdGFydCgpIDw9IHBvc2l0aW9uICYmIHBvc2l0aW9uIDwgbm9kZS5nZXRFbmQoKSkge1xuICAgIHJldHVybiBub2RlLmZvckVhY2hDaGlsZChjID0+IGZpbmRUaWdodGVzdE5vZGUoYywgcG9zaXRpb24pKSB8fCBub2RlO1xuICB9XG59XG5cbi8qKlxuICogUmV0dXJucyBhIHByb3BlcnR5IGFzc2lnbm1lbnQgZnJvbSB0aGUgYXNzaWdubWVudCB2YWx1ZSBpZiB0aGUgcHJvcGVydHkgbmFtZVxuICogbWF0Y2hlcyB0aGUgc3BlY2lmaWVkIGBrZXlgLCBvciBgdW5kZWZpbmVkYCBpZiB0aGVyZSBpcyBubyBtYXRjaC5cbiAqL1xuZXhwb3J0IGZ1bmN0aW9uIGdldFByb3BlcnR5QXNzaWdubWVudEZyb21WYWx1ZSh2YWx1ZTogdHMuTm9kZSwga2V5OiBzdHJpbmcpOiB0cy5Qcm9wZXJ0eUFzc2lnbm1lbnR8XG4gICAgdW5kZWZpbmVkIHtcbiAgY29uc3QgcHJvcEFzc2lnbm1lbnQgPSB2YWx1ZS5wYXJlbnQ7XG4gIGlmICghcHJvcEFzc2lnbm1lbnQgfHwgIXRzLmlzUHJvcGVydHlBc3NpZ25tZW50KHByb3BBc3NpZ25tZW50KSB8fFxuICAgICAgcHJvcEFzc2lnbm1lbnQubmFtZS5nZXRUZXh0KCkgIT09IGtleSkge1xuICAgIHJldHVybjtcbiAgfVxuICByZXR1cm4gcHJvcEFzc2lnbm1lbnQ7XG59XG5cbi8qKlxuICogR2l2ZW4gYSBkZWNvcmF0b3IgcHJvcGVydHkgYXNzaWdubWVudCwgcmV0dXJuIHRoZSBDbGFzc0RlY2xhcmF0aW9uIG5vZGUgdGhhdCBjb3JyZXNwb25kcyB0byB0aGVcbiAqIGRpcmVjdGl2ZSBjbGFzcyB0aGUgcHJvcGVydHkgYXBwbGllcyB0by5cbiAqIElmIHRoZSBwcm9wZXJ0eSBhc3NpZ25tZW50IGlzIG5vdCBvbiBhIGNsYXNzIGRlY29yYXRvciwgbm8gZGVjbGFyYXRpb24gaXMgcmV0dXJuZWQuXG4gKlxuICogRm9yIGV4YW1wbGUsXG4gKlxuICogQENvbXBvbmVudCh7XG4gKiAgIHRlbXBsYXRlOiAnPGRpdj48L2Rpdj4nXG4gKiAgIF5eXl5eXl5eXl5eXl5eXl5eXl5eXl5eLS0tLSBwcm9wZXJ0eSBhc3NpZ25tZW50XG4gKiB9KVxuICogY2xhc3MgQXBwQ29tcG9uZW50IHt9XG4gKiAgICAgICAgICAgXi0tLS0gY2xhc3MgZGVjbGFyYXRpb24gbm9kZVxuICpcbiAqIEBwYXJhbSBwcm9wQXNnbiBwcm9wZXJ0eSBhc3NpZ25tZW50XG4gKi9cbmV4cG9ydCBmdW5jdGlvbiBnZXRDbGFzc0RlY2xGcm9tRGVjb3JhdG9yUHJvcChwcm9wQXNnbk5vZGU6IHRzLlByb3BlcnR5QXNzaWdubWVudCk6XG4gICAgdHMuQ2xhc3NEZWNsYXJhdGlvbnx1bmRlZmluZWQge1xuICBpZiAoIXByb3BBc2duTm9kZS5wYXJlbnQgfHwgIXRzLmlzT2JqZWN0TGl0ZXJhbEV4cHJlc3Npb24ocHJvcEFzZ25Ob2RlLnBhcmVudCkpIHtcbiAgICByZXR1cm47XG4gIH1cbiAgY29uc3Qgb2JqTGl0RXhwck5vZGUgPSBwcm9wQXNnbk5vZGUucGFyZW50O1xuICBpZiAoIW9iakxpdEV4cHJOb2RlLnBhcmVudCB8fCAhdHMuaXNDYWxsRXhwcmVzc2lvbihvYmpMaXRFeHByTm9kZS5wYXJlbnQpKSB7XG4gICAgcmV0dXJuO1xuICB9XG4gIGNvbnN0IGNhbGxFeHByTm9kZSA9IG9iakxpdEV4cHJOb2RlLnBhcmVudDtcbiAgaWYgKCFjYWxsRXhwck5vZGUucGFyZW50IHx8ICF0cy5pc0RlY29yYXRvcihjYWxsRXhwck5vZGUucGFyZW50KSkge1xuICAgIHJldHVybjtcbiAgfVxuICBjb25zdCBkZWNvcmF0b3IgPSBjYWxsRXhwck5vZGUucGFyZW50O1xuICBpZiAoIWRlY29yYXRvci5wYXJlbnQgfHwgIXRzLmlzQ2xhc3NEZWNsYXJhdGlvbihkZWNvcmF0b3IucGFyZW50KSkge1xuICAgIHJldHVybjtcbiAgfVxuICBjb25zdCBjbGFzc0RlY2xOb2RlID0gZGVjb3JhdG9yLnBhcmVudDtcbiAgcmV0dXJuIGNsYXNzRGVjbE5vZGU7XG59XG5cbmludGVyZmFjZSBEaXJlY3RpdmVDbGFzc0xpa2Uge1xuICBkZWNvcmF0b3JJZDogdHMuSWRlbnRpZmllcjsgIC8vIGRlY29yYXRvciBpZGVudGlmaWVyLCBsaWtlIEBDb21wb25lbnRcbiAgY2xhc3NJZDogdHMuSWRlbnRpZmllcjtcbn1cblxuLyoqXG4gKiBSZXR1cm4gbWV0YWRhdGEgYWJvdXQgYG5vZGVgIGlmIGl0IGxvb2tzIGxpa2UgYW4gQW5ndWxhciBkaXJlY3RpdmUgY2xhc3MuXG4gKiBJbiB0aGlzIGNhc2UsIHBvdGVudGlhbCBtYXRjaGVzIGFyZSBgQE5nTW9kdWxlYCwgYEBDb21wb25lbnRgLCBgQERpcmVjdGl2ZWAsXG4gKiBgQFBpcGVgLCBldGMuXG4gKiBUaGVzZSBjbGFzcyBkZWNsYXJhdGlvbnMgYWxsIHNoYXJlIHNvbWUgY29tbW9uIGF0dHJpYnV0ZXMsIG5hbWVseSB0aGVpclxuICogZGVjb3JhdG9yIHRha2VzIGV4YWN0bHkgb25lIHBhcmFtZXRlciBhbmQgdGhlIHBhcmFtZXRlciBtdXN0IGJlIGFuIG9iamVjdFxuICogbGl0ZXJhbC5cbiAqXG4gKiBGb3IgZXhhbXBsZSxcbiAqICAgICB2LS0tLS0tLS0tLSBgZGVjb3JhdG9ySWRgXG4gKiBATmdNb2R1bGUoeyAgICAgICAgICAgPFxuICogICBkZWNsYXJhdGlvbnM6IFtdLCAgIDwgY2xhc3NEZWNsXG4gKiB9KSAgICAgICAgICAgICAgICAgICAgPFxuICogY2xhc3MgQXBwTW9kdWxlIHt9ICAgIDxcbiAqICAgICAgICAgIF4tLS0tLSBgY2xhc3NJZGBcbiAqXG4gKiBAcGFyYW0gbm9kZSBQb3RlbnRpYWwgbm9kZSB0aGF0IHJlcHJlc2VudHMgYW4gQW5ndWxhciBkaXJlY3RpdmUuXG4gKi9cbmV4cG9ydCBmdW5jdGlvbiBnZXREaXJlY3RpdmVDbGFzc0xpa2Uobm9kZTogdHMuTm9kZSk6IERpcmVjdGl2ZUNsYXNzTGlrZXx1bmRlZmluZWQge1xuICBpZiAoIXRzLmlzQ2xhc3NEZWNsYXJhdGlvbihub2RlKSB8fCAhbm9kZS5uYW1lIHx8ICFub2RlLmRlY29yYXRvcnMpIHtcbiAgICByZXR1cm47XG4gIH1cbiAgZm9yIChjb25zdCBkIG9mIG5vZGUuZGVjb3JhdG9ycykge1xuICAgIGNvbnN0IGV4cHIgPSBkLmV4cHJlc3Npb247XG4gICAgaWYgKCF0cy5pc0NhbGxFeHByZXNzaW9uKGV4cHIpIHx8IGV4cHIuYXJndW1lbnRzLmxlbmd0aCAhPT0gMSB8fFxuICAgICAgICAhdHMuaXNJZGVudGlmaWVyKGV4cHIuZXhwcmVzc2lvbikpIHtcbiAgICAgIGNvbnRpbnVlO1xuICAgIH1cbiAgICBjb25zdCBhcmcgPSBleHByLmFyZ3VtZW50c1swXTtcbiAgICBpZiAodHMuaXNPYmplY3RMaXRlcmFsRXhwcmVzc2lvbihhcmcpKSB7XG4gICAgICByZXR1cm4ge1xuICAgICAgICBkZWNvcmF0b3JJZDogZXhwci5leHByZXNzaW9uLFxuICAgICAgICBjbGFzc0lkOiBub2RlLm5hbWUsXG4gICAgICB9O1xuICAgIH1cbiAgfVxufVxuXG4vKipcbiAqIEZpbmRzIHRoZSB2YWx1ZSBvZiBhIHByb3BlcnR5IGFzc2lnbm1lbnQgdGhhdCBpcyBuZXN0ZWQgaW4gYSBUeXBlU2NyaXB0IG5vZGUgYW5kIGlzIG9mIGEgY2VydGFpblxuICogdHlwZSBULlxuICpcbiAqIEBwYXJhbSBzdGFydE5vZGUgbm9kZSB0byBzdGFydCBzZWFyY2hpbmcgZm9yIG5lc3RlZCBwcm9wZXJ0eSBhc3NpZ25tZW50IGZyb21cbiAqIEBwYXJhbSBwcm9wTmFtZSBwcm9wZXJ0eSBhc3NpZ25tZW50IG5hbWVcbiAqIEBwYXJhbSBwcmVkaWNhdGUgZnVuY3Rpb24gdG8gdmVyaWZ5IHRoYXQgYSBub2RlIGlzIG9mIHR5cGUgVC5cbiAqIEByZXR1cm4gbm9kZSBwcm9wZXJ0eSBhc3NpZ25tZW50IHZhbHVlIG9mIHR5cGUgVCwgb3IgdW5kZWZpbmVkIGlmIG5vbmUgaXMgZm91bmRcbiAqL1xuZXhwb3J0IGZ1bmN0aW9uIGZpbmRQcm9wZXJ0eVZhbHVlT2ZUeXBlPFQgZXh0ZW5kcyB0cy5Ob2RlPihcbiAgICBzdGFydE5vZGU6IHRzLk5vZGUsIHByb3BOYW1lOiBzdHJpbmcsIHByZWRpY2F0ZTogKG5vZGU6IHRzLk5vZGUpID0+IG5vZGUgaXMgVCk6IFR8dW5kZWZpbmVkIHtcbiAgaWYgKHRzLmlzUHJvcGVydHlBc3NpZ25tZW50KHN0YXJ0Tm9kZSkgJiYgc3RhcnROb2RlLm5hbWUuZ2V0VGV4dCgpID09PSBwcm9wTmFtZSkge1xuICAgIGNvbnN0IHtpbml0aWFsaXplcn0gPSBzdGFydE5vZGU7XG4gICAgaWYgKHByZWRpY2F0ZShpbml0aWFsaXplcikpIHJldHVybiBpbml0aWFsaXplcjtcbiAgfVxuICByZXR1cm4gc3RhcnROb2RlLmZvckVhY2hDaGlsZChjID0+IGZpbmRQcm9wZXJ0eVZhbHVlT2ZUeXBlKGMsIHByb3BOYW1lLCBwcmVkaWNhdGUpKTtcbn1cbiJdfQ==