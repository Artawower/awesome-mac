"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.PUG_EMPTY_ATTRIBUTES_FORCE_QUOTES_OPTION = exports.PUG_EMPTY_ATTRIBUTES_OPTION = void 0;
const __1 = require("..");
exports.PUG_EMPTY_ATTRIBUTES_OPTION = {
    since: '1.10.0',
    category: __1.CATEGORY_PUG,
    type: 'choice',
    default: 'as-is',
    description: 'Change behavior of boolean attributes.',
    choices: [
        {
            value: 'as-is',
            description: 'Nothing is changed.'
        },
        {
            value: 'none',
            description: 'Every attribute with empty quotes will have them removed.'
        },
        {
            value: 'all',
            description: 'Every boolean attribute will be expressed with empty quotes.'
        }
    ]
};
exports.PUG_EMPTY_ATTRIBUTES_FORCE_QUOTES_OPTION = {
    since: '1.10.0',
    category: __1.CATEGORY_PUG,
    type: 'path',
    default: [{ value: [] }],
    array: true,
    description: 'Define a list of patterns for attributes that will be forced to have empty quotes even with "none" selected.'
};
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiaW5kZXguanMiLCJzb3VyY2VSb290IjoiIiwic291cmNlcyI6WyIuLi8uLi8uLi9zcmMvb3B0aW9ucy9lbXB0eS1hdHRyaWJ1dGVzL2luZGV4LnRzIl0sIm5hbWVzIjpbXSwibWFwcGluZ3MiOiI7OztBQUNBLDBCQUFrQztBQUVyQixRQUFBLDJCQUEyQixHQUE0QztJQUNuRixLQUFLLEVBQUUsUUFBUTtJQUNmLFFBQVEsRUFBRSxnQkFBWTtJQUN0QixJQUFJLEVBQUUsUUFBUTtJQUNkLE9BQU8sRUFBRSxPQUFPO0lBQ2hCLFdBQVcsRUFBRSx3Q0FBd0M7SUFDckQsT0FBTyxFQUFFO1FBQ1I7WUFDQyxLQUFLLEVBQUUsT0FBTztZQUNkLFdBQVcsRUFBRSxxQkFBcUI7U0FDbEM7UUFDRDtZQUNDLEtBQUssRUFBRSxNQUFNO1lBQ2IsV0FBVyxFQUFFLDJEQUEyRDtTQUN4RTtRQUNEO1lBQ0MsS0FBSyxFQUFFLEtBQUs7WUFDWixXQUFXLEVBQUUsOERBQThEO1NBQzNFO0tBQ0Q7Q0FDRCxDQUFDO0FBRVcsUUFBQSx3Q0FBd0MsR0FBMkI7SUFDL0UsS0FBSyxFQUFFLFFBQVE7SUFDZixRQUFRLEVBQUUsZ0JBQVk7SUFDdEIsSUFBSSxFQUFFLE1BQU07SUFDWixPQUFPLEVBQUUsQ0FBQyxFQUFFLEtBQUssRUFBRSxFQUFFLEVBQUUsQ0FBQztJQUN4QixLQUFLLEVBQUUsSUFBSTtJQUNYLFdBQVcsRUFDViw4R0FBOEc7Q0FDL0csQ0FBQyJ9