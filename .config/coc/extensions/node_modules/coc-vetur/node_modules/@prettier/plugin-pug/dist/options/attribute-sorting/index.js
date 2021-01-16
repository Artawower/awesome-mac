"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.PUG_SORT_ATTRIBUTES_OPTION = exports.PUG_SORT_ATTRIBUTES_END_OPTION = exports.PUG_SORT_ATTRIBUTES_BEGINNING_OPTION = void 0;
const __1 = require("..");
const pugSortAttributesOption = {
    since: '1.7.0',
    category: __1.CATEGORY_PUG,
    type: 'path',
    array: true,
    default: [{ value: [] }],
    description: ''
};
exports.PUG_SORT_ATTRIBUTES_BEGINNING_OPTION = {
    ...pugSortAttributesOption,
    description: 'Define a list of patterns for attributes that are sorted to the beginning.'
};
exports.PUG_SORT_ATTRIBUTES_END_OPTION = {
    ...pugSortAttributesOption,
    description: 'Define a list of patterns for attributes that are sorted at the end.'
};
exports.PUG_SORT_ATTRIBUTES_OPTION = {
    since: '1.8.0',
    category: __1.CATEGORY_PUG,
    type: 'choice',
    default: 'as-is',
    description: 'Change how the attributes between _beginning_ and _end_ should be sorted.',
    choices: [
        { value: 'asc', description: 'Sort middle attributes ascending.' },
        { value: 'desc', description: 'Sort middle attributes descending.' },
        { value: 'as-is', description: 'Middle attributes are leave untouched.' }
    ]
};
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiaW5kZXguanMiLCJzb3VyY2VSb290IjoiIiwic291cmNlcyI6WyIuLi8uLi8uLi9zcmMvb3B0aW9ucy9hdHRyaWJ1dGUtc29ydGluZy9pbmRleC50cyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiOzs7QUFDQSwwQkFBa0M7QUFFbEMsTUFBTSx1QkFBdUIsR0FBMkI7SUFDdkQsS0FBSyxFQUFFLE9BQU87SUFDZCxRQUFRLEVBQUUsZ0JBQVk7SUFDdEIsSUFBSSxFQUFFLE1BQU07SUFDWixLQUFLLEVBQUUsSUFBSTtJQUNYLE9BQU8sRUFBRSxDQUFDLEVBQUUsS0FBSyxFQUFFLEVBQUUsRUFBRSxDQUFDO0lBQ3hCLFdBQVcsRUFBRSxFQUFFO0NBQ2YsQ0FBQztBQUVXLFFBQUEsb0NBQW9DLEdBQTJCO0lBQzNFLEdBQUcsdUJBQXVCO0lBQzFCLFdBQVcsRUFBRSw0RUFBNEU7Q0FDekYsQ0FBQztBQUVXLFFBQUEsOEJBQThCLEdBQTJCO0lBQ3JFLEdBQUcsdUJBQXVCO0lBQzFCLFdBQVcsRUFBRSxzRUFBc0U7Q0FDbkYsQ0FBQztBQUVXLFFBQUEsMEJBQTBCLEdBQXdDO0lBQzlFLEtBQUssRUFBRSxPQUFPO0lBQ2QsUUFBUSxFQUFFLGdCQUFZO0lBQ3RCLElBQUksRUFBRSxRQUFRO0lBQ2QsT0FBTyxFQUFFLE9BQU87SUFDaEIsV0FBVyxFQUFFLDJFQUEyRTtJQUN4RixPQUFPLEVBQUU7UUFDUixFQUFFLEtBQUssRUFBRSxLQUFLLEVBQUUsV0FBVyxFQUFFLG1DQUFtQyxFQUFFO1FBQ2xFLEVBQUUsS0FBSyxFQUFFLE1BQU0sRUFBRSxXQUFXLEVBQUUsb0NBQW9DLEVBQUU7UUFDcEUsRUFBRSxLQUFLLEVBQUUsT0FBTyxFQUFFLFdBQVcsRUFBRSx3Q0FBd0MsRUFBRTtLQUN6RTtDQUNELENBQUMifQ==