"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.resolveAttributeSeparatorOption = exports.PUG_ATTRIBUTE_SEPARATOR_OPTION = exports.ATTRIBUTE_SEPARATOR_OPTION = void 0;
const _1 = require(".");
exports.ATTRIBUTE_SEPARATOR_OPTION = {
    since: '1.0.0',
    category: _1.CATEGORY_PUG,
    type: 'choice',
    default: 'always',
    description: 'Change when attributes are separated by commas in tags.',
    choices: [
        {
            value: 'always',
            description: 'Always separate attributes with commas. Example: `button(type="submit", (click)="play()", disabled)`'
        },
        {
            value: 'as-needed',
            description: 'Only add commas between attributes where required. Example: `button(type="submit", (click)="play()" disabled)`'
        },
        {
            value: 'none',
            description: 'Never add commas between attributes. Example: `button(type="submit" @click="play()" :style="style" disabled)`'
        }
    ]
};
exports.PUG_ATTRIBUTE_SEPARATOR_OPTION = {
    ...exports.ATTRIBUTE_SEPARATOR_OPTION,
    since: '1.6.0',
    default: null,
    choices: [
        {
            value: null,
            description: 'Use `attributeSeparator` value.'
        },
        {
            value: 'always',
            description: 'Always separate attributes with commas. Example: `button(type="submit", (click)="play()", disabled)`'
        },
        {
            value: 'as-needed',
            description: 'Only add commas between attributes where required. Example: `button(type="submit", (click)="play()" disabled)`'
        },
        {
            value: 'none',
            description: 'Never add commas between attributes. Example: `button(type="submit" @click="play()" :style="style" disabled)`'
        }
    ]
};
function resolveAttributeSeparatorOption(attributeSeparator) {
    switch (attributeSeparator) {
        case 'always':
        case 'as-needed':
        case 'none':
            return attributeSeparator;
    }
    throw new Error(`Invalid option for pug attributeSeparator. Found '${attributeSeparator}'. Possible options: 'always', 'as-needed' or 'none'`);
}
exports.resolveAttributeSeparatorOption = resolveAttributeSeparatorOption;
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiYXR0cmlidXRlLXNlcGFyYXRvci5qcyIsInNvdXJjZVJvb3QiOiIiLCJzb3VyY2VzIjpbIi4uLy4uL3NyYy9vcHRpb25zL2F0dHJpYnV0ZS1zZXBhcmF0b3IudHMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6Ijs7O0FBQ0Esd0JBQWlDO0FBRXBCLFFBQUEsMEJBQTBCLEdBQTRDO0lBQ2xGLEtBQUssRUFBRSxPQUFPO0lBQ2QsUUFBUSxFQUFFLGVBQVk7SUFDdEIsSUFBSSxFQUFFLFFBQVE7SUFDZCxPQUFPLEVBQUUsUUFBUTtJQUNqQixXQUFXLEVBQUUseURBQXlEO0lBQ3RFLE9BQU8sRUFBRTtRQUNSO1lBQ0MsS0FBSyxFQUFFLFFBQVE7WUFDZixXQUFXLEVBQ1Ysc0dBQXNHO1NBQ3ZHO1FBQ0Q7WUFDQyxLQUFLLEVBQUUsV0FBVztZQUNsQixXQUFXLEVBQ1YsZ0hBQWdIO1NBQ2pIO1FBQ0Q7WUFDQyxLQUFLLEVBQUUsTUFBTTtZQUNiLFdBQVcsRUFDViwrR0FBK0c7U0FDaEg7S0FDRDtDQUNELENBQUM7QUFFVyxRQUFBLDhCQUE4QixHQUFtRDtJQUM3RixHQUFHLGtDQUEwQjtJQUM3QixLQUFLLEVBQUUsT0FBTztJQUNkLE9BQU8sRUFBRSxJQUFJO0lBQ2IsT0FBTyxFQUFFO1FBQ1I7WUFDQyxLQUFLLEVBQUUsSUFBSTtZQUNYLFdBQVcsRUFBRSxpQ0FBaUM7U0FDOUM7UUFDRDtZQUNDLEtBQUssRUFBRSxRQUFRO1lBQ2YsV0FBVyxFQUNWLHNHQUFzRztTQUN2RztRQUNEO1lBQ0MsS0FBSyxFQUFFLFdBQVc7WUFDbEIsV0FBVyxFQUNWLGdIQUFnSDtTQUNqSDtRQUNEO1lBQ0MsS0FBSyxFQUFFLE1BQU07WUFDYixXQUFXLEVBQ1YsK0dBQStHO1NBQ2hIO0tBQ0Q7Q0FDRCxDQUFDO0FBSUYsU0FBZ0IsK0JBQStCLENBQUMsa0JBQXNDO0lBQ3JGLFFBQVEsa0JBQWtCLEVBQUU7UUFDM0IsS0FBSyxRQUFRLENBQUM7UUFDZCxLQUFLLFdBQVcsQ0FBQztRQUNqQixLQUFLLE1BQU07WUFDVixPQUFPLGtCQUFrQixDQUFDO0tBQzNCO0lBQ0QsTUFBTSxJQUFJLEtBQUssQ0FDZCxxREFBcUQsa0JBQWtCLHNEQUFzRCxDQUM3SCxDQUFDO0FBQ0gsQ0FBQztBQVZELDBFQVVDIn0=