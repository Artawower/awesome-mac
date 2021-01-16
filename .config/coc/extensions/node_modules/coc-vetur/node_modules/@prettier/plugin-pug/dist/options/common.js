"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.PUG_ARROW_PARENS_OPTION = exports.PUG_SEMI_OPTION = exports.PUG_BRACKET_SPACING_OPTION = exports.PUG_USE_TABS_OPTION = exports.PUG_TAB_WIDTH_OPTION = exports.PUG_SINGLE_QUOTE_OPTION = exports.PUG_PRINT_WIDTH_OPTION = void 0;
const _1 = require(".");
exports.PUG_PRINT_WIDTH_OPTION = {
    since: '1.6.0',
    category: _1.CATEGORY_PUG,
    type: 'int',
    default: -1,
    description: 'The line length where Prettier will try wrap.',
    range: { start: -1, end: Infinity, step: 1 }
};
exports.PUG_SINGLE_QUOTE_OPTION = {
    since: '1.6.0',
    category: _1.CATEGORY_PUG,
    type: 'choice',
    default: null,
    description: '',
    choices: [
        {
            value: null,
            description: 'Use `singleQuote` value.'
        },
        {
            value: true,
            description: 'Use single quotes instead of double quotes.'
        },
        {
            value: 'true',
            description: 'Use single quotes instead of double quotes.'
        },
        {
            value: false,
            description: 'Use double quotes instead of double quotes.'
        }
    ]
};
exports.PUG_TAB_WIDTH_OPTION = {
    since: '1.6.0',
    category: _1.CATEGORY_PUG,
    type: 'int',
    default: -1,
    description: 'Number of spaces per indentation level.',
    range: { start: -1, end: Infinity, step: 1 }
};
exports.PUG_USE_TABS_OPTION = {
    since: '1.6.0',
    category: _1.CATEGORY_PUG,
    type: 'choice',
    default: null,
    description: '',
    choices: [
        {
            value: null,
            description: 'Use `useTabs` value.'
        },
        {
            value: true,
            description: 'Indent with tabs instead of spaces.'
        },
        {
            value: 'true',
            description: 'Indent with tabs instead of spaces.'
        },
        {
            value: false,
            description: 'Indent with spaces instead of tabs.'
        }
    ]
};
exports.PUG_BRACKET_SPACING_OPTION = {
    since: '1.6.0',
    category: _1.CATEGORY_PUG,
    type: 'choice',
    default: null,
    description: '',
    choices: [
        {
            value: null,
            description: 'Use `bracketSpacing` value.'
        },
        {
            value: true,
            description: 'Print spaces between brackets.'
        },
        {
            value: 'true',
            description: 'Print spaces between brackets.'
        },
        {
            value: false,
            description: 'Do not print spaces between brackets.'
        }
    ]
};
exports.PUG_SEMI_OPTION = {
    since: '1.6.0',
    category: _1.CATEGORY_PUG,
    type: 'choice',
    default: null,
    description: '',
    choices: [
        {
            value: null,
            description: 'Use `bracketSpacing` value.'
        },
        {
            value: true,
            description: 'Print semicolons.'
        },
        {
            value: 'true',
            description: 'Print semicolons.'
        },
        {
            value: false,
            description: 'Do not print semicolons, except at the beginning of lines which may need them.'
        }
    ]
};
exports.PUG_ARROW_PARENS_OPTION = {
    since: '1.7.0',
    category: _1.CATEGORY_PUG,
    type: 'choice',
    default: null,
    description: 'Include parentheses around a sole arrow function parameter.',
    choices: [
        {
            value: null,
            description: 'Use `arrowParens` value.'
        },
        {
            value: 'always',
            description: 'Always add parens. Example: `(x) => x`'
        },
        {
            value: 'avoid',
            description: 'Omit parens when possible. Example: `x => x`'
        }
    ]
};
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiY29tbW9uLmpzIiwic291cmNlUm9vdCI6IiIsInNvdXJjZXMiOlsiLi4vLi4vc3JjL29wdGlvbnMvY29tbW9uLnRzIl0sIm5hbWVzIjpbXSwibWFwcGluZ3MiOiI7OztBQUNBLHdCQUFpQztBQUVwQixRQUFBLHNCQUFzQixHQUFxQjtJQUN2RCxLQUFLLEVBQUUsT0FBTztJQUNkLFFBQVEsRUFBRSxlQUFZO0lBQ3RCLElBQUksRUFBRSxLQUFLO0lBQ1gsT0FBTyxFQUFFLENBQUMsQ0FBQztJQUNYLFdBQVcsRUFBRSwrQ0FBK0M7SUFDNUQsS0FBSyxFQUFFLEVBQUUsS0FBSyxFQUFFLENBQUMsQ0FBQyxFQUFFLEdBQUcsRUFBRSxRQUFRLEVBQUUsSUFBSSxFQUFFLENBQUMsRUFBRTtDQUM1QyxDQUFDO0FBRVcsUUFBQSx1QkFBdUIsR0FBaUQ7SUFDcEYsS0FBSyxFQUFFLE9BQU87SUFDZCxRQUFRLEVBQUUsZUFBWTtJQUN0QixJQUFJLEVBQUUsUUFBUTtJQUNkLE9BQU8sRUFBRSxJQUFJO0lBQ2IsV0FBVyxFQUFFLEVBQUU7SUFDZixPQUFPLEVBQUU7UUFDUjtZQUNDLEtBQUssRUFBRSxJQUFJO1lBQ1gsV0FBVyxFQUFFLDBCQUEwQjtTQUN2QztRQUNEO1lBQ0MsS0FBSyxFQUFFLElBQUk7WUFDWCxXQUFXLEVBQUUsNkNBQTZDO1NBQzFEO1FBQ0Q7WUFFQyxLQUFLLEVBQUUsTUFBTTtZQUNiLFdBQVcsRUFBRSw2Q0FBNkM7U0FDMUQ7UUFDRDtZQUNDLEtBQUssRUFBRSxLQUFLO1lBQ1osV0FBVyxFQUFFLDZDQUE2QztTQUMxRDtLQUNEO0NBQ0QsQ0FBQztBQUVXLFFBQUEsb0JBQW9CLEdBQXFCO0lBQ3JELEtBQUssRUFBRSxPQUFPO0lBQ2QsUUFBUSxFQUFFLGVBQVk7SUFDdEIsSUFBSSxFQUFFLEtBQUs7SUFDWCxPQUFPLEVBQUUsQ0FBQyxDQUFDO0lBQ1gsV0FBVyxFQUFFLHlDQUF5QztJQUN0RCxLQUFLLEVBQUUsRUFBRSxLQUFLLEVBQUUsQ0FBQyxDQUFDLEVBQUUsR0FBRyxFQUFFLFFBQVEsRUFBRSxJQUFJLEVBQUUsQ0FBQyxFQUFFO0NBQzVDLENBQUM7QUFFVyxRQUFBLG1CQUFtQixHQUFpRDtJQUNoRixLQUFLLEVBQUUsT0FBTztJQUNkLFFBQVEsRUFBRSxlQUFZO0lBQ3RCLElBQUksRUFBRSxRQUFRO0lBQ2QsT0FBTyxFQUFFLElBQUk7SUFDYixXQUFXLEVBQUUsRUFBRTtJQUNmLE9BQU8sRUFBRTtRQUNSO1lBQ0MsS0FBSyxFQUFFLElBQUk7WUFDWCxXQUFXLEVBQUUsc0JBQXNCO1NBQ25DO1FBQ0Q7WUFDQyxLQUFLLEVBQUUsSUFBSTtZQUNYLFdBQVcsRUFBRSxxQ0FBcUM7U0FDbEQ7UUFDRDtZQUVDLEtBQUssRUFBRSxNQUFNO1lBQ2IsV0FBVyxFQUFFLHFDQUFxQztTQUNsRDtRQUNEO1lBQ0MsS0FBSyxFQUFFLEtBQUs7WUFDWixXQUFXLEVBQUUscUNBQXFDO1NBQ2xEO0tBQ0Q7Q0FDRCxDQUFDO0FBRVcsUUFBQSwwQkFBMEIsR0FBaUQ7SUFDdkYsS0FBSyxFQUFFLE9BQU87SUFDZCxRQUFRLEVBQUUsZUFBWTtJQUN0QixJQUFJLEVBQUUsUUFBUTtJQUNkLE9BQU8sRUFBRSxJQUFJO0lBQ2IsV0FBVyxFQUFFLEVBQUU7SUFDZixPQUFPLEVBQUU7UUFDUjtZQUNDLEtBQUssRUFBRSxJQUFJO1lBQ1gsV0FBVyxFQUFFLDZCQUE2QjtTQUMxQztRQUNEO1lBQ0MsS0FBSyxFQUFFLElBQUk7WUFDWCxXQUFXLEVBQUUsZ0NBQWdDO1NBQzdDO1FBQ0Q7WUFFQyxLQUFLLEVBQUUsTUFBTTtZQUNiLFdBQVcsRUFBRSxnQ0FBZ0M7U0FDN0M7UUFDRDtZQUNDLEtBQUssRUFBRSxLQUFLO1lBQ1osV0FBVyxFQUFFLHVDQUF1QztTQUNwRDtLQUNEO0NBQ0QsQ0FBQztBQUVXLFFBQUEsZUFBZSxHQUFpRDtJQUM1RSxLQUFLLEVBQUUsT0FBTztJQUNkLFFBQVEsRUFBRSxlQUFZO0lBQ3RCLElBQUksRUFBRSxRQUFRO0lBQ2QsT0FBTyxFQUFFLElBQUk7SUFDYixXQUFXLEVBQUUsRUFBRTtJQUNmLE9BQU8sRUFBRTtRQUNSO1lBQ0MsS0FBSyxFQUFFLElBQUk7WUFDWCxXQUFXLEVBQUUsNkJBQTZCO1NBQzFDO1FBQ0Q7WUFDQyxLQUFLLEVBQUUsSUFBSTtZQUNYLFdBQVcsRUFBRSxtQkFBbUI7U0FDaEM7UUFDRDtZQUVDLEtBQUssRUFBRSxNQUFNO1lBQ2IsV0FBVyxFQUFFLG1CQUFtQjtTQUNoQztRQUNEO1lBQ0MsS0FBSyxFQUFFLEtBQUs7WUFDWixXQUFXLEVBQUUsZ0ZBQWdGO1NBQzdGO0tBQ0Q7Q0FDRCxDQUFDO0FBRVcsUUFBQSx1QkFBdUIsR0FBNEM7SUFDL0UsS0FBSyxFQUFFLE9BQU87SUFDZCxRQUFRLEVBQUUsZUFBWTtJQUN0QixJQUFJLEVBQUUsUUFBUTtJQUNkLE9BQU8sRUFBRSxJQUFJO0lBQ2IsV0FBVyxFQUFFLDZEQUE2RDtJQUMxRSxPQUFPLEVBQUU7UUFDUjtZQUNDLEtBQUssRUFBRSxJQUFJO1lBQ1gsV0FBVyxFQUFFLDBCQUEwQjtTQUN2QztRQUNEO1lBQ0MsS0FBSyxFQUFFLFFBQVE7WUFDZixXQUFXLEVBQUUsd0NBQXdDO1NBQ3JEO1FBQ0Q7WUFDQyxLQUFLLEVBQUUsT0FBTztZQUNkLFdBQVcsRUFBRSw4Q0FBOEM7U0FDM0Q7S0FDRDtDQUNELENBQUMifQ==