"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.resolveClosingBracketPositionOption = exports.PUG_CLOSING_BRACKET_POSITION_OPTION = exports.CLOSING_BRACKET_POSITION_OPTION = void 0;
const _1 = require(".");
exports.CLOSING_BRACKET_POSITION_OPTION = {
    since: '1.3.0',
    category: _1.CATEGORY_PUG,
    type: 'choice',
    default: 'new-line',
    description: 'Determines position of closing bracket which wraps attributes.',
    choices: [
        {
            value: 'new-line',
            description: `
				Closing bracket ends with a new line.
				Example:
				input(
					type='text',
					value='my_value',
					name='my_name',
					alt='my_alt',
					autocomplete='on'
				)
				`
        },
        {
            value: 'last-line',
            description: `
			Closing bracket remains with last attribute's line.
			Example:
			input(
				type='text',
				value='my_value',
				name='my_name',
				alt='my_alt',
				autocomplete='on')
			`
        }
    ]
};
exports.PUG_CLOSING_BRACKET_POSITION_OPTION = {
    ...exports.CLOSING_BRACKET_POSITION_OPTION,
    since: '1.6.0',
    default: null,
    choices: [
        {
            value: null,
            description: 'Use `closingBracketPosition` value.'
        },
        {
            value: 'new-line',
            description: `
				Closing bracket ends with a new line.
				Example:
				input(
					type='text',
					value='my_value',
					name='my_name',
					alt='my_alt',
					autocomplete='on'
				)
				`
        },
        {
            value: 'last-line',
            description: `
			Closing bracket remains with last attribute's line.
			Example:
			input(
				type='text',
				value='my_value',
				name='my_name',
				alt='my_alt',
				autocomplete='on')
			`
        }
    ]
};
function resolveClosingBracketPositionOption(closingBracketPosition) {
    switch (closingBracketPosition) {
        case 'new-line':
            return true;
        case 'last-line':
            return false;
    }
    throw new Error(`Invalid option for pug closingBracketPosition. Found '${closingBracketPosition}'. Possible options: 'new-line' or 'last-line'`);
}
exports.resolveClosingBracketPositionOption = resolveClosingBracketPositionOption;
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiY2xvc2luZy1icmFja2V0LXBvc2l0aW9uLmpzIiwic291cmNlUm9vdCI6IiIsInNvdXJjZXMiOlsiLi4vLi4vc3JjL29wdGlvbnMvY2xvc2luZy1icmFja2V0LXBvc2l0aW9uLnRzIl0sIm5hbWVzIjpbXSwibWFwcGluZ3MiOiI7OztBQUNBLHdCQUFpQztBQUVwQixRQUFBLCtCQUErQixHQUFnRDtJQUMzRixLQUFLLEVBQUUsT0FBTztJQUNkLFFBQVEsRUFBRSxlQUFZO0lBQ3RCLElBQUksRUFBRSxRQUFRO0lBQ2QsT0FBTyxFQUFFLFVBQVU7SUFDbkIsV0FBVyxFQUFFLGdFQUFnRTtJQUM3RSxPQUFPLEVBQUU7UUFDUjtZQUNDLEtBQUssRUFBRSxVQUFVO1lBQ2pCLFdBQVcsRUFBRTs7Ozs7Ozs7OztLQVVYO1NBQ0Y7UUFDRDtZQUNDLEtBQUssRUFBRSxXQUFXO1lBQ2xCLFdBQVcsRUFBRTs7Ozs7Ozs7O0lBU1o7U0FDRDtLQUNEO0NBQ0QsQ0FBQztBQUVXLFFBQUEsbUNBQW1DLEdBQXVEO0lBQ3RHLEdBQUcsdUNBQStCO0lBQ2xDLEtBQUssRUFBRSxPQUFPO0lBQ2QsT0FBTyxFQUFFLElBQUk7SUFDYixPQUFPLEVBQUU7UUFDUjtZQUNDLEtBQUssRUFBRSxJQUFJO1lBQ1gsV0FBVyxFQUFFLHFDQUFxQztTQUNsRDtRQUNEO1lBQ0MsS0FBSyxFQUFFLFVBQVU7WUFDakIsV0FBVyxFQUFFOzs7Ozs7Ozs7O0tBVVg7U0FDRjtRQUNEO1lBQ0MsS0FBSyxFQUFFLFdBQVc7WUFDbEIsV0FBVyxFQUFFOzs7Ozs7Ozs7SUFTWjtTQUNEO0tBQ0Q7Q0FDRCxDQUFDO0FBSUYsU0FBZ0IsbUNBQW1DLENBQUMsc0JBQThDO0lBQ2pHLFFBQVEsc0JBQXNCLEVBQUU7UUFDL0IsS0FBSyxVQUFVO1lBQ2QsT0FBTyxJQUFJLENBQUM7UUFDYixLQUFLLFdBQVc7WUFDZixPQUFPLEtBQUssQ0FBQztLQUNkO0lBQ0QsTUFBTSxJQUFJLEtBQUssQ0FDZCx5REFBeUQsc0JBQXNCLGdEQUFnRCxDQUMvSCxDQUFDO0FBQ0gsQ0FBQztBQVZELGtGQVVDIn0=