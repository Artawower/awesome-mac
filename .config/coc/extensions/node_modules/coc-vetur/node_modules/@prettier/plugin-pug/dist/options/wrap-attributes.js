"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.WRAP_ATTRIBUTES_PATTERN = exports.WRAP_ATTRIBUTES_THRESHOLD = void 0;
const _1 = require(".");
exports.WRAP_ATTRIBUTES_THRESHOLD = {
    since: '1.8.0',
    category: _1.CATEGORY_PUG,
    type: 'int',
    default: -1,
    description: 'The maximum amount of attributes that an element can appear with on one line before it gets wrapped.',
    range: { start: -1, end: Infinity, step: 1 }
};
exports.WRAP_ATTRIBUTES_PATTERN = {
    since: '1.8.0',
    category: _1.CATEGORY_PUG,
    type: 'path',
    default: '',
    description: 'Regex pattern to match attributes against that should always trigger wrapping.'
};
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoid3JhcC1hdHRyaWJ1dGVzLmpzIiwic291cmNlUm9vdCI6IiIsInNvdXJjZXMiOlsiLi4vLi4vc3JjL29wdGlvbnMvd3JhcC1hdHRyaWJ1dGVzLnRzIl0sIm5hbWVzIjpbXSwibWFwcGluZ3MiOiI7OztBQUNBLHdCQUFpQztBQUVwQixRQUFBLHlCQUF5QixHQUFxQjtJQUMxRCxLQUFLLEVBQUUsT0FBTztJQUNkLFFBQVEsRUFBRSxlQUFZO0lBQ3RCLElBQUksRUFBRSxLQUFLO0lBQ1gsT0FBTyxFQUFFLENBQUMsQ0FBQztJQUNYLFdBQVcsRUFBRSxzR0FBc0c7SUFDbkgsS0FBSyxFQUFFLEVBQUUsS0FBSyxFQUFFLENBQUMsQ0FBQyxFQUFFLEdBQUcsRUFBRSxRQUFRLEVBQUUsSUFBSSxFQUFFLENBQUMsRUFBRTtDQUM1QyxDQUFDO0FBRVcsUUFBQSx1QkFBdUIsR0FBc0I7SUFDekQsS0FBSyxFQUFFLE9BQU87SUFDZCxRQUFRLEVBQUUsZUFBWTtJQUN0QixJQUFJLEVBQUUsTUFBTTtJQUNaLE9BQU8sRUFBRSxFQUFFO0lBQ1gsV0FBVyxFQUFFLGdGQUFnRjtDQUM3RixDQUFDIn0=