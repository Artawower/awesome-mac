"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.formatCommentPreserveSpaces = exports.PUG_COMMENT_PRESERVE_SPACES_OPTION = exports.COMMENT_PRESERVE_SPACES_OPTION = void 0;
const _1 = require(".");
exports.COMMENT_PRESERVE_SPACES_OPTION = {
    since: '1.1.0',
    category: _1.CATEGORY_PUG,
    type: 'choice',
    default: 'keep-all',
    description: 'Change behavior of spaces within comments.',
    choices: [
        {
            value: 'keep-all',
            description: 'Keep all spaces within comments. Example: `//    this  is   a   comment`'
        },
        {
            value: 'keep-leading',
            description: 'Keep leading spaces within comments. Example: `//    this is a comment`'
        },
        {
            value: 'trim-all',
            description: 'Trim all spaces within comments. Example: `// this is a comment`'
        }
    ]
};
exports.PUG_COMMENT_PRESERVE_SPACES_OPTION = {
    ...exports.COMMENT_PRESERVE_SPACES_OPTION,
    since: '1.6.0',
    default: null,
    choices: [
        {
            value: null,
            description: 'Use `commentPreserveSpaces` value.'
        },
        {
            value: 'keep-all',
            description: 'Keep all spaces within comments. Example: `//    this  is   a   comment`'
        },
        {
            value: 'keep-leading',
            description: 'Keep leading spaces within comments. Example: `//    this is a comment`'
        },
        {
            value: 'trim-all',
            description: 'Trim all spaces within comments. Example: `// this is a comment`'
        }
    ]
};
function formatCommentPreserveSpaces(input, commentPreserveSpaces, pipeless = false) {
    switch (commentPreserveSpaces) {
        case 'keep-leading': {
            let result = '';
            let firstNonSpace = 0;
            for (firstNonSpace; firstNonSpace < input.length && input[firstNonSpace] === ' '; firstNonSpace++) {
                result += ' ';
            }
            result += input.slice(firstNonSpace).trim().replace(/\s\s+/g, ' ');
            return result;
        }
        case 'trim-all': {
            let result = input.trim();
            result = result.replace(/\s\s+/g, ' ');
            if (!pipeless && input[0] === ' ') {
                result = ` ${result}`;
            }
            return result;
        }
        case 'keep-all':
        default:
            return input;
    }
}
exports.formatCommentPreserveSpaces = formatCommentPreserveSpaces;
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiY29tbWVudC1wcmVzZXJ2ZS1zcGFjZXMuanMiLCJzb3VyY2VSb290IjoiIiwic291cmNlcyI6WyIuLi8uLi9zcmMvb3B0aW9ucy9jb21tZW50LXByZXNlcnZlLXNwYWNlcy50cyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiOzs7QUFDQSx3QkFBaUM7QUFFcEIsUUFBQSw4QkFBOEIsR0FBK0M7SUFDekYsS0FBSyxFQUFFLE9BQU87SUFDZCxRQUFRLEVBQUUsZUFBWTtJQUN0QixJQUFJLEVBQUUsUUFBUTtJQUNkLE9BQU8sRUFBRSxVQUFVO0lBQ25CLFdBQVcsRUFBRSw0Q0FBNEM7SUFDekQsT0FBTyxFQUFFO1FBQ1I7WUFDQyxLQUFLLEVBQUUsVUFBVTtZQUNqQixXQUFXLEVBQUUsMEVBQTBFO1NBQ3ZGO1FBQ0Q7WUFDQyxLQUFLLEVBQUUsY0FBYztZQUNyQixXQUFXLEVBQUUseUVBQXlFO1NBQ3RGO1FBQ0Q7WUFDQyxLQUFLLEVBQUUsVUFBVTtZQUNqQixXQUFXLEVBQUUsa0VBQWtFO1NBQy9FO0tBQ0Q7Q0FDRCxDQUFDO0FBRVcsUUFBQSxrQ0FBa0MsR0FBc0Q7SUFDcEcsR0FBRyxzQ0FBOEI7SUFDakMsS0FBSyxFQUFFLE9BQU87SUFDZCxPQUFPLEVBQUUsSUFBSTtJQUNiLE9BQU8sRUFBRTtRQUNSO1lBQ0MsS0FBSyxFQUFFLElBQUk7WUFDWCxXQUFXLEVBQUUsb0NBQW9DO1NBQ2pEO1FBQ0Q7WUFDQyxLQUFLLEVBQUUsVUFBVTtZQUNqQixXQUFXLEVBQUUsMEVBQTBFO1NBQ3ZGO1FBQ0Q7WUFDQyxLQUFLLEVBQUUsY0FBYztZQUNyQixXQUFXLEVBQUUseUVBQXlFO1NBQ3RGO1FBQ0Q7WUFDQyxLQUFLLEVBQUUsVUFBVTtZQUNqQixXQUFXLEVBQUUsa0VBQWtFO1NBQy9FO0tBQ0Q7Q0FDRCxDQUFDO0FBSUYsU0FBZ0IsMkJBQTJCLENBQzFDLEtBQWEsRUFDYixxQkFBNEMsRUFDNUMsV0FBb0IsS0FBSztJQUV6QixRQUFRLHFCQUFxQixFQUFFO1FBQzlCLEtBQUssY0FBYyxDQUFDLENBQUM7WUFDcEIsSUFBSSxNQUFNLEdBQVcsRUFBRSxDQUFDO1lBQ3hCLElBQUksYUFBYSxHQUFXLENBQUMsQ0FBQztZQUM5QixLQUFLLGFBQWEsRUFBRSxhQUFhLEdBQUcsS0FBSyxDQUFDLE1BQU0sSUFBSSxLQUFLLENBQUMsYUFBYSxDQUFDLEtBQUssR0FBRyxFQUFFLGFBQWEsRUFBRSxFQUFFO2dCQUNsRyxNQUFNLElBQUksR0FBRyxDQUFDO2FBQ2Q7WUFDRCxNQUFNLElBQUksS0FBSyxDQUFDLEtBQUssQ0FBQyxhQUFhLENBQUMsQ0FBQyxJQUFJLEVBQUUsQ0FBQyxPQUFPLENBQUMsUUFBUSxFQUFFLEdBQUcsQ0FBQyxDQUFDO1lBQ25FLE9BQU8sTUFBTSxDQUFDO1NBQ2Q7UUFDRCxLQUFLLFVBQVUsQ0FBQyxDQUFDO1lBQ2hCLElBQUksTUFBTSxHQUFXLEtBQUssQ0FBQyxJQUFJLEVBQUUsQ0FBQztZQUNsQyxNQUFNLEdBQUcsTUFBTSxDQUFDLE9BQU8sQ0FBQyxRQUFRLEVBQUUsR0FBRyxDQUFDLENBQUM7WUFDdkMsSUFBSSxDQUFDLFFBQVEsSUFBSSxLQUFLLENBQUMsQ0FBQyxDQUFDLEtBQUssR0FBRyxFQUFFO2dCQUNsQyxNQUFNLEdBQUcsSUFBSSxNQUFNLEVBQUUsQ0FBQzthQUN0QjtZQUNELE9BQU8sTUFBTSxDQUFDO1NBQ2Q7UUFDRCxLQUFLLFVBQVUsQ0FBQztRQUNoQjtZQUVDLE9BQU8sS0FBSyxDQUFDO0tBQ2Q7QUFDRixDQUFDO0FBNUJELGtFQTRCQyJ9