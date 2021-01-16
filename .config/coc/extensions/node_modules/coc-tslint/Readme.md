# coc-tslint

Tslint language server extension for [coc.nvim](https://github.com/neoclide/coc.nvim).

The `tslint` module is resolved from current file and then global modules of `npm` and `yarn`.

**Note**: this extension can't detect semantic issues, use [coc-tslint-plugin](https://github.com/neoclide/coc-tslint-plugin) instead.

## Install

In your vim/neovim run command:

```sh
:CocInstall coc-tslint
```

## Features

- Lint `typescript` and `javascript` files using tslint.
- Provide `codeActions` for fix lint issues.
- Provide tslint commands:
  - `tslint.fixAllProblems` fix problems of current buffer.
  - `tslint.createConfig` create tslint config file.
  - `tslint.lintProject` lint current project

## Configuration options

**Notice** this configuration settings allow you to configure the behaviour of the vscode-tslint extension. To configure rules and tslint options you should use the `tslint.json` file.

- `tslint.enable` - enable/disable tslint.
- `tslint.jsEnable` - enable/disable tslint for .js files, default is `false`.
- `tslint.run` - run the linter `onSave` or `onType`, default is `onType`.
- `tslint.rulesDirectory` - an additional rules directory, for user-created rules.
- `tslint.configFile` - the configuration file that tslint should use instead of the default `tslint.json`.
- `tslint.ignoreDefinitionFiles` - control if TypeScript definition files should be ignored, default is `true`.
- `tslint.exclude` - configure glob patterns of file paths to exclude from linting. The pattern is matched against the **absolute path** of the linted file.
- `tslint.validateWithDefaultConfig` - validate a file for which no custom tslint configuration was found. The default is `false`.
- `tslint.nodePath` - custom path to node modules directory, used to load tslint from a different location than the default of the current workspace or the global node modules directory.
- `tslint.autoFixOnSave` - turns auto fix on save on or off, or defines an array of rules (e.g. [`no-var-keyword`]) to auto fix on save. **Note:** Auto-fixing is only done when manually saving a file. It is not performed when the file is automatically saved based on the `files.autoSave` setting. Executing a manual save on an already-saved document will trigger auto-fixing.
- `tslint.alwaysShowStatus` - always show the `TSLint` status bar item and not only when there are errors. The default is `false`.
- `tslint.alwaysShowRuleFailuresAsWarnings` - always show rule failures as warnings, ignoring the severity configuration in the `tslint.json` configuration.
- `tslint.packageManager`: use this package manager to locate the `tslint` and `typescript` modules. Valid values are `"npm"` or `"yarn"`. This setting is only consulted when the modules are installed globally.

## Auto-fixing

The extension supports automatic fixing of warnings to the extent supported by tslint. For warnings which support an auto-fix. You can apply the quick fix by either:

- Trigger `<Plug>(coc-codeaction)` with mapped keys, and select a fix action in input list.
- use the command `Fix all auto-fixable problems`.

When there are overlapping auto fixes a user will have to trigger `Fix all auto-fixable problems` more than once.

## License

MIT
