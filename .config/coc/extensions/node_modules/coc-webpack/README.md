# coc-webpack

> webpack's config options are crazy much 😢 and this is an
> simple autocomplete support for (neo)vim

**Note**: you may not need this plugin anymore see: [type check in javascript](https://github.com/microsoft/TypeScript/issues/14377)

> this plugin will default disable when use `@type` check

features:

- auto-complete
- hover document

![image](https://user-images.githubusercontent.com/5492542/64189804-99667600-cea7-11e9-8eb5-133dfa9b0561.png)

## Installation

``` vim
:CocInstall coc-webpack
```

## Usage

For now it support below format:

file name: `webpack.config.js`:

``` javascript
module.exports = {
    |
}
```

### Command

start webpack watch task for current project

``` vim
:CocCommand webpack.watch
```

### coc-list

``` vim
:CocList webpack

:CocList webpackErrors
```

### Configuration

``` jsonc
"coc-webpack.enable": true, // enable coc-webpack
"coc-webpack.disableWhenUseTypeCheck": true, // disable coc-webpack when use @type {import('webpack').Configuration} comment
"coc-webpack.trace.server": "off" // Trace level of coc-webpack
```
