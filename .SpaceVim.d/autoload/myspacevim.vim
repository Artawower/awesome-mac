function! myspacevim#before() abort

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" 
"      								      TEMPLATES
"
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  let g:ayucolor="light"

  function! SetPalenightTheme()
    let g:spacevim_custom_color_palette = []
    colorscheme palenight
    let g:spacevim_colorscheme = 'palenight'
    :set background=dark
    highlight Conceal guifg=#c792ea guibg=NONE
    " let g:airline_theme='palenight'
  endfunction

  nnoremap <F16> :call SetPalenightTheme()<cr>

  function! SetLightPapperTheme()
    let g:spacevim_custom_color_palette = [
        \ ["#eeeeee", "#005faf", 246, 235],
        \ ["#444444", "#afd7ff", 239, 246],
        \ ["#444444", "#afd7ff", 237, 246],
        \ ["#b2b2b2", 241],
        \ ["#e4e4e4", "#83a598", 235, 109],
        \ ["#e4e4e4", "#fe8019", 235, 208],
        \ ["#e4e4e4", "#8ec07c", 235, 108],
        \ ["#e4e4e4", "#689d6a", 235, 72],
        \ ["#e4e4e4", "#8f3f71", 235, 132],
    \ ]
    colorscheme PaperColor
    set background=light
    let g:airline_theme='PaperColor'
    let g:spacevim_colorscheme = 'PaperColor'
    let g:spacevim_colorscheme_bg = 'light'
    highlight Conceal guifg=#005faf  guibg=NONE
  endfunction

  nnoremap <F4> :call SetLightPapperTheme()<cr>

  function! SetGruvTheme()
     let g:spacevim_custom_color_palette = []
     colorscheme gruvbox
     :set background=dark
     highlight Conceal guifg=#fe8019 guibg=NONE
  endfunction

  nnoremap <F15> :call SetGruvTheme()<cr>
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" 
"      											MAPPING
"
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
   " Resize pane
   noremap <silent> <C-S-Left> :vertical resize -5<CR>
   noremap <silent> <C-S-Right> :vertical resize +5<CR>
   noremap <silent> <C-S-l> :vertical resize -5<CR>
   noremap <silent> <C-S-h> :vertical resize +5<CR>
   noremap <silent> <C-S-Up> :resize -5<CR>
   noremap <silent> <C-S-Down> :resize +5<CR>
   noremap <silent> <C-S-k> :resize -5<CR>
   noremap <silent> <C-S-j> :resize +5<CR>

   " Save file like VsCode
   noremap <silent> <C-S> :update<CR>
   vnoremap <silent> <C-S> <C-C>:update<CR>
   inoremap <silent> <C-S> <C-O>:update<CR>
   noremap <Leader>k :noa w<CR>

   " Tab switching
   map tn :bn<cr>
   map tp :bp<cr>
   map tc :bd<cr>
   " Close buffer, preserve window
   map <leader>q :bp<bar>sp<bar>bn<bar>bd<CR>


   nmap <silent> <C-p> :Files<CR> " Fast file opening for search

   " Clipboard
   noremap <Leader>y "*y
   noremap <Leader>p "*p
   noremap <Leader>Y "+y
   noremap <Leader>P "+p

   " MAC os binding for NERDTree
   nnoremap † <Esc> :NERDTreeToggle<CR> " ALT+T - toggle file
   nnoremap ƒ <Esc> :NERDTreeFind<CR>== " ALT+F reveal current file in nerdtree
   nnoremap ø :NERDTree<CR>== " ALT+O open nerdtree 

   " FZF binding for file History
   noremap ˙ <Esc> :History<CR>
   
   noremap <F20> :PrevColorScheme<CR>
   noremap <F21> :RandomColorScheme<CR>

   " " Go debug, enable if use instead vimspecotr
   " autocmd FileType go          nnoremap <buffer> <F5> :GoDebugStart<CR>
   " autocmd FileType go          nnoremap <buffer> <F4> :GoDebugBreakpoint<CR>
   " autocmd FileType go          nnoremap <buffer> <F6> :GoDebugNext<CR>
   " autocmd FileType go          nnoremap <buffer> <F7> :GoDebugContinue<CR>

   " Git 
   noremap <Leader>s <Esc> :Gstatus<CR> " Get git status
   noremap <Leader>l <Esc> :Gdiffsplit \| HEAD~1<CR> " Split git info for compare last commit


   " Quickfix navigation
   noremap <Leader>o :.cc<CR>

   " Notes
   noremap <Leader>n :SearchNotes 

 """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
 " 
 "      											PLUGIN CONFIGS
 "
 """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
 let g:blamer_enabled = 1 " enable blamer
 let g:blamer_show_in_insert_modes = 0 " disable blamer in insert mode

 " Closetag
 let g:closetag_filenames = '*.html,*.xhtml,*.phtml'
 let g:closetag_filetypes = 'html,xhtml,phtml'
 let g:closetag_xhtml_filetypes = 'xhtml,jsx'
 let g:closetag_regions = {
     \ 'typescript.tsx': 'jsxRegion,tsxRegion',
     \ 'javascript.jsx': 'jsxRegion',
     \ }
 " coc
 let g:coc_global_extensions = ['coc-tslint-plugin', 'coc-tsserver', 'coc-emmet', 'coc-css', 'coc-html', 'coc-json', 'coc-yank', 'coc-vetur']
 inoremap <silent><expr> <c-space> coc#refresh()

 let g:LanguageClient_serverCommands = {
     \ 'vue': ['vls']
     \ }
 " Vue
 let g:vim_vue_plugin_use_pug = 1
 " let g:vue_disable_pre_processors=1
 let g:vue_pre_processors = 'detect_on_enter'
 " let g:vue_pre_processors = ['sass', 'pug']
 autocmd BufRead,BufNewFile *.vue setlocal filetype=vue.html.javascript.pug
 "
 let g:neomake_javascript_enabled_makers = ['eslint']
 " GoTo code navigation.
 nmap <silent> <Leader>gd <Plug>(coc-definition)
 nmap <silent> <Leader>d <Plug>(coc-definition)
 nmap <silent> td <Plug>(coc-definition)
 nmap <silent> gy <Plug>(coc-type-definition)
 nmap <silent> gi <Plug>(coc-implementation)
 nmap <silent> ti <Plug>(coc-implementation)
 nmap <silent> gr <Plug>(coc-references)

 nmap <silent> [g <Plug>(coc-diagnostic-prev)
 nmap <silent> ]g <Plug>(coc-diagnostic-next)



 " FZF
 let $FZF_DEFAULT_OPTS    = '--reverse'
 let $FZF_DEFAULT_COMMAND = "rg --files --hidden --glob '!.git/**'"

 let g:fzf_colors = { 'fg': ['fg', 'Normal'], 
                 \ 'bg': ['bg', 'Normal'], 
                 \ 'hl': ['fg', 'Comment'], 
                 \ 'fg+': ['fg', 'CursorLine', 'CursorColumn', 'Normal'], 
                 \ 'bg+': ['bg', 'CursorLine', 'CursorColumn'], 
                 \ 'hl+': ['fg', 'Statement'], 
                 \ 'info': ['fg', 'PreProc'], 
                 \ 'border': ['fg', 'Ignore'], 
                 \ 'prompt': ['fg', 'Conditional'], 
                 \ 'pointer': ['fg', 'Exception'], 
                 \ 'marker': ['fg', 'Keyword'], 
                 \ 'spinner': ['fg', 'Label'], 
                 \ 'header': ['fg', 'Comment'] }

 " noremap <silent> <Leader>ag :Ag <C-R><C-W><CR>
 function! FzfSearchCurrentWord()
   let l:word = expand('<cword>')
     call fzf#vim#ag(l:word)
 endfunction

 noremap <Leader>[ :call FzfSearchCurrentWord()<cr>

 let $FZF_DEFAULT_OPTS    = '--reverse'
 let $FZF_DEFAULT_COMMAND = "rg --files --hidden --glob '!.git/**'"
 " let $FZF_DEFAULT_OPTS="--layout=reverse --info=inline"
 let g:fzf_layout = {
             \ 'window': {
             \ 'width': 0.94,
             \ 'height': 0.68,
             \ }
             \ }
 autocmd! FileType fzf tnoremap <buffer> <esc> <c-c> " FZF closing on esc (neovim support)

 let g:fzf_colors = { 'fg': ['fg', 'Normal'], 
                 \ 'bg': ['bg', 'Normal'], 
                 \ 'hl': ['fg', 'Comment'], 
                 \ 'fg+': ['fg', 'CursorLine', 'CursorColumn', 'Normal'], 
                 \ 'bg+': ['bg', 'CursorLine', 'CursorColumn'], 
                 \ 'hl+': ['fg', 'Statement'], 
                 \ 'info': ['fg', 'PreProc'], 
                 \ 'border': ['fg', 'Ignore'], 
                 \ 'prompt': ['fg', 'Conditional'], 
                 \ 'pointer': ['fg', 'Exception'], 
                 \ 'marker': ['fg', 'Keyword'], 
                 \ 'spinner': ['fg', 'Label'], 
                 \ 'header': ['fg', 'Comment'] }

 " noremap <silent> <Leader>ag :Ag <C-R><C-W><CR>
" JS
" let g:javascript_conceal_function             = "ƒ"
" let g:javascript_conceal_null                 = "ø"
" let g:javascript_conceal_this                 = "@"
" let g:javascript_conceal_return               = "⇚"
" let g:javascript_conceal_undefined            = "¿"
" let g:javascript_conceal_NaN                  = "ℕ"
" let g:javascript_conceal_prototype            = "¶"
" let g:javascript_conceal_static               = "•"
" let g:javascript_conceal_super                = "Ω"
" let g:javascript_conceal_arrow_function       = "⇒"
" let g:javascript_conceal_noarg_arrow_function = "🞅"
" let g:javascript_conceal_underscore_arrow_function = "🞅"
let g:javascript_conceal = 0

let g:yats_host_keyword = 1
 " Python jedo
 let g:jedi#goto_command = "<C-]>"

 let g:pymode_lint_write = 0
 let g:virtualenv_auto_activate = 1 " autoactivate venv

 " Node/golang inspect and debug
 " let g:vimspector_enable_mappings = 'HUMAN'
 let g:vimspector_enable_mappings = 'VISUAL_STUDIO'
 let g:vimspector_install_gadgets = ['enable-go', 'force-enable-chrome', 'force-enable-node']
 nmap <F17> <Plug>VimspectorStop


 " Prettier
 " autocmd BufWritePre *.js,*.jsx,*.mjs,*.ts,*.tsx,*.css,*.less,*.scss,*.json,*.graphql,*.md,*.vue,*.yaml,*.html PrettierAsync
 autocmd BufWritePre *.js,*.jsx,*.mjs,*.ts,*.tsx,*.css,*.less,*.scss,*.json,*.graphql,*.md,*.yaml,*.html PrettierAsync
 " *.js,*.jsx,*.mjs,*.ts,*.tsx,*.css,*.less,*.scss,*.json,*.graphql,*.py PrettierAsync
 " autocmd TextChanged,InsertLeave *.js,*.jsx,*.mjs,*.ts,*.tsx,*.css,*.less,*.scss,*.json,*.graphql,*.md,*.vue,*.yaml,*.html PrettierAsync
 autocmd FileType typescript setlocal completeopt-=menu
 noremap <Leader>f :PrettierAsync<CR>

 let g:prettier#autoformat = 0
 let g:prettier#exec_cmd_async = 0
 let g:prettier#config#parser = 'babylon'

 let g:prettier#autoformat_require_pragma = 'true'
 let g:prettier#config#semi = 1
 let g:prettier#autoformat_config_present = 0
 let g:prettier#quickfix_enabled = 0

 " GBrowse
 let g:github_enterprise_urls = ['https://git@gitlab.com:ayfool']
 let g:fugitive_bitbucketservers_domains = ['https://bitbucket.org']

 " Ident line
 let g:indentLine_setColors = 0 " color for line indent
 let g:indentLine_char = '⎸' " separate simbol

 " Ultisnipets
 let g:UltiSnipsExpandTrigger="<tab>"
 let g:UltiSnipsJumpForwardTrigger="<c-b>"
 let g:UltiSnipsJumpBackwardTrigger="<c-z>"

 " If you want :UltiSnipsEdit to split your window.
 let g:UltiSnipsEditSplit="vertical"

 " Vim signatures
 let g:SignatureMarkTextHL = 1

 " Markdown
 let g:mkdp_browserfunc = ''
 let g:mkdp_auto_start = 1

 " Polyglot
 let g:polyglot_disabled = ['coffee-script']

 " Golang
 let g:go_highlight_functions = 1
 let g:go_highlight_function_calls = 1
 let g:go_metalinter_enabled = ['vet', 'errcheck']
 " let g:go_metalinter_enabeld = ['deadcode', 'errcheck', 'gosimple', 'govet', 'staticcheck', 'typecheck', 'unused', 'varcheck']
 " let g:go_debug=['shell-commands']
 " let g:go_metalinter_autosave_enabled = ['vet', 'errcheck', 'errcheck']
 let g:go_metalinter_command = "golangci-lint"
 let g:go_metalinter_autosave = 0
 let g:go_metalinter_deadline = "5s"
 let g:go_list_type = "quickfix"
 setlocal expandtab

 autocmd BufWritePre *.go :call CocAction('organizeImport') " Missing import for golang
 autocmd BufWritePost *.go GoMetaLinter

 map <C-n> :cnext<CR>
 map <C-l> :cprevious<CR>
 nnoremap <leader>a :cclose<CR>
 " Markdown
 nmap <C-m> <Plug>MarkdownPreview
 nmap <M-s> <Plug>MarkdownPreviewStop
 nmap <C-t> <Plug>MarkdownPreviewToggle

 
 """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
 " 
 "      								   FUNCTIONS
 "
 """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
 function! JsonToType()
    let extension = expand('%:e')
    echom extension
    :%!quicktype -l golang<CR>
 endfunction

noremap <Leader>c :call JsonToType()
 

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" 
"      								        NOTES
"
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
:let g:notes_directories = ['/Users/arturarosenko/Yandex.Disk.localized/notes']
:let g:notes_suffix = '.md'

 """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
 " 
 "      										COMMON CONFIG
 "
 """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
 set mouse=''
 set colorcolumn=120
 " let g:spacevim_max_column=120
 let g:spacevim_max_column = 110
 hi ColorColumn ctermbg=red guibg=red
 " performance
 set synmaxcol=120
 set nocursorcolumn
 set nocursorline
 set norelativenumber
 syntax sync minlines=256
 autocmd BufEnter * :syn sync maxlines=200
 set scrolljump=5
 set re=1
 set viminfo='100,<50,s10,h,%

endfunction

function! myspacevim#after() abort


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" 
"      								     AUTOCMD
"
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

   call matchadd('Conceal', 'this\ze\.\w\+\d*', 1, -1, {'conceal':'@'})
   call matchadd('Conceal', '(\(NaN\))|( NaN )|( NaN\n+)|(NaN(?=\n|;))', 10, -1, {'conceal':'ℕ'})
   call matchadd('Conceal', 'NaN\ze\(;\|,\|)|\n\|\>\)', 10, -1, {'conceal':'ℕ'})
   call matchadd('Conceal', 'null\ze\(;\|,\|)|\n\|\>\)', 10, -1, {'conceal':'∅'})
   call matchadd('Conceal', '()\ze => ', 10, -1, {'conceal': '⨍'})
   call matchadd('Conceal', 'return', 10, -1, {'conceal': "⏎"})
   call matchadd('Conceal', 'return\ze;', 10, -1, {'conceal': "⏎';"})
   call matchadd('Conceal', 'Object\ze.', 10, -1, {'conceal': '𝚯'})
   call matchadd('Conceal', '>=', 1, -1, {'conceal': '≥'})
   call matchadd('Conceal', '<=', 1, -1, {'conceal': '≤'})
   call matchadd('Conceal', '=>', 1, -1, {'conceal': '⇒'})
   call matchadd('Conceal', '===', 1, -1, {'conceal': '≡'})

   set conceallevel=1

   function! HighlightConceal()
      set conceallevel=1
      " highlight Conceal guifg=#c792ea guibg=NONE
   endfunction

   augroup conceal
      autocmd!
      autocmd InsertEnter * :set conceallevel=0
      autocmd InsertLeave * :call HighlightConceal()
      autocmd bufenter * :call HighlightConceal()
      autocmd BufEnter *.vue :set norelativenumber
   augroup END


endfunction
