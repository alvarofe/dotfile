set t_Co=256
set noexpandtab
set smartindent
set cino=:0,+0,(2,J0,{1,}0,>4,)1,m2
execute pathogen#infect()
filetype plugin on


" pancake's exposee for vim:
let fs=0
fun Exposee()
if (g:fs == 0)
  res 1000
  vertical res 1000
  let g:fs=1
else
  exe "normal \<C-W>="
  let g:fs=0
endif
endfun

let relativenumber=0
set number
set relativenumber
fun NumberToggle()
  if(g:relativenumber == 1)
    set norelativenumber
    let g:relativenumber=0
  else
    set relativenumber
    let g:relativenumber=1
  endif
endfunc

nnoremap <C-n> :call NumberToggle()<CR>

"some nice keymappings
let mapleader=","
map <leader>e :call Exposee()<CR>
map <leader>w :w<CR>
map <leader>q :q<CR>
map <leader>v :vsp<CR>
map <leader>h :sp<CR>
map <leader>n :NERDTreeToggle<CR>
map <leader>p :set paste<CR>
map <leader>o :set nopaste<CR>
map <leader>b :e#<CR>
map <leader>f :ClangFormat<CR>
map <leader>l :TagbarToggle<CR>
map <leader><Space> :%s/\s\+$//e<CR>
map <F5> <C-W>=

map <F9> :cnext<cr>
map <F7> :cprevious<cr>

" fine zooming
map <C-J> 2<C-W>+
map <C-K> 2<C-W>-
map <C-L> 2<C-W>>
map <C-H> 2<C-W><

" fine frame moving
map <C-Y> <C-W>l
map <C-U> <C-W>j
map <C-I> <C-W>k
map <C-O> <C-W>h

au BufNewFile,BufRead *.vala setf cs
au BufNewFile,BufRead *.vapi setf cs
au BufNewFile,BufRead *.gtkaml setf cs
au BufNewFile,BufRead *.gtkon setf cs

filetype indent on
set hlsearch
set paste
syntax enable
"set nu
set mouse=a
"au BufNewFile,BufRead *.c set colorcolumn=80
set vb t_vb="."
highlight Comment ctermfg=green
set wildmenu            " visual autocomplete for command menu
set lazyredraw          " redraw only when we need to.
set showmatch           " highlight matching [{()}]
set foldenable          " enable folding
"set foldmethod=indent   " fold based on indent level

syntax on

" toggle gundo
nnoremap <leader>u :GundoToggle<CR>

" open ag.vim
nnoremap <leader>a :Ag

" CtrlP settings
let g:ctrlp_match_window = 'bottom,order:ttb'
let g:ctrlp_switch_buffer = 0
let g:ctrlp_working_path_mode = 0
let g:ctrlp_user_command = 'ag %s -l --nocolor --hidden -g ""'

noremap % v%
set listchars+=nbsp:.
set clipboard=unnamed

set hidden
let g:racer_cmd = "/Users/alvaro_fe/racer/target/release/racer"
let $RUST_SRC_PATH="/Users/alvaro_fe/projects/rust/rust/src"
let g:rustfmt_autosave = 1

au BufNewFile,BufRead *.rs set omnifunc=racer#Complete
au BufNewFile,BufRead *.rs nnoremap <buffer> gd :call racer#JumpToDefinition()<CR>

if has('gui_running')
  set guifont=Inconsolata\ for\ Powerline:h16
endif

highlight VertSplit cterm=none gui=none
set laststatus=0

"clang-format
let g:clang_format#style_options = {
	\ "Language": "Cpp",
	\ "MaxEmptyLinesToKeep": "1",
	\ "SpaceBeforeParens": "Always",
	\ "BasedOnStyle": "Google",
	\ "ContinuationIndentWidth": 8,
	\ "IndentCaseLabels": "false",
	\ "IndentFunctionDeclarationAfterType": "false",
	\ "IndentWidth": 8,
	\ "UseTab": "Always",
	\ "BreakBeforeBraces": "Attach",
	\ "AlignConsecutiveAssignments" : "true",
	\ "AllowAllParametersOfDeclarationOnNextLine" : "true",
	\ "BreakBeforeTernaryOperators": "false",
	\ "AllowShortIfStatementsOnASingleLine": "true",
	\ "AllowShortCaseLabelsOnASingleLine": "true",
	\ "AllowShortFunctionsOnASingleLine": "Inline",
	\ "AllowShortLoopsOnASingleLine": "true"}

"go
let g:go_highlight_functions = 1
let g:go_highlight_methods = 1
let g:go_highlight_structs = 1
let g:go_highlight_interfaces = 1
let g:go_highlight_operators = 1
let g:go_highlight_build_constraints = 1

let g:tagbar_type_go = {
    \ 'ctagstype' : 'go',
    \ 'kinds'     : [
        \ 'p:package',
        \ 'i:imports:1',
        \ 'c:constants',
        \ 'v:variables',
        \ 't:types',
        \ 'n:interfaces',
        \ 'w:fields',
        \ 'e:embedded',
        \ 'm:methods',
        \ 'r:constructor',
        \ 'f:functions'
    \ ],
    \ 'sro' : '.',
    \ 'kind2scope' : {
        \ 't' : 'ctype',
        \ 'n' : 'ntype'
    \ },
    \ 'scope2kind' : {
        \ 'ctype' : 't',
        \ 'ntype' : 'n'
    \ },
    \ 'ctagsbin'  : 'gotags',
    \ 'ctagsargs' : '-sort -silent'
\ }
colorscheme colorful256 

let g:rust_doc#downloaded_rust_doc_dir = '~/projects/rust/rust-docs/rust-docs'
