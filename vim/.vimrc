" my own indentation for C using the coding styles
set cindent
set tabstop=4
set noexpandtab
set smartindent
set cino=:0,+0,(2,J0,{1,}0,>4,)1,m2

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

"some nice keymappings
let mapleader=","
map <leader>e :call Exposee()<CR>
map <leader>w :w<CR>
map <leader>q :q<CR>
map <leader>v :vsp<CR>
map <leader>h :sp<CR>
map <leader>n :Vexplore<CR>
map <leader>p :set paste<CR>
map <leader>o :set nopaste<CR>
map <leader>f :ClangFormat<CR>
map <leader><Space> :%s/\s\+$//e<CR>

" fine zooming
map <C-J> 2<C-W>+
map <C-K> 2<C-W>-
map <C-L> 2<C-W>>
map <C-H> 2<C-W><

" fine frame moving
map <C-Y> <C-W>h
map <C-U> <C-W>j
map <C-I> <C-W>k
map <C-O> <C-W>l


au BufNewFile,BufRead *.vala setf cs
au BufNewFile,BufRead *.vapi setf cs
au BufNewFile,BufRead *.gtkaml setf cs
au BufNewFile,BufRead *.gtkon setf cs

filetype indent on
filetype plugin on
set foldmethod=marker
set hlsearch
set paste
set mouse=a
set clipboard=unnamed
set vb t_vb="."
sy on
set relativenumber
set nu
set t_Co=256 " 256 Color Term
set ic "case insensitive searches
cmap w!! w !sudo tee % >/dev/null
"catch unicode annoying error
set listchars+=nbsp:x
set background=dark
colorscheme PaperColor
"let g:netrw_banner=0        " disable annoying banner
"let g:netrw_browse_split=4  " open in prior window
"let g:netrw_altv=1          " open splits to the right
"let g:netrw_liststyle=3     " tree view
"let g:netrw_list_hide=netrw_gitignore#Hide()
"let g:netrw_list_hide.=',\(^\|\s\s\)\zs\.\S\+'
"let g:netrw_winsize = 25
set path+=**
set wildmenu
autocmd FileType python setlocal smartindent shiftwidth=4 ts=4 et cinwords=if,elif,else,for,while,try,except,finally,def,class
set tags+=/usr/include/tags

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

"rust

let $RUST_SRC_PATH="/Users/alvaro/.multirust/toolchains/stable-x86_64-apple-darwin/lib/rustlib/src/rust/src"
let g:rustfmt_autosave = 1
let g:racer_experimental_completer = 1
