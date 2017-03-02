set nocompatible
filetype plugin on 
filetype indent on
let mapleader = ","
set nu
set relativenumber
noremap <leader>e :call Exposee()<CR>
noremap <leader>w :w<CR>
noremap <leader>q :q<CR>
noremap <leader>v :vsp<CR>
noremap <leader>h :sp<CR>
noremap <leader>p :set paste<CR>
noremap <leader>f :ClangFormat<CR>
noremap <leader>o :set nopaste<CR>
noremap <leader><Space> :%s/\s\+$//e<CR>

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

sy on
set path+=**
set hlsearch
set mouse=a
set clipboard=unnamed
set vb t_vb="."
set t_Co=256 " 256 Color Term
set colorcolumn=80
 
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
	   \ "ColumnLimit" : 80,
       \ "BreakBeforeBraces": "Attach",
       \ "AlignConsecutiveAssignments" : "true",
       \ "AllowAllParametersOfDeclarationOnNextLine" : "true",
       \ "BreakBeforeTernaryOperators": "false",
       \ "AllowShortIfStatementsOnASingleLine": "true",
       \ "AllowShortCaseLabelsOnASingleLine": "true",
       \ "AllowShortFunctionsOnASingleLine": "Inline",
       \ "AllowShortLoopsOnASingleLine": "true"}
