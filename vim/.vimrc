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
noremap <leader><tab> :tabnext<cr>
noremap <leader><s-tab> :tabprev<cr>

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
set colorcolumn=81
set listchars+=nbsp:x

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
set paste

" Search tweaks
set hlsearch
set incsearch
set ignorecase
set smartcase

set wildmenu
set wildmode=list:longest

set wildignore+=.hg,.git,.svn                    " Version control
set wildignore+=*.aux,*.out,*.toc                " LaTeX intermediate files
set wildignore+=*.jpg,*.bmp,*.gif,*.png,*.jpeg   " binary images
set wildignore+=*.o,*.obj,*.exe,*.dll,*.manifest " compiled object files
set wildignore+=*.spl                            " compiled spelling word lists
set wildignore+=*.sw?                            " Vim swap files
set wildignore+=*.DS_Store                       " OSX bullshit
set wildignore+=*.dSYM                           " OSX debug info directories

set wildignore+=*.luac                           " Lua byte code
set wildignore+=*.pyc                            " Python byte code

" increase history
set history=1000

" UI tweaks
set ruler
set nolazyredraw
set number

autocmd! bufwritepost vimrc source ~/.vimrc

" Remove the Windows ^M - when the encodings gets messed up
noremap <Leader>m mmHmt:%s/<C-V><cr>//ge<cr>'tzt'm

" Highlightning
hi clear CursorLine
set cursorline
set background=light
