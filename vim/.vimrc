set nocompatible
filetype plugin on
filetype indent on
let mapleader = ","
"set nu
"set relativenumber
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
noremap <leader>s :cs find s <C-R>=expand("<cword>")<CR><CR>	
noremap <leader>g :cs find g <C-R>=expand("<cword>")<CR><CR>	
noremap <leader>c :cs find c <C-R>=expand("<cword>")<CR><CR>	
noremap <leader>t :cs find t <C-R>=expand("<cword>")<CR><CR>	
noremap <leader>i :cs find i ^<C-R>=expand("<cfile>")<CR>$<CR>
noremap <leader>d :cs find d <C-R>=expand("<cword>")<CR><CR>

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

set wildignore+=.hg,.git,.svn                    " Version control
set wildignore+=*.aux,*.out,*.toc                " LaTeX intermediate files
set wildignore+=*.jpg,*.bmp,*.gif,*.png,*.jpeg   " binary images
set wildignore+=*.o,*.obj,*.exe,*.dll,*.manifest " compiled object files
set wildignore+=*.spl                            " compiled spelling word lists
set wildignore+=*.sw?                            " Vim swap files
set wildignore+=*.DS_Store                       " OSX bullshit
set wildignore+=*.dSYM                           " OSX debug info directories
set wildignore+=*.o,*.d                          " compilation 

set wildignore+=*.luac                           " Lua byte code
set wildignore+=*.pyc                            " Python byte code

" increase history
set history=1000

" UI tweaks
set ruler
set nolazyredraw

autocmd! bufwritepost vimrc source ~/.vimrc

" Remove the Windows ^M - when the encodings gets messed up
"noremap <Leader>m mmHmt:%s/<C-V><cr>//ge<cr>'tzt'm

" Highlightning
" hi clear CursorLine
" set cursorline
set background=dark
set listchars=tab:\|\ 
set listchars+=nbsp:x
"set list

" Tweaks for browsing
"let g:netrw_banner=0        " disable annoying banner
"let g:netrw_browse_split=4  " open in prior window
"let g:netrw_altv=1          " open splits to the right
"let g:netrw_liststyle=3     " tree view
"let g:netrw_list_hide=netrw_gitignore#Hide()
"let g:netrw_list_hide.='.*\.o$,.*\.d$,.*\.swp$,\(^\|\s\s\)\zs\.\S\+'
"let g:netrw_winsize = 25

function! LoadCscope()
  let db = findfile("cscope.out", ".;")
  if (!empty(db))
    let path = strpart(db, 0, match(db, "/cscope.out$"))
    set nocscopeverbose " suppress 'duplicate connection' error
    exe "cs add " . db . " " . path
    set cscopeverbose
  endif
endfunction
au BufEnter /* call LoadCscope()

let g:ycm_confirm_extra_conf = 0
let g:ycm_server_python_interpreter = 'python2'
let g:ycm_global_ycm_extra_conf = "~/.ycm_extra_conf.py"
let g:ycm_show_diagnostics_ui = 0
colorscheme mrkn256



function MoveToPrevTab()
  "there is only one window
  if tabpagenr('$') == 1 && winnr('$') == 1
    return
  endif
  "preparing new window
  let l:tab_nr = tabpagenr('$')
  let l:cur_buf = bufnr('%')
  if tabpagenr() != 1
    close!
    if l:tab_nr == tabpagenr('$')
      tabprev
    endif
    sp
  else
    close!
    exe "0tabnew"
  endif
  "opening current buffer in new window
  exe "b".l:cur_buf
endfunc

function MoveToNextTab()
  "there is only one window
  if tabpagenr('$') == 1 && winnr('$') == 1
    return
  endif
  "preparing new window
  let l:tab_nr = tabpagenr('$')
  let l:cur_buf = bufnr('%')
  if tabpagenr() < tab_nr
    close!
    if l:tab_nr == tabpagenr('$')
      tabnext
    endif
    sp
  else
    close!
    tabnew
  endif
  "opening current buffer in new window
  exe "b".l:cur_buf
endfunc

map <C-m> :call MoveToNextTab()<CR><C-w>H
map <C-n> :call MoveToPrevTab()<CR><C-w>H

" switch tabs with Ctrl left and right
nnoremap <C-right> :tabnext<CR>
nnoremap <C-left> :tabprevious<CR>
" and whilst in insert mode
inoremap <C-right> <Esc>:tabnext<CR>
inoremap <C-left> <Esc>:tabprevious<CR>

set directory^=$HOME/.vim/swapfiles/
set encoding=utf-8
set autoindent
hi Visual term=reverse cterm=reverse guibg=Grey
