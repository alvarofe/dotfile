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
noremap <leader>m :Gstatus<CR>
noremap <leader>f :Explore<CR>
noremap <leader>o :set nopaste<CR>
noremap <leader><Space> :%s/\s\+$//e<CR>
noremap <leader><tab> :tabnext<cr>
noremap <leader><s-tab> :tabprev<cr>
noremap <leader>b :Buffer<cr>
noremap <leader>s :cs find s <C-R>=expand("<cword>")<CR><CR>	
noremap <leader>g :cs find g <C-R>=expand("<cword>")<CR><CR>	
noremap <leader>c :cs find c <C-R>=expand("<cword>")<CR><CR>	
noremap <leader>t :Tags<cr>
noremap <leader>i :cs find i ^<C-R>=expand("<cfile>")<CR>$<CR>
nnoremap <leader>gm /\v^\<\<\<\<\<\<\< \|\=\=\=\=\=\=\=$\|\>\>\>\>\>\>\> /<cr>
"noremap <leader>d :cs find d <C-R>=expand("<cword>")<CR><CR>

runtime! config/**/*.vim

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

map <c-p> :Files<cr>

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

set paste

" Search tweaks
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
let g:netrw_list_hide='.*\.o$,$.*\.so$.*\.d$,.*\.swp$,\(^\|\s\s\)\zs\.\S\+'
"let g:netrw_browse_split=4  " open in prior window
"let g:netrw_altv=1          " open splits to the right
"let g:netrw_liststyle=3     " tree view
"let g:netrw_list_hide=netrw_gitignore#Hide()
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

colorscheme desert256 

set showmatch           " show matching brackets
set mat=5               " how many tenths of a second to blink matching brackets for
set incsearch           " search as you type
set hls noignorecase    " Highlight search

set directory^=$HOME/.vim/swapfiles/
set encoding=utf-8
set autoindent

set wildignore+=*.so,*.o,*.a,*.la,*.class,*.obj,.git,*.beam,*.mo,*.swp,*.jpg,*.png,*.xpm,*.gif,*.pyc
set wildmenu

set completeopt=menu,longest
hi Visual term=reverse cterm=reverse guibg=Grey

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

noremap <f1> :bprev<CR> 
noremap <f2> :bnext<CR>

map <C-c> :s/^/\/\//<Enter>
map <C-u> :s/^\/\///<Enter>
set foldmethod=syntax
set foldlevelstart=20
set rtp+=~/.fzf

" rust
"let g:rustfmt_autosave = 1
set hidden
let g:racer_cmd = "/home/alvaro/.cargo/bin/racer"
let g:racer_experimental_completer = 1

