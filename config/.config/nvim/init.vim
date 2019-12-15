set runtimepath^=~/.vim runtimepath+=~/.vim/after
let &packpath = &runtimepath

set nocompatible
filetype plugin on
filetype indent on
set nu
set relativenumber

runtime! config/**/*.vim

let mapleader = ","

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
noremap <leader>e :call Exposee()<CR>
noremap <leader>w :w<CR>
noremap <leader>q :q<CR>
noremap <leader>v :vsp<CR>
noremap <leader>h :sp<CR>
noremap <leader>m :Gstatus<CR>
noremap <leader>f :Explore<CR>
noremap <leader>o :set nopaste<CR>
noremap <leader><Space> :%s/\s\+$//e<CR>
noremap <leader><tab> :tabnext<cr>
noremap <leader><s-tab> :tabprev<cr>
noremap <leader>b :Buffer<cr>
noremap <leader>s :TagbarToggle<CR>
noremap <leader>g :cs find g <C-R>=expand("<cword>")<CR><CR>
noremap <leader>c :cs find c <C-R>=expand("<cword>")<CR><CR>
noremap <leader>t :Tags<cr>
noremap <leader>i :cs find i ^<C-R>=expand("<cfile>")<CR>$<CR>
nnoremap <leader>gm /\v^\<\<\<\<\<\<\< \|\=\=\=\=\=\=\=$\|\>\>\>\>\>\>\> /<cr>


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
set paste

" Search tweaks
" UI tweaks
set ruler
set nolazyredraw

autocmd! bufwritepost vimrc source ~/.vimrc

" Remove the Windows ^M - when the encodings gets messed up
"noremap <Leader>m mmHmt:%s/<C-V><cr>//ge<cr>'tzt'm

let g:netrw_list_hide='.*\.o$,$.*\.so$.*\.d$,.*\.swp$,\(^\|\s\s\)\zs\.\S\+'

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

if (isdirectory(".git"))
  map <c-p> :GFiles<cr>
else
  map <c-p> :Files<cr>
endif

set background=dark
set t_Co=256

if (has("termguicolors"))
  let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
  let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
  set termguicolors
endif

"colorscheme koehler

hi Cursor guibg=#b60900 guifg=#000000
set showmatch           " show matching brackets
set mat=5               " how many tenths of a second to blink matching brackets for
set incsearch           " search as you type
set hls noignorecase    " Highlight search

set directory^=$HOME/.vim/swapfiles/
set encoding=utf-8
set autoindent

set wildignore+=*.so,*.o,*.a,*.la,*.class,*.obj,.git,*.beam,*.mo,*.swp,*.jpg,*.png,*.xpm,*.gif,*.pyc
set wildmenu

hi Visual term=reverse cterm=reverse guibg=Grey

map <C-c> :s/^/\/\//<Enter>
map <C-u> :s/^\/\///<Enter>
set foldmethod=syntax
set foldlevelstart=20

let uname = substitute(system('uname'), '\n', '', '')

" FZF
if uname == "Darwin"
  set rtp+=/usr/local/opt/fzf
else
  set rtp+=~/.fzf
endif

let $FZF_DEFAULT_OPTS .= ' --border --margin=0,2'

function! FloatingFZF()
  let buf = nvim_create_buf(v:false, v:true)
  call setbufvar(buf, '&signcolumn', 'no')

  let height = float2nr(40)
  let width = float2nr(80)
  let horizontal = float2nr((&columns - width) / 2)
  let vertical = 1
  let opts = {
        \ 'relative': 'editor',
        \ 'row': vertical,
        \ 'col': horizontal,
        \ 'width': width,
        \ 'height': height,
        \ 'style': 'minimal'
        \ }

  call nvim_open_win(buf, v:true, opts)
endfunction

let g:fzf_layout = { 'window': 'call FloatingFZF()' }

" rust
let g:rustfmt_autosave = 0
"set hidden
let g:racer_cmd = "/home/alvaro/.cargo/bin/racer"
let g:racer_experimental_completer = 1

"clipboard
noremap <leader>y "*y
noremap <leader>p "*p
noremap <leader>Y "+y
noremap <leader>P "+p

set completeopt=menu,menuone,noinsert,noselect
set completeopt-=preview

set fillchars+=vert:│
highlight NormalFloat cterm=NONE ctermfg=14 ctermbg=0 gui=NONE guifg=#93a1a1 guibg=#002931
autocmd ColorScheme * highlight VertSplit cterm=NONE ctermfg=Green ctermbg=NONE
