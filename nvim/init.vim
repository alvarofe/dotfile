set runtimepath^=~/.vim runtimepath+=~/.vim/after
let &packpath = &runtimepath

set nocompatible
filetype plugin on
filetype indent on

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

" fine zooming
map <C-J> 15<C-W>+
map <C-K> 15<C-W>-
map <C-L> 15<C-W>>
map <C-H> 15<C-W><

"some nice keymappings
noremap <leader>e :call Exposee()<CR>
noremap <leader>w :w<CR>
noremap <leader>q :q<CR>
noremap <leader>m :Gstatus<CR>
noremap <leader>f :Explore<CR>
noremap <leader><Space> :%s/\s\+$//e<CR>
noremap <leader>b :Buffer<cr>
noremap <leader>g :cs find g <C-R>=expand("<cword>")<CR><CR>
noremap <leader>c :cs find c <C-R>=expand("<cword>")<CR><CR>
noremap <leader>t :Tags<cr>
nnoremap <leader>gm /\v^\<\<\<\<\<\<\< \|\=\=\=\=\=\=\=$\|\>\>\>\>\>\>\> /<cr>
map <C-m> i<CR><C-t><C-t><Esc>h
map <C-Return> i<CR><CR><C-o>k<C-t>
map <C-c> :s/^/\/\//<Enter>
map <C-u> :s/^\/\///<Enter>

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
set termguicolors
set t_Co=256
set showmatch           " show matching brackets
set mat=5               " how many tenths of a second to blink matching brackets for
set incsearch           " search as you type
set hls noignorecase    " Highlight search
set wildignore+=*.so,*.o,*.a,*.la,*.class,*.obj,.git,*.beam,*.mo,*.swp,*.jpg,*.png,*.xpm,*.gif,*.pyc
set wildmenu
set foldmethod=syntax
set foldlevelstart=20
set completeopt=menu,menuone,noinsert,noselect
set completeopt-=preview
set noswapfile
set noundofile
set fillchars+=vert:│
set autoindent
set smartindent
sy on
set path+=**
set hlsearch
set mouse=a
set clipboard=unnamedplus
set vb t_vb="."
set paste
set expandtab
set tw=792034

let uname = substitute(system('uname'), '\n', '', '')

" FZF
if uname == "Darwin"
  set rtp+=/usr/local/opt/fzf
else
  set rtp+=~/.fzf
endif

"clipboard
noremap <leader>y "*y
noremap <leader>p "*p
noremap <leader>Y "+y
noremap <leader>P "+p

hi Visual term=reverse cterm=reverse guibg=Grey
highlight NormalFloat cterm=NONE ctermfg=14 ctermbg=0 gui=NONE guifg=#93a1a1 guibg=#002931
autocmd ColorScheme * highlight VertSplit cterm=NONE ctermfg=Green ctermbg=NONE

