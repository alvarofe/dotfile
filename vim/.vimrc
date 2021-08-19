call plug#begin('~/.vim/plugged')

Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-fugitive'
Plug 'ackyshake/VimCompletesMe'
Plug 'vimwiki/vimwiki'
Plug 'preservim/nerdcommenter'
Plug 'godlygeek/tabular'
Plug 'plasticboy/vim-markdown'
Plug 'AndrewRadev/splitjoin.vim'
Plug 'bignimbus/pop-punk.vim'
Plug 'fcpg/vim-shore'
Plug 'prabirshrestha/vim-lsp'
Plug 'prabirshrestha/async.vim'
Plug 'prabirshrestha/asyncomplete.vim'
Plug 'prabirshrestha/asyncomplete-lsp.vim'
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
Plug 'thomasfaingnaert/vim-lsp-snippets'
Plug 'thomasfaingnaert/vim-lsp-ultisnips'
Plug 'ervandew/supertab'
Plug 'rust-lang/rust.vim'

call plug#end()

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
map <C-m> i<CR><C-t><Esc>h
map <C-Return> i<CR><C-o>k<C-t>

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

sy on
set background=dark
set termguicolors
set tw=80
set t_Co=256
set showmatch           " show matching brackets
set mat=5               " how many tenths of a second to blink matching brackets for
set incsearch           " search as you type
set hls noignorecase    " Highlight search
set wildignore+=*.so,*.o,*.a,*.la,*.class,*.obj,.git,*.beam,*.mo,*.swp,*.jpg,*.png,*.xpm,*.gif,*.pyc
set wildmenu
set foldmethod=syntax
set foldlevelstart=20
set noswapfile
set noundofile
set fillchars+=vert:│
set path+=**
set hlsearch
set mouse=ivn
map <ScrollWheelUp> <C-Y>
map <ScrollWheelDown> <C-E>
set clipboard=unnamedplus
set vb t_vb="."
set expandtab
set autoindent

"clipboard
noremap <leader>y "*y
noremap <leader>p "*p
noremap <leader>Y "+y
noremap <leader>P "+p

" if hidden is not set, TextEdit might fail.
set hidden

" Some servers have issues with backup files, see #649
set nobackup
set nowritebackup

" Better display for messages
set cmdheight=2

" You will have bad experience for diagnostic messages when it's default 4000.
set updatetime=300

" don't give |ins-completion-menu| messages.
"set shortmess+=c

" always show signcolumns
set signcolumn=no

" Remap keys for LSP
nmap <silent> gd :LspDefinition<CR>
nmap <silent> gy :LspTypeDefinition<CR>
nmap <silent> gi :LspImplementation<CR>
nmap <silent> gr :LspReferences<CR>
nmap <silent> gR :LspRename<CR>
nmap <silent> gk :LspHover<CR>

hi Visual term=reverse cterm=reverse guibg=Grey
highlight NormalFloat cterm=NONE ctermfg=14 ctermbg=0 gui=NONE guifg=#93a1a1 guibg=#002931
autocmd ColorScheme * highlight VertSplit cterm=NONE ctermfg=Green ctermbg=NONE

"NerdCommenter
" Add spaces after comment delimiters by default
let g:NERDSpaceDelims = 1

" Use compact syntax for prettified multi-line comments
let g:NERDCompactSexyComs = 3

" MARKDOWN
autocmd BufNewFile,BufRead *.md set filetype=markdown
" Hide and format markdown elements like **bold**
autocmd FileType markdown set conceallevel=2
let g:vim_markdown_conceal = 2
let g:vim_markdown_conceal_code_blocks = 0
let g:vim_markdown_math = 1
let g:vim_markdown_toml_frontmatter = 1
let g:vim_markdown_frontmatter = 1
let g:vim_markdown_strikethrough = 1
let g:vim_markdown_autowrite = 1
let g:vim_markdown_edit_url_in = 'tab'
let g:vim_markdown_follow_anchor = 1


set statusline=
set statusline+=%<%f\ %h%m%r             " filename and flags
set statusline+=%{fugitive#statusline()} " git info
set statusline+=%=                       " alignment separator
set statusline+=[%{&ft}]                 " filetype
set statusline+=%-14.([%l/%L],%c%V%)     " cursor info

set number
set relativenumber
set ruler
set diffopt+=vertical,iwhite,algorithm:patience,indent-heuristic
set linebreak showbreak=+
set listchars=eol:.,tab:\|-
set laststatus=2

" vim-shore
let g:shore_stayonfront = 1

" Colorscheme
set termguicolors
colorscheme torte

" Use deoplete.
let g:deoplete#enable_at_startup = 1

if executable('ccls')
   au User lsp_setup call lsp#register_server({
      \ 'name': 'ccls',
      \ 'cmd': {server_info->['ccls']},
      \ 'root_uri': {server_info->lsp#utils#path_to_uri(
      \   lsp#utils#find_nearest_parent_file_directory(
      \     lsp#utils#get_buffer_path(), ['.ccls', 'compile_commands.json', '.git/']))},
      \ 'initialization_options': {
      \   'highlight': { 'lsRanges' : v:true },
      \   'cache': {'directory': '/tmp/cache' . '/ccls' },
      \ },
      \ 'whitelist': ['c', 'cpp', 'objc', 'objcpp', 'cc', 'h', 'hpp'],
      \ })
endif

" if executable('rls')
    " au User lsp_setup call lsp#register_server({
        " \ 'name': 'rls',
        " \ 'cmd': {server_info->['rustup', 'run', 'stable', 'rls']},
        " \ 'workspace_config': {'rust': {'clippy_preference': 'on'}},
        " \ 'whitelist': ['rust'],
        " \ })
" endif
"
if executable('rust-analyzer')
  au User lsp_setup call lsp#register_server({
        \   'name': 'Rust Language Server',
        \   'cmd': {server_info->['rust-analyzer']},
        \   'whitelist': ['rust'],
        \   'initialization_options': {
        \     'cargo': {
        \       'loadOutDirsFromCheck': v:true,
        \     },
        \     'procMacro': {
        \       'enable': v:true,
        \     },
        \   },
        \ })
endif

if executable('pyls')
    " pip install python-language-server
    au User lsp_setup call lsp#register_server({
          \ 'name': 'pyls',
          \ 'cmd': {server_info->['pyls']},
          \ 'allowlist': ['python'],
          \ 'workspace_config': {
          \    'pyls':
          \        {'configurationSources': ['flake8'],
          \         'plugins': {'flake8': {'enabled': v:false},
          \                     'pyflakes': {'enabled': v:false},
          \                     'pycodestyle': {'enabled': v:false},
          \                    }
          \         }
          \ }})
endif

let g:lsp_diagnostics_enabled = 0         " disable diagnostics support
let g:lsp_document_highlight_enabled = 0

set guioptions-=m  "remove menu bar
set guioptions-=T  "remove toolbar
set guioptions-=r  "remove right-hand scroll bar
set guioptions-=L  "remove left-hand scroll bar
set guifont=Source\ Code\ Pro\ Medium\ 9
set noerrorbells visualbell t_vb=
autocmd GUIEnter * set visualbell t_vb=

let g:SuperTabDefaultCompletionType    = '<C-n>'
let g:SuperTabCrMapping                = 1
let g:UltiSnipsExpandTrigger           = '<tab>'
let g:UltiSnipsJumpForwardTrigger      = '<tab>'
let g:UltiSnipsJumpBackwardTrigger     = '<s-tab>'
hi Pmenu guibg=blue

