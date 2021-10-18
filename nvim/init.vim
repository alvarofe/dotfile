call plug#begin('~/.local/share/nvim/site/plugged')

Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-fugitive'
Plug 'vimwiki/vimwiki'
Plug 'preservim/nerdcommenter'
Plug 'plasticboy/vim-markdown'
Plug 'neovim/nvim-lspconfig'
Plug 'glepnir/lspsaga.nvim'
Plug 'rust-lang/rust.vim'
Plug 'nvim-lua/lsp_extensions.nvim'
Plug 'nvim-lua/completion-nvim'
Plug 'hrsh7th/nvim-compe'
Plug 'hrsh7th/vim-vsnip'
Plug 'godlygeek/tabular'
Plug 'szw/vim-maximizer'
Plug 'kristijanhusak/orgmode.nvim'
Plug 'vimlab/split-term.vim'
Plug 'nvim-lua/plenary.nvim'
Plug 'Mofiqul/vscode.nvim'
Plug 'Mofiqul/codedark.nvim'
Plug 'togglebyte/togglerust'
Plug 'unblevable/quick-scope'       " When using vim-plug

" Plug 'ray-x/guihua.lua', {'do': 'cd lua/fzy && make' }
" Plug 'ray-x/navigator.lua'
Plug 'ray-x/aurora'
" optional, if you need treesitter symbol support
" Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}

call plug#end()

set nocompatible
filetype plugin on
filetype indent on

let mapleader = ","

let fs=0
fun Expose()
if (g:fs == 0)
  res 1000
  vertical res 1000
  let g:fs=1
else
  exe "normal \<C-W>="
  let g:fs=0
endif
endfun

fun Linux()
  set ts=8
  set sw=8
  set noexpandtab
endfun

" fine zooming
map <C-J> 15<C-W>+
map <C-K> 15<C-W>-
map <C-L> 15<C-W>>
map <C-H> 15<C-W><

"some nice keymappings
noremap <leader>w :w<CR>
noremap <leader>q :q<CR>
noremap <leader>m :Git<CR>
noremap <leader>l :call Linux()<CR>
noremap <leader><Space> :%s/\s\+$//e<CR>
" noremap <leader>b :Buffer<cr>
noremap <leader>c :MinimapToggle<CR>
nmap <C-f> :Rg<CR>
map <C-m> i<CR><Esc>h
map <C-Return> i<CR><CR><C-o>k<C-t><C-t>

nnoremap <C-A-j> :m .+1<CR>==
nnoremap <C-A-k> :m .-2<CR>==
inoremap <C-A-j> <Esc>:m .+1<CR>==gi
inoremap <C-A-k> <Esc>:m .-2<CR>==gi
vnoremap <C-A-j> :m '>+1<CR>gv=gv
vnoremap <C-A-k> :m '<-2<CR>gv=gv

" Find files using Telescope command-line sugar.
" noremap <leader>e :MaximizerToggle<CR>
noremap <leader>f :Explore<CR>
nnoremap <leader>b  :Buffer<cr>
nnoremap <leader>t  :Tags<cr>
map <c-p> :Files<cr>

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

" -----------------------------------------------------------------------------
"     - Status line -
" -----------------------------------------------------------------------------

function s:GetMode()
    if mode() == "n"
        return " N "
    elseif mode() == "i"
        return "%#ModeInsert# I %*"
    elseif mode() == "v"
        return "%#ModeVisual# V %*"
    elseif mode() == "V"
        return "%#ModeVisual# V. %*"
    elseif mode() == "\<C-V>"
        return "%#ModeVisual# VB %*"
    else
        return "[mode: " . mode() . "]"
endfunction

function! s:PasteForStatusline()
    let paste_status = &paste
    if paste_status == 1
        return " [paste] "
    else
        return ""
    endif
endfunction

function s:GetFileType()
        return "%y"
endfunction

function GetStatusLine()
    let l:status_line_left = s:GetMode()
    let l:status_line_left = status_line_left . " %f" " Filename
    let l:status_line_left = status_line_left . " %M" " Modified
    let l:status_line_left = status_line_left . " %r" " Read only
    let l:status_line_left = status_line_left . s:PasteForStatusline()
    let l:status_line_right = "%=" " Alignt right statusline
    let l:status_line_right = status_line_right . " %c:%l/%L (%p%%) " " col, line, tot. lines
    let l:status_line_right = status_line_right . s:GetFileType() . " " " File type
    return l:status_line_left . l:status_line_right
endfunction

set statusline=%!GetStatusLine()      " File type

sy on
set background=dark
let &t_8f="\<Esc>[38;2;%lu;%lu;%lum"
let &t_8b="\<Esc>[48;2;%lu;%lu;%lum"
set t_AB=^[[48;5;%dm
set t_AF=^[[38;5;%dm
set termguicolors
set tw=79
set t_Co=256
colorscheme codedark
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
set mouse=a
set clipboard=unnamedplus
set vb t_vb="."
set nopaste
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
nmap <silent>gd <cmd>lua vim.lsp.buf.definition()<CR>
nmap <silent>gd <cmd>lua vim.lsp.buf.definition()<CR>
nnoremap <silent>gi :Lspsaga preview_definition<CR>
nmap <silent>gr <cmd>lua vim.lsp.buf.references()<CR>
noremap <silent>gk :Lspsaga hover_doc<CR>
nnoremap <silent>gh :Lspsaga lsp_finder<CR>

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

let g:minimap_width = 10
let g:minimap_auto_start = 0
let g:minimap_auto_start_win_enter = 1

" set number
" set relativenumber
set ruler
set diffopt+=vertical,iwhite,algorithm:patience,indent-heuristic
set guioptions=crb
set linebreak showbreak=+
set listchars=eol:.,tab:\|-
set laststatus=2

" vim-shore
let g:shore_stayonfront = 1

" Configure LSP

if (executable('pylsp'))
  lua require'lspconfig'.pylsp.setup{}
endif

if (executable('ccls'))
  lua require'lspconfig'.ccls.setup{ init_options = { cache = { directory = "/tmp/cache/ccls" }; index = {threads = 2 }; } };
endif

lua <<EOF

require('orgmode').setup({
  org_agenda_files = {'~/org/**/*'},
  org_default_notes_file = '~/org/refile.org',
})

-- nvim_lsp object
local nvim_lsp = require'lspconfig'

-- Disable diagnostics

vim.o.completeopt = "menuone,noselect"

require'compe'.setup {
  enabled = true;
  autocomplete = true;
  debug = false;
  min_length = 1;
  preselect = 'enable';
  throttle_time = 80;
  source_timeout = 200;
  incomplete_delay = 400;
  max_abbr_width = 100;
  max_kind_width = 100;
  max_menu_width = 100;
  documentation = false;

  source = {
    path = true;
    buffer = true;
    calc = true;
    vsnip = true;
    nvim_lsp = true;
    nvim_lua = true;
    spell = true;
    tags = false;
    snippets_nvim = false;
    treesitter = true;
  };
}

local t = function(str)
  return vim.api.nvim_replace_termcodes(str, true, true, true)
end
_G.s_tab_complete = function()
  if vim.fn.pumvisible() == 1 then
    return t "<C-p>"
  elseif vim.fn.call("vsnip#jumpable", {-1}) == 1 then
    return t "<Plug>(vsnip-jump-prev)"
  else
    return t "<S-Tab>"
  end
end

--vim.api.nvim_set_keymap("i", "<Tab>", "v:lua.tab_complete()", {expr = true})
--vim.api.nvim_set_keymap("i", "<S-Tab>", "v:lua.s_tab_complete()", {expr = true})
--vim.api.nvim_set_keymap("s", "<S-Tab>", "v:lua.s_tab_complete()", {expr = true})

nvim_lsp.rust_analyzer.setup({
  settings = {
    ['rust-analyzer'] = {
      checkOnSave = {
        extraArgs = {
          "--target-dir", "/tmp/rust-analyzer-check"
        }
      }
    }
  }
})

local saga = require 'lspsaga'
saga.init_lsp_saga()
saga.init_lsp_saga {
 use_saga_diagnostic_sign = false,
 max_preview_lines = 10,
 error_sign = '',
 warn_sign = '',
 hint_sign = '',
 infor_sign = '',
 finder_definition_icon = '> ',
 code_action_icon = '',
 code_action_prompt = {
   enable = false,
   sign = false,
   sign_priority = 20,
   virtual_text = true,
 },
 finder_action_keys = {
   open = 'o', vsplit = 's',split = 'h',quit = 'q',scroll_down = '<C-f>', scroll_up = '<C-b>' -- quit can be a table
 },
 finder_reference_icon = '> '
}

vim.lsp.handlers["textDocument/publishDiagnostics"] = function() end

EOF

set formatoptions=croqnlj

runtime! config/**/*.vim

" Trigger a highlight only when pressing f and F.
let g:qs_highlight_on_keys = ['f', 'F']
