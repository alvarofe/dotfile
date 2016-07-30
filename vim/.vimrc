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
map <leader>n :NERDTreeToggle<CR>
map <leader>p :set paste<CR>
map <leader>o :set nopaste<CR>
map <leader>b :e#<CR>
map <leader>f :ClangFormat<CR>
map <leader>l :TagbarToggle<CR>
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
set foldmethod=marker
set hlsearch
set paste
sy on
set mouse=a
set clipboard=unnamed
set vb t_vb="."

autocmd FileType python setlocal smartindent shiftwidth=4 ts=4 et cinwords=if,elif,else,for,while,try,except,finally,def,class
set relativenumber
set nu
set background=light 
set t_Co=256 " 256 Color Term
set ic "case insensitive searches
cmap w!! w !sudo tee % >/dev/null
"catch unicode annoying error
set listchars+=nbsp:x
