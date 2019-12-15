setlocal tabstop=4
setlocal softtabstop=4
setlocal shiftwidth=4
setlocal textwidth=80
setlocal smarttab
setlocal expandtab
packadd vim-jsbeautify
packadd vim-node
set autoindent
noremap <leader>j :call JsBeautify()<CR>
