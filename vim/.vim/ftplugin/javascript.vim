setlocal tabstop=2
setlocal softtabstop=2
setlocal shiftwidth=2
setlocal textwidth=80
setlocal smarttab
setlocal expandtab
packadd vim-jsbeautify
packadd vim-node
noremap <leader>j :call JsBeautify()<CR>
