" Enable ruler for better debugging purposes
set ruler

" Nvim can't yet send any VimL events through its msgpack-rpc protocol.
" But it is possible to send arbitrary events through the rpcnotiy function.
" Therefore we receive any VimL events here and forward them to the client
" through with help of rpcnotify.
function! SendWinEnter(fname)
  call rpcnotify(0, "_WinEnter", a:fname)
endfunction

autocmd WinEnter * :call SendWinEnter(expand("<afile>"))
