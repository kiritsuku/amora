" Enable ruler for better debugging purposes
set ruler

" Nvim can't yet send any VimL events through its msgpack-rpc protocol.
" But it is possible to send arbitrary events through the rpcnotiy function.
" Therefore we receive any VimL events here and forward them to the client
" through with help of rpcnotify.
function! SendWinEnter()
  let a:fname = expand("<afile>")
  let a:winId = winnr()
  call rpcnotify(0, "_WinEnter", a:winId, a:fname)
endfunction

function! SendWinLeave()
  let a:fname = expand("<afile>")
  let a:winId = winnr()
  call rpcnotify(0, "_WinLeave", a:winId, a:fname)
endfunction

autocmd WinEnter * :call SendWinEnter()
autocmd WinLeave * :call SendWinLeave()
