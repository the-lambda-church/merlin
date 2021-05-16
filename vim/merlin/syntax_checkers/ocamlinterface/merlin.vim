" Enable Syntastic support
" Make sure syntax_checkers directory is on runtime path, then set
" :let g:syntastic_ocamlinterface_checkers=['merlin']

function! SyntaxCheckers_ocamlinterface_merlin_IsAvailable()
  if !exists("*merlin#SelectBinary")
    return 0
  endif
  try
    let l:path = merlin#SelectBinary()
    return executable(l:path)
  catch
    return 0
  endtry
endfunction

function! SyntaxCheckers_ocamlinterface_merlin_GetLocList()
  return merlin#ErrorLocList()
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'ocamlinterface',
    \ 'name': 'merlin'})
