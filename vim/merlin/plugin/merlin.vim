" If the mode didn't work for you, ensure that ocamlmerlin binary can be found
" in PATH. 
" Most commands are accessible only from omlet/ocaml buffers, check from an
" appropriate buffer if merlin.vim is effectively loaded.

" If you are using syntastic and don't want warnings notified, set the following
" variable to "true"

if !exists('g:merlin') | let g:merlin = {} | endif | let s:c = g:merlin
let g:merlin_ignore_warnings = "false"

if exists("$OCAMLMERLIN_PATH")
  let s:c.ocamlmerlin_path = $OCAMLMERLIN_PATH
else
  let s:c.merlin_home = expand('<sfile>:h:h:h:h')
  let s:c.merlin_parent = expand('<sfile>:h:h:h:h:h')
endif

" Highlight the expression which type is given
hi def link EnclosingExpr IncSearch
