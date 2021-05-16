function! neomake#makers#ft#ocamlinterface#EnabledMakers() abort
  return ['merlin']
endfunction

function! neomake#makers#ft#ocamlinterface#merlin() abort
  let maker = {}
  function! maker.get_list_entries(jobinfo)
    return merlin#ErrorLocList()
  endfunction
  return maker
endfunction
