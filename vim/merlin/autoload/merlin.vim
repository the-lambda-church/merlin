if !exists('g:merlin') | let g:merlin = {} | endif | let s:c = g:merlin

if !exists('g:merlin_python_version')
  if has('python3')
    let g:merlin_python_version = 3
  elseif has('python') || has('python2')
    let g:merlin_python_version = 2
  else
    echoerr "Error: Required vim compiled with +python or +python3"
    finish
  endif
endif

if g:merlin_python_version == 3
  command! -nargs=1 MerlinPy python3 <args>
elseif g:merlin_python_version == 2
  command! -nargs=1 MerlinPy python <args>
else
  echoerr "Error: Unknown version of python, expecting 2 or 3 (g:merlin_python_version = " . g:merlin_python_version . ")"
  finish
endif

if !exists('g:merlin_split_method')
  let g:merlin_split_method = 'horizontal'
endif

if !exists('g:merlin_locate_preference')
  let g:merlin_locate_preference = 'ml'
endif

if !exists('g:merlin_binary_flags')
  let g:merlin_binary_flags = []
endif

if !exists("g:merlin_ignore_warnings")
  " strings are ugly, but at least I'm sure it's not converted in some weird
  " value when passing it to python
  let g:merlin_ignore_warnings = "false"
endif

if !exists("g:merlin_display_occurrence_list")
  let g:merlin_display_occurrence_list = 1
endif

if !exists("g:merlin_display_error_list")
  let g:merlin_display_error_list = 1
endif

if !exists("g:merlin_close_error_list")
  let g:merlin_close_error_list = 1
endif

if !exists("g:merlin_type_history_height")
  let g:merlin_type_history_height = 5
endif

if !exists("g:merlin_type_history_auto_open")
  let g:merlin_type_history_auto_open = 5
endif

if !exists("g:merlin_completion_dwim")
  let g:merlin_completion_dwim = 1
endif

if !exists("g:merlin_completion_argtype")
  let g:merlin_completion_argtype = 'several'
endif

if !exists("g:merlin_completion_with_doc")
  let g:merlin_completion_with_doc = "false"
endif

if !exists("g:merlin_disable_default_keybindings")
  let g:merlin_disable_default_keybindings = 0
endif

if !exists('g:merlin_debug')
  let g:merlin_debug = 0
endif

let s:current_dir=expand("<sfile>:p:h")
silent! MerlinPy import sys, vim
MerlinPy if not vim.eval("s:current_dir") in sys.path:
\    sys.path.append(vim.eval("s:current_dir"))

MerlinPy import merlin

function! s:get_visual_selection()
  let [lnum1, col1] = getpos("'<")[1:2]
  let [lnum2, col2] = getpos("'>")[1:2]
  let lines = getline(lnum1, lnum2)
  let lines[-1] = lines[-1][: col2 - 1]
  let lines[0] = lines[0][col1 - 1:]
  return join(lines, "\n")
endfunction

function! merlin#WordUnderCursor()
  return substitute(substitute(expand("<cWORD>"),"[;:),]*$","",""), "^[;:(,]*", "", "")
endfunction

function! merlin#FindFile(ext,file)
  MerlinPy <<EOF
fname = merlin.catch_and_print(lambda: merlin.vim_which(vim.eval("a:file"), vim.eval("a:ext")))
if fname != None: vim.command("e "+ fname.replace(' ','\\ '))
EOF
endfunction

function! merlin#Version()
  MerlinPy merlin.command_version()
endfunction

function! merlin#Path(var,...)
  if !exists("b:merlin_flags")
    let b:merlin_flags = []
  endif

  for i in a:000
    call add(b:merlin_flags, a:var)
    call add(b:merlin_flags, fnameescape(i))
  endfor
endfunction

function! merlin#MLList(ArgLead, CmdLine, CursorPos)
  let l:files = []
  MerlinPy merlin.vim_which_ext([".ml",".mli"], "l:files")
  return join(l:files, "\n")
endfunction

function! merlin#MLIList(ArgLead, CmdLine, CursorPos)
  let l:files = []
  MerlinPy merlin.vim_which_ext([".mli",".ml"], "l:files")
  return join(l:files, "\n")
endfunction

function! merlin#ExpandPrefix(ArgLead, CmdLine, CursorPos)
  let l:compl = []
  MerlinPy merlin.vim_expand_prefix(vim.eval("a:ArgLead"), "l:compl")
  return l:compl
endfunction

function! merlin#ExpandTypePrefix(ArgLead, CmdLine, CursorPos)
  let l:compl = []
  MerlinPy merlin.vim_expand_prefix(vim.eval("a:ArgLead"), "l:compl",kinds=["type"])
  return l:compl
endfunction

function! s:MakeCompletionList(var, pyfun)
  let l:all = []
  execute 'MerlinPy ' . a:pyfun . '("l:all")'
  if exists(a:var)
    let l:existing = copy(eval(a:var))
    let l:all = filter(l:all, "index(l:existing, v:val) == -1")
    call insert(l:all, join(map(l:existing, "fnameescape(v:val)"), " "))
  endif
  return join(l:all, "\n")
endfunction

function! merlin#CompleteExtensions(ArgLead, CmdLine, CursorPos)
  return s:MakeCompletionList("b:merlin_extensions", "merlin.vim_extension_list")
endfunction

function! merlin#Extensions(...)
  let b:merlin_extensions = a:000
endfunction

function! merlin#CompletePackages(ArgLead, CmdLine, CursorPos)
  let l:all = map(systemlist("ocamlfind list"), "split(v:val)[0]")
  if exists("b:merlin_packages")
    let l:existing = copy(b:merlin_packages)
    let l:all = filter(l:all, "index(l:existing, v:val) == -1")
    call insert(l:all, join(map(l:existing, "fnameescape(v:val)"), " "))
  endif
  return join(l:all, "\n")
endfunction

function! merlin#Packages(...)
  let b:merlin_packages = copy(a:000)
  let arguments = join(map(b:merlin_packages, "shellescape(v:val)"), ' ')
  let cmd = 'ocamlfind query ' . arguments
  let b:merlin_packages_path = systemlist(cmd)
endfunction

function! merlin#CompleteFlags(ArgLead, CmdLine, CursorPos)
  return s:MakeCompletionList("b:merlin_flags", "merlin.vim_flags_list")
endfunction

function! merlin#Flags(...)
  let b:merlin_flags = a:000
endfunction

function! merlin#LogBuffer() abort
  let l:filename = ":merlin-log:"
  let l:buffer = bufnr(l:filename)

  if l:buffer == -1
    let l:buffer = bufnr(l:filename, v:true)

    " Set up the buffer
    call setbufvar(l:buffer, "&buftype", "nofile")
    call setbufvar(l:buffer, "&bufhidden", "hide")
    " 1 is 'noswapfile'
    call setbufvar(l:buffer, "&swapfile", 0)
  endif

  return l:buffer
endfunction

function! merlin#DebugEnable()
  let g:merlin_debug=1
  split
  execute "buffer " . merlin#LogBuffer()
endfunction

function! merlin#DebugDisable()
  let g:merlin_debug=0
endfunction

function! s:ShowTypeEnclosing(type)
  call s:StopHighlight()
  if empty(a:type)
    return
  endif

  let w:enclosing_zone = matchadd('EnclosingExpr', a:type['matcher'])
  augroup MerlinHighlighting
    au!
    autocmd InsertEnter <buffer> call merlin#StopHighlight()
    autocmd BufWinLeave <buffer> call merlin#StopHighlight()
  augroup END

  if ! has_key(a:type, 'type')
    echohl WarningMsg
    echo "didn't manage to type '" . a:type['atom'] . "'"
    echohl None
    return
  endif

  let g:merlin_latest_type = a:type['type']

  if g:merlin_type_history_height <= 0 || (!has("nvim") && (v:version <= 703 || !has("patch-7.4.424")))
    echo a:type['type'] . a:type['tail_info']
    return
  endif

  call merlin_type#Show(a:type['type'], a:type['tail_info'])
endfunction

function! merlin#YankLatestType()
  if ! exists("g:merlin_latest_type")
    echohl ErrorMsg | echo "no type available" | echohl None
    return
  endif
  call setreg(v:register, g:merlin_latest_type)
  echo "yanked" g:merlin_latest_type
endfunction

function! merlin#TypeOf(...)
    if (a:0 > 1)
        echoerr "TypeOf: too many arguments (expected 0 or 1)"
    elseif (a:0 == 0) || (a:1 == "")
        MerlinPy vim.command("let l:type = " + merlin.vim_type_enclosing())
        call s:ShowTypeEnclosing(l:type)
    else
        MerlinPy vim.command("let l:type = " + merlin.vim_type(vim.eval("a:1")))
        call s:ShowTypeEnclosing(l:type)
    endif
endfunction

function! merlin#TypeOfSel()
  call merlin#TypeOf(s:get_visual_selection())
endfunction

function! merlin#PolaritySearch(debug,query)
  let s:search_result = []
  MerlinPy merlin.vim_polarity_search(vim.eval("a:query"), "s:search_result")
  if a:debug != 1 && s:search_result != []
    call feedkeys("i=merlin#PolarityComplete()\<CR>","n")
  endif
endfunction

function! merlin#PolarityComplete()
  call complete(col('.'), s:search_result)
  return ''
endfunction

function! s:StopHighlight()
  if exists('w:enclosing_zone') && w:enclosing_zone != -1
    call matchdelete(w:enclosing_zone)
    let w:enclosing_zone = -1
  endif
endfunction

function! merlin#StopHighlight()
  MerlinPy merlin.vim_type_reset()
  call s:StopHighlight()
  augroup MerlinHighlighting
    au!
  augroup END
endfunction

function! merlin#GrowEnclosing()
  MerlinPy vim.command("let l:type = " + merlin.vim_next_enclosing())
  call s:ShowTypeEnclosing(l:type)
endfunction

function! merlin#ShrinkEnclosing()
  MerlinPy vim.command("let l:type = " + merlin.vim_prev_enclosing())
  call s:ShowTypeEnclosing(l:type)
endfunction

function! merlin#Complete(findstart,base)
  if a:findstart
    " Synchronize merlin before completion, since vim modify the buffer
    " (prefix is removed)
    " Locate the start of the item, including ".", "->" and "[...]".
    let line = getline('.')
    let start = col('.') - 1
    let lastword = -1
    while start > 0
      if line[start - 1] =~ '\(\w\|''\)'
        let start -= 1
      elseif line[start - 1] =~ '\.'
        if lastword == -1
          let lastword = start
        endif
        let start -= 1
      else
        break
      endif
    endwhile
    if start > 0 && line[start - 1] =~ '[~?`]'
      let start -= 1
    endif

    let s:compl_base = strpart(line, start, col('.') - 1 - start)

    " Return the column of the last word, which is going to be changed.
    " Remember the text that comes before it in s:compl_prefix.
    if lastword == -1
      let s:compl_prefix = ''
      let s:compl_suffix = s:compl_base
    else
      let s:compl_prefix = strpart(line, start, lastword - start)
      let s:compl_suffix = strpart(line, lastword, col('.') - 1 - lastword)
    endif

    " Query completion
    let s:compl_result = []
    MerlinPy vim.command("let l:compl_succeed = %d" %
\    merlin.vim_complete_cursor(vim.eval("s:compl_base"),vim.eval("s:compl_suffix"),"s:compl_result"))

    " If empty, switch to dwim
    let s:compl_dwim = g:merlin_completion_dwim && !l:compl_succeed
    if s:compl_dwim
      let s:compl_prefix = ''
      MerlinPy merlin.vim_expand_prefix(vim.eval("s:compl_base"),"s:compl_result")
    endif

    if lastword == -1 || s:compl_dwim
      return start
    else
      return lastword
    end
  endif

  " If prefix changed, update completion
  let base = s:compl_prefix . a:base
  if base != s:compl_base
    let s:compl_base = base
    if s:compl_dwim
      MerlinPy merlin.vim_expand_prefix(vim.eval("base"),"s:compl_result")
    else
      MerlinPy merlin.vim_complete_cursor(vim.eval("base"),vim.eval("s:compl_suffix"),"s:compl_result")
    endif
  endif

  " Workaround https://github.com/ocaml/merlin/issues/223 vim 704
  return s:compl_result
  "if v:version <= 703
  "  return l:props
  "else
  "  return {'words': l:props, 'refresh': 'always'}
  "endif
endfunction

function! merlin#Locate(...)
  if (a:0 > 1)
    echoerr "Locate: too many arguments (expected 0 or 1)"
  elseif (a:0 == 0) || (a:1 == "")
    MerlinPy merlin.vim_locate_under_cursor()
  else
    MerlinPy merlin.vim_locate_at_cursor(vim.eval("a:1"))
  endif
endfunction

function! merlin#LocateImpl(...)
  let l:pref = g:merlin_locate_preference
  let g:merlin_locate_preference = 'implementation'
  call call("merlin#Locate", a:000)
  let g:merlin_locate_preference = l:pref
endfunction

function! merlin#LocateIntf(...)
  let l:pref = g:merlin_locate_preference
  let g:merlin_locate_preference = 'interface'
  call call("merlin#Locate", a:000)
  let g:merlin_locate_preference = l:pref
endfunction

function! merlin#Jump(...)
  if (a:0 > 1)
    echoerr "Jump: too many arguments (expected 0 or 1)"
  elseif (a:0 == 0) || (a:1 == "")
    MerlinPy merlin.vim_jump_default()
  else
    MerlinPy merlin.vim_jump_to(vim.eval("a:1"))
  endif
endfunction

function! merlin#PhrasePrev()
  MerlinPy merlin.vim_phrase_prev()
endfunction

function! merlin#PhraseNext()
  MerlinPy merlin.vim_phrase_next()
endfunction

function! merlin#Document(...)
  if (a:0 > 1)
    echoerr "Document: to many arguments (expected 0 or 1)"
  elseif (a:0 == 0) || (a:1 == "")
    MerlinPy merlin.vim_document_under_cursor()
  else
    MerlinPy merlin.vim_document_at_cursor(vim.eval("a:1"))
  endif
endfunction

function! merlin#InteractiveLocate()
  if !exists('g:loaded_ctrlp')
    echo "This function requires the CtrlP plugin to work"
    " ctrl doesn't exist? Exiting.
  else
    if exists('g:ctrlp_match_func')
        let l:match_fun = g:ctrlp_match_func
    else
        let l:match_fun = {}
    endif

    call ctrlp#locate#update_cursor_pos()

    let g:ctrlp_match_func = { 'match': 'ctrlp#locate#filter' }
    call ctrlp#init(ctrlp#locate#id())
    let g:ctrlp_match_func = l:match_fun
  endif
endfunction

function! merlin#Outline()
  if !exists('g:loaded_ctrlp')
    echo "This function requires the CtrlP plugin to work"
    " ctrl doesn't exist? Exiting.
  else
    call ctrlp#outline#preinit()
    call ctrlp#init(ctrlp#outline#id())
  endif
endfunction

function! merlin#Occurrences()
  let l:occurrences = []
  let l:pos = 0
  MerlinPy vim.command ("let l:pos = %d" % merlin.vim_occurrences("l:occurrences"))

  if l:occurrences == []
    return
  endif

  call setloclist(0, l:occurrences)
  execute ":ll! " . l:pos
  if g:merlin_display_occurrence_list
    lopen
  endif
endfunction

function! merlin#OccurrencesRename(text)
  MerlinPy merlin.vim_occurrences_replace(vim.eval("a:text"))
endfunction

function! merlin#RefactorOpen(str)
  MerlinPy merlin.vim_refactor_open(vim.eval("a:str"))
endfunction

function! merlin#ErrorLocList()
  let l:errors = []
  if !exists('b:merlin_error_check') || b:merlin_error_check == 1
    MerlinPy <<EOF
try:
  merlin.vim_loclist("l:errors", "g:merlin_ignore_warnings")
except merlin.MerlinException as e:
  merlin.try_print_error(e)
EOF
  endif
  return l:errors
endfunction

function! merlin#Errors()
  let l:errors = merlin#ErrorLocList()
  call setloclist(0, l:errors)
  if len(l:errors) > 0
    if g:merlin_display_error_list
      lopen
    endif
  else
    if g:merlin_close_error_list
      lclose
    endif
  endif
endfunction

function! merlin#Destruct()
  MerlinPy merlin.vim_case_analysis()
endfunction

function! merlin#PreviousHole()
  MerlinPy merlin.vim_previous_hole()
endfunction

function! merlin#NextHole()
  MerlinPy merlin.vim_next_hole()
endfunction

"'''''''''''"
" CONSTRUCT "
"'''''''''''"

let b:merlin_construct_depth = 1
let b:merlin_construct_done = 1
let b:merlin_construct_lines_before = 0

function! merlin#ConstructComplete(findstart, base)
  " The function is called in two different ways:
  " - First the function is called to find the start of the text to be completed.
  " - Later the function is called to actually find the matches.
  if a:findstart
    let start = col('.') - 1
    return start
  endif
  return b:constr_result
endfunction

function! merlin#ConstructDone()
  if b:merlin_construct_done
    " After the subtitution we try to go to the the next hole
    " TODO Don't if no substitution happened and rewrite the hole
    call setpos('.', b:construct_saved_pos)
    let start_line = b:construct_saved_pos[1]
    let end_line = start_line + line('$') - b:merlin_construct_lines_before
    MerlinPy merlin.vim_next_hole(vim.eval("start_line"), vim.eval("end_line"))
    :call feedkeys("\<esc>")
    :call feedkeys("\<Right>")

    " We reset the depth and the mappings
    let b:merlin_construct_depth = 1
    :iunmap <buffer><expr> <c-i>
    :iunmap <buffer><expr> <c-u>

    " And we hook back the standard completion
    setlocal omnifunc=merlin#Complete
  else
    let b:merlin_construct_done = 1
  endif
endfunction

function! merlin#ConstructMore()
  let b:merlin_construct_depth += 1
  let b:merlin_construct_done = 0

  " We cancel the previous omnicomplete and trigger a new Construct
  return "\<c-e>\_\<esc>:MerlinConstruct\<enter>"
endfunction

function! merlin#ConstructLess()
  if b:merlin_construct_depth > 1
    let b:merlin_construct_depth -= 1
    let b:merlin_construct_done = 0

    " We cancel the previous omnicomplete and trigger a new Construct
    return "\<c-e>\_\<esc>:MerlinConstruct\<enter>"
  else
    return ""
  endif
endfunction

function! merlin#Construct()
  " We save the number of lines in the file for future comparison
  let b:merlin_construct_lines_before = line('$')

  " We call construct
  MerlinPy merlin.vim_construct(vim.eval("b:merlin_construct_depth"))
  let b:construct_saved_pos = getpos(".")

  " If multiple choices where found they are in the b:constr_result list
  if len(b:constr_result) > 0
    " We start Omnicomplete with the custom complete function
    setlocal omnifunc=merlin#ConstructComplete
    startinsert
    call feedkeys("\<c-x>\<c-o>")

    " Map keys for more or less
    " TODO better choice of keys ?
    :inoremap <buffer><expr> <c-i> merlin#ConstructMore()
    :inoremap <buffer><expr> <c-u> merlin#ConstructLess()

    " When it's done we switch back to merlin default completion
    augroup MerlinConstruct
      au!
      autocmd CompleteDone <buffer> call merlin#ConstructDone()
    augroup END
  endif
endfunction

function! merlin#PreviousHole()
  MerlinPy merlin.vim_previous_hole()
endfunction

function! merlin#NextHole()
  MerlinPy merlin.vim_next_hole()
endfunction

function! merlin#Restart()
  MerlinPy merlin.vim_restart()
endfunction

function! merlin#Reload()
  MerlinPy merlin.vim_reload()
endfunction

" Copy-pasted from
" https://github.com/ngn/dotfiles/blob/master/vim/autoload/ngn/common.vim
function! merlin#setVisualSelection(a, b)
" Save existing positions of marks 'a and 'b
  let markASave = getpos("'a")
  let markBSave = getpos("'b")
" Move to a, enter visual mode, and move to b
  call setpos("'a", [0, a:a[0], a:a[1], 0])
  call setpos("'b", [0, a:b[0], a:b[1], 0])
  normal! `bv`a
" Restore positions of marks 'a and 'b
  call setpos("'a", markASave)
  call setpos("'b", markBSave)
endfunction

let s:phrase_counter = 0

function! merlin#Phrase()
  if s:phrase_counter
      let s:phrase_counter = s:phrase_counter - 1
  else
    let [l1, c1] = getpos("'<")[1:2]
    let [l2, c2] = getpos("'>")[1:2]
    let s:phrase_counter = l2 - l1
    MerlinPy merlin.vim_selectphrase("l1","c1","l2","c2")
    call merlin#setVisualSelection([l1,c1],[l2,c2])
  endif
endfunction

function! merlin#Register()
  if @% == ":merlin-type-history:"
    return
  endif

  """ Version  -----------------------------------------------------------------
  command! -buffer -nargs=0 MerlinVersion call merlin#Version()

  """ Error reporting  ---------------------------------------------------------
  command! -buffer -nargs=0 MerlinErrorCheck call merlin#Errors()

  """ Completion  --------------------------------------------------------------
  setlocal omnifunc=merlin#Complete

  """ TypeOf  ------------------------------------------------------------------
  command! -buffer -complete=customlist,merlin#ExpandPrefix -nargs=? MerlinTypeOf call merlin#TypeOf(<q-args>)
  command! -buffer -range -nargs=0 MerlinTypeOfSel call merlin#TypeOfSel()

  command! -buffer -nargs=0 MerlinClearEnclosing  call merlin#StopHighlight()
  command! -buffer -nargs=0 MerlinGrowEnclosing   call merlin#GrowEnclosing()
  command! -buffer -nargs=0 MerlinShrinkEnclosing call merlin#ShrinkEnclosing()

  command! -buffer -nargs=0 MerlinYankLatestType    call merlin#YankLatestType()
  command! -buffer -nargs=0 MerlinToggleTypeHistory call merlin_type#ToggleTypeHistory()

  if !exists('g:merlin_disable_default_keybindings') || !g:merlin_disable_default_keybindings
    map  <silent><buffer> <LocalLeader>t :MerlinTypeOf<return>
    map  <silent><buffer> <LocalLeader>n :MerlinGrowEnclosing<return>
    map  <silent><buffer> <LocalLeader>p :MerlinShrinkEnclosing<return>
    vmap <silent><buffer> <LocalLeader>t :MerlinTypeOfSel<return>
  endif

  """ Destruct  ----------------------------------------------------------------
  command! -buffer -nargs=0 MerlinDestruct call merlin#Destruct()

  """ Holes  ---------------------------------------------------------------
  command! -buffer -nargs=0 MerlinNextHole call merlin#NextHole()
  command! -buffer -nargs=0 MerlinPreviousHole call merlin#PreviousHole()

  """ Construct  ---------------------------------------------------------------
  command! -buffer -nargs=0 MerlinNextHole call merlin#NextHole()
  command! -buffer -nargs=0 MerlinPreviousHole call merlin#PreviousHole()
  command! -buffer -nargs=0 MerlinConstruct call merlin#Construct()

  """ Locate  ------------------------------------------------------------------
  command! -buffer -complete=customlist,merlin#ExpandPrefix -nargs=? MerlinLocate call merlin#Locate(<q-args>)
  command! -buffer -complete=customlist,merlin#ExpandPrefix -nargs=? MerlinLocateImpl call merlin#LocateImpl(<q-args>)
  command! -buffer -complete=customlist,merlin#ExpandPrefix -nargs=? MerlinLocateIntf call merlin#LocateIntf(<q-args>)
  command! -buffer -nargs=0 MerlinILocate call merlin#InteractiveLocate()


  if !exists('g:merlin_disable_default_keybindings') || !g:merlin_disable_default_keybindings
    nmap <silent><buffer> gd  :MerlinLocate<return>
  endif

  """ Jump and Phrase motion ---------------------------------------------------
  command! -buffer -complete=customlist,merlin#ExpandPrefix -nargs=? MerlinJump call merlin#Jump(<q-args>)
  command! -buffer MerlinPhrasePrev call merlin#PhrasePrev()
  command! -buffer MerlinPhraseNext call merlin#PhraseNext()
  nmap <silent><buffer> [[ :MerlinPhrasePrev<cr>
  nmap <silent><buffer> ]] :MerlinPhraseNext<cr>

  """ Document  ----------------------------------------------------------------
  command! -buffer -complete=customlist,merlin#ExpandPrefix -nargs=? MerlinDocument call merlin#Document(<q-args>)

  """ Outline  -----------------------------------------------------------------
  command! -buffer -nargs=0 MerlinOutline call merlin#Outline()

  """ Occurrences  -------------------------------------------------------------
  " Search
  command! -buffer -nargs=0 MerlinOccurrences call merlin#Occurrences()
  nmap <silent><buffer> <Plug>(MerlinSearchOccurrencesForward)  :call merlin_find#OccurrencesSearch('/')<cr>:let v:searchforward=1<cr>
  nmap <silent><buffer> <Plug>(MerlinSearchOccurrencesBackward) :call merlin_find#OccurrencesSearch('?')<cr>:let v:searchforward=0<cr>

  " Rename
  command! -buffer -nargs=* MerlinRename call merlin#OccurrencesRename(<f-args>)
  nmap <silent><buffer> <Plug>(MerlinRename) :call merlin_find#IncrementalRename()<cr>//<cr>c//e<cr>
  nmap <silent><buffer> <Plug>(MerlinRenameAppend) :call merlin_find#IncrementalRename()<cr>//e<cr>a

  " Text Objects
  if exists("g:merlin_textobject_grow") && g:merlin_textobject_grow != ''
    let l:k = g:merlin_textobject_grow

    exe "vmap <silent><buffer>  " . l:k ":<C-U>call merlin_visual#Grow('v')<return>"
    exe "vmap <silent><buffer> a" . l:k ":<C-U>call merlin_visual#GrowAround('v')<return>"
    exe "vmap <silent><buffer> i" . l:k ":<C-U>call merlin_visual#GrowInside('v')<return>"

    exe "omap <silent><buffer>  " . l:k ":<C-U>call merlin_visual#Grow('o')<return>"
    exe "omap <silent><buffer> a" . l:k ":<C-U>call merlin_visual#GrowAround('o')<return>"
    exe "omap <silent><buffer> i" . l:k ":<C-U>call merlin_visual#GrowInside('o')<return>"
  endif

  if exists("g:merlin_textobject_shrink") && g:merlin_textobject_shrink != ''
    let l:k = g:merlin_textobject_shrink

    exe "vmap <silent><buffer>  " . l:k ":<C-U>call merlin_visual#Shrink('v')<return>"
    exe "vmap <silent><buffer> a" . l:k ":<C-U>call merlin_visual#ShrinkAround('v')<return>"
    exe "vmap <silent><buffer> i" . l:k ":<C-U>call merlin_visual#ShrinkInside('v')<return>"

    exe "omap <silent><buffer>  " . l:k ":<C-U>call merlin_visual#Shrink('o')<return>"
    exe "omap <silent><buffer> i" . l:k ":<C-U>call merlin_visual#ShrinkInside('o')<return>"
    exe "omap <silent><buffer> a" . l:k ":<C-U>call merlin_visual#ShrinkAround('o')<return>"
  endif

  """ Open / Unopen ------------------------------------------------------------
  command! -buffer -nargs=0 MerlinRefactorOpen call merlin#RefactorOpen("unqualify")
  command! -buffer -nargs=0 MerlinRefactorOpenQualify call merlin#RefactorOpen("qualify")

  """ Path management  ---------------------------------------------------------
  command! -buffer -nargs=* -complete=dir MerlinSourcePath call merlin#Path("-source-path", <f-args>)
  command! -buffer -nargs=* -complete=dir MerlinBuildPath  call merlin#Path("-build-path", <f-args>)

  """ Findlib  -----------------------------------------------------------------
  command! -buffer -complete=custom,merlin#CompletePackages -nargs=* MerlinPackages call merlin#Packages(<f-args>)
  " Backward compatibility
  command! -buffer -complete=custom,merlin#CompletePackages -nargs=* MerlinUse      call merlin#Packages(<f-args>)

  """ Flags management  --------------------------------------------------------
  command! -buffer -complete=custom,merlin#CompleteFlags -nargs=* MerlinFlags    call merlin#Flags(<f-args>)
  " Backward compatibility
  command! -buffer -complete=custom,merlin#CompleteFlags -nargs=* MerlinSetFlags call merlin#Flags(<f-args>)

  """ Extensions  --------------------------------------------------------------
  command! -buffer -complete=custom,merlin#CompleteExtensions -nargs=* MerlinExtensions  call merlin#Extensions(<f-args>)

  """ .merlin  -----------------------------------------------------------------
  command! -buffer -nargs=0 GotoDotMerlin call merlin#GotoDotMerlin()
  command! -buffer -nargs=0 EchoDotMerlin call merlin#EchoDotMerlin()
  command! -buffer -nargs=0 MerlinGotoDotMerlin call merlin#GotoDotMerlin()
  command! -buffer -nargs=0 MerlinEchoDotMerlin call merlin#EchoDotMerlin()

  """ 'semantic movement'  -----------------------------------------------------
  " TODO: bind (,),{,} ?
  command! -buffer -nargs=0 MerlinPhrase call merlin#Phrase()

  """ Polarity search
  command! -buffer -complete=customlist,merlin#ExpandTypePrefix -nargs=+ MerlinSearch call merlin#PolaritySearch(0,<q-args>)

  """ debug --------------------------------------------------------------------
  command! -buffer -nargs=0 MerlinDebugLastCommands MerlinPy merlin.vim_last_commands()
  command! -buffer -nargs=0 MerlinDebugDisable call merlin#DebugDisable()
  command! -buffer -nargs=0 MerlinDebugEnable  call merlin#DebugEnable()

  if !exists('g:merlin_disable_default_keybindings') || !g:merlin_disable_default_keybindings
    vmap <silent><buffer> <TAB>         :<C-u>MerlinPhrase<return>
  endif

  call merlin#LoadProject()
endfunction

function! merlin#LoadProject()
  if isdirectory(expand('%:p:h'))
    MerlinPy merlin.setup_merlin()
    if exists("b:dotmerlin") && exists("g:merlin_move_to_project") && g:merlin_move_to_project && len(b:dotmerlin) > 0
      execute ":lchdir " . fnamemodify(b:dotmerlin[0], ":p:h")
    endif
  endif
endfunction

function! merlin#EchoDotMerlin()
  if exists("b:dotmerlin")
    echom "Using .merlin: " . join(b:dotmerlin, ', ')
  else
    echo "No .merlin found"
  endif
endfunction

function! merlin#GotoDotMerlin()
    if exists("b:dotmerlin")
        execute ":e " . b:dotmerlin[0]
    " TODO : it's plausible to create an empty one here instead by guessing
    " where it should be located
    else
        echo "No .merlin found"
    endif
endfunction

function! merlin#FindBinary()
  if !has_key(s:c, 'ocamlmerlin_path') && has_key(s:c, 'merlin_home') && has_key(s:c, 'merlin_parent')
    let s:choices = map(['ocamlmerlin','ocamlmerlin.native'], 's:c.merlin_parent."/bin/".v:val') + map(['ocamlmerlin','ocamlmerlin.native'], 's:c.merlin_home."/".v:val')
    let s:available_choices = filter(s:choices, 'filereadable(v:val)')
    if len(s:available_choices) > 0
      let s:c.ocamlmerlin_path =  s:available_choices[0]
    elseif executable('ocamlmerlin')
      let s:c.ocamlmerlin_path = 'ocamlmerlin'
    else
      echoe "ocamlmerlin not found!"
    endif
    unlet s:choices
  endif
  return s:c.ocamlmerlin_path
endfunction

function! merlin#SelectBinary()
  if !exists("b:merlin_path")
    if exists("*MerlinSelectBinary")
      let l:merlin_path = MerlinSelectBinary()
      if !exists("b:merlin_path")
        let b:merlin_path = l:merlin_path
      end
    else
      let b:merlin_path = merlin#FindBinary()
    end
  endif
  return b:merlin_path
endfunction

function! merlin#ShortEcho(msg)
  " From http://vim.wikia.com/wiki/Get_shortened_messages_from_using_echomsg
  " Suggested by @jordwalke
  let saved=&shortmess
  set shortmess+=T
  exe "norm :echomsg a:msg\n"
  let &shortmess=saved
endfunction

command! -nargs=1 -complete=custom,merlin#MLList  ML  call merlin#FindFile(["ml","mli"],<f-args>)
command! -nargs=1 -complete=custom,merlin#MLIList MLI call merlin#FindFile(["mli","ml"],<f-args>)
