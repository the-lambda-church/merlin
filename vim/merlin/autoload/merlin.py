import subprocess
import json
import vim
import re
import os
import sys
from sys import platform

enclosing_types = [] # nothing to see here
current_enclosing = -1
atom_bound = re.compile('[a-z_0-9A-Z\'`.]')
re_wspaces = re.compile("[\n ]+")
re_spaces = re.compile(" +")
re_spaces_around_nl = re.compile(" *\n *")
re_error_warning = re.compile(r"Error \(warning (\d+)\): ")

protocol_version = 3

######## ERROR MANAGEMENT

class MerlinExc(Exception):
    def __init__(self, value):
        self.value = value
    def __str__(self):
        return repr(self.value)

class Failure(MerlinExc):
    pass

class Error(MerlinExc):
    pass

class MerlinException(MerlinExc):
    pass

def vimprint(msg):
    msg = msg.replace('"',r'\"')
    vim.command("call merlin#ShortEcho(\"%s\")" % msg)

def try_print_error(e, msg=None):
    try:
        raise e
    except Error as e:
        if msg: vimprint(msg)
        else: vimprint(e.value)
    except Exception as e:
        # Always print to stdout
        # vim try to be 'smart' and prepend a backtrace when writing to stderr
        # WTF?!
        if msg: vimprint(msg)
        else:
            msg = str(e)
            if re.search('Not_found',msg):
                vimprint("error: Not found")
                return None
            elif re.search('Cmi_format.Error', msg):
                if vim.eval('exists("b:merlin_incompatible_version")') == '0':
                    vim.command('let b:merlin_incompatible_version = 1')
                    vimprint("The version of merlin you're using doesn't support this version of ocaml")
                return None
            vimprint(msg)

def vim_codec():
    # Vim passed incorrectly encoded strings to python2.
    # This could be worked around by manually decoding using the buffer
    # encoding.
    # However, python3 handling of unicode is a bit better, so "str()"
    # shouldn't be decoded. So we assume that vim did the right thing before.
    if sys.version_info >= (3,0):
        return ((lambda str: str), (lambda str: str))
    else:
        encoding = vim.eval("&fileencoding") or \
                   vim.eval("&encoding") or \
                   "ascii"
        return ((lambda str: str.encode(encoding)), \
                (lambda str: str.decode(encoding)))

def catch_and_print(f, msg=None):
    try:
        return f()
    except MerlinExc as e:
        try_print_error(e, msg=msg)

def concat_map(f, args):
    return [item for arg in args for item in f(arg)]

######## PROCESS MANAGEMENT

def current_context():
    filename = vim.eval("expand('%:p')")
    content = "\n".join(vim.current.buffer) + "\n"
    return (filename, content)

last_commands = []

def merlin_exec(args, input=""):
    global last_commands
    env = os.environ
    path = vim.eval("merlin#SelectBinary()")
    if vim.eval("exists('b:merlin_env')") == '1':
        env = env.copy()
        newenv = vim.eval("b:merlin_env")
        for key in newenv:
            env[key] = newenv[key]
    else:
        env = os.environ
    try:
        cmd = [path] + list(args)
        last_commands.insert(0, cmd)
        if len(last_commands) > 5: last_commands.pop()
        # As for OCaml, 64-bit Python still has sys.platform == win32
        # Note that owing to a long-standing bug in Python, stderr must be given
        # (see https://bugs.python.org/issue3905)
        if platform == "win32":
            info = subprocess.STARTUPINFO()
            info.dwFlags |= subprocess.STARTF_USESHOWWINDOW
            info.wShowWindow = subprocess.SW_HIDE
            process = subprocess.Popen(
                    cmd,
                    stdin=subprocess.PIPE,
                    stdout=subprocess.PIPE,
                    stderr=subprocess.PIPE,
                    startupinfo=info,
                    universal_newlines=True,
                    env=env
                    )
        else:
            process = subprocess.Popen(
                    cmd,
                    stdin=subprocess.PIPE,
                    stdout=subprocess.PIPE,
                    stderr=subprocess.PIPE,
                    universal_newlines=True,
                    env=env
                    )
        # Send buffer content
        (response, errors) = process.communicate(input=input)
        if errors:
            buf = int(vim.eval("merlin#LogBuffer()"))
            vim.buffers[buf].append(errors.split('\n'))
        return response
    except OSError as e:
        vimprint("Failed starting ocamlmerlin. Please ensure that ocamlmerlin binary is executable.")
        raise e

verbosity_counter = (None,None)

def command2(args, context=None, track_verbosity=None):
    global verbosity_counter
    if track_verbosity:
        if track_verbosity is True:
            track_verbosity = args
        if verbosity_counter[0] == track_verbosity:
            verbosity_counter = (track_verbosity,verbosity_counter[1]+1)
        else:
            verbosity_counter = (track_verbosity,0)
        verbosity = ["-verbosity",str(verbosity_counter[1])]
    else:
        verbosity = []
    (filename, content) = context or current_context()
    if vim_is_set("g:merlin_debug"):
        log_errors = ["-log-file", "-"]
    else:
        log_errors = []
    cmdline = ["server"] + list(args) + ["-filename",filename] + verbosity + \
            concat_map(lambda ext: ("-extension",ext), vim_list_if_set("b:merlin_extensions")) + \
            concat_map(lambda pkg: ("-I",pkg), vim_list_if_set("b:merlin_packages_path")) + \
            concat_map(lambda dm: ("-dot-merlin",dm), vim_list_if_set("b:merlin_dot_merlins")) + \
            log_errors + \
            vim.eval('g:merlin_binary_flags') + \
            vim_list_if_set('b:merlin_flags')

    result = json.loads(merlin_exec(cmdline,input=content))
    if result['notifications']:
        notifications = "\n".join(result['notifications'])
        vimprint("(merlin) notifications:\n" + notifications)
    class_ = result['class']
    value = result['value']
    if class_ == "return":
        return value
    elif class_ == "failure":
        raise Failure(value)
    elif class_ == "error":
        raise Error(value)
    elif class_ == "exception":
        raise MerlinException(value)

def command(*args):
    return command2(args)

def uniq(seq):
    seen = set()
    seen_add = seen.add
    return [ x for x in seq if not (x in seen or seen_add(x))]

def vim_is_set(name, default=False):
    if not vim.eval('exists("%s")' % name):
        return default
    return not (vim.eval(name) in ["", "0", "false"])

def vim_list_if_set(name):
    return vim.eval('exists("{0}") ? {0} : []'.format(name))

def fmtpos(arg):
    if arg is None:
        return "end"
    elif isinstance(arg, dict):
        line = arg['line']
        col = arg['col']
    elif isinstance(arg, tuple) or isinstance(arg, list):
        (line, col) = arg
    else:
        raise ValueError("fmtpos takes None, (line,col) or { 'line' : _, 'col' : _ }")
    return "{0}:{1}".format(line, col)

######## BASIC COMMANDS

def command_version():
    try:
        str = merlin_exec(["-version"])
        print(str)
    except MerlinExc as e:
        try_print_error(e)

def display_load_failures(result):
    if 'failures' in result and result['failures']:
        failures = ", ".join(result['failures'])
        vimprint("merlin: " + failures)

def command_complete_cursor(base,pos):
    with_doc = vim_is_set('g:merlin_completion_with_doc', default=True)
    cmd = ["complete-prefix", "-position", fmtpos(pos), "-prefix", base,
           "-doc", (with_doc and "y" or "n")]
    return command2(cmd,track_verbosity=True)

def command_document(path, pos):
    try:
        if path is not None:
            cmd = ["document", "-identifier", path, "-position", fmtpos(pos)]
        else:
            cmd = ["document", "-position", fmtpos(pos)]
        print(command2(cmd))
    except MerlinExc as e:
        try_print_error(e)

def differs_from_current_file(path):
    buf_path = vim.eval("expand('%:p')")
    return buf_path != path

def vim_fnameescape(s):
    return vim.eval("fnameescape('%s')" % s.replace("'","''"))

def command_locate(path, pos):
    try:
        choice = vim.eval('g:merlin_locate_preference')
        if pos is None:
            return command("locate", "-prefix", path, "-look-for", choice)
        else:
            if path is None:
                pos_or_err = command("locate", "-look-for", choice, "-position", fmtpos(pos))
            else:
                pos_or_err = command("locate", "-prefix", path, "-look-for", choice, "-position", fmtpos(pos))
        if not isinstance(pos_or_err, dict):
            print(pos_or_err)
        else:
            l = pos_or_err['pos']['line']
            c = pos_or_err['pos']['col']
            split_method = vim.eval('g:merlin_split_method')
            # save the current position in the jump list
            vim.command("normal! m'")
            if "file" in pos_or_err and differs_from_current_file(pos_or_err['file']):
                fname = vim_fnameescape(pos_or_err['file'])
                if split_method == "never":
                    vim.command(":keepjumps e %s" % fname)
                elif "tab" in split_method:
                    if "always" in split_method:
                        vim.command(":keepjumps tab split %s" % fname)
                    else:
                        vim.command(":keepjumps tab drop %s" % fname)
                elif "vertical" in split_method:
                    vim.command(":keepjumps vsplit %s" % fname)
                else:
                    vim.command(":keepjumps split %s" % fname)
            elif "always" in split_method:
                if "tab" in split_method:
                    vim.command(":tab split")
                elif "vertical" in split_method:
                    vim.command(":vsplit")
                else:
                    vim.command(":split")
            # TODO: move the cursor using vimscript, so we can :keepjumps?
            vim.current.window.cursor = (l, c)
    except MerlinExc as e:
        try_print_error(e)

def command_motion(cmd, target, pos):
    try:
        pos_or_err = command(cmd, "-target", target, "-position", fmtpos(pos))
        if not isinstance(pos_or_err, dict):
            print(pos_or_err)
        else:
            l = pos_or_err['pos']['line']
            c = pos_or_err['pos']['col']
            # save the current position in the jump list
            vim.command("normal! m'")
            # TODO: move the cursor using vimscript, so we can :keepjumps?
            try:
                vim.current.window.cursor = (l, c)
            except:
                vim.command("$")
    except MerlinExc as e:
        try_print_error(e)

def command_occurrences(pos):
    try:
        lst_or_err = command("occurrences", "-identifier-at", fmtpos(pos))
        if not isinstance(lst_or_err, list):
            print(lst_or_err)
        else:
            return lst_or_err
    except MerlinExc as e:
        try_print_error(e)

def command_holes():
    try:
        lst_or_err = command("holes")
        if not isinstance(lst_or_err, list):
            print(lst_or_err)
        else:
            return lst_or_err
    except MerlinExc as e:
        try_print_error(e)

######## VIM FRONTEND

def vim_complete_prepare(str):
    return re.sub(re_wspaces, " ", str).replace("'", "''").strip()

def vim_complete_prepare_preserve_newlines(str):
    return re.sub(re_spaces_around_nl, "\n", re.sub(re_spaces, " ", str)).replace("'", "''").strip()

def vim_fillentries(entries, vimvar):
    prep = vim_complete_prepare
    prep_nl = vim_complete_prepare_preserve_newlines
    for prop in entries:
        vim.command("let tmp = {'word':'%s','menu':'%s','info':'%s','kind':'%s'}" %
                (prep(prop['name']),prep(prop['desc']),prep_nl(prop['info']),prep(prop['kind'][:1])))
        vim.command("call add(%s, tmp)" % vimvar)

# Complete
def vim_complete_cursor(base, suffix, vimvar):
    vim.command("let %s = []" % vimvar)
    try:
        completions = command_complete_cursor(base,vim.current.window.cursor)
        nb_entries = len(completions['entries'])
        prep = vim_complete_prepare
        if completions['context'] and completions['context'][0] == 'application':
            app = completions['context'][1]
            if not base or base == suffix:
                for label in app['labels']:
                    name = label['name']
                    if not name.startswith(suffix): name = name.replace("?","~")
                    if name.startswith(suffix):
                        nb_entries = nb_entries + 1
                        vim.command("let l:tmp = {'word':'%s','menu':'%s','info':'%s','kind':'%s'}" %
                                (prep(name),prep(label['name'] + ':' + label['type']),'','~'))
                        vim.command("call add(%s, l:tmp)" % vimvar)
            show_argtype = vim.eval("g:merlin_completion_argtype")
            if ((show_argtype == 'always' or (show_argtype == 'several' and nb_entries > 1))
                    and (not suffix or atom_bound.match(suffix[0]))
                    and app['argument_type'] != "'_a"):
                vim.command("let l:tmp = {'word':'%s','abbr':'<type>','kind':':','menu':'%s','empty':1}" %
                        (prep(suffix),prep(app['argument_type'])))
                vim.command("call insert(%s, l:tmp)" % vimvar)
        vim_fillentries(completions['entries'], vimvar)
        return (nb_entries > 0)
    except MerlinExc as e:
        try_print_error(e)
        return False

def vim_expand_prefix(base, vimvar, kinds=[]):
    vim.command("let %s = []" % vimvar)
    try:
        kinds = concat_map(lambda kind: ("-kind",kind), kinds)
        args = ["expand-prefix",
                "-position", fmtpos(vim.current.window.cursor),
                "-prefix", base] + kinds
        l = command2(args)
        l = l['entries']
        l = map(lambda prop: prop['name'], l)
        l = uniq(sorted(l))
        for prop in l:
            name = prop.replace("'", "''")
            vim.command("call add(%s, '%s')" % (vimvar, name))
    except MerlinExc as e:
        try_print_error(e)

def vim_polarity_search(query, vimvar):
    vim.command("let %s = []" % vimvar)
    try:
        l = command("search-by-polarity", "-query", query, "-position", fmtpos(vim.current.window.cursor))
        vim_fillentries(l['entries'], vimvar)
    except MerlinExc as e:
        try_print_error(e)

# Error listing
def vim_loclist(vimvar, ignore_warnings):
    vim.command("let %s = []" % vimvar)
    errors = command("errors")
    bufnr = vim.current.buffer.number
    nr = 0
    for error in errors:
        ty = 'E'
        if error['type'] == 'warning':
            if vim.eval(ignore_warnings) == 'true':
                continue
            ty = 'W'
        msg = re.sub(re_wspaces, " ", error['message']).replace("'", "''")
        if msg.startswith("Warning "):
            msg = msg[8:]
        elif msg.startswith("Error: "):
            msg = msg[7:]
        elif msg.startswith("Error (warning"):
            msg = re.sub(re_error_warning, r"\1: ", msg)
        lnum = 1
        col = 1
        if 'start' in error:
            lnum = error['start']['line']
            col = error['start']['col'] + 1
        end_lnum = 1
        end_col = 1
        if 'end' in error:
            end_lnum = error['end']['line']
            end_col = error['end']['col']
        vim.command("let l:tmp = {'bufnr':%d,'lnum':%d,'col':%d,'end_lnum':%d,'end_col':%d,'vcol':0,'nr':%d,'pattern':'','text':'%s','type':'%s','valid':1}" %
                (bufnr, lnum, col, end_lnum, end_col, nr, msg, ty))
        nr = nr + 1
        vim.command("call add(%s, l:tmp)" % vimvar)

# Locate
def vim_locate_at_cursor(path):
    command_locate(path, vim.current.window.cursor)

def vim_locate_under_cursor():
    vim_locate_at_cursor(None)

# Jump and Phrase motion
def vim_jump_to(target):
    command_motion("jump", target, vim.current.window.cursor)

def vim_jump_default():
  vim_jump_to("fun let module match")

def vim_phrase_prev():
    command_motion("phrase", "prev", vim.current.window.cursor)

def vim_phrase_next():
    command_motion("phrase", "next", vim.current.window.cursor)

# Document
def vim_document_at_cursor(path):
    command_document(path, vim.current.window.cursor)

def vim_document_under_cursor():
    vim_document_at_cursor(None)

# Occurrences
def vim_occurrences(vimvar):
    vim.command("let %s = []" % vimvar)
    line, col = vim.current.window.cursor
    lst = command_occurrences((line, col))
    lst = map(lambda x: x['start'], lst)
    bufnr = vim.current.buffer.number
    nr = 0
    cursorpos = 0
    for pos in lst:
        lnum = pos['line']
        lcol = pos['col']
        if (lnum, lcol) <= (line, col): cursorpos = nr
        text = vim.current.buffer[lnum - 1]
        text = text.replace("'", "''")
        vim.command("let l:tmp = {'bufnr':%d,'lnum':%d,'col':%d,'vcol':0,'nr':%d,'pattern':'','text':'%s','type':'I','valid':1}" %
                (bufnr, lnum, lcol + 1, nr, text))
        nr = nr + 1
        vim.command("call add(%s, l:tmp)" % vimvar)
    return cursorpos + 1

def vim_occurrences_search():
    line, col = vim.current.window.cursor
    lst = command_occurrences((line, col))
    result = ""
    over = ""
    start_col = 0
    for pos in lst:
        current = easy_matcher_wide(pos['start'], pos['end'])
        l1 = pos['start']['line']
        c1 = pos['start']['col']
        c2 = pos['end']['col']
        if line == l1 and col >= c1 and col <= c2:
            over = current
            start_col = c1
        elif result == "":
            result = current
        else:
            result = result + "\\|" + current
    return "[%s, '%s', '%s']" % (start_col, over, result)

def vim_occurrences_replace(content):
    cursor = vim.current.window.cursor
    lst = command_occurrences(cursor)
    lst.reverse()
    for pos in lst:
        if pos['start']['line'] == pos['end']['line']:
            mlen = pos['end']['col'] - pos['start']['col']
            matcher = make_matcher(pos['start'], pos['end'])
            query = ":%s/{0}.\\{{{1}\\}}/{2}/".format(matcher,mlen,content)
            vim.command(query)
    vim.current.window.cursor = cursor

def vim_refactor_open(mode):
    cursor = vim.current.window.cursor
    lst = command("refactor-open","-position",fmtpos(cursor),"-action",mode)
    lst.reverse()
    for pos in lst:
        if pos['start']['line'] == pos['end']['line']:
            mlen = pos['end']['col'] - pos['start']['col']
        matcher = make_matcher(pos['start'], pos['end'])
        query = ":%s/{0}.\\{{{1}\\}}/{2}/".format(matcher,mlen,pos['content'])
        vim.command(query)
    vim.current.window.cursor = cursor

# Expression typing
def vim_type(expr):
    cmd = ["type-expression",
            "-expression", expr,
            "-position", fmtpos(vim.current.window.cursor)]
    try:
        ty = command2(cmd)
        res = {'type': str(ty), 'matcher': '', 'tail_info':''}
        return json.dumps(res)
    except MerlinExc as e:
        if re.search('Not_found',str(e)):
            return '{}'
        else:
            try_print_error(e)
            return '{}'

def bounds_of_ocaml_atom_at_pos(to_line, col):
    line = vim.current.buffer[to_line]
    start = col
    stop = col
    while start > 0:
        if atom_bound.match(line[start - 1]) is None:
            break
        else:
            start -= 1
    while stop < len(line):
        if atom_bound.match(line[stop]) is None:
            break
        else:
            stop += 1
    return (line[start:stop], start, stop)

def vim_type_reset():
    global enclosing_types
    global current_enclosing
    enclosing_types = [] # reset
    current_enclosing = -1

def replace_buffer_portion(start, end, txt, jump = True):
    (encode,decode) = vim_codec()

    start_line = start['line'] - 1
    b = vim.current.buffer

    fst_line = b[start_line]
    lst_line = b[end['line'] - 1]

    prefix = fst_line[0:start['col']]
    suffix = lst_line[end['col']:len(lst_line)]

    del b[start_line:end['line']]

    txt = decode(prefix) + txt + decode(suffix)
    lines = txt.split('\n')
    lines.reverse()
    nb_lines = 0
    for line in lines:
        nb_lines += 1
        b[start_line:start_line] = [ encode(line) ]

    # Properly reindent the modified lines
    vim.current.window.cursor = (start['line'], 0)
    vim.command('silent normal %d==' % nb_lines)

    if jump:
      # We look for a hole to move the cursor to in the range we replaced
      vim_next_hole(start_line, start_line + nb_lines)

def vim_case_analysis():
    global enclosing_types
    global current_enclosing

    if enclosing_types == []:
        to_line, to_col = vim.current.window.cursor
        try:
            enclosing_types = command("type-enclosing", "-position", fmtpos((to_line,to_col)))
            if enclosing_types != []:
                current_enclosing = 0
            else:
                atom, _, _ = bounds_of_ocaml_atom_at_pos(to_line - 1, to_col)
                print("didn't manage to destruct '%s'" % atom)
                return
        except MerlinExc as e:
            try_print_error(e)
            return

    tmp = enclosing_types[current_enclosing]
    try:
        result = command("case-analysis", "-start", fmtpos(tmp['start']),
                                          "-end", fmtpos(tmp['end']))
        tmp = result[0]
        txt = result[1]
        replace_buffer_portion(tmp['start'], tmp['end'], txt)


    except MerlinExc as e:
        try_print_error(e)

    vim_type_reset()

def vim_type_enclosing():
    global enclosing_types
    global current_enclosing
    vim_type_reset()
    try:
        to_line, to_col = vim.current.window.cursor
        enclosing_types = command2(
                ["type-enclosing",
                 "-position", fmtpos((to_line,to_col)),
                 "-index", "0"
                ],
                track_verbosity=True
                )
        if enclosing_types != []:
            return vim_next_enclosing()
        else:
            atom, start, stop = bounds_of_ocaml_atom_at_pos(to_line - 1, to_col)
            tmp = {'start': {'line':to_line, 'col':start},
                   'end':   {'line':to_line, 'col':stop }}
            tmp['matcher'] = make_matcher(tmp['start'], tmp['end'])
            tmp['atom'] = atom
            return json.dumps(tmp)
    except MerlinExc as e:
        try_print_error(e)
        return '{}'

def move_cursor_and_type(line, col):
    vim.current.window.cursor = (line, col)
    typ = json.loads(vim_type_enclosing())
    return typ['type']

def vim_previous_hole():
    line, col = vim.current.window.cursor
    holes = command_holes()
    holes.reverse()
    for hole in holes:
      hline = hole['start']['line']
      hcol = hole['start']['col']
      if (hline, hcol) < (line, col):
        vim.current.window.cursor = (hline, hcol)
        print(hole['type'])
        return
    # If no hole was found before the cursor we jump
    # to the last hole of the file if any.
    if len(holes) > 0:
      hline = holes[0]['start']['line']
      hcol = holes[0]['start']['col']
      vim.current.window.cursor = (hline, hcol)
      print(holes[0]['type'])

def vim_next_hole(min = 0, max = float('inf')):
    min = float(min)
    max = float(max)
    line, col = vim.current.window.cursor
    holes = command_holes()

    for hole in holes:
      hline = hole['start']['line']
      hcol = hole['start']['col']
      if hline >= min and (hline, hcol) >= (line, col) and hline <= max:
        vim.current.window.cursor = (hline, hcol)
        print(hole['type'])
        return

    # If no hole was found after the cursor we jump
    # to the first hole of the file if any.
    if max == float('inf') and len(holes) > 0:
      hline = holes[0]['start']['line']
      hcol = holes[0]['start']['col']
      vim.current.window.cursor = (hline, hcol)
      print(holes[0]['type'])

def vim_construct(depth):
    vimvar = "b:constr_result"
    vim.command("let %s = []" % vimvar)
    line, col = vim.current.window.cursor
    try:
      result = command(
        "construct",
        "-max-depth", depth,
        "-position", fmtpos((line, col)))
      loc = result[0]
      txts = result[1]

      if len(txts) == 1:
        # If there is only one answer we replace it immediately
        vim.current.window.cursor = (loc['start']['line'], loc['start']['col'])
        replace_buffer_portion(loc['start'], loc['end'], txts[0])

      elif len(txts) > 1:
        # If there is more we remove the hole
        replace_buffer_portion(loc['start'], loc['end'], " ", jump = False)
        vim.current.window.cursor = (loc['start']['line'], loc['start']['col'])

        # and write the alternatives in the b:constr_result list:
        for txt in txts:
          vim.command("call add(%s, {'word':'%s'})" % (vimvar, txt))

    except MerlinExc as e:
      try_print_error(e)

def easy_matcher_wide(start, stop):
    startl = ""
    startc = ""
    if start['line'] > 0:
        startl = "\%{0}l".format(start['line'])
    if start['col'] > 0:
        startc = "\%{0}c".format(start['col'] + 1)
    return '{0}{1}.*\%{2}l\%{3}c'.format(startl, startc, stop['line'], stop['col'] + 1)

def easy_matcher(start, stop):
    startl = ""
    startc = ""
    if start['line'] > 0:
        startl = "\%>{0}l".format(start['line'] - 1)
    if start['col'] > 0:
        startc = "\%>{0}c".format(start['col'])
    return '{0}{1}\%<{2}l\%<{3}c'.format(startl, startc, stop['line'] + 1, stop['col'] + 1)

def hard_matcher(start, stop):
    first_start = {'line' : start['line'], 'col' : start['col']}
    first_stop =  {'line' : start['line'], 'col' : 4242}
    first_line = easy_matcher(first_start, first_stop)
    mid_start = {'line' : start['line']+1, 'col' : 0}
    mid_stop =  {'line' : stop['line']-1 , 'col' : 4242}
    middle = easy_matcher(mid_start, mid_stop)
    last_start = {'line' : stop['line'], 'col' : 0}
    last_stop =  {'line' : stop['line'], 'col' : stop['col']}
    last_line = easy_matcher(last_start, last_stop)
    return "{0}\|{1}\|{2}".format(first_line, middle, last_line)

def make_matcher(start, stop):
    if start['line'] == stop['line']:
        return easy_matcher(start, stop)
    else:
        return hard_matcher(start, stop)

def enclosing_tail_info(record):
    if record['tail'] == 'call': return ' (* tail call *)'
    if record['tail'] == 'position': return ' (* tail position *)'
    return ''

def enclosing_type_text(record):
    global enclosing_types

    # The server has an undocumented functionality where it still returns *all*
    # enclosing nodes when the `type-enclosing` command is passed `-index` (this
    # is contrary to the documentation of the protocol); with only the requested
    # element having an actual type-string attached. The remaining elements do
    # not have their type calculated (which *is* in line with the protocol
    # documentation); and instead simply have their *index in the response*
    # reported in the `type` field.
    #
    # tl;dr If our `enclosing_types` cache has an `int` value in `type`, then
    # the actual value has to be requested from the server again.
    if isinstance(record['type'], int):
        # The indexes in the cache correspond to the *innermost* request - but
        # changing the cursor-postion of the request, will change the indexes of
        # the response. Thus, I re-use the position of the innermost cached
        # enclosing-type.
        innermost_type = enclosing_types[0]

        types = command2(
                ["type-enclosing",
                 "-position", fmtpos(innermost_type['start']),
                 "-index", str(record['type'])
                ],
                track_verbosity=True
                )

        record['type'] = types[record['type']]['type']

def vim_current_enclosing():
    global enclosing_types
    global current_enclosing
    tmp = enclosing_types[current_enclosing]
    tmp['matcher'] = make_matcher(tmp['start'], tmp['end'])

    enclosing_type_text(tmp)

    tmp['tail_info'] = enclosing_tail_info(tmp)
    return json.dumps(tmp)

def vim_next_enclosing():
    if enclosing_types != []:
        global current_enclosing
        if current_enclosing < len(enclosing_types):
            current_enclosing += 1
        if current_enclosing < len(enclosing_types):
            return vim_current_enclosing()
    return '{}'

def vim_prev_enclosing():
    if enclosing_types != []:
        global current_enclosing
        if current_enclosing >= 0:
            current_enclosing -= 1
        if current_enclosing >= 0:
            return vim_current_enclosing()
    return '{}'

# Finding files
def vim_which(name,exts):
    if not isinstance(exts, list): exts = [exts]
    files = concat_map(lambda ext: ("-file",name+"."+ext), exts)
    return command('path-of-source', *files)

def vim_which_ext(exts,vimvar):
    files = command('list-modules', *concat_map(lambda ext: ("-ext",ext), exts))
    vim.command("let %s = []" % vimvar)
    for f in sorted(set(files)):
        vim.command("call add(%s, '%s')" % (vimvar, f))

# Options listing
def vim_flags_list(vimvar):
    for x in command('flags-list'):
        vim.command("call add(%s, '%s')" % (vimvar, x))

def vim_extension_list(vimvar):
    for x in command('extension-list'):
        vim.command("call add(%s, '%s')" % (vimvar, x))

def vim_findlib_list(vimvar):
    for x in command('findlib-list'):
        vim.command("call add(%s, '%s')" % (vimvar, x))

# Stuff

def setup_merlin():
    result = command("check-configuration")
    display_load_failures(result)
    vim.command('let b:dotmerlin=[]')
    # Tell merlin the content of the buffer.
    # This allows merlin idle-job to preload content if nothing else is requested.
    if 'dot_merlins' in result:
        fnames = ','.join(map(lambda fname: '"'+fname+'"', result['dot_merlins']))
        (enc, dec) = vim_codec()
        fnames = enc(fnames)
        vim.command('let b:dotmerlin=[{0}]'.format(fnames))

def vim_last_commands():
    global last_commands
    args = map(lambda x: " ".join(x), last_commands)
    print("Last merlin commands:\n" + "\n".join(args))
