" Vim omni completion file
" Language:     Erlang
" Author:       Oscar Hellström <oscar@oscarh.net>
" Contributors: kTT (http://github.com/kTT)
"               Ricardo Catalinas Jiménez <jimenezrick@gmail.com>
"               Eduardo Lopez (http://github.com/tapichu)
"               Kjell Winblad (https://dupwin.se)
" License:      Vim license

" Completion program path
let s:erlang_complete_file = expand('<sfile>:p:h') . '/erlang_complete.erl'

" Returns whether "substring" is a prefix of "string".
function s:StartsWith(string, substring)
    let string_start = strpart(a:string, 0, len(a:substring))
    return string_start ==# a:substring
endfunction

" If we are running in Cygwin, the path needs to be converted.
" See: https://github.com/vim-erlang/vim-erlang-omnicomplete/issues/21
if has('win32') == 0 && s:StartsWith(system('uname'), 'CYGWIN')
    " Cygwin system. Now check if erlang is Windows or cygwin (currently only
    " Windows is possible)
    let cygwin_base_path = system('cygpath -w /')
    if !s:StartsWith(s:erlang_complete_file, cygwin_base_path)
        " Windows, as expected
        let s:erlang_complete_file = system('cygpath -w ' . s:erlang_complete_file)
    endif
endif

if !exists('g:erlang_completion_cache')
    let g:erlang_completion_cache = 1
endif

if !exists('g:erlang_completion_preview_help')
    let g:erlang_completion_preview_help = 1
end

if !exists('g:erlang_completion_zero_arity_paren')
    let g:erlang_completion_zero_arity_paren = '()'
end

if !exists('g:erlang_completion_nonzero_arity_paren')
    let g:erlang_completion_nonzero_arity_paren = '('
end

if !exists('g:erlang_completion_extend_arity')
    let g:erlang_completion_extend_arity = 1
end

if !exists('g:erlang_completion_extend_code_path_wildcard')
    let g:erlang_completion_extend_code_path_wildcard = ""
end
" Modules cache used to speed up the completion.
"
" This dictionary contains completion items that represent functions exported
" from this module. Each value in this dictionary is a list of completion
" items. A completion item is itself a dictionary (see ":help complete-items"
" for the format of the completion item dictionaries).
"
" Type: module -> [complete_item]
"
" Example: {'mymod': [{'word': 'myfun(',  # text inserted during the completion
"                      'abbr': 'myfun/1', # text displayed in the popup menu
"                      'kind': 'f',       # this is a function
"                      'dup': 1},         # 'word' values might be duplicates
"                     {'word': 'myfun(',
"                      'abbr': 'myfun/2',
"                      'kind': 'f',
"                      'dup': 1} ,
"                     {'word': 'myfun_with_type_spec(',
"                      'abbr': 'myfun_with_type_spec(A, B)',
"                      'kind': 'f',
"                      'dup': 1}],
"           'myothermod': [{'word': 'myotherfun(',
"                           'abbr': 'myotherfun/2',
"                           'kind': 'f',
"                           'dup': 1}]}
let s:modules_cache = {}

" Patterns for completions
let s:erlang_local_func_beg    = '\(\<[0-9A-Za-z_-]*\|\s*\)$'
let s:erlang_external_func_beg = '\<[0-9A-Za-z_-]\+:[0-9A-Za-z_-]*$'
let s:erlang_blank_line        = '^\s*\(%.*\)\?$'

" This list comes from http://www.erlang.org/doc/man/erlang.html (minor
" modifications have been performed).
let s:auto_imported_bifs = [
    \ 'abs(Number) -> number()',
    \ 'apply(Fun, Args) -> term()',
    \ 'apply(Module, Function, Args) -> term()',
    \ 'atom_to_binary(Atom, Encoding) -> binary()',
    \ 'atom_to_list(Atom) -> string()',
    \ 'binary_part(Subject, PosLen) -> binary()',
    \ 'binary_part(Subject, Start, Length) -> binary()',
    \ 'binary_to_atom(Binary, Encoding) -> atom()',
    \ 'binary_to_existing_atom(Binary, Encoding) -> atom()',
    \ 'binary_to_float(Binary) -> float()',
    \ 'binary_to_integer(Binary) -> integer()',
    \ 'binary_to_integer(Binary, Base) -> integer()',
    \ 'binary_to_list(Binary) -> [byte()]',
    \ 'binary_to_list(Binary, Start, Stop) -> [byte()]',
    \ 'bitstring_to_list(Bitstring) -> [byte() | bitstring()]',
    \ 'binary_to_term(Binary) -> term()',
    \ 'binary_to_term(Binary, Opts) -> term()',
    \ 'bit_size(Bitstring) -> integer() >= 0',
    \ 'byte_size(Bitstring) -> integer() >= 0',
    \ 'check_old_code(Module) -> boolean()',
    \ 'check_process_code(Pid, Module) -> boolean()',
    \ 'date() -> Date',
    \ 'delete_module(Module) -> true | undefined',
    \ 'demonitor(MonitorRef) -> true',
    \ 'demonitor(MonitorRef, OptionList) -> boolean()',
    \ 'disconnect_node(Node) -> boolean() | ignored',
    \ 'element(N, Tuple) -> term()',
    \ 'erase() -> [{Key, Val}]',
    \ 'erase(Key) -> Val | undefined',
    \ 'error(Reason) -> no_return()',
    \ 'error(Reason, Args) -> no_return()',
    \ 'exit(Reason) -> no_return()',
    \ 'exit(Pid, Reason) -> true',
    \ 'float(Number) -> float()',
    \ 'float_to_binary(Float) -> binary()',
    \ 'float_to_binary(Float, Options) -> binary()',
    \ 'float_to_list(Float) -> string()',
    \ 'float_to_list(Float, Options) -> string()',
    \ 'garbage_collect() -> true',
    \ 'garbage_collect(Pid) -> boolean()',
    \ 'get() -> [{Key, Val}]',
    \ 'get(Key) -> Val | undefined',
    \ 'get_keys(Val) -> [Key]',
    \ 'group_leader() -> pid()',
    \ 'group_leader(GroupLeader, Pid) -> true',
    \ 'halt() -> no_return()',
    \ 'halt(Status) -> no_return()',
    \ 'halt(Status, Options) -> no_return()',
    \ 'hd(List) -> term()',
    \ 'integer_to_binary(Integer) -> binary()',
    \ 'integer_to_binary(Integer, Base) -> binary()',
    \ 'integer_to_list(Integer) -> string()',
    \ 'integer_to_list(Integer, Base) -> string()',
    \ 'iolist_to_binary(IoListOrBinary) -> binary()',
    \ 'iolist_size(Item) -> integer() >= 0',
    \ 'is_alive() -> boolean()',
    \ 'is_atom(Term) -> boolean()',
    \ 'is_binary(Term) -> boolean()',
    \ 'is_bitstring(Term) -> boolean()',
    \ 'is_boolean(Term) -> boolean()',
    \ 'is_float(Term) -> boolean()',
    \ 'is_function(Term) -> boolean()',
    \ 'is_function(Term, Arity) -> boolean()',
    \ 'is_integer(Term) -> boolean()',
    \ 'is_list(Term) -> boolean()',
    \ 'is_number(Term) -> boolean()',
    \ 'is_pid(Term) -> boolean()',
    \ 'is_port(Term) -> boolean()',
    \ 'is_process_alive(Pid) -> boolean()',
    \ 'is_record(Term, RecordTag) -> boolean()',
    \ 'is_record(Term, RecordTag, Size) -> boolean()',
    \ 'is_reference(Term) -> boolean()',
    \ 'is_tuple(Term) -> boolean()',
    \ 'length(List) -> integer() >= 0',
    \ 'link(PidOrPort) -> true',
    \ 'list_to_atom(String) -> atom()',
    \ 'list_to_binary(IoList) -> binary()',
    \ 'list_to_bitstring(BitstringList) -> bitstring()',
    \ 'list_to_existing_atom(String) -> atom()',
    \ 'list_to_float(String) -> float()',
    \ 'list_to_integer(String) -> integer()',
    \ 'list_to_integer(String, Base) -> integer()',
    \ 'list_to_pid(String) -> pid()',
    \ 'list_to_tuple(List) -> tuple()',
    \ 'load_module(Module, Binary) -> {module, Module} | {error, Reason}',
    \ 'make_ref() -> reference()',
    \ 'max(Term1, Term2) -> Maximum',
    \ 'min(Term1, Term2) -> Minimum',
    \ 'module_loaded(Module) -> boolean()',
    \ 'monitor(Type, Item) -> MonitorRef',
    \ 'monitor_node(Node, Flag) -> true',
    \ 'node() -> Node',
    \ 'node(Arg) -> Node',
    \ 'nodes() -> Nodes',
    \ 'nodes(Arg) -> Nodes',
    \ 'now() -> Timestamp',
    \ 'open_port(PortName, PortSettings) -> port()',
    \ 'pid_to_list(Pid) -> string()',
    \ 'port_close(Port) -> true',
    \ 'port_command(Port, Data) -> true',
    \ 'port_command(Port, Data, OptionList) -> boolean()',
    \ 'port_connect(Port, Pid) -> true',
    \ 'port_control(Port, Operation, Data) -> iodata() | binary()',
    \ 'pre_loaded() -> [module()]',
    \ 'process_flag(Flag :: trap_exit, Boolean) -> OldBoolean',
    \ 'process_flag(Pid, Flag, Value) -> OldValue',
    \ 'process_info(Pid) -> Info',
    \ 'process_info(Pid, Item) -> InfoTuple | [] | undefined',
    \ 'process_info(Pid, ItemList) -> InfoTupleList | [] | undefined',
    \ 'processes() -> [pid()]',
    \ 'purge_module(Module) -> true',
    \ 'put(Key, Val) -> term()',
    \ 'register(RegName, PidOrPort) -> true',
    \ 'registered() -> [RegName]',
    \ 'round(Number) -> integer()',
    \ 'self() -> pid()',
    \ 'setelement(Index, Tuple1, Value) -> Tuple2',
    \ 'size(Item) -> integer() >= 0',
    \ 'spawn(Fun) -> pid()',
    \ 'spawn(Node, Fun) -> pid()',
    \ 'spawn(Module, Function, Args) -> pid()',
    \ 'spawn(Node, Module, Function, Args) -> pid()',
    \ 'spawn_link(Fun) -> pid()',
    \ 'spawn_link(Node, Fun) -> pid()',
    \ 'spawn_link(Module, Function, Args) -> pid()',
    \ 'spawn_link(Node, Module, Function, Args) -> pid()',
    \ 'spawn_monitor(Fun) -> {pid(), reference()}',
    \ 'spawn_monitor(Module, Function, Args) -> {pid(), reference()}',
    \ 'spawn_opt(Fun, Options) -> pid() | {pid(), reference()}',
    \ 'spawn_opt(Node, Fun, Options) -> pid() | {pid(), reference()}',
    \ 'spawn_opt(Module, Function, Args, Options) -> pid() | {pid(), reference()}',
    \ 'spawn_opt(Node, Module, Function, Args, Options) -> pid() | {pid(), reference()}',
    \ 'split_binary(Bin, Pos) -> {binary(), binary()}',
    \ 'statistics(Item :: context_switches) -> {ContextSwitches, 0}',
    \ 'term_to_binary(Term) -> ext_binary()',
    \ 'term_to_binary(Term, Options) -> ext_binary()',
    \ 'throw(Any) -> no_return()',
    \ 'time() -> Time',
    \ 'tl(List) -> term()',
    \ 'trunc(Number) -> integer()',
    \ 'tuple_size(Tuple) -> integer() >= 0',
    \ 'tuple_to_list(Tuple) -> [term()]',
    \ 'unlink(Id) -> true',
    \ 'unregister(RegName) -> true',
    \ 'whereis(RegName) -> pid() | port() | undefined']

" Return the informational line displayed at the end of the preview window.
function s:GetPreviewLine()
    if g:erlang_completion_preview_help == 1
        return "\n\nClose preview window: CTRL-W z in normal mode." .
             \ "\nDisable preview window: :set cot-=preview." .
             \ "\nDon't show this message: :let g:erlang_completion_preview_help = 0."
    else
        return ""
    end
endfunction

" Find the next non-blank line
function s:ErlangFindNextNonBlank(lnum)
    let lnum = nextnonblank(a:lnum + 1)
    let line = getline(lnum)

    while line =~ s:erlang_blank_line && 0 != lnum
        let lnum = nextnonblank(lnum + 1)
        let line = getline(lnum)
    endwhile

    return lnum
endfunction

" Return an argument list that contains a given number of arguments.
"
" Example:
"
" - Call: s:CreateArgumentList(3)
" - Return value: "(T1, T2, T3)"
function s:CreateArgumentList(arity)
    let argument_list = []
    let i = 1
    while i <= a:arity
        call add(argument_list, 'T' .. i)
        let i += 1
    endwhile
    return '(' .. join(argument_list, ', ') .. ')' 
endfunction

" Return a completion item.
"
" Parameters:
"
" - type: Type of the "spec" parameter. Either 'module', 'function_name' or
"   'function_spec'.
" - spec: A module name, a function name or a function specification.
"
" See the documentation of the completion items in ":help complete-items".
function s:CreateComplItem(type, spec)

    if a:type == 'module'
        " module example = "my_mod"
        let target_type = 'module'
        let module = a:spec
        let compl_word = module . ':'
        let compl_abbr = module
        let compl_info = module . s:GetPreviewLine()
        let compl_kind = 'm'
    elseif a:type == 'function_name'
        " function_spec example: "my_fun"
        " function_name example: "my_fun"
        " function_args example: ""
        let target_type = 'function'
        let function_spec = a:spec
        let function_name = a:spec
        let function_args = ''
    elseif a:type == 'function_spec'
        " function_spec examples:
        "   - "my_fun/2"
        "   - "my_fun(A, B) -> integer()"
        " function_name example: "my_fun"
        " function_args example:
        "   - "/2"
        "   - "(A, B) -> integer()"
        let target_type = 'function'
        let function_spec = a:spec
        let function_name = matchstr(function_spec, '\w*')
        let function_args = function_spec[len(function_name):]
    endif

    if target_type == 'function'

        " Calculate which parenthesis to insert after the function name
        " (depending on whether the function has a zero arity).
        if function_args == '/0' || function_args =~# '^()'
            let paren = g:erlang_completion_zero_arity_paren
        else
            let paren = g:erlang_completion_nonzero_arity_paren
        endif

        " Extend the function's arity to an argument list (if necessary)
        if g:erlang_completion_extend_arity && function_args[0] == '/'
            let arity = str2nr(function_args[1:])
            let function_args = s:CreateArgumentList(arity) .. ' -> any()'
            let function_spec = function_name .. function_args
        endif

        let compl_word = function_name . paren
        let compl_abbr = function_spec
        let compl_info = function_spec . s:GetPreviewLine()
        let compl_kind = 'f'
    endif

    return {'word': compl_word,
           \'abbr': compl_abbr,
           \'info': compl_info,
           \'kind': compl_kind,
           \'dup': 1}
endfunction

" Add error as completion items.
"
" Parameters:
"
" - base: The word to be completed.
" - error_output: The output of the erlang_complete.erl script, which
"   describes the error.
"
" This functions adds a short help text ("Completion error...") to the
" completion popup, and it adds the actual error to the preview window.
"
" See the documentation of the completion items in ":help complete-items".
function s:AddComplErrorItems(compl_words, base, error_output)

    " Vim inserts compl_word instead of the word already typed by the user. We
    " don't want to modify the text already typed by the user (a:base), so we
    " set compl_word to a:base, making Vim replace the already typed word with
    " itself.
    "
    " There is one exception. If the already typed word is an empty string, we
    " need to replace it with something, because if compl_word is an empty
    " string, then Vim will ignore that completion item, and it will say "Omni
    " completion Pattern not found), and it will not show either the
    " completion popup or the preview window. To make Vim show the error
    " popup, we set compl_word to ' ' in this case.
    let compl_word = (a:base == '') ? ' ' : a:base

    " Let's show the error output in the preview window (if it is enabled).
    let compl_info = a:error_output . s:GetPreviewLine()

    let help_lines =
        \ ["Completion error.",
        \  " ",
        \  "The preview window contains the error output.",
        \  "Enable the preview window with: ':set cot+=preview'.",
        \  " ",
        \  "See ':help vim-erlang-omnicomplete-errors' for more information."]

    " In order to show the help lines in the completion popup, we add each
    " help line in a separate completion item.
    for help_line in help_lines
        let compl_item = {'word': compl_word,
                         \'abbr': help_line,
                         \'info': compl_info,
                         \'dup': 1}
        call add(a:compl_words, compl_item)
    endfor

endfunction

" Find external function names
"
" Parameters:
"
" - module: the module being edited.
" - base: the word to be completed.
function s:ErlangFindExternalFunc(module, base)

    " If the module is cached, load its functions
    if has_key(s:modules_cache, a:module)
        let compl_words = []
        let module_cache = get(s:modules_cache, a:module)
        for compl_item in module_cache
            " If a:base is a prefix of compl_item, add compl_item to the list
            " of possible completions
            if match(compl_item.word, a:base) == 0
                call add(compl_words, compl_item)
            endif
        endfor

        return compl_words
    endif

    let compl_words = []
    let output = system('escript ' . fnameescape(s:erlang_complete_file) .
                        \' list-functions ' . fnameescape(a:module) .
                        \' --basedir ' .  fnameescape(expand('%:p:h')) .
                        \' --extend-code-path-wildcard ' .  fnameescape(g:erlang_completion_extend_code_path_wildcard))
    let output_lines = split(output, '\n')

    " There are two possibilities:
    "
    " 1.  If the completion items were successfully calculated, the script
    "     output has the following format:
    "
    "         <warning 1>
    "         ...
    "         <warning k>
    "         execution_successful
    "         <completion 1>
    "         ...
    "         <completion n>
    "
    "     If the user wants to complete a module, we do not prevent them from
    "     doing that. So we simply ignore the warnings.
    "
    " 2.  If there was an error, the script output has the following format:
    "
    "         <error line 1>
    "         ...
    "         <error line k>
    "
    "     In this case we present the errors to the user in the preview
    "     window.
    "
    " The marker_index variable is the index of the line that contains
    " the 'execution_successful' marker (counting the first line as 0). If
    " there is no such marker, marker_index is -1.
    let marker_index = index(output_lines, 'execution_successful')

    if marker_index != -1

        " We found possible completions, so we add them as completions items.

        " We iterate on functions in the given module that start with `base` and
        " add them to the completion list.
        let function_specs = output_lines[(marker_index + 1):]
        for function_spec in function_specs
            " - When the function doesn't have a type spec, its function_spec
            "   will be e.g. "f/2".
            " - When the function has a type spec, its function_spec will be e.g.
            "   "f(A, B)".
            if match(function_spec, a:base) == 0
                let compl_item = s:CreateComplItem('function_spec', function_spec)
                call add(compl_words, compl_item)

                " Populate the cache only when iterating over all the
                " module functions (i.e. no prefix for the completion)
                if g:erlang_completion_cache && a:base == ''
                    if !has_key(s:modules_cache, a:module)
                        let s:modules_cache[a:module] = [compl_item]
                    else
                        let module_cache = get(s:modules_cache, a:module)
                        let s:modules_cache[a:module] =
                          \ add(module_cache, compl_item)
                    endif
                endif

                " The user entered some text, so stop the completion
                if complete_check()
                    " The module couldn't be entirely cached
                    if has_key(s:modules_cache, a:module)
                        call remove(s:modules_cache, a:module)
                    endif
                    break
                endif
            endif
        endfor

    else

        " There was an error during completion.
        call s:AddComplErrorItems(compl_words, a:base, output)

    endif

    return compl_words
endfunction

" Find local function names, BIFs and modules.
"
" This function is called when a word (without a ":") needs to be completed,
" such as base = "lis". This could be a local function call (e.g.
" "list_things"), a BIF ("list_to_binary") or a module ("lists").
"
" Parameter:
"
" - base: the word to be completed.
function s:ErlangFindLocalFunc(base)
    " Begin at line 1
    let lnum = s:ErlangFindNextNonBlank(1)

    if "" == a:base
        let base = '^\w' " Used to match against word symbol
    else
        let base = '^' . a:base
    endif

    " Find local functions that start with `base`.
    let compl_words = []
    while 0 != lnum && !complete_check()
        let line = getline(lnum)
        let function_name = matchstr(line, base . '[0-9A-Za-z_-]\+(\@=')
        if function_name != ""
            " We found such a local function.
            let compl_item = s:CreateComplItem('function_name', function_name)
            call add(compl_words, compl_item)
        endif
        let lnum = s:ErlangFindNextNonBlank(lnum)
    endwhile

    if "" == a:base
        let base = ''
    else
        let base = '^' . a:base
    endif

    " Find BIFs that start with `base`.
    for bif_spec in s:auto_imported_bifs
        if bif_spec =~# base
            let compl_item = s:CreateComplItem('function_spec', bif_spec)
            call add(compl_words, compl_item)
        endif
    endfor

    " Find modules that start with `base`.
    let modules = system('escript ' . fnameescape(s:erlang_complete_file) .
                        \' list-modules ' .
                        \' --basedir ' . fnameescape(expand('%:p:h')) .
                        \' --extend-code-path-wildcard ' .  fnameescape(g:erlang_completion_extend_code_path_wildcard))
    for module in split(modules, '\n')
        if module =~# base
            let compl_item = s:CreateComplItem('module', module)
            call add(compl_words, compl_item)
        endif
    endfor

    return compl_words
endfunction

" Main function for completion.
"
" - If findstart = 1, then the function must return the column where the base
"   (the word to be completed) starts.
" - If findstart = 0, then a:base is the word to be completed, and the
"   function must return a list with the possible completions.
"
" See ":help complete-functions" for the exact specification of this function.
function erlang_complete#Complete(findstart, base)
    let lnum = line('.')
    let column = col('.')
    let line = strpart(getline('.'), 0, column - 1)

    " 1) If the char to the left of us is not the part of a function call, the
    " user probably wants to type a local function, a module or a BIF
    if line[column - 2] !~ '[0-9A-Za-z:_-]'
        if a:findstart
            return column
        else
            return s:ErlangFindLocalFunc(a:base)
        endif
    endif

    " 2) Function in external module
    if line =~ s:erlang_external_func_beg
        let delimiter = match(line, ':[0-9A-Za-z_-]*$') + 1
        if a:findstart
            return delimiter
        else
            let module = matchstr(line[:-2], '\<\k*\>$')
            return s:ErlangFindExternalFunc(module, a:base)
        endif
    endif

    " 3) Local function
    if line =~ s:erlang_local_func_beg
        let funcstart = match(line, ':\@<![0-9A-Za-z_-]*$')
        if a:findstart
            return funcstart
        else
            return s:ErlangFindLocalFunc(a:base)
        endif
    endif

    " 4) Unhandled situation
    if a:findstart
        return -1
    else
        return []
    endif
endfunction

function erlang_complete#ClearAllCache()
    let s:modules_cache = {}
endfunction

function erlang_complete#ClearOneCache(mod)
    if has_key(s:modules_cache, a:mod)
        call remove(s:modules_cache, a:mod)
    endif
endfunc

" When a module is saved, delete it from the cache.
autocmd BufWritePre *.erl :call erlang_complete#ClearOneCache(expand('%:t:r'))
