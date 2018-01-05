" Vim omni completion file
" Language:     Erlang
" Author:       Oscar Hellström <oscar@oscarh.net>
" Contributors: kTT (http://github.com/kTT)
"               Ricardo Catalinas Jiménez <jimenezrick@gmail.com>
"               Eduardo Lopez (http://github.com/tapichu)
" License:      Vim license
" Version:      2012/01/14

" Completion program path
let s:erlang_complete_file = expand('<sfile>:p:h') . '/erlang_complete.erl'

au BufWritePre *.erl :call erlang_complete#ClearOneCache(expand('%:t:r'))

if !exists('g:erlang_completion_cache')
    let g:erlang_completion_cache = 1
endif

if !exists('g:erlang_complete_left_bracket')
    let g:erlang_complete_left_bracket = 1
endif

if !exists('g:erlang_complete_extend_arbit')
    let g:erlang_complete_extend_arbit = 0
endif


" Modules cache used to speed up the completion
let s:modules_cache = {}

" Patterns for completions
let s:erlang_local_func_beg    = '\(\<[0-9A-Za-z_-]*\|\s*\)$'
let s:erlang_external_func_beg = '\<[0-9A-Za-z_-]\+:[0-9A-Za-z_-]*$'
let s:erlang_blank_line        = '^\s*\(%.*\)\?$'

" Main function for completion
"
" See ":help complete-functions" for what the function should return.
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

" Find external function names
function s:ErlangFindExternalFunc(module, base)
    " If the module is cached, load its functions
    if has_key(s:modules_cache, a:module)
        let compl_words = []
        for field_cache in get(s:modules_cache, a:module)
            if match(field_cache.word, a:base) == 0
                call add(compl_words, field_cache)
            endif
        endfor

        return compl_words
    endif

    let compl_words = []
    let functions = system('escript ' . fnameescape(s:erlang_complete_file) .
                          \' list-functions ' . fnameescape(a:module) .
                          \' --basedir ' .  fnameescape(expand('%:p:h')))
    for function_spec in split(functions, '\n')
        if match(function_spec, a:base) == 0
            let processed_spec = s:ExtendArbitToParameters(function_spec)
            let function_name = s:GetFunctionNameFromEscriptOutput(function_spec)
            let field = {'word': function_name , 'abbr': function_spec,
                  \  'kind': 'f', 'dup': 1, 'info': processed_spec}
            call add(compl_words, field)

            " Populate the cache only when iterating over all the
            " module functions (i.e. no prefix for the completion)
            if g:erlang_completion_cache && a:base == ''
                if !has_key(s:modules_cache, a:module)
                    let s:modules_cache[a:module] = [field]
                else
                    let fields_cache = get(s:modules_cache, a:module)
                    let s:modules_cache[a:module] = add(fields_cache, field)
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

    return compl_words
endfunction

function s:ExtendArbitToParameters(function_spec)
    if g:erlang_complete_extend_arbit == 1
        let function_name = substitute(a:function_spec, '/.*', '', '')
        let parameter_num = matchstr(a:function_spec, '\(/\)\@<=\d\+')
        if parameter_num != ''
            let int_parameter_num = str2nr(parameter_num)
            let parameter_list = []
            let i = 0
            while int_parameter_num !=  i
                call add(parameter_list, 'T' . i)
                let i = i + 1
            endwhile
            return function_name . '('.join(parameter_list, ', ').') -> term()' 
        else
           return a:function_spec
        endif
    else
        return a:function_spec
    endif 
endfunc


function s:GetFunctionNameFromEscriptOutput(function_spec)
    let pre_function_name = substitute(a:function_spec, ').*', ')', '')
    let pre_function_name_2 = substitute(pre_function_name, '\s', '', 'g')
    if g:erlang_complete_left_bracket == 1
        let function_name = substitute(pre_function_name_2, '([^)]\+)', '(', '')
    else
        let function_name = substitute(pre_function_name_2, '([^)]\+)', '', '')
    endif

    let parameter_num = matchstr(function_name, '/\d\+')
    let real_function_name = substitute(function_name, '/\d\+', '', '')
    echom 'pre:'.pre_function_name.',function name:'.function_name.'parameter num:'.parameter_num.'real name:'.real_function_name
    if parameter_num == '/0'
        if g:erlang_complete_left_bracket == 1
            return real_function_name. '()'
        else
            return real_function_name
        endif
    else 
        if parameter_num != ''
            if g:erlang_complete_left_bracket == 1
                return real_function_name . '('
            else
                return real_function_name
            endif
        else
            return real_function_name
        endif
    endif
endfunction

" Find local function names
function s:ErlangFindLocalFunc(base)

    let buffer_list = getline(1, '$')
    let buffer = join(buffer_list, " ")

    let compl_words = []
    let i = 1
    while !complete_check()
        let function_name_match = matchstr(buffer, '\(\.\s*\|^\)\@<='.a:base.'[0-9A-Za-z_-]\+([_a-zA-Z0-9, \t]*)\(\s*->\)\@=', 0, i)
        if function_name_match != ""
            echom function_name_match
            let pre_function_name = substitute(function_name_match, '\s', '', 'g')
            if g:erlang_complete_left_bracket == 1
                let function_name = substitute(pre_function_name, '([^)]\+)', '(', '')
            else
                let function_name = substitute(pre_function_name, '([^)]\+)', '', '')
            endif
            echom pre_function_name
            call add(compl_words, {'word': function_name ,
                                  \'abbr': function_name, 'info': function_name_match,
                                  \'kind': 'f'})
        else
            break
        endif

        let i = i + 1
    endwhile

    if "" == a:base
        let base = ''
    else
        let base = '^' . a:base
    endif

    for bif_line in s:auto_imported_bifs
        if bif_line =~# base
            let pre_bif_name = substitute(bif_line, ').*', ')', '')
            let pre_bif_name2 = substitute(pre_bif_name, '\s', '', 'g')
            if g:erlang_complete_left_bracket == 1
                let bif_name = substitute(pre_bif_name2, '([^)]\+)', '(', '')
            else
                let bif_name = substitute(pre_bif_name2, '([^)]\+)', '', '')
            endif

            call add(compl_words, {'word': bif_name,
                                   \'abbr': bif_line, 'info': bif_line,
                                   \'kind': 'f'})
        endif
    endfor

    let modules = system('escript ' . fnameescape(s:erlang_complete_file) .
                        \' list-modules ' .
                        \' --basedir ' . fnameescape(expand('%:p:h')))
    for module in split(modules, '\n')
        if module =~# base
            call add(compl_words, {'word': module . ':',
                                  \'abbr': module,
                                  \'kind': 'm'})
        endif
    endfor

    return compl_words
endfunction

function erlang_complete#ClearAllCache()
    let s:modules_cache = {}
endfunction

function erlang_complete#ClearOneCache(mod)
    if has_key(s:modules_cache, a:mod)
        call remove(s:modules_cache, a:mod)
    endif
endfunc

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
