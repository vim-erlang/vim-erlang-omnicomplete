#!/usr/bin/env escript

main([ModName]) ->
    code:add_path("ebin"),
    case file:consult("rebar.config") of
        {ok, Terms} ->
            RebarDeps = proplists:get_value(deps_dir, Terms, "deps"),
            code:add_paths(filelib:wildcard(RebarDeps ++ "/*/ebin"));
        _ ->
            ok
    end,
    Mod = erlang:list_to_atom(ModName),
    Edoc = try module_edoc(Mod)
    catch
        throw:bad_module -> []
    end,
    Info = try module_info2(Mod)
    catch
        error:undef -> []
    end,
    Funs = merge_functions(Edoc, Info),
    lists:foreach(fun(Fun) -> print_function(Fun) end, Funs);
main(_) ->
    bad_module.

module_edoc(_Mod) ->
    [].
module_info2(Mod) ->
    lists:keysort(1, Mod:module_info(exports)).

merge_functions(Edoc, Info) ->
    merge_functions(Edoc, Info, []).

merge_functions([], [], Funs) ->
    lists:reverse(Funs);
merge_functions([], Info, Funs) ->
    lists:reverse(Funs, Info);
merge_functions(Edoc, [], Funs) ->
    lists:reverse(Funs, Edoc);
merge_functions(Edoc, Info, Funs) ->
    [H1 | T1] = Edoc,
    [H2 | T2] = Info,
    if
        H1 == H2 ->
            merge_functions(T1, T2, [H1 | Funs]);
        H1 < H2 ->
            merge_functions(T1, Info, [H1 | Funs]);
        H1 > H2 ->
            merge_functions(Edoc, T2, [H2 | Funs])
    end.

print_function({Name, Arity}) ->
    io:format("~s/~B~n", [Name, Arity]);
print_function({Name, Args, Return}) ->
    io:format("~s(~s) -> ~s~n", [Name, string:join(Args, ", "), Return]).
