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
    try Mod:module_info(exports) of
        Funs ->
            lists:foreach(
                fun({FunName, ArgsCount}) ->
                        io:format("~s/~B~n", [FunName, ArgsCount])
                end,
                Funs)
    catch
        error:undef ->
            bad_module
    end;
main(_) ->
    bad_module.





merge_module_functions(Edoc, ModInfo) ->
    merge_module_functions(Edoc, ModInfo, []).

merge_module_functions([], [], Funs) ->
    lists:reverse(Funs);
merge_module_functions([], ModInfo, Funs) ->
    lists:reverse(Funs, ModInfo);
merge_module_functions(Edoc, [], Funs) ->
    lists:reverse(Funs, Edoc);
merge_module_functions(Edoc, ModInfo, Funs) ->
    [H1 | T1] = Edoc,
    [H2 | T2] = ModInfo,
    if
        H1 == H2 ->
            merge_module_functions(T1, T2, [H1 | Funs]);
        H1 < H2 ->
            merge_module_functions(T1, ModInfo, [H1 | Funs]);
        H1 > H2 ->
            merge_module_functions(Edoc, T2, [H2 | Funs])
    end.
