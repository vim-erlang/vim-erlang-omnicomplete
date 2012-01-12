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
