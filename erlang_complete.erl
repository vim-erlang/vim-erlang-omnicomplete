#!/usr/bin/env escript

-include_lib("xmerl/include/xmerl.hrl").

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
    Edoc = try
        module_edoc(Mod)
    catch
        throw:bad_module -> []
    end,
    Info = try
        module_info2(Mod)
    catch
        error:undef -> []
    end,
    FunSpecs = merge_functions(Edoc, Info),
    lists:foreach(fun(Fun) -> print_function(Fun) end, FunSpecs);
main(_) ->
    bad_module.

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

%%% ---------------------------------------------------------------------------
%%% XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
%%% ---------------------------------------------------------------------------

module_edoc(Mod) ->
    File = case filename:find_src(Mod) of
        {error, _} ->
            throw(bad_module);
        {File0, _} ->
            File0 ++ ".erl"
    end,
    {_, Doc} = edoc:get_doc(File),
    Funs = xmerl_xpath:string("/module/functions/function", Doc),
    FunSpecs = lists:map(fun(Fun) -> analyze_function(Fun) end, Funs),
    lists:keysort(1, FunSpecs).

analyze_function(Fun) ->
    Name = get_attribute(Fun, "name"),
    Args0 = xmerl_xpath:string("typespec/type/fun/argtypes/type", Fun),
    Args = lists:map(fun(Arg) -> get_attribute(Arg, "name") end, Args0),
    Return = analyze_return(Fun),
    {Name, Args, Return}.

analyze_return(Fun) ->
    [ReturnType] = xmerl_xpath:string("typespec/type/fun/type/*", Fun),
    simplify_return(xmerl_lib:simplify_element(ReturnType)).

simplify_return({type, _, [Type]}) ->
    simplify_return(Type);
simplify_return({tuple, _, Types}) ->
    Elems = lists:map(fun(Type) -> simplify_return(Type) end, Types),
    "{" ++ string:join(Elems, ", ") ++ "}";
simplify_return({list, _, Types}) ->
    Elems = lists:map(fun(Type) -> simplify_return(Type) end, Types),
    "[" ++ string:join(Elems, ", ") ++ "]";
simplify_return({typevar, [{name, Name}], _}) ->
    Name;
simplify_return({atom, [{value, Val}], _}) ->
    Val;
simplify_return({abstype, _, [Type]}) ->
    {erlangName, [{name, Name}], []} = Type,
    Name ++ "()";
simplify_return({union, _, Types}) ->
    Elems = lists:map(fun(Type) -> simplify_return(Type) end, Types),
    string:join(Elems, " | ").

get_attribute(Elem, AttrName) ->
    [Attr] = xmerl_xpath:string("@" ++ AttrName, Elem),
    Attr#xmlAttribute.value.
