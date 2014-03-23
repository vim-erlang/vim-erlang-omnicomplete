#!/usr/bin/env escript

-include_lib("xmerl/include/xmerl.hrl").

main([ModString, CurrFile]) ->

    Mod = list_to_atom(ModString),
    Dir = filename:dirname(CurrFile),
    AbsDir = filename:absname(Dir),

    case read_rebar_config(AbsDir) of
        {ok, {ConfigAbsDir, ConfigFileName, Terms}} ->
            log("rebar.config read: ~s~n", [ConfigFileName]),
            file:set_cwd(ConfigAbsDir),
            apply_rebar_settings(Terms);
        {error, not_found} ->
            log("rebar.config not found.~n");
        {error, {consult_error, ConfigFileName, Reason}} ->
            log_error("rebar.config (~s) consult unsuccessful: ~p~n",
                      [ConfigFileName, Reason])
    end,

    code:add_patha("ebin"),

    Edoc =
        try
            module_edoc(Mod)
        catch
            throw:not_found ->
                [];
            error:{badmatch, _} ->
                [];
            exit:error ->
                []
        end,

    Info =
        try
            module_info2(Mod)
        catch
            error:undef ->
                []
        end,

    FunSpecs = merge_functions(Edoc, Info),
    [print_function(Fun) || Fun <- FunSpecs ],

    ok;
main(_) ->
    io:format("Usage: ~s <module>~n", [escript:script_name()]),
    halt(1).

%%------------------------------------------------------------------------------
%% @doc Log the given entry if we are in verbose mode.
%% @end
%%------------------------------------------------------------------------------
-spec log(io:format()) -> ok.
log(Format) ->
    log(Format, []).

-spec log(io:format(), [term()]) -> ok.
log(Format, Data) ->
    case get(verbose) of
        true ->
            io:format(Format, Data);
        _ ->
            ok
    end.

%%------------------------------------------------------------------------------
%% @doc Log the given error.
%% @end
%%------------------------------------------------------------------------------
-spec log_error(io:format(), [term()]) -> ok.
log_error(Format, Data) ->
    io:format(standard_error, Format, Data).

%%------------------------------------------------------------------------------
%% @doc Find and read the rebar config appropriate for the given path.
%%
%% This function traverses the directory tree upward until it finds
%% "rebar.config". Afterwards it reads the terms in that file and returns them.
%% @end
%%------------------------------------------------------------------------------
-spec read_rebar_config(AbsDir :: string()) -> {ok, Result} |
                                               {error, Reason}
          when Result :: {ConfigAbsDir :: string(),
                          ConfigFileName :: string(),
                          ConfigTerms :: [term()]},
               Reason :: not_found |
                         {consult_error,
                          ConfigFileName :: string(),
                          ConsultError},
               ConsultError :: atom() |
                               {Line :: integer(),
                                Mod :: module(),
                                Term :: term()}.
read_rebar_config(AbsDir) ->
    ConfigFileName = filename:join(AbsDir, "rebar.config"),
    case filelib:is_file(ConfigFileName) of
        true ->
            case file:consult(ConfigFileName) of
                {ok, ConfigTerms} ->
                    {ok, {AbsDir, ConfigFileName, ConfigTerms}};
                {error, ConsultReason} ->
                    {error, {consult_error, ConfigFileName, ConsultReason}}
            end;
        false ->
            case AbsDir of
                "/" ->
                    {error, not_found};
                _ ->
                    read_rebar_config(filename:dirname(AbsDir))
            end
    end.

-spec apply_rebar_settings(ConfigTerms :: [term()]) -> ok.
apply_rebar_settings(Terms) ->
    % deps -> code path
    RebarDepsDir = proplists:get_value(deps_dir, Terms, "deps"),
    code:add_pathsa(filelib:wildcard(RebarDepsDir ++ "/*/ebin")),

    % sub_dirs -> code_path
    [ code:add_pathsa(filelib:wildcard(SubDir ++ "/ebin"))
      || SubDir <- proplists:get_value(sub_dirs, Terms, []) ],

    ok.

module_edoc(Mod) ->
    File = case filename:find_src(Mod) of
        {error, _} ->
            BeamFile = atom_to_list(Mod) ++ ".beam",
            case code:where_is_file(BeamFile) of
                non_existing ->
                    throw(not_found);
                BeamPath ->
                    SrcPath = beam_to_src_path(BeamPath),
                    case filelib:is_regular(SrcPath) of
                        true ->
                            SrcPath;
                        false ->
                            throw(not_found)
                    end
            end;
        {File0, _} ->
            File0 ++ ".erl"
    end,
    {_, Doc} = edoc:get_doc(File),
    Funs = xmerl_xpath:string("/module/functions/function", Doc),
    FunSpecs = map_functions(fun(Fun) -> analyze_function(Fun) end, Funs),
    lists:keysort(1, FunSpecs).

beam_to_src_path(BeamPath) ->
    PathParts = filename:split(BeamPath),
    {Dirs, [BeamFile]} = lists:split(length(PathParts) - 1, PathParts),
    {Dirs2, [DirsLast]} = lists:split(length(Dirs) - 1, Dirs),
    case filename:pathtype(BeamPath) of
        absolute ->
            Dirs3 = case DirsLast of
                "ebin" ->
                    Dirs2 ++ ["src"];
                _ ->
                    Dirs
            end;
        relative ->
            Dirs3 = Dirs
    end,
    filename:join(Dirs3 ++ [beam_to_src_file(BeamFile)]).

beam_to_src_file(BeamFile) ->
    [ModName, "beam"] = string:tokens(BeamFile, "."),
    ModName ++ ".erl".

map_functions(_, []) ->
    [];
map_functions(F, [H | T]) ->
    try
        [F(H) | map_functions(F, T)]
    catch
        throw:no_spec ->
            map_functions(F, T)
    end.

analyze_function(Fun) ->
    Name = list_to_atom(get_attribute(Fun, "name")),
    Args0 = xmerl_xpath:string("typespec/type/fun/argtypes/type", Fun),
    Args = lists:map(fun(Arg) -> get_attribute(Arg, "name") end, Args0),
    Return = analyze_return(Fun),
    {Name, Args, Return}.

analyze_return(Fun) ->
    case xmerl_xpath:string("typespec/type/fun/type/*", Fun) of
        [Return] ->
            simplify_return(xmerl_lib:simplify_element(Return));
        [] ->
            throw(no_spec)
    end.

simplify_return({typevar, [{name, Name}], _}) ->
    Name;
simplify_return({type, _, [Type]}) ->
    simplify_return(Type);
simplify_return({abstype, _, [Type]}) ->
    {erlangName, Attrs, _} = Type,
    Name = proplists:get_value(name, Attrs),
    Name ++ "()";
simplify_return({record, _, [Type]}) ->
    simplify_return(Type) ++ "()";
simplify_return({nonempty_list, _, [Type]}) ->
    "[" ++ simplify_return(Type) ++ "]";
simplify_return({tuple, _, Types}) ->
    Elems = lists:map(fun(Type) -> simplify_return(Type) end, Types),
    "{" ++ string:join(Elems, ", ") ++ "}";
simplify_return({list, _, Types}) ->
    Elems = lists:map(fun(Type) -> simplify_return(Type) end, Types),
    "[" ++ string:join(Elems, ", ") ++ "]";
simplify_return({paren, _, Types}) ->
    Elems = lists:map(fun(Type) -> simplify_return(Type) end, Types),
    "(" ++ string:join(Elems, ", ") ++ ")";
simplify_return({union, _, Types}) ->
    Elems = lists:map(fun(Type) -> simplify_return(Type) end, Types),
    string:join(Elems, " | ");
simplify_return({integer, [{value, Val}], _}) ->
    Val;
simplify_return({atom, [{value, Val}], _}) ->
    Val;
simplify_return({nil, _, _}) ->
    "[]".

get_attribute(Elem, AttrName) ->
    [Attr] = xmerl_xpath:string("@" ++ AttrName, Elem),
    Attr#xmlAttribute.value.

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
    [H1 = {K1, _, _} | T1] = Edoc,
    [H2 = {K2, _} | T2] = Info,
    if
        K1 == K2 ->
            merge_functions(T1, T2, [H1 | Funs]);
        K1 < K2 ->
            merge_functions(T1, Info, [H1 | Funs]);
        K1 > K2 ->
            merge_functions(Edoc, T2, [H2 | Funs])
    end.

print_function({Name, Arity}) ->
    io:format("~s/~B~n", [Name, Arity]);
print_function({Name, Args, Return}) ->
    io:format("~s(~s) -> ~s~n", [Name, string:join(Args, ", "), Return]).
