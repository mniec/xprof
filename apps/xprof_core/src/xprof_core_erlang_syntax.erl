%%%
%%% @doc Module to parse and format expressions in Erlang syntax
%%%
-module(xprof_core_erlang_syntax).

-behaviour(xprof_core_language).

-export([parse_query/1,
         parse_incomplete_query/1,
         parse_match_spec/1,
         hidden_function/1,
         fmt_mfa/3,
         fmt_mod_and_delim/1,
         fmt_mod/1,
         fmt_fun_and_arity/2,
         fmt_fun/1,
         fmt_cmd/1,
         fmt_param/1,
         fmt_param_and_delim/1,
         fmt_exception/2,
         fmt_term/1]).

%% exported to fool dialyzer about breaking an opaque type
-export([id/1]).

%% in OTP 21 `format_exception/7' was moved from `lib' module to `erl_error'
-ifdef(OTP_RELEASE).
-if(?OTP_RELEASE >= 21).
%% in OTP 21 or hifher
-define(ERL_ERROR_MOD, erl_error).
-endif.
-else.
%% in OTP 20 or lower
-define(ERL_ERROR_MOD, lib).
-endif.

%% @doc Parse a query string that represents either an xprof-flavoured
%% match-spec fun or an extended xprof query in Erlang syntax.
%%
%% The `mfa' key has special handling. All param values are returned as AST (to
%% support non-term expressions like patterns), except mfa value, which is
%% returned as string, so that the old parser (`parse_match_spec') can do its
%% job on it.
%%
%% Notes:
%% - the query should be fully tokenizable
%% - all param values should be fully parsable except the value of mfa key
%% which may be parsable if it is in the module-function-arity form,
%% but not in case of an xprof-flavoured match-spec fun
%% - to address this it is mandatory to have the mfa key as the last one, if
%% present (so that we can cut the rest of the query string from after `mfa ='
%% and process it separately)
-spec parse_query(string()) -> {ok, xprof_core:cmd(),
                                [{mfa, string()} |
                                 {atom(), erl_parse:abstract_expr()}]}
                                   | {error, Reason :: any()}.
parse_query("#" ++ Query) ->
    %% extended query
    try
       {ok, Tokens} = tokens_query(Query, error),
       case parse_query_tokens(Tokens, cmd, undefined, [], _Rest = false) of
           {ok, Cmd, Params} ->
               {ok, Cmd, mfa_to_str(Params, Query)};
           {more, What, _Cmd, _Params} ->
               case What of
                   cmd ->
                       xprof_core_lib:fmt_err("Missing command name", []);
                   key ->
                       xprof_core_lib:fmt_err("Expected parameter name missing at the end of the query", []);
                   {eq, Key} ->
                       xprof_core_lib:fmt_err("Missing = and value for parameter ~w", [Key]);
                   {value, Key} ->
                       xprof_core_lib:fmt_err("Missing value for parameter ~w", [Key]);
                   {value, Key, _Value} ->
                       xprof_core_lib:fmt_err("Incomplete value for parameter ~w", [Key])
               end;
           {unexpected, Token, _State} ->
               xprof_core_lib:fmt_err("unexpected token \"~s\" at column ~p", [text(Token), column(Token)])
       end
    catch
        throw:Error ->
            Error
    end;
parse_query(Query) when is_list(Query) ->
    {ok, funlatency, [{mfa, Query}]}.

-spec parse_incomplete_query(string()) ->
    {ok, Cmd, Params}
  | {incomplete_cmd, CmdPrefix}
  | {incomplete_key, KeyPrefix, Cmd, ParamsSoFar}
  | {incomplete_value, Key, ValuePrefix, Cmd, ParamsSoFar}
  | {error, Reason :: any()}
  when
      Cmd :: xprof_core:cmd(),
      CmdPrefix :: string(),
      Params :: xprof_core:params(),
      ParamsSoFar :: xprof_core:params(),
      Key :: atom(),
      KeyPrefix :: string(),
      ValuePrefix :: string().
%% throw:{error, Reason :: term()}
parse_incomplete_query(Query) ->
    {ok, Tokens, Rest} = tokens_query(Query, incomplete),
    case parse_query_tokens(Tokens, cmd, undefined, [], Rest =/= []) of
        {ok, Cmd, Params} ->
            {ok, Cmd, Params};
        {more, cmd, _, _} ->
            {incomplete_cmd, Rest};
        {more, key, Cmd, Params} ->
            {incomplete_key, Rest, Cmd, Params};
        {more, {eq, Key}, Cmd, Params} ->
            {incomplete_key, {Key, Rest}, Cmd, Params};
        {more, {value, Key, ValueTokens}, Cmd, Params} ->
            ValueRest = rest_from_tokens(ValueTokens, Query),
            {incomplete_value, Key, ValueRest, Cmd, Params};
        {more, {value, Key}, Cmd, Params} ->
            {incomplete_value, Key, Rest, Cmd, Params};
        {unexpected, Token, _State} ->
            xprof_core_lib:fmt_err("unexpected token \"~s\" at column ~p",
                                   [text(Token), column(Token)])
    end.

tokens_query(Str, error) ->
    case erl_scan:string(Str, {1, 2}, [text]) of
        {error, {_Loc, Mod, Err}, Loc} ->
            xprof_core_lib:err(Loc, Mod, Err);
        {ok, Tokens, _EndLoc} ->
            {ok, Tokens}
    end;
tokens_query(Str, incomplete) ->
    case tokens_incomplete(Str) of
        {done, {ok, _Tokens, _EndLoc}, _Rest} ->
            %% unexpected - Str contains a dot in the middle
            %% FIXME extract location - {dot, Loc} = lists:last(Tokens)
            throw({error, unexpected_dot});
        {done, {error, {Loc, Mod, Err}, _EndLoc}, _Rest} ->
            %% FIXME throw or return error
            xprof_core_lib:err(Loc, Mod, Err);
        {more, {erl_scan_continuation, Cs, _Col,
                RevTokens,
                _Line, _St, Any, _Fun}} when is_list(RevTokens) ->
            Rest = case {Cs, Any} of
                       {[], [_|_]} ->
                           %% unterminated atom
                           lists:reverse(Any);
                       {".", [_|_]} ->
                           %% unterminated float
                           lists:reverse(Any, ".");
                       {[],{RevCs, _, _StartLine, _StartCol}} ->
                           %% scan_string/scan_qatom
                           %% FIXME: add case for "... 'asd" -> Any = {"dsa","dsa",1,11}
                           %% idea: Str+eof => {done,{error,{_Loc,erl_scan,{string,$',"asd"}},_EndLoc}, eof}
                           lists:reverse(RevCs);
                       {[], {BaseInt, NumRev, _TextBase}} when is_integer(BaseInt), is_list(NumRev) ->
                           %% scan_based_int
                           integer_to_list(BaseInt) ++ "#" ++ lists:reverse(NumRev);
                       {[], Int} when is_integer(Int) ->
                           %% scan whitespace
                           "";
                       {_, Any} when is_list(Cs), (Any =:= [] orelse not is_list(Any)) ->
                           Cs
                   end,
            {ok, lists:reverse(RevTokens), Rest}
    end.

%% Continuation is an opaque type which should not be disassembled. In order to
%% silence dialyzer we run the result through the exported id/1 function.
-spec tokens_incomplete(string()) -> Return when
      Return :: {'done', Result :: erl_scan:tokens_result(), Rest :: string()}
              | {'more', Continuation :: tuple()}.
tokens_incomplete(Str) ->
    id(erl_scan:tokens([], Str, {1, 2}, [text])).

%% @hidden
-spec id(any()) -> any().
id(Any) -> Any.

parse_query_tokens([{atom, _, Cmd}|T], cmd, _C, _P, MoreStr) ->
    parse_query_tokens(T, key, Cmd, [], MoreStr);
parse_query_tokens([{atom, _, Key}|T], key, Cmd, Params, MoreStr) ->
    parse_query_tokens(T, {eq, Key}, Cmd, Params, MoreStr);
parse_query_tokens([{'=', _}|T], {eq, Key}, Cmd, Params, MoreStr) ->
    parse_query_tokens(T, {value, Key}, Cmd, Params, MoreStr);
parse_query_tokens([_|_] = T, {value, Key}, Cmd, Params, MoreStr) ->
    case parse_value(T, []) of
        {ok, _ValueAst, []} when MoreStr =:= true ->
            %% There is non-empty trailing string that cannot be scanned yet.
            %% If it would start with a comma that would be scannable,
            %% so it does not start with a comma.
            %% Hence we need to assume that it still belongs to the current value
            {more, {value, Key, T}, Cmd, Params};
        {ok, _ValueAst, TRest} when Key =:= mfa ->
            %% spec handling of `mfa' - store the tokens instead of the AST
            ValueToken = lists:sublist(T, 1, length(T) - length(TRest)),
            parse_query_tokens(TRest, comma, Cmd, [{Key, ValueToken}|Params], MoreStr);
        {ok, [ValueAst], TRest} ->
            parse_query_tokens(TRest, comma, Cmd, [{Key, ValueAst}|Params], MoreStr);
        {ok, ValueListAst, TRest} ->
            %% this should never happen
            %% the value cannot be more than one expression
            erlang:error({value_is_expr_list, ValueListAst, TRest});
        _Error when Key =:= mfa, MoreStr =:= false ->
            {ok, Cmd, lists:reverse(Params, [{mfa, T}])};
        _Error ->
            {more, {value, Key, T}, Cmd, Params}
    end;
parse_query_tokens([{',', _}|T], comma, Cmd, Params, MoreStr) ->
    parse_query_tokens(T, key, Cmd, Params, MoreStr);
parse_query_tokens([], comma, Cmd, Params, _MoreStr = false) ->
    {ok, Cmd, lists:reverse(Params)};
parse_query_tokens([], _State = key, Cmd, _Params = [], _MoreStr = false) ->
    %% single command without parameters
    {ok, Cmd, []};
parse_query_tokens([], State, Cmd, Params, _MoreStr) ->
    {more, State, Cmd, Params};
parse_query_tokens([H|_], State, _, _, _) ->
    {unexpected, H, State}.


parse_value([_|_] = T, Head) ->
    {TH, TT} = tokens_to_comma(T),
    case {erl_parse:parse_exprs(Head ++ TH ++ [{dot,{2,1}}]), TT} of
        {{error, _} = Error, []} ->
            Error;
        {{error, _} = Error, [{',', _}]} ->
            Error;
        {{error, _}, [{',', _} = Comma|TTT]} ->
            parse_value(TTT, Head ++ TH ++ [Comma]);
        {{ok, AST}, _} ->
            {ok, AST, TT}
    end.

tokens_to_comma(Tokens) ->
    lists:splitwith(
      fun(Token) -> element(1, Token) =/= ',' end,
      Tokens).

mfa_to_str([{mfa, Tokens}|Params], OrigQuery) ->
    MFAQuery = rest_from_tokens(Tokens, OrigQuery),
    [{mfa, MFAQuery}|Params];
mfa_to_str([KeyValue|Params], OrigQuery) ->
    [KeyValue|mfa_to_str(Params, OrigQuery)];
mfa_to_str([], _) ->
    [].

rest_from_tokens([FirstToken|_], OrigQuery) ->
    %% OrigQuery does not contain leading `#'
    %% so it starts at column 2
    StartColumn = column(FirstToken) - 1,
    _RestStr = lists:sublist(OrigQuery, StartColumn, length(OrigQuery)).

text(Token) ->
    erl_scan:text(Token).

column(Token) ->
    erl_scan:column(Token).

%% @doc Parse a query string that represents either a module-function-arity
%% or an xprof-flavoured match-spec fun in Erlang syntax.
%% In the later case the last element of the tuple is the abstract syntax tree
%% of the clauses of the anonimous function.
parse_match_spec(Str) ->
    case tokens_ms(Str) of
        {mfa, _} = MFA ->
            MFA;
        {clauses, M, F, Tokens} ->
            Clauses = parse_ms(Tokens),
            {clauses, M, F, Clauses}
    end.

tokens_ms(Str) ->
    case erl_scan:string(Str, {1,1}) of
        {error, {_Loc, Mod, Err}, Loc} ->
            xprof_core_lib:err(Loc, Mod, Err);
        {ok, [{atom, _, M}, {':', _},
              {atom, _, F}, {'/', _},
              {integer, _, A}], _EndLoc} ->
            {mfa, {M, F, A}};
        {ok, [{atom, _, M}, {':', _},
              {atom, _, F}|Tokens], _EndLoc} when Tokens =/= [] ->
            {clauses, M, F, [{'fun', 0}|ensure_end(ensure_body(Tokens))]};
        {ok, Tokens, _EndLoc} ->
            xprof_core_lib:err("expression is not an xprof match-spec fun ~w", [Tokens])
    end.

%% @doc Ensure the fun has at least a trivial function body "-> true".
%% Omitting body is only allowed if there is only a single clause.
ensure_body(Tokens) ->
    case lists:keymember('->', 1, Tokens) of
        true ->
            Tokens;
        false ->
            Loc = get_loc(lists:last(Tokens)),
            Tokens ++ [{'->', Loc}, {atom, Loc, true}]
    end.

get_loc({_, Loc}) -> Loc;
get_loc({_, Loc, _}) -> Loc.

%% @doc Ensure the fun is properly closed with "end."
ensure_end(Tokens) ->
    case lists:reverse(Tokens) of
        [{dot, _}, {'end', _}| _] -> Tokens;
        [{dot, Loc}|T] -> lists:reverse(T, [{'end', Loc}, {dot, Loc}]);
        [Last|_] = R ->
            Loc = element(2, Last),
            lists:reverse(R, [{'end', Loc}, {dot, Loc}])
    end.

parse_ms(Tokens) ->
    case erl_parse:parse_exprs(Tokens) of
        {error, {Loc, Mod, Err}} ->
            xprof_core_lib:err(Loc, Mod, Err);
        {ok, [{'fun', _Loc, {clauses, Clauses}}]} ->
            Clauses;
        {ok, _} ->
            xprof_core_lib:err("expression is not an xprof match-spec fun")
    end.

%%
%% Functions for autocomplete
%%

hidden_function(behaviour_info) -> true;
hidden_function(module_info) -> true;
hidden_function(Fun) ->
    case atom_to_list(Fun) of
        "-" ++ _ ->
            %% filter out local functions generated for fun objects
            %% and list comprehensions like '-filter_funs/2-fun-0-'
            true;
        _ -> false
    end.

fmt_mfa(Mod, Fun, Arity) ->
    fmt("~w:~w/~b", [Mod, Fun, Arity]).

fmt_mod(Mod) ->
    fmt("~w", [Mod]).

fmt_mod_and_delim(Mod) ->
    fmt("~w:", [Mod]).

fmt_fun(Fun) ->
    fmt("~w", [Fun]).

fmt_fun_and_arity(Fun, Arity) ->
    fmt("~w/~b", [Fun, Arity]).

fmt_cmd(Cmd) ->
    fmt("~w", [Cmd]).

fmt_param(Param) ->
    fmt("~w", [Param]).

fmt_param_and_delim(Param) ->
    fmt("~w = ", [Param]).

fmt_exception(Class, Reason) ->
    Stacktrace = [],
    SkipFun = fun(_M, _F, _A) -> false end,
    PrettyFun = fun(Term, _Indent) -> do_fmt_term(Term) end,
    Encoding = unicode,
    unicode:characters_to_binary(
      ["** "|?ERL_ERROR_MOD:format_exception(1, Class, Reason, Stacktrace,
                                             SkipFun, PrettyFun, Encoding)]).

fmt_term(Term) ->
    unicode:characters_to_binary(do_fmt_term(Term)).

%% @doc Format a term with record pretty printing and line-wrapping hints

%% Use line-length=1, so that the term will be broken in new lines in every
%% place the formatter thinks it can be. (The formatter will add new-line plus
%% indentation spaces.) This would look ugly in a terminal but browsers usually
%% collapse all whitespaces into a single space. The `replace_whitespace/1'
%% wrapper does part of the browser's job, in order to not break the term where
%% is shouldn't be.
do_fmt_term(Term) ->
    replace_whitespace(
      io_lib_pretty:print(
        Term, [{record_print_fun, xprof_core_records:record_print_fun()},
               {line_length, 1}])).

fmt(Fmt, Args) ->
    unicode:characters_to_binary(io_lib:format(Fmt, Args)).

%% @doc Replace whitespace with browser friendly variants

%% Replace a new line and indentation with a single space, and spaces (not after
%% new line) with a non-breaking space. This way the browser can display as much
%% of the formatted term in one line as much fits and break the line at any
%% point where the erlang formatter would also break. (The only places where a
%% non-breaking space is inserted is after record and map keys.)
replace_whitespace(IoList) ->
    replace_ws(lists:flatten(IoList)).

replace_ws([$\n, $\s|T]) ->
    replace_ws([$\n|T]);
replace_ws([$\n|T]) ->
    [$\s|replace_ws(T)];
replace_ws([$\s|[Next|_] = T]) when Next =:= $\s; Next =:= $\n ->
    %% Remove duplicate spaces or space before new-line.
    %% This can only happen in old erlang versions where there was always a
    %% space after = in a record, even if it was followed by a new line. This
    %% was fixed in OTP 19.3
    replace_ws(T);
replace_ws([$\s|T]) ->
    [160|replace_ws(T)];
replace_ws([H|T]) ->
    [H|replace_ws(T)];
replace_ws([]) ->
    [].
