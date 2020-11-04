-module(xprof_core_vm_info).

-include_lib("compiler/src/beam_disasm.hrl").

-export([
    get_available_funs/1,
    get_called_funs/1,
    ensure_mfa/1
]).

%% @doc Return list of existing module/funcion/arity that match query.
%%
%% If query does not contain colon only the global functions
%% (excluding module_info) of all the matching modules are listed.
%% If query contains colon both the global and local functions
%% (excluding generated local functions) are listed of that particular module
%% matching the query.

-spec get_available_funs(binary()) -> [MFA :: binary()].
get_available_funs(Query) ->
    ModeCB = xprof_core_lib:get_mode_cb(),

    AllMods = get_modules(),

    %% find the module which is fully writen out in the query
    %% and return all its functions
    ExactMatch = find_mods(Query, AllMods, ModeCB),
    ExactMods = [Mod||{Mod, _} <- ExactMatch],
    AllFuns =
        lists:flatmap(
          fun({Mod, FunPrefix}) ->
                  Funs = get_all_functions(Mod, ModeCB),
                  [ModeCB:fmt_mfa(Mod, Fun, Arity)
                   || {Fun, Arity} <- filter_funs(FunPrefix, Funs, ModeCB)]
          end, ExactMatch),

    %% find modules which query is a partial prefix of
    MatchingMods = filter_mods(Query, AllMods, ModeCB),
    IncompleteMods =
        [ModeCB:fmt_mod_and_delim(Mod)
         || Mod <- MatchingMods -- ExactMods],

    AllFuns ++ IncompleteMods.


find_mods(Query, AllMods, ModeCB) ->
    lists:filtermap(
      fun(Mod) ->
              ModBin = ModeCB:fmt_mod_and_delim(Mod),
              case xprof_core_lib:prefix_rest(ModBin, Query) of
                  false -> false;
                  Rest -> {true, {Mod, Rest}}
              end
      end, AllMods).

filter_mods(Prefix, Mods, ModeCB) ->
    lists:filter(fun(Mod) ->
                         xprof_core_lib:prefix(Prefix, ModeCB:fmt_mod(Mod))
                 end, Mods).

filter_funs(Prefix, Funs, ModeCB) ->
    lists:filter(fun({Fun, Arity}) ->
                         is_fun_arity(Prefix, Fun, Arity, ModeCB)
                             orelse is_ms_fun(Prefix, Fun, ModeCB)
                 end, Funs).

is_fun_arity(Prefix, Fun, Arity, ModeCB) ->
    FunArityBin = ModeCB:fmt_fun_and_arity(Fun, Arity),
    xprof_core_lib:prefix(Prefix, FunArityBin).

%% @doc Check if Prefix string can be a match-spec fun declaration.
%% The heuristic is that the function name must be followed by an open
%% parenthesis or space character.
%% (Prefix is definitely longer than FunBin otherwise it would have been a
%%  prefix of FunArityBin already)
is_ms_fun(Prefix, Fun, ModeCB) ->
    FunBin = ModeCB:fmt_fun(Fun),
    case xprof_core_lib:prefix_rest(FunBin, Prefix) of
        <<"(", _/binary>> -> true;
        <<" ", _/binary>> -> true;
        _ -> false
    end.

-spec get_all_functions(module(), module()) -> [{atom(), arity()}].
get_all_functions(Mod, ModeCB) ->
    AllFuns =
        case erlang:module_loaded(Mod) of
            true ->
                get_functions_loaded(Mod);
            false ->
                get_functions_not_loaded(Mod)
        end,
    [FA || FA = {F, _} <- AllFuns,
           not ModeCB:hidden_function(F)].

get_functions_loaded(Mod) ->
    Exports = lists:sort(Mod:module_info(exports)),
    AllFuns = lists:sort(Mod:module_info(functions)),
    Locals = ordsets:subtract(AllFuns, Exports),
    Exports ++ Locals.

get_functions_not_loaded(Mod) ->
    case beam_lib:chunks(code:which(Mod), [exports, locals]) of
        {ok, {Mod, [{exports, Exports},
                    {locals, Locals}]}} ->
            lists:sort(Exports) ++ lists:sort(Locals);
        {error, beam_lib, {missing_chunk, _, "LocT"}} ->
            %% Module might be stripped and locals chunk missing
            %% but there is hope exports are still there
            case beam_lib:chunks(code:which(Mod), [exports]) of
                {ok, {Mod, [{exports, Exports}]}} ->
                    lists:sort(Exports);
                _ ->
                    []
            end;
        _ ->
            []
    end.

get_modules() ->
    ModsFiles = lists:sort(all_available()),
    [ list_to_atom(Mod) || {Mod, _File, _IsLoaded} <- ModsFiles].

%% @doc Return list of called functions for given mfa tuple.
-spec get_called_funs(mfa()) -> [mfa()].
get_called_funs({Mod, Fun, Arity}) ->
    try
        %% Get file for given module and disassemble it
        File = case code:which(Mod) of
                   File0 when is_list(File0), File0 =/= "" ->
                       File0;
                   _ ->
                       %% File might be cover_compiled or preloaded
                       %% (inspired by c:find_beam/1)
                       BaseName = atom_to_list(Mod) ++ code:objfile_extension(),
                       case code:where_is_file(BaseName) of
                           File0 when is_list(File0) ->
                               File0;
                           Error ->
                               throw(Error)
                       end
               end,
        #beam_file{module = Mod, code = Disasm} = beam_disasm:file(File),

        %% extract beamasm operations for given function
        Operations = lists:flatten(
            lists:filtermap(
                fun(Entry) ->
                    case Entry of
                        #function{name = Fun, arity = Arity, code = Opcodes} ->
                            {true, Opcodes};
                        _ ->
                            false
                    end
                end,
                Disasm
            )
        ),

        %% extract function calls from beamasm
        Calls = lists:filtermap(
            fun(Opcode) ->
                case Opcode of
                    {call, _, {M, F, A}} ->
                        {true, {M, F, A}};
                    {call_only, _, {M, F, A}} ->
                        {true, {M, F, A}};
                    {call_last, _, {M, F, A}, _} ->
                        {true, {M, F, A}};
                    {call_ext, _, {extfunc, M, F, A}} ->
                        {true, {M, F, A}};
                    {call_ext_only, _, {extfunc, M, F, A}} ->
                            {true, {M, F, A}};
                    {call_ext_last, _, {extfunc, M, F, A}, _} ->
                        {true, {M, F, A}};
                    _ ->
                        false
                end
            end,
            Operations
        ),

        %% return filtered and sorted list of calls
        ModeCB = xprof_core_lib:get_mode_cb(),
        FilterList = [{Mod, Fun, Arity},
                      {erlang, nif_error, 1}],

        lists:usort([Call || Call = {_, F, _} <- Calls,
                     not ModeCB:hidden_function(F),
                     not lists:member(Call, FilterList)
                    ])
    catch
        _:_ ->
            %% TODO: proper error handling
            []
    end.

-spec ensure_mfa(mfa()) -> ok | {error, string()}.
ensure_mfa({Mod, Fun, Arity}) ->
    case code:ensure_loaded(Mod) of
        {module, Mod} ->
            %% `functions' may not include certain BIFs for example
            %% on OTP 21 `os:timestamp/0' was missing and only
            %% included in `exports' (fixed by OTP 23)
            FA = {Fun, Arity},
            case lists:member(FA, Mod:module_info(functions)) orelse
                 lists:member(FA, Mod:module_info(exports)) of
                true ->
                    ok;
                false ->
                    ModeCB = xprof_core_lib:get_mode_cb(),
                    xprof_core_lib:fmt_err("Undefined function ~s",
                                           [ModeCB:fmt_mfa(Mod, Fun, Arity)])
            end;
        {error, _} ->
            ModeCB = xprof_core_lib:get_mode_cb(),
            xprof_core_lib:fmt_err("Undefined module ~s", [ModeCB:fmt_mod(Mod)])
    end.

-ifdef(OTP_RELEASE).
%% OTP 21 or newer supports -if directive
-if(?OTP_RELEASE >= 23).
-define(HAS_ALL_AVAILABLE, true).
-endif.
-endif.

-ifdef(HAS_ALL_AVAILABLE).
all_available() ->
    code:all_available().

-else.

%% copy of code:all_available from OTP 23
-spec all_available() -> [{Module, Filename, Loaded}] when
      Module :: string(),
      Filename :: file:filename() | atom(),
      Loaded :: boolean().
all_available() ->
    case code:get_mode() of
        interactive ->
            all_available(code:get_path(), maps:new());
        embedded ->
            all_available([], maps:new())
    end.
all_available([Path|Tail], Acc) ->
    case erl_prim_loader:list_dir(Path) of
        {ok, Files} ->
            all_available(Tail, all_available(Path, Files, Acc));
        _Error ->
            all_available(Tail, Acc)
    end;
all_available([], AllModules) ->
    AllLoaded = [{atom_to_list(M),Path,true} || {M,Path} <- code:all_loaded()],
    AllAvailable =
        maps:fold(
          fun(File, Path, Acc) ->
                  [{filename:rootname(File), filename:append(Path, File), false} | Acc]
          end, [], AllModules),
    OrderFun = fun F({A,_,_},{B,_,_}) ->
                       F(A,B);
                   F(A,B) ->
                       A =< B
               end,
    lists:umerge(OrderFun, lists:sort(OrderFun, AllLoaded), lists:sort(OrderFun, AllAvailable)).

all_available(Path, [File | T], Acc) ->
    case filename:extension(File) of
        ".beam" ->
            case maps:is_key(File, Acc) of
                false ->
                    all_available(Path, T, Acc#{ File => Path });
                true ->
                    all_available(Path, T, Acc)
            end;
        _Else ->
                    all_available(Path, T, Acc)
    end;
all_available(_Path, [], Acc) ->
    Acc.

-endif.
