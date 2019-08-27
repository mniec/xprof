%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et

-module(xprof_core_tracer).

-behaviour(gen_server).

-export([start_link/0,
         trace/1,
         monitor/1,
         monitor_query/2,
         monitor_cmd/2,
         demonitor/1,
         all_monitored/0,
         trace_status/0]).

%% gen_server callbacks

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {trace_spec  = all,         %% trace specification
                status      = initialized, %% tracing status
                funs        = [],          %% functions monitored by xprof
                max_queue_len              %% maximum message queue length for overload protection
               }).

%% @doc Starts xprof tracer process.
-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Starts monitoring specified function calls.
-spec monitor(mfa()) -> ok | {error, already_traced | string()}.
monitor(Mfa) ->
    monitor_cmd(funlatency, [{mfa, Mfa}]).

%% @doc Start monitoring based on the specified query string with additional
%% parameters.
-spec monitor_query(binary(), [{binary(), binary()}]) -> ok | {error, Reason :: already_traced | string()}.
monitor_query(Query, AdditionalParams) ->
    case xprof_core_cmd:process_query(Query, AdditionalParams) of
        {error, _} = Error ->
            Error;
        StartCmd ->
            prepare_and_start_cmd(StartCmd)
    end.

%%% @doc Start monitoring based on the specified command and parameters.
-spec monitor_cmd(xprof_core:cmd(), xprof_core:params()) -> ok | {error, Reason :: already_traced | string()}.
monitor_cmd(Cmd, Params) ->
    case xprof_core_cmd:process_cmd(Cmd, Params) of
        {error, _} = Error ->
            Error;
        StartCmd ->
            prepare_and_start_cmd(StartCmd)
    end.

prepare_and_start_cmd(StartCmd) ->
    case xprof_core_cmd:prepare_start(StartCmd) of
        {error, _} = Error ->
            Error;
        ok ->
            gen_server:call(?MODULE, StartCmd)
    end.

%% @doc Stops monitoring specified function calls.
-spec demonitor(xprof_core:mfa_id()) -> ok.
demonitor({Mod, Fun, Arity} = MFA) ->
    lager:info("Stopping monitoring ~w:~w/~w",[Mod,Fun,Arity]),
    gen_server:call(?MODULE, {demonitor, MFA}).

%% @doc Returns list of monitored functions
-spec all_monitored() -> list({xprof_core:mfa_id(), binary()}).
all_monitored() ->
    gen_server:call(?MODULE, all_monitored).

%% @doc Turns on or resumes tracing for a process specified by pid, all
%% processes or processes that are spawned by specified spawner pid.
-spec trace(pid() | pause| resume | all | {spawner, pid()}) -> ok.
trace(PidOrSpec) ->
    lager:info("Tracing ~p", [PidOrSpec]),
    gen_server:call(?MODULE, {trace, PidOrSpec}).

%% @doc Returns current tracing state.
-spec trace_status() -> {all | {spawner, pid(), float()} | pid(),
                         Status :: paused | running | overflow | initialized}.
trace_status() ->
    gen_server:call(?MODULE, trace_status).


%% gen_server callbacks

init([]) ->
    init_tracer(),
    MaxQueueLen = application:get_env(xprof_core, max_tracer_queue_len, 1000),
    {ok, #state{max_queue_len = MaxQueueLen}}.

handle_call({start_cmd, Cmd, Options, CmdCB, Query}, _From, State) ->
    CmdId = CmdCB:get_cmd_id(Options),
    case get_pid(CmdId) of
        Pid when is_pid(Pid) ->
            {reply, {error, already_traced}, State};
        undefined ->
            case supervisor:start_child(xprof_core_trace_handler_sup,
                                        [Cmd, Options, CmdCB]) of
                {ok, Pid} ->
                    put_pid(CmdId, Pid),
                    %% funs stored in reversed order of start monitoring
                    NState = setup_trace_all_if_initialized(State),
                    {reply, ok, NState#state{funs = [{CmdId, Query}|State#state.funs]}};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end
    end;
handle_call({demonitor, MFA}, _From, State) ->
    xprof_core_trace_handler:trace_mfa_off(MFA),

    Pid = erase_pid(MFA),
    NewFuns = lists:filter(fun({E, _}) -> E =/= MFA end, State#state.funs),

    supervisor:terminate_child(xprof_core_trace_handler_sup, Pid),
    {reply, ok, State#state{funs=NewFuns}};
handle_call(all_monitored, _From, State = #state{funs=MFAs}) ->
    {reply, MFAs, State};
handle_call({trace, PidSpec}, _From, State) ->
    NewState = setup_trace(PidSpec, State),
    {reply, ok, NewState};
handle_call(trace_status, _From, State = #state{trace_spec=TraceSpec,
                                                status=Status}) ->
    {reply, {TraceSpec, Status}, State};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(Msg = {trace_ts, _TracedPid, Tag, MFA, _TraceTerm, _StartTime}, State)
  when Tag =:= call;
       Tag =:= return_from;
       Tag =:= exception_from ->
    NewState = check_for_overflow(State),
    send2pids(MFA, Msg),
    {noreply, NewState};
handle_info({trace_ts, _Spawner, spawn, NewProc, _MFArgs,_TimeStamp},
            State = #state{trace_spec=TraceSpec}) ->
    NewState = check_for_overflow(State),

    %% trace spec could have been changed while there were late messages
    %% in the queue
    Sample = case TraceSpec of
                {spawner, _Pid, X} -> X;
                _ -> 0.0
            end,

    case rand:uniform() < Sample of
        true ->
            catch erlang:trace(NewProc, true, [call, procs, timestamp]);
        false ->
            ok
    end,
    {noreply, NewState};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions

init_tracer() ->
    erlang:trace_pattern({'_','_','_'}, false, [local]).

check_for_overflow(State = #state{status = running,
                                  trace_spec = TraceSpec,
                                  max_queue_len = MaxQueueLen}) ->
    {_, QLen} = erlang:process_info(self(), message_queue_len),
    case QLen >= MaxQueueLen of
        true ->
            set_trace_opts(false, TraceSpec),
            State#state{status=overflow};
        false ->
            State
    end;
check_for_overflow(State) ->
    State.

setup_trace_all_if_initialized(#state{status = initialized} = State) ->
    setup_trace(all, State);
setup_trace_all_if_initialized(State) ->
    State.

setup_trace(pause, State) ->
    set_trace_opts(false, State#state.trace_spec),
    State#state{status=paused};
setup_trace(resume, State = #state{trace_spec=Spec}) ->
    setup_trace(Spec, State#state{trace_spec=undefined});
setup_trace(Spec, State = #state{trace_spec=undefined}) ->
    set_trace_opts(true, Spec),
    State#state{trace_spec=Spec, status=running};
setup_trace(Spec, State) ->
    set_trace_opts(false, State#state.trace_spec),
    setup_trace(Spec, State#state{trace_spec=undefined}).

set_trace_opts(How, {spawner, SpwPid, _Sampl}) ->
    trace(SpwPid, How, [procs, timestamp]);
set_trace_opts(How, all) ->
    trace(all, How, [call, arity, timestamp]);
set_trace_opts(How, Pid) when is_pid(Pid) ->
    trace(Pid, How, [call, arity, timestamp]);
set_trace_opts(_How, undefined) ->
    true.

trace(PidSpec, How, Flags) ->
    try
        erlang:trace(PidSpec, How, Flags)
    catch
        error:badarg ->
            case is_pid(PidSpec) andalso not is_process_alive(PidSpec) of
                true ->
                    0;
                _ ->
                    error(badarg, [PidSpec, How, Flags])
            end
    end.

-spec send2pids(mfa(), term()) -> any().
send2pids({M, F, _} = MFA, Msg) ->
    send2pid(MFA, Msg),
    send2pid({M, F, '_'}, Msg),
    ok.

-spec send2pid(xprof_core:mfa_id(), term()) -> any().
send2pid(MFA, Msg) ->
    case get_pid(MFA) of
        undefined -> ok;
        Pid -> erlang:send(Pid, Msg)
    end.

-spec get_pid(xprof_core:mfa_id()) -> pid() | undefined.
get_pid(MFA) ->
    get({handler, MFA}).

-spec put_pid(xprof_core:mfa_id(), pid()) -> any().
put_pid(MFA, Pid) ->
    put({handler, MFA}, Pid).

-spec erase_pid(xprof_core:mfa_id()) -> pid() | undefined.
erase_pid(MFA) ->
    erase({handler, MFA}).
