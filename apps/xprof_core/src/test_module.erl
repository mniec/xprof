%%% @doc Module to generate sample data
%%% start tracing on `test_module:expensive_fun/1'
%%% @end
-module(test_module).
-export([start/0]).

start() ->
    loop().

loop() ->
    lists:foreach(fun(_) ->
                          SleepTime = 100 + round(math:pow(2,rand:uniform(6))),
                          spawn(fun() -> expensive_fun(SleepTime) end)
                  end, lists:seq(1,100)),
    timer:sleep(1000),
    loop().

expensive_fun(SleepTime) ->
    timer:sleep(SleepTime).
