%%
 % Copyright (c) 2017. Madalin Grigore-Enescu
 %
-module(q_utils_benchmark).
-author("madalin").

-include_lib("q/include/q.hrl").

%% API
-export([function/4]).

%% @doc Benchmark the result of applying Function in Module to Args.
%% The applied function must be exported from Module.
function(Module, Function, Params, HowManyTimes) ->

  io:format("~nBENCKMARK: Calling ~p:~p/~p function ~p times.", [Module, Function, erlang:length(Params), HowManyTimes]),

  StartTime = erlang:system_time(microsecond),
  apply_function(Module, Function, Params, HowManyTimes),
  EndTime   = erlang:system_time(microsecond),

  %% Time spend
  TimeSpend = EndTime-StartTime,

  %% Display benchmark result
  io:format("~nProcess took ~f seconds", [TimeSpend/1000000]),

  %% Returns time spend
  TimeSpend.

apply_function(_Module, _Function, _Params, 0) -> ok;
apply_function(Module, Function, Params, HowManytimes) ->
  erlang:apply(Module, Function, Params),
  apply_function(Module, Function, Params, HowManytimes-1).
