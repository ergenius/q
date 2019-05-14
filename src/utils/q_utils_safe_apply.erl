%%%-------------------------------------------------------------------
%%% @author madalin
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Jul 2017 8:49 AM
%%%-------------------------------------------------------------------
-module(q_utils_safe_apply).
-author("madalin").

-include_lib("q/include/q.hrl").

%% API
-export([apply/4]).

-spec apply(Module, Function, Args, Error) -> term() when
    Module      :: module(),
    Function    :: atom(),
    Args        :: [term()],
    Error       :: term().

apply(Module, Function, Params, Error) ->

    try
        erlang:apply(Module, Function, Params)
    catch
        _:_ -> Error
    end.