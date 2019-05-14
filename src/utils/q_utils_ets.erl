%%%-------------------------------------------------------------------
%%% @author madalin
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Jul 2017 7:54 AM
%%%-------------------------------------------------------------------
-module(q_utils_ets).
-author("madalin").

-include_lib("q/include/q.hrl").

%% API
-export([recreate_all/0, recreate_all/1]).

%% @doc Recreate all ETS tables defined in q.hrl
%% If a table already exists it is deleted and created again. If table does not exist it is created.

-spec recreate_all() -> ok.

recreate_all() -> recreate_all(?Q_ETS_LIST).

%% @doc Recreate a list of ETS tables
%% If a table already exists it is deleted and created again. If table does not exist it is created.

-spec recreate_all(List) -> ok when
    List :: [{atom(), list()}].

recreate_all([{Name, Options}|T]) ->

    %% Delete any existing ETS first
    Info = ets:info(Name),
    case Info of
        undefined -> ok;
        _ -> ets:delete(Name)
    end,

    %% Create the ETS
    ets:new(Name, Options),
    recreate_all(T);

recreate_all([]) -> ok.
