%%%-------------------------------------------------------------------
%%% @author madalin
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Jul 2017 7:49 AM
%%%-------------------------------------------------------------------
-module(qsup_boot).
-author("madalin").

-behaviour(supervisor).

%% supervisor exports
-export([start_link/0]).
-export([init/1]).
-export([upgrade/0]).

%% @spec start_link() -> startlink_ret()
%% @doc Creates a supervisor process as part of a supervision tree.
%%
%% The function ensures that the supervisor is linked to the calling process (its supervisor).
%% The created supervisor process calls Module:init/1 to find out about restart strategy, maximum restart intensity, and child processes.
%% To ensure a synchronized startup procedure, start_link/2,3 does not return until Module:init/1 has returned and all child processes have been started.
start_link() ->

    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec init([]) -> {ok,{SupFlags,[ChildSpec]}} | ignore
%% @doc Returns supervisor flags and child specifications.
%%
%% Whenever a supervisor is started using start_link/2,3, this function is called by the new process to find out about restart strategy,
%% maximum restart intensity, and child specifications.
%%
%% Notice that this function can also be called as a part of a code upgrade procedure.
init([]) ->

    %% Ensure all ETS tables defined in q.hrl are recreated
    q_utils_ets:recreate_all(),

    {ok,{{one_for_one, 10, 10}, [

        %% Starts local config server
        {qsrv_config, {qsrv_config, start_link, []}, permanent, 5000, worker, [qsrv_config]}

      ]}}.

%% @spec upgrade() -> ok
%% @doc Handle the upgrade process.
upgrade() -> q_utils_supervisor:on_upgrade(?MODULE).
