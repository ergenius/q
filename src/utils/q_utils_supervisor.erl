%%%-------------------------------------------------------------------
%%% @author madalin
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Jul 2017 5:04 PM
%%%-------------------------------------------------------------------
-module(q_utils_supervisor).
-author("madalin").

%% API
-export([on_upgrade/1]).

%% @doc Retrieve supervisor specifications and restart all children
on_upgrade(Module) ->

    %% Retrieve specifications again
    {ok, {_, Specs}} = Module:init([]),

    Old  = sets:from_list([Name || {Name, _, _, _} <- supervisor:which_children(Module)]),
    New  = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
    Kill = sets:subtract(Old, New),

    %% Terminate all children
    sets:fold(fun (Id, ok) ->
        supervisor:terminate_child(Module, Id),
        supervisor:delete_child(Module, Id),
        ok
              end, ok, Kill),

    %% Restart children
    [supervisor:start_child(Module, Spec) || Spec <- Specs],

    ok.
