%%%-------------------------------------------------------------------
%%% @author madalin
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Jul 2017 5:24 PM
%%%-------------------------------------------------------------------
-module(q_config).
-author("madalin").

-include_lib("q/include/q.hrl").

%% API
-export([get_option/1, get_option/2]).
-export([get_node_option/1, get_node_option/2, get_node_option/3]).
-export([get_cluster_option/1, get_cluster_option/2, get_cluster_option/3]).
-export([get_universe_option/1, get_universe_option/2]).

-export([set_node_option/2]).
-export([set_cluster_option/2]).
-export([set_universe_option/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% get
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Return the specified configuration option value or raise exception if the specified configuration option is not available.
%% The option is returned from current node options, current cluster options or universe options. Current node options take precedence
%% over current cluster options. Current cluster options take precedence over universe options.
%% @throws #q_exception{name = config_option_not_found, params = [{name, Name::binary()}]}
get_option(Name) ->

    case ets:lookup(?Q_ETS_CONFIG_OPTIONS, Name) of
        [Value] -> Value;
        _ -> ?Q_THROW(q, config_option_not_found, [{name, Name}])
    end.

%% @doc Return the specified config option value if it's available, otherwise returns Default parameter.
%% The option is returned from current node options, current cluster options or universe options. Current node options take precedence
%% over current cluster options. Current cluster options take precedence over universe options.
get_option(Name, Default) ->

    case ets:lookup(?Q_ETS_CONFIG_OPTIONS, Name) of
        [Value] -> Value;
        _ -> Default
    end.

%% @doc Return the specified configuration option value for the current node
%% or raise exception if the specified configuration option is not available.
%% @throws #q_exception{name = config_node_option_not_found, params = [{name, Name::binary()}]}
get_node_option(Name) -> get_node_option(Name, q_universe:get_node()).

%% @doc Return the specified configuration option value for the specified node
%% or raise exception if the specified configuration option is not available.
%% @throws #q_exception{name = config_node_option_not_found, params = [{name, Name::binary()}]}
get_node_option(Name, Node) ->

    case ets:lookup(?Q_ETS_CONFIG_OPTIONS_NODES, {Name, Node}) of
        [Value] -> Value;
        _ -> ?Q_THROW(q, config_node_option_not_found, [{name, Name}, {node, Node}])
    end.

%% @spec get_node_option(Name :: binary(), Node :: q_node(), Default :: term()) -> term() | Default
%% @doc Return the specified configuration option value for the specified node if it's available, otherwise returns Default parameter.
get_node_option(Name, Node, Default) ->

    case ets:lookup(?Q_ETS_CONFIG_OPTIONS_NODES, {Name, Node}) of
        [Value] -> Value;
        _ -> Default
    end.

%% @doc Return the specified configuration option value for current cluster
%% or raise exception if the specified configuration option is not available.
%% @throws #q_exception{name = config_cluster_option_not_found, params = [{name, Name::binary()}]}
get_cluster_option(Name) -> get_cluster_option(Name, q_universe:get_cluster()).

%% @doc Return the specified configuration option value from the specified cluster
%% or raise exception if the specified configuration option is not available.
%% @throws #q_exception{name = config_cluster_option_not_found, params = [{name, Name::binary()}]}
get_cluster_option(Name, Cluster) ->

    case ets:lookup(?Q_ETS_CONFIG_OPTIONS_CLUSTERS, {Name, Cluster}) of
        [Value] -> Value;
        _ -> ?Q_THROW(q, config_cluster_option_not_found, [{name, Name}, {cluster, Cluster}])
    end.

%% @doc Return the specified configuration option value from the specified cluster if it's available,
%% otherwise returns Default parameter.
get_cluster_option(Name, Cluster, Default) ->

    case ets:lookup(?Q_ETS_CONFIG_OPTIONS_CLUSTERS, {Name, Cluster}) of
        [Value] -> Value;
        _ -> Default
    end.

%% @doc Return the specified universe configuration option value or raise exception if the specified configuration option is not available.
%% @throws #q_exception{name = config_universe_option_not_found, params = [{name, Name::binary()}]}
get_universe_option(Name) ->

    case ets:lookup(?Q_ETS_CONFIG_OPTIONS_UNIVERSE, Name) of
        [Value] -> Value;
        _ -> ?Q_THROW(q, config_universe_option_not_found, [{name, Name}])
    end.

%% @doc Return the specified universe config option value if it's available, otherwise returns Default parameter.
get_universe_option(Name, Default) ->

    case ets:lookup(?Q_ETS_CONFIG_OPTIONS_UNIVERSE, Name) of
        [Value] -> Value;
        _ -> Default
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% set
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Set current node configuration option value. If exists the old configuration option is replaced.
set_node_option(Name, Value) -> set_node_option(Name, q_universe:get_node(), Value).
set_node_option(Name, Node, Value) ->

  q_universe:multicall(q_srv_config, set_node_option, [Name, Node, Value], infinity).

%% @doc Set a node configuration option value. If exists the old configuration option is replaced.
set_cluster_option(Name, Value) -> set_cluster_option(Name, q_universe:get_cluster(), Value).
set_cluster_option(Name, Cluster, Value)
    when erlang:is_binary(Name) ->

  q_universe:multicall(q_srv_config, set_cluster_option, [Name, Cluster, Value], infinity).

%% @doc Set a node configuration option value. If exists the old configuration option is replaced.
set_universe_option(Name, Value) when erlang:is_binary(Name) ->

  q_universe:multicall(q_srv_config, set_universe_option, [Name, Value], infinity).
