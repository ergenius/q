%%%-------------------------------------------------------------------
%%% @author madalin
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Apr 2019 9:20 PM
%%%-------------------------------------------------------------------
-module(q_utils_qri).
-author("madalin").

-include_lib("q/include/q.hrl").

%% API
-export([
  get_components/1,
  get_components_scheme_storage/1
  ]).

%% @doc Returns QRI components
get_components(Qri) when erlang:is_binary(Qri) -> q_utils_binaries:explode(Qri, $:).

%% @doc Parse qri and get storage components
get_components_scheme_storage(Qri) ->

  case q_utils_qri:get_components(Qri) of
    [?Q_QRI_SCHEME_STORAGE, Namespace, Table, Id] -> #q_qri_storage_components{namespace = Namespace, table = Table, id = Id};
    _ -> invalid
  end.
