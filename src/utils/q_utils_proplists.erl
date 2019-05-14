%%%-------------------------------------------------------------------
%%% @author madalin
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Apr 2019 10:20 PM
%%%-------------------------------------------------------------------
-module(q_utils_proplists).
-author("madalin").

%% API
-export([remove_key_fast/2]).
-export([remove_and_prepend_fast/3]).

%% @doc Remove any occurrence of the specified key from the specified proplists.
%% Please notice the proplists is reversed, and there is no effort to keep the original properties order.
remove_key_fast(Key, List) -> remove_key_fast(Key, List, []).
remove_key_fast(Key, [{Key, _}| T], Acum) -> remove_key_fast(Key, T, Acum);
remove_key_fast(Key, [H | T], Acum) -> remove_key_fast(Key, T, [H | Acum]);
remove_key_fast(_Key, [], Acum) -> Acum.

%% @doc Remove any occurrence of the specified key from the specified proplists and prepend the new key value tuple.
%% Please notice the proplists is reversed, and there is no effort to keep the original props positions.
remove_and_prepend_fast(Key, Value, List) -> [{Key, Value} | remove_key_fast(Key, List)].



