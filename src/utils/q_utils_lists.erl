%%%-------------------------------------------------------------------
%%% @author madalin
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Jul 2017 3:03 PM
%%%-------------------------------------------------------------------
-module(q_utils_lists).
-author("madalin").

-include_lib("q/include/q.hrl").

%% API
-export([is_latin1_string/1]).
-export([is_unicode_string/1]).

-export([join_list_to_binary/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% is
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec is_latin1_string(Term) -> true | false when Term :: term().

%% @doc Returns true if Term is a flat list of characters in the ISO Latin-1 range, otherwise false.
is_latin1_string([C|Cs]) when erlang:is_integer(C), C >= $\000, C =< $\377 -> is_latin1_string(Cs);
is_latin1_string([]) -> true;
is_latin1_string(_) -> false.

-spec is_unicode_string(Term) -> true | false when Term :: term().

%% @doc Returns true if Term is a flat list of characters in the Unicode range, otherwise false.
is_unicode_string([C|Cs]) when
    erlang:is_integer(C), C >= 0, C < 16#D800;
    erlang:is_integer(C), C > 16#DFFF, C < 16#FFFE;
    erlang:is_integer(C), C > 16#FFFF, C =< 16#10FFFF ->
    is_unicode_string(Cs);
is_unicode_string([]) -> true;
is_unicode_string(_) -> false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% join
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec join_list_to_binary(List, Separator) -> binary() when
    List :: [string() | integer() | atom() | binary()],
    Separator :: string() | integer() | atom() | binary().

%% @doc Join all list elements with a separator term
join_list_to_binary(List, Separator) ->

    BinSeparator= q_utils_type_convert:term_to_binary(Separator),
    JoinedList  = join_list_to_binary(List, BinSeparator, []),
    ReversedList= lists:reverse(JoinedList),
    erlang:iolist_to_binary(ReversedList).

join_list_to_binary([], _Sep, Acc) -> Acc;
join_list_to_binary([Head | []], _Sep, Acc) when is_binary(Head) -> [Head | Acc];
join_list_to_binary([Head | []], _Sep, Acc) ->
    BinHead = q_utils_type_convert:term_to_binary(Head),
    [BinHead | Acc];
join_list_to_binary([Head | Tail], Sep, Acc) when is_binary(Head) ->
    join_list_to_binary(Tail, Sep, [Sep, Head | Acc]);
join_list_to_binary([Head | Tail], Sep, Acc) ->
    BinHead = q_utils_type_convert:term_to_binary(Head),
    join_list_to_binary(Tail, Sep, [Sep, BinHead | Acc]).



