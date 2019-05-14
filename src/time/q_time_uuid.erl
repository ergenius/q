%%%-------------------------------------------------------------------
%%% @author madalin
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Jul 2018 9:07 AM
%%%-------------------------------------------------------------------
-module(q_time_uuid).
-author("madalin").

%% API
-export([origin/0]).
-export([infinity/0]).

-export([new/1]).
-export([now/0]).

-export([get_components/1]).
-export([get_datetime/1]).
-export([get_fulltime/1]).
-export([get_posix_seconds/1]).
-export([get_posix_milliseconds/1]).
-export([get_posix_microseconds/1]).
-export([get_posix_nanoseconds/1]).

-export([is_valid/1]).
-export([guard_is_valid/1]).

-export([max/2]).
-export([min/2]).
-export([compare/2]).
-export([order_asc/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ORIGIN
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Returns an origin time uuid
%% Origin time uuid is the 0 reference point in the past for all other time uuid that can be used in comparisons with others time uuid
origin() -> <<"0">>.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% INFINITY
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Returns an infinity time uuid
%% An infinity time uuid is a time uuid far away in the future.
%% This time uuid is so far away in the future that no other time uuid generated by calling now() will be bigger than this sequence.
%% Infinity time uuid can be used in time comparisons between 2 different time uuids.
infinity() -> <<"99999999999999999999999999999999">>.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% new (create a new timeuuid)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @spec new() -> {integer(), binary()}
%% @doc Creates a new Time UUID guaranteed to be unique at global level.
%% Out time UUID format supports:
%% - times in microseconds covering
%% - unsigned 32 bit integer local node unique counter
%% - different clouds having different nodes each
%% The size of the components can be tuned via LEN defines.
new({_Seconds, _Milliseconds, Microseconds, _}) ->

    XMicroseconds = Microseconds - ?XUTILS_LIB_TIMEUUID_ORIGIN_MICROSECONDS,
    case XMicroseconds > 999999999999999 of
        false ->

            BinXMicroseconds    = erlang:integer_to_binary(XMicroseconds),
            Counter             = xdb_lib_ets:get_node_counter_len(?XUTILS_LIB_TIMEUUID_COUNTER_LEN),
            BinCounter          = erlang:integer_to_binary(Counter),
            BinCloudId          = xconfig_lib_cloud:get_cloud_id(),
            BinNodeId           = xconfig_lib_cloud:get_node_id(),

            FSXMicroseconds     = xutils_lib_binaries:prepend_byte_to_size(BinXMicroseconds, 15, 48),
            FSCounter           = xutils_lib_binaries:prepend_byte_to_size(BinCounter, ?XUTILS_LIB_TIMEUUID_COUNTER_LEN, 48),
            FSCloudId           = xutils_lib_binaries:prepend_byte_to_size(BinCloudId, ?XUTILS_LIB_TIMEUUID_CLOUD_ID_LEN, 48),
            FSNodeId            = xutils_lib_binaries:prepend_byte_to_size(BinNodeId,  ?XUTILS_LIB_TIMEUUID_NODE_ID_LEN, 48),

            <<FSXMicroseconds/binary, FSCounter/binary, FSCloudId/binary, FSNodeId/binary>>;

        _ -> throw(timeuuid_invalid_time)

    end;

new({{Year, Month, Day}, {Hour, Minute, Second}}) -> new(xutils_lib_time:convert_datetime_to_fulltime({{Year, Month, Day}, {Hour, Minute, Second}}));
new({Year, Month, Day}) -> new(xutils_lib_time:convert_datetime_to_fulltime({{Year, Month, Day}, {0, 0, 0}})).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% now (create a new timeuuid)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @spec global_new() -> {{Seconds, Milliseconds, Microseconds, {{Year, Month, Day}, {Hour, Minute, Second}}}, binary()}
%% @doc Creates a new Time UUID guaranted to be unique at global level.
now() ->

    FullTime = xutils_lib_time:full(),
    {FullTime, new(FullTime)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% get_
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Returns the specified time uuid components
get_components(<<Microseconds:15/binary, Counter:9/binary, CloudId:4/binary, NodeId:4/binary>>) ->

    TrimedMicroseconds = xutils_lib_binaries:ltrim_byte(48, Microseconds),
    TrimedCounter      = xutils_lib_binaries:ltrim_byte(48, Counter),
    TrimedCloudId      = xutils_lib_binaries:ltrim_byte(48, CloudId),
    TrimedNodeId       = xutils_lib_binaries:ltrim_byte(48, NodeId),
    {
        erlang:binary_to_integer(TrimedMicroseconds) + ?XUTILS_LIB_TIMEUUID_ORIGIN_MICROSECONDS,
        erlang:binary_to_integer(TrimedCounter),
        TrimedCloudId,
        TrimedNodeId
    }.

%% @doc Returns gregorian date time touple from the specified timeuuid
get_datetime(TimeUuid) ->

    UnixSeconds       = get_seconds(TimeUuid),
    GregorianSeconds  = UnixSeconds + ?XUTILS_TIME_UNIX_EPOCH_GREGORIAN_SECONDS,
    calendar:gregorian_seconds_to_datetime(GregorianSeconds).

%% @doc Returns timestamp seconds
get_seconds(TimeUuid) -> get_microseconds(TimeUuid) div 1000000.

%% @doc Returns timestamp milliseconds
get_milliseconds(TimeUuid) -> get_microseconds(TimeUuid) div 1000.

%% @doc Get timestamp microseconds component
get_microseconds(<<Microseconds:15/binary, _/binary>>) ->

    TrimedMicroseconds = xutils_lib_binaries:ltrim_byte(48, Microseconds),
    erlang:binary_to_integer(TrimedMicroseconds) + ?XUTILS_LIB_TIMEUUID_ORIGIN_MICROSECONDS.

%% @doc Returns full time from the specified timeuuid
get_fulltime(TimeUuid) ->

    Microseconds      = get_microseconds(TimeUuid),
    Milliseconds      = Microseconds div 1000,
    Seconds           = Microseconds div 1000000,
    GregorianSeconds  = Seconds + ?XUTILS_TIME_UNIX_EPOCH_GREGORIAN_SECONDS,
    DateTime          = calendar:gregorian_seconds_to_datetime(GregorianSeconds),
    {Seconds, Milliseconds, Microseconds, DateTime}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% IS... CHECKS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @spec is_valid(TimeUuid : term()) -> true | false
%% @doc Check the specified Time UUID to see if it is valid.
is_valid(Term) when is_binary(Term), Term /= <<>> -> true;
is_valid(_Term) -> false.

%%is_valid(Term) when is_binary(Term) -> is_valid_iterate(Term, 0);
%%is_valid(_Term) -> false.
%%is_valid_iterate(<<>>, ?XUTILS_LIB_TIMEUUID_LEN) -> true;
%%is_valid_iterate(<<Byte:1/integer-unit:8, Rest/binary>>, Len) when Byte > 47, Byte < 58 -> is_valid_iterate(Rest, Len+1);
%%is_valid_iterate(_, _) -> false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% COMPARISON FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Return the largest of TimeUuid1 and TimeUuid2.
%% If the terms compare equal, TimeUuid1 will be returned.
max(TimeUuid1, TimeUuid2) -> byte_max(TimeUuid1, TimeUuid2, TimeUuid1, TimeUuid2).
byte_max(TimeUuid1, _TimeUuid2, <<H1,_/binary>>, <<H2,_/binary>>) when H1 > H2 -> TimeUuid1;
byte_max(_TimeUuid1, TimeUuid2, <<H1,_/binary>>, <<H2,_/binary>>) when H1 < H2 -> TimeUuid2;
byte_max(TimeUuid1, TimeUuid2, <<_,T1/binary>>, <<_,T2/binary>>) -> byte_max(TimeUuid1, TimeUuid2, T1, T2);
byte_max(TimeUuid1, _TimeUuid2, <<_,_/binary>>, <<>>) -> TimeUuid1;
byte_max(_TimeUuid1, TimeUuid2, <<>>, <<_,_/binary>>) -> TimeUuid2;
byte_max(TimeUuid1, _TimeUuid2, <<>>, <<>>) -> TimeUuid1.

%% @doc Return the smallest of TimeUuid1 and TimeUuid2.
%% If the terms compare equal, TimeUuid1 will be returned.
min(TimeUuid1, TimeUuid2) -> byte_min(TimeUuid1, TimeUuid2, TimeUuid1, TimeUuid2).
byte_min(TimeUuid1, _TimeUuid2, <<H1,_/binary>>, <<H2,_/binary>>) when H1 < H2 -> TimeUuid1;
byte_min(_TimeUuid1, TimeUuid2, <<H1,_/binary>>, <<H2,_/binary>>) when H1 > H2 -> TimeUuid2;
byte_min(TimeUuid1, TimeUuid2, <<_,T1/binary>>, <<_,T2/binary>>) -> byte_min(TimeUuid1, TimeUuid2, T1, T2);
byte_min(_TimeUuid1, TimeUuid2, <<_,_/binary>>, <<>>) -> TimeUuid2;
byte_min(TimeUuid1, _TimeUuid2, <<>>, <<_,_/binary>>) -> TimeUuid1;
byte_min(TimeUuid1, _TimeUuid2, <<>>, <<>>) -> TimeUuid1.

%% @doc Order two time uuid ascending by comparing them
order_asc(TimeUuid1, TimeUuid2) -> byte_order_asc(TimeUuid1, TimeUuid2, TimeUuid1, TimeUuid2).
byte_order_asc(TimeUuid1, TimeUuid2, <<H1,_/binary>>, <<H2,_/binary>>) when H1 < H2 -> {TimeUuid1, TimeUuid2};
byte_order_asc(TimeUuid1, TimeUuid2, <<H1,_/binary>>, <<H2,_/binary>>) when H1 > H2 -> {TimeUuid2, TimeUuid1};
byte_order_asc(TimeUuid1, TimeUuid2, <<_,T1/binary>>, <<_,T2/binary>>) -> byte_order_asc(TimeUuid1, TimeUuid2, T1, T2);
byte_order_asc(TimeUuid1, TimeUuid2, <<_,_/binary>>, <<>>) -> {TimeUuid2, TimeUuid1};
byte_order_asc(TimeUuid1, TimeUuid2, <<>>, <<_,_/binary>>) -> {TimeUuid1, TimeUuid2};
byte_order_asc(TimeUuid1, TimeUuid2, <<>>, <<>>) -> {TimeUuid1, TimeUuid2}.

%% @doc Compare two uuid returning 'equal' if they are the same,
%% 'first' if first uuid is considered to be bigger or 'second' if second is the bigger one.
compare(<<H1,_/binary>>, <<H2,_/binary>>) when H1 > H2 -> first;
compare(<<H1,_/binary>>, <<H2,_/binary>>) when H1 < H2 -> second;
compare(<<_,T1/binary>>, <<_,T2/binary>>) -> compare(T1, T2);
compare(<<_,_/binary>>, <<>>) -> first;
compare(<<>>, <<_,_/binary>>) -> second;
compare(<<>>, <<>>) -> equal.