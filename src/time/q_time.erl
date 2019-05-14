%%%-------------------------------------------------------------------
%%% @author Madalin Grigore-Enescu
%%% @copyright (C) 2017, <Madalin Grigore-Enescu>
%%% @doc
%%%
%%% @end
%%% Created : 07. Feb 2017 6:08 PM
%%%-------------------------------------------------------------------
-module(q_time).
-author("madalin").

-include_lib("q/include/q.hrl").

%% API
-export([posix_seconds/0]).
-export([posix_milliseconds/0]).
-export([posix_microseconds/0]).
-export([posix_nanoseconds/0]).

-export([strict_monotonic_seconds/0]).
-export([strict_monotonic_milliseconds/0]).
-export([strict_monotonic_microseconds/0]).
-export([strict_monotonic_nanoseconds/0]).

-export([datetime/0]).
-export([date/0]).
-export([time/0]).

-export([convert_datetime_to_posix_seconds/1]).
-export([convert_datetime_to_posix_milliseconds/1]).
-export([convert_datetime_to_posix_microseconds/1]).
-export([convert_datetime_to_posix_nanoseconds/1]).

-export([convert_posix_seconds_to_datetime/1]).
-export([convert_posix_milliseconds_to_datetime/1]).
-export([convert_posix_microseconds_to_datetime/1]).
-export([convert_posix_nanoseconds_to_datetime/1]).

%% @doc Returns the seconds
posix_seconds() -> erlang:system_time(second).

%% @doc Returns the milliseconds
posix_milliseconds() -> erlang:system_time(millisecond).

%% @doc Returns the microseconds
posix_microseconds() -> erlang:system_time(microsecond).

%% @doc Returns the nanosecond
posix_nanoseconds() -> erlang:system_time(nanosecond).

%% @doc Returns the current Q monotonic time in seconds.
%% This is a strictly monotonically increasing time regardless Erlang time warp mode currently in use.
%% That is, consecutive calls to q_lib_time:strict_monotonic_seconds/0 can NOT produce the same result.
strict_monotonic_seconds() -> q_time_srv_strict_monotonic:get_seconds().

%% @doc Returns the current Q monotonic time in milliseconds.
%% This is a strictly monotonically increasing time regardless Erlang time warp mode currently in use.
%% That is, consecutive calls to q_lib_time:strict_monotonic_milliseconds/0 can NOT produce the same result.
strict_monotonic_milliseconds() -> q_time_srv_strict_monotonic:get_milliseconds().

%% @doc Returns the current Q monotonic time in microseconds
%% This is a strictly monotonically increasing time regardless Erlang time warp mode currently in use.
%% That is, consecutive calls to q_lib_time:strict_monotonic_microseconds/0 can NOT produce the same result.
strict_monotonic_microseconds() -> q_time_srv_strict_monotonic:get_microseconds().

%% @doc Returns the current Q monotonic time in nanoseconds
%% This is a strictly monotonically increasing time regardless Erlang time warp mode currently in use.
%% That is, consecutive calls to q_lib_time:strict_monotonic_nanoseconds/0 can NOT produce the same result.
strict_monotonic_nanoseconds() -> q_time_srv_strict_monotonic:get_nanoseconds().

%% @doc Returns the current date and time according to Universal Time Coordinated (UTC), also called GMT
datetime() -> calendar:gregorian_seconds_to_datetime(posix_seconds() + ?Q_TIME_SECONDS_UNIX_EPOCH).

%% @doc Returns the current date according to Universal Time Coordinated (UTC), also called GMT
date() -> {Date, _} = datetime(), Date.

%% @doc Returns the current time according to Universal Time Coordinated (UTC), also called GMT
time() -> {_, Time} = datetime(), Time.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% convert_datetime_
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Convert datetime() to posix seconds
convert_datetime_to_posix_seconds(DateTime) -> calendar:datetime_to_gregorian_seconds(DateTime) - ?Q_TIME_SECONDS_UNIX_EPOCH.

%% @doc Convert datetime() to posix microseconds
convert_datetime_to_posix_milliseconds(DateTime) -> convert_datetime_to_posix_seconds(DateTime) * 1000.

%% @doc Convert datetime() to posix microseconds
convert_datetime_to_posix_microseconds(DateTime) -> convert_datetime_to_posix_seconds(DateTime) * 1000000.

%% @doc Convert datetime() to posix microseconds
convert_datetime_to_posix_nanoseconds(DateTime) -> convert_datetime_to_posix_seconds(DateTime) * 1000000000.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% convert_posix_
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Convert posix seconds to datetime()
convert_posix_seconds_to_datetime(Seconds) -> calendar:gregorian_seconds_to_datetime(Seconds + ?Q_TIME_SECONDS_UNIX_EPOCH).

%% @doc Convert posix milliseconds to datetime()
convert_posix_milliseconds_to_datetime(Milliseconds) -> convert_posix_seconds_to_datetime(round(Milliseconds/1000)).

%% @doc Convert posix microseconds to datetime()
convert_posix_microseconds_to_datetime(Microseconds) -> convert_posix_seconds_to_datetime(round(Microseconds/1000000)).

%% @doc Convert posix nanoseconds to datetime()
convert_posix_nanoseconds_to_datetime(Nanoseconds) -> convert_posix_seconds_to_datetime(round(Nanoseconds/1000000000)).

