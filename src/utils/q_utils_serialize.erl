%%%-------------------------------------------------------------------
%%% @author madalin
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Jul 2017 11:01 AM
%%%-------------------------------------------------------------------
-module(q_utils_serialize).
-author("madalin").

-include_lib("q/include/q.hrl").

-export([encode/1]).
-export([encode/2]).

-export([decode/1]).
-export([decode_safe/1]).
-export([decode_try/1, decode_try/2]).
-export([decode_safe_try/1, decode_safe_try/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% encode
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Returns a binary data object which is the result of encoding Term without any compression.
%% This is the fastest serialisation.
encode(Term) -> erlang:term_to_binary(Term, [{minor_version, 1}, {compressed, 0}]).

%% @doc Returns a binary data object which is the result of encoding Term with the specified compression level.
%% This is slower serialisation than calling encode(Term) with no compression level specified.
encode(Term, CompressLevel) -> erlang:term_to_binary(Term, [{minor_version, 1}, {compressed,CompressLevel}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% decode
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Returns an Erlang term which is the result of decoding the binary object Binary, which must be encoded using our encode() function
%% When decoding binaries from untrusted sources, consider using decode_safe/1 to prevent denial of service attacks.
decode(Binary) -> erlang:binary_to_term(Binary).

%% @doc Returns an Erlang term which is the result of decoding the binary object Binary, which must be encoded using our encode() function
%%
%% Compared to decode/1 this prevents decoding data that may be used to attack the Erlang system.
%% In the event of receiving unsafe data, decoding fails with a badarg error.
%% Currently, this prevents creation of new atoms directly, creation of new atoms indirectly (as they are embedded in certain structures like pids, refs, funs, etc.),
%% and creation of new external function references. None of those resources are currently garbage collected,
%% so unchecked creation of them can exhaust available memory.
decode_safe(Binary) -> erlang:binary_to_term(Binary, [safe]).

%% @doc Returns the decoded Term or return q_error() on failure instead of throwing exception.
decode_try(Term) ->
    try erlang:binary_to_term(Term) of
        Decoded -> {ok, Decoded}
    catch
        _:_ -> ?Q_ERROR(serialize_decode_failed)
    end.

%% @doc Returns the decoded Term or return Default on failure instead of throwing exception.
decode_try(Term, Default) ->
    try erlang:binary_to_term(Term) of
        Decoded -> {ok, Decoded}
    catch
        _:_ -> Default
    end.

%% @doc Returns the decoded Term or return q_error() on failure instead of throwing exception.
decode_safe_try(Binary) ->
    try erlang:binary_to_term(Binary, [safe]) of
        Decoded -> {ok, Decoded}
    catch
        _:_ -> ?Q_ERROR(serialize_decode_safe_failed)
    end.

%% @doc Returns the decoded Term or return Default on failure instead of throwing exception.
decode_safe_try(Binary, Default) ->
    try erlang:binary_to_term(Binary, [safe]) of
        Decoded -> {ok, Decoded}
    catch
        _:_ -> Default
    end.

