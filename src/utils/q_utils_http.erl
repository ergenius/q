%%%-------------------------------------------------------------------
%%% @author madalin
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. May 2019 1:09 PM
%%%-------------------------------------------------------------------
-module(q_utils_http).
-author("madalin").

-export([url_encode/1]).
-export([multipart_form_data_bondary/0]).
-export([multipart_form_data_format/2]).
-export([multipart_form_data_format/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% url_encode
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @spec url_encode(Str) -> binary()
%% Str = binary() | latin1_chardata() | chardata() | external_chardata() | integer() | float() | atom()
%% @doc URL-encodes a string based on RFC 1738. Returns a binary.
%% @throw exceptions if the input is invalid unicode
%%
%% application/x-www-form-urlencoded, as per RFC 1738, except for that space characters are replaced by '+'
%% Taken from <http://erlangexamples.com/>, from <http://github.com/CapnKernul/httparadise> and <http://www.erlang.org/doc/man/edoc_lib.html>
%% Modified to handle all common terms
url_encode(Str) when is_binary(Str) -> url_encode(unicode:characters_to_list(Str));
url_encode(Str) when is_integer(Str) -> url_encode(erlang:integer_to_list(Str));
url_encode(Str) when is_float(Str) -> url_encode(erlang:float_to_list(Str));
url_encode(Str) when is_atom(Str) -> url_encode(erlang:atom_to_list(Str));
url_encode(Str) when is_list(Str) -> unicode:characters_to_binary(url_encode_iterate(Str)).

%% Skip a-z
url_encode_iterate([C | Cs]) when C >= $a, C =< $z -> [C | url_encode_iterate(Cs)];
%% Skip A-Z
url_encode_iterate([C | Cs]) when C >= $A, C =< $Z -> [C | url_encode_iterate(Cs)];
%% Skip 0-9
url_encode_iterate([C | Cs]) when C >= $0, C =< $9 -> [C | url_encode_iterate(Cs)];
%% Space to +
url_encode_iterate([C | Cs]) when C == 16#20 -> [$+ | url_encode_iterate(Cs)];
%% Skip -_.!~*'()
url_encode_iterate([C = $- | Cs]) -> [C | url_encode_iterate(Cs)];
url_encode_iterate([C = $_ | Cs]) -> [C | url_encode_iterate(Cs)];
url_encode_iterate([C = 46 | Cs]) -> [C | url_encode_iterate(Cs)];
url_encode_iterate([C = $! | Cs]) -> [C | url_encode_iterate(Cs)];
url_encode_iterate([C = $~ | Cs]) -> [C | url_encode_iterate(Cs)];
url_encode_iterate([C = $* | Cs]) -> [C | url_encode_iterate(Cs)];
url_encode_iterate([C = 39 | Cs]) -> [C | url_encode_iterate(Cs)];
url_encode_iterate([C = $( | Cs]) -> [C | url_encode_iterate(Cs)];
url_encode_iterate([C = $) | Cs]) -> [C | url_encode_iterate(Cs)];
url_encode_iterate([C | Cs]) when C =< 16#7f ->
	url_encode_escape_byte(C) ++ url_encode_iterate(Cs);
url_encode_iterate([C | Cs]) when (C >= 16#7f) and (C =< 16#07FF) ->
	url_encode_escape_byte((C bsr 6) + 16#c0) ++ url_encode_escape_byte(C band 16#3f + 16#80) ++ url_encode_iterate(Cs);
url_encode_iterate([C | Cs]) when (C > 16#07FF) ->
	url_encode_escape_byte((C bsr 12) + 16#e0) %% (0xe0 | C >> 12)
	++ url_encode_escape_byte((16#3f band (C bsr 6)) + 16#80) %% 0x80 | ((C >> 6) & 0x3f)
		++ url_encode_escape_byte(C band 16#3f + 16#80) %% 0x80 | (C >> 0x3f)
		++ url_encode_iterate(Cs);
url_encode_iterate([C | Cs]) -> url_encode_escape_byte(C) ++ url_encode_iterate(Cs);
url_encode_iterate([]) -> [].

%% From edoc_lib source
url_encode_hex_octet(N) when N =< 9 -> [$0 + N];
url_encode_hex_octet(N) when N > 15 -> url_encode_hex_octet(N bsr 4) ++ url_encode_hex_octet(N band 15);
url_encode_hex_octet(N) -> [N - 10 + $a].

url_encode_escape_byte(C) ->
	H = url_encode_hex_octet(C),
	url_encode_normalize(H).

%% Append 0 if length == 1
url_encode_normalize(H) when length(H) == 1 -> "%0" ++ H;
url_encode_normalize(H) -> "%" ++ H.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% multipart
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @spec multipart_form_data_bondary() -> [byte()]
%% @doc Generate random multipart form data bondary
multipart_form_data_bondary() ->
	{_,_,MicroSecs} = erlang:now(),
	binary:bin_to_list(crypto:hash(md4,binary:encode_unsigned(MicroSecs))).

%% @spec multipart_form_data_format(Boundary, Fields) -> string()
%% Boundary = string()
%% Fields = [{FieldName, FieldContent}]
%% @doc Generate multipart form data
multipart_form_data_format(Boundary, Fields) ->
	FieldParts = lists:map(fun({FieldName, FieldContent}) ->
		[lists:concat(["--", Boundary]),
			lists:concat(["Content-Disposition: form-data; name=\"",atom_to_list(FieldName),"\""]),
			"",
			FieldContent]
	                       end, Fields),
	FieldParts2 = lists:append(FieldParts),
	EndingParts = [lists:concat(["--", Boundary, "--"]), ""],
	Parts = lists:append([FieldParts2, EndingParts]),
	string:join(Parts, "\r\n").

%% @spec multipart_form_data_format(Boundary, Fields, Files) -> string()
%% Boundary = string()
%% Fields = [{FieldName, FieldContent}]
%% Files = [{FieldName, FileName, FileContent}]
%% @doc Generate multipart form data including files.
multipart_form_data_format(Boundary, Fields, Files) ->
	FieldParts = lists:map(fun({FieldName, FieldContent}) ->
		[lists:concat(["--", Boundary]),
			lists:concat(["Content-Disposition: form-data; name=\"",atom_to_list(FieldName),"\""]),
			"",
			FieldContent]
	                       end, Fields),
	FieldParts2 = lists:append(FieldParts),
	FileParts = lists:map(fun({FieldName, FileName, FileContent}) ->
		[lists:concat(["--", Boundary]),
			lists:concat(["Content-Disposition: format-data; name=\"",atom_to_list(FieldName),"\"; filename=\"",FileName,"\""]),
			lists:concat(["Content-Type: ", "application/octet-stream"]),
			"",
			FileContent]
	                      end, Files),
	FileParts2 = lists:append(FileParts),
	EndingParts = [lists:concat(["--", Boundary, "--"]), ""],
	Parts = lists:append([FieldParts2, FileParts2, EndingParts]),
	string:join(Parts, "\r\n").
