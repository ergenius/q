%%%-------------------------------------------------------------------
%%% @author madalin
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Jul 2017 8:16 AM
%%%-------------------------------------------------------------------
-module(q_utils_base).
-author("madalin").

-include_lib("q/include/q.hrl").

%% API
-export([encode_basel/1]).
-export([encode_base32/1]).

-export([decode_basel/1]).
-export([decode_base32/1]).

-export([test/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% encode_basel
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Encodes a plain ASCII string or binary into basel.
encode_basel(Term) when erlang:is_list(Term) -> encode_basel(erlang:list_to_binary(Term));
encode_basel(Term) when erlang:is_binary(Term) ->
    <<<<(encode_basel_byte(Byte))/binary>> || <<Byte>> <= Term>>.

%% BaseL (base language transfer encoding)
%%
%% This transfer encoding base is best for encoding texts based on latin alphabet.
%%
%% Most alpha-numeric ASCII code (except ZQXJz) remain untouched.
%% This approach allow basel encoding to achieve almost same output size as input data
%% when input data is composed mainly by english texts.
%%
%% On best case scenario we can achieve the 1:1 input/output length ratio.
%% For bytes outside alpha-numeric ASCII characters BaseL encoding use 2 bytes.
%% In worst case scenarios we can achieve 1:2 input/output length ratio.
%%
%% ZQXJz where chose based on relative frequencies of letters in the dictionaries of 15 languages that use all or parts of
%% the latin alphabet. We also used information from Robert Lewand's Cryptological Mathematics for english language.
%% According to Robert Lewand the less frequent letter in english language are:
%%
%% z	0.074%
%% q	0.095%
%% x	0.150%
%% j	0.153%
%%
%% We took into consideration that in most languages upper case letters are always used less than lower case ones.
%% That's why we decided to use ZQXJz letters as markers for double byte encoded bytes (bytes outside alpha-numeric characters).
%%
%% We also took into consideration that in most languages space character usage is quite high.
%% That's why we substituted ASCII space character with z and decided to encode z character with 2 bytes.

%% Substitute space with with z
encode_basel_byte(32) -> <<"z">>;
%% Keep all alphanumeric 0-9, A-I, K-P, R-W, Y, a-y untouched
encode_basel_byte(Byte) when
    Byte > 47, Byte < 58; %% 0-9
    Byte > 64, Byte < 81; %% A-I
    Byte > 64, Byte < 81; %% K-P
    Byte > 81, Byte < 90; %% R-W
    Byte > 81, Byte < 90; %% Y
    Byte > 96, Byte < 122 %% a-y
                      -> <<Byte>>;
%% Encode all ASCII until 0
encode_basel_byte(Byte) when Byte < 48 -> encode_basel_byte_hole(Byte);
%% Ignore 0-9 hole
encode_basel_byte(Byte) when Byte > 57, Byte < 65 -> encode_basel_byte_hole(Byte - 10);
%% Ignore 0-9, A-I hole
encode_basel_byte(Byte) when Byte > 57, Byte < 65 -> encode_basel_byte_hole(Byte - 10);
%% Ignore 0-9, A-I, K-P hole
encode_basel_byte(81) -> encode_basel_byte_hole(55);
%% Ignore 0-9, A-I, K-P, R-W hole
encode_basel_byte(Byte) when Byte > 89, Byte < 97 -> encode_basel_byte_hole(Byte - 34);
%% Ignore 0-9, A-I, K-P, R-W, Y hole
encode_basel_byte(113) -> encode_basel_byte_hole(63);
%% Ignore 0-9, A-I, K-P, R-W, Y, a-y hole
encode_basel_byte(Byte) when Byte > 121 -> encode_basel_byte_hole(Byte - 58).

%% Encode byte with hole
encode_basel_byte_hole(Byte) ->
    Division  = encode_basel_byte_division(Byte div 58),
    Remainder = encode_basel_byte_remainder(Byte rem 58),
    <<Division, Remainder>>.

%% Convert integer division into proper ZQXJ prefix
encode_basel_byte_division(0) -> $Z;
encode_basel_byte_division(1) -> $Q;
encode_basel_byte_division(2) -> $X;
encode_basel_byte_division(3) -> $J.

%% Convert 0-9 remainder to 0-9 ASCII letters
encode_basel_byte_remainder(Rem) when Rem < 10 -> Rem + 48;
%% Convert 10-25 remainder to A-P ASCII letters
encode_basel_byte_remainder(Rem) when Rem < 26 -> Rem + 55;
%% Convert 26-33 remainder to R-Y ASCII letters
encode_basel_byte_remainder(Rem) when Rem < 34 -> Rem + 56;
%% Convert 34-49 remainder to a-p ASCII letters
encode_basel_byte_remainder(Rem) when Rem < 50 -> Rem + 63;
%% Convert 50-57 remainder to r-y ASCII letters
encode_basel_byte_remainder(Rem) -> Rem + 64.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% decode_basel
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Decode a basel encoded string or binary to ASCII binary
decode_basel(Term) when erlang:is_list(Term) -> decode_basel(erlang:list_to_binary(Term), <<>>);
decode_basel(Term) when erlang:is_binary(Term) -> decode_basel(Term, <<>>).
decode_basel(<<>>, Acum) -> Acum;

%% Decode Z (space)
decode_basel(<<$z, Rest/binary>>, Acum) -> decode_basel(Rest, <<Acum/binary, " ">>);

%% Keep all alphanumeric 0-9, A-I, K-P, R-W, Y, a-y untouched
decode_basel(<<Byte, Rest/binary>>, Acum) when
    Byte > 47, Byte < 58; %% 0-9
    Byte > 64, Byte < 81; %% A-P
    Byte > 81, Byte < 90; %% R-Y
    Byte > 96, Byte < 113; %% a-p following r-y
    Byte > 113, Byte < 122 ->
    decode_basel(Rest, <<Acum/binary, Byte>>);

%% Decode 2 byte encoded bytes
decode_basel(<<$Z, Rem, Rest/binary>>, Acum) ->
    DecodedRem = decode_basel_byte_remainder(Rem),
    DecodedByte = decode_basel_byte_hole(DecodedRem),
    decode_basel(Rest, <<Acum/binary, DecodedByte>>);
decode_basel(<<$Q, Rem, Rest/binary>>, Acum) ->
    DecodedRem = decode_basel_byte_remainder(Rem) + 58,
    DecodedByte = decode_basel_byte_hole(DecodedRem),
    decode_basel(Rest, <<Acum/binary, DecodedByte>>);
decode_basel(<<$X, Rem, Rest/binary>>, Acum) ->
    DecodedRem = decode_basel_byte_remainder(Rem) + 116,
    DecodedByte = decode_basel_byte_hole(DecodedRem),
    decode_basel(Rest, <<Acum/binary, DecodedByte>>);
decode_basel(<<$J, Rem, Rest/binary>>, Acum) ->
    DecodedRem = decode_basel_byte_remainder(Rem) + 174,
    DecodedByte = decode_basel_byte_hole(DecodedRem),
    decode_basel(Rest, <<Acum/binary, DecodedByte>>);

%% Handle invalid encoding case
decode_basel(_, _) -> throw(qbase_invalid_basel_encoding).

%% Keep all ASCII until 0 untouched
decode_basel_byte_hole(Byte) when Byte < 48 -> Byte;
%% Restore 0-9 hole
decode_basel_byte_hole(Byte) when Byte > 47, Byte < 55 -> Byte + 10;
%% Restore 0-9 AND A-P hole (Q ASCII letter)
decode_basel_byte_hole(55) -> 81;
%% Restore 0-9, A-P AND R-Y hole
decode_basel_byte_hole(Byte) when Byte > 55, Byte < 63 -> Byte + 34;
%% Restore 0-9, A-P, R-Y AND a-p hole (q ASCII letter)
decode_basel_byte_hole(63) -> 113;
%% Restore 0-9, A-P, R-Y, a-p AND r-y hole
decode_basel_byte_hole(Byte) when Byte > 63 -> Byte + 58.

%% Convert 0-9 ASCII letters to 0-9 remainder
decode_basel_byte_remainder(Rem) when Rem < 58 -> Rem - 48;
%% Convert A-P ASCII letters to 10-25 remainder
decode_basel_byte_remainder(Rem) when Rem < 81 -> Rem - 55;
%% Convert R-Y ASCII letters to 26-33 remainder
decode_basel_byte_remainder(Rem) when Rem < 90 -> Rem - 56;
%% Convert a-p ASCII letters to 34-49 remainder
decode_basel_byte_remainder(Rem) when Rem < 113 -> Rem - 63;
%% Convert r-y ASCII letters to 50-57 remainder
decode_basel_byte_remainder(Rem) -> Rem - 64.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% encode_base32
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Encodes a plain ASCII string or binary into base32
encode_base32(Term) when erlang:is_list(Term) -> encode_base32(erlang:list_to_binary(Term), []);
encode_base32(Term) -> encode_base32(Term, []).
encode_base32(Term, Options) when
    erlang:is_binary(Term),
    erlang:is_list(Options) ->

    AddPadding = proplists:get_value(padding, Options, true),

    {EncodedBody, Rest} = encode_base32_body(Term),
    case AddPadding of
        true ->

            {EncodedRest, PaddingSize} = encode_base32_rest_padding(Rest),
            Padding = erlang:list_to_binary(lists:duplicate(PaddingSize, $=)),
            <<EncodedBody/binary, EncodedRest/binary, Padding/binary>>;

        false ->

            EncodedRest = encode_base32_rest(Rest),
            <<EncodedBody/binary, EncodedRest/binary>>
    end.

encode_base32_body(Bin) ->

    Offset = 5 * (byte_size(Bin) div 5),
    <<Body:Offset/binary, Rest/binary>> = Bin,
    {<<<<(encode_base32_letter(L))>> || <<L:5>> <= Body>>, Rest}.

encode_base32_rest_padding(Bin) ->

    Whole = bit_size(Bin) div 5,
    Offset = 5 * Whole,
    <<Body:Offset/bits, Rest/bits>> = Bin,
    Body0 = <<<<(encode_base32_letter(L))>> || <<L:5>> <= Body>>,

    {Body1, Pad} = case Rest of
                       <<L:3>> -> {<<(encode_base32_letter(L bsl 2))>>, 6};
                       <<L:1>> -> {<<(encode_base32_letter(L bsl 4))>>, 4};
                       <<L:4>> -> {<<(encode_base32_letter(L bsl 1))>>, 3};
                       <<L:2>> -> {<<(encode_base32_letter(L bsl 3))>>, 1};
                       <<>> -> {<<>>, 0}
                   end,
    {<<Body0/binary, Body1/binary>>, Pad}.

encode_base32_rest(Bin) ->

    Whole = bit_size(Bin) div 5,
    Offset = 5 * Whole,
    <<Body:Offset/bits, Rest/bits>> = Bin,
    Body0 = <<<<(encode_base32_letter(L))>> || <<L:5>> <= Body>>,

    Body1 = case Rest of
                <<L:3>> -> <<(encode_base32_letter(L bsl 2))>>;
                <<L:1>> -> <<(encode_base32_letter(L bsl 4))>>;
                <<L:4>> -> <<(encode_base32_letter(L bsl 1))>>;
                <<L:2>> -> <<(encode_base32_letter(L bsl 3))>>;
                <<>> -> <<>>
            end,
    <<Body0/binary, Body1/binary>>.

encode_base32_letter(L) when L >= 26, L =< 31 -> L + 24;
encode_base32_letter(L) when L >= 0, L =< 25 -> L + $A.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% decode_base32
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Decode a base32 encoded string into coresponding ASCII binary
decode_base32(Term) when erlang:is_list(Term) -> decode_base32(erlang:list_to_binary(Term));
decode_base32(Term) when erlang:is_binary(Term) -> decode_base32_iterate(Term, <<>>).

decode_base32_iterate(<<L, "======">>, Bin) -> <<Bin/bits, (decode_base32_letter(L) bsr 2):3>>;
decode_base32_iterate(<<L, "====">>, Bin) -> <<Bin/bits, (decode_base32_letter(L) bsr 4):1>>;
decode_base32_iterate(<<L, "===">>, Bin) -> <<Bin/bits, (decode_base32_letter(L) bsr 1):4>>;
decode_base32_iterate(<<L, "=">>, Bin) -> <<Bin/bits, (decode_base32_letter(L) bsr 3):2>>;
decode_base32_iterate(<<L, Rest/binary>>, Bin) ->
    decode_base32_iterate(Rest, <<Bin/bits, (decode_base32_letter(L)):5>>);
decode_base32_iterate(<<>>, Bin) -> Bin.

decode_base32_letter(L) when L >= $2, L =< $7 -> L - 24;
decode_base32_letter(L) when L >= $a, L =< $z -> L - $a;
decode_base32_letter(L) when L >= $A, L =< $Z -> L - $A.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Test encoding and decoding a full ASCII table (including the extended set)
test() ->

    %% Generate the table
    Table = test_generate_table(),
    io:format("~n~nOriginal table (size ~p): ~n~p", [erlang:size(Table), Table]),

    %% BaseL encode the table
    EncodedBaseLTable = encode_basel(Table),
    io:format("~n~nEncoded BaseL table (size ~p): ~n~p", [erlang:size(EncodedBaseLTable), EncodedBaseLTable]),

    %% BaseL decode the table
    DecodedBaseLTable = decode_basel(EncodedBaseLTable),
    io:format("~n~nDecoded BaseL table (size ~p): ~n~p", [erlang:size(DecodedBaseLTable), DecodedBaseLTable]),

    %% Base32 encode the table
    EncodedBase32Table = encode_base32(Table),
    io:format("~n~nEncoded Base32 table (size ~p): ~n~p", [erlang:size(EncodedBase32Table), EncodedBase32Table]),

    %% Base32 decode the table
    DecodedBase32Table = decode_base32(EncodedBase32Table),
    io:format("~n~nDecoded Base32 table (size ~p): ~n~p", [erlang:size(DecodedBase32Table), DecodedBase32Table]),

    %% Benchmark
    q_utils_benchmark:function(q_utils_base, encode_basel, [Table], 100),
    q_utils_benchmark:function(q_utils_base, encode_base32, [Table], 100),

    %% Check
    Table = DecodedBaseLTable,
    Table = DecodedBase32Table.

test_generate_table() -> test_generate_table(0, <<>>).
test_generate_table(256, Acum) -> Acum;
test_generate_table(Byte, Acum) -> test_generate_table(Byte + 1, <<Acum/binary, Byte>>).