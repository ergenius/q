%%
 % Copyright (c) 2017. Madalin Grigore-Enescu
 %
-module(q_utils_binaries).
-author("madalin").

-include_lib("q/include/q.hrl").

-export([reverse/1]).
-export([reverse_recursive/1]).
-export([explode/2]).
-export([trim_left/2]).
-export([trim_right/2]).

-export([max/2]).
-export([min/2]).

-export([compare/2]).

-export([order_ascending/1, order_ascending/2]).
-export([order_descending/1, order_descending/2]).

-export([convert_to_hex/1, convert_to_hex/2]).

-export([test/0, benchmark/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% internal defines
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(Q_LIB_BINARIES_HEX_UPPER_BYTE(Byte), (hex_upper_byte(Byte))/binary).
-define(Q_LIB_BINARIES_HEX_LOWER_BYTE(Byte), (hex_lower_byte(Byte))/binary).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% various functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Reverse the specified binary
reverse(Binary) when erlang:is_binary(Binary) ->
  Size = size(Binary)*8,
  <<LittleInteger:Size/integer-little>> = Binary,
  <<LittleInteger:Size/integer-big>>.

%% @doc Reverse the specified binary using a recursive reversing method
reverse_recursive(Binary) -> reverse_recursive(Binary, <<>>).
reverse_recursive(<<H:1/binary, Rest/binary>>, Acc) -> reverse_recursive(Rest, <<H/binary, Acc/binary>>);
reverse_recursive(<<>>, Acc) -> Acc.

%% @doc Explode binary by specified byte
%% Returns a list of exploded binaries.
explode(Binary, Byte) when erlang:is_binary(Binary), erlang:is_integer(Byte) -> explode(Binary, Byte, <<>>, []);
explode(Binary, <<Byte>>) when erlang:is_binary(Binary) -> explode(Binary, Byte, <<>>, []).
explode(<<>>, _Byte, <<>>, AcumL) -> AcumL;
explode(<<>>, _Byte, AcumB, []) -> AcumB;
explode(<<>>, _Byte, AcumB, AcumL) -> lists:append(AcumL, [AcumB]);
explode(<<NextByte, Rest/binary>>, Byte, AcumB, AcumL) ->
  case NextByte of
    Byte ->
      NewAcumL = lists:append(AcumL, [AcumB]),
      explode(Rest, Byte, <<>>, NewAcumL);
    _ ->
      NewAcumB = <<AcumB/binary, NextByte>>,
      explode(Rest, Byte, NewAcumB, AcumL)
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% trim
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Left trim the specified byte from the specified binary
trim_left(<<Byte, Rest/binary>>, Byte) -> trim_left(Rest, Byte);
trim_left(Rest, _Byte) -> Rest.

%% @doc Right trim the specified byte from the specified binary
trim_right(Binary, Byte) ->
  ReversedBinary = reverse(Binary),
  TrimLeftBinary = trim_left(ReversedBinary, Byte),
  reverse(TrimLeftBinary).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% comparison/order
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Return the largest of Bin1 and Bin2.
%% If the terms compare equal, Bin1 will be returned.
max(Bin1, Bin2) -> max(Bin1, Bin2, Bin1, Bin2).
max(Bin1, _Bin2, <<H1,_/binary>>, <<H2,_/binary>>) when H1 > H2 -> Bin1;
max(_Bin1, Bin2, <<H1,_/binary>>, <<H2,_/binary>>) when H1 < H2 -> Bin2;
max(Bin1, Bin2, <<_,T1/binary>>, <<_,T2/binary>>) -> max(Bin1, Bin2, T1, T2);
max(Bin1, _Bin2, <<_,_/binary>>, <<>>) -> Bin1;
max(_Bin1, Bin2, <<>>, <<_,_/binary>>) -> Bin2;
max(Bin1, _Bin2, <<>>, <<>>) -> Bin1.

%% @doc Return the smallest of Bin1 and Bin2.
%% If the terms compare equal, Bin1 will be returned.
min(Bin1, Bin2) -> min(Bin1, Bin2, Bin1, Bin2).
min(Bin1, _Bin2, <<H1,_/binary>>, <<H2,_/binary>>) when H1 < H2 -> Bin1;
min(_Bin1, Bin2, <<H1,_/binary>>, <<H2,_/binary>>) when H1 > H2 -> Bin2;
min(Bin1, Bin2, <<_,T1/binary>>, <<_,T2/binary>>) -> min(Bin1, Bin2, T1, T2);
min(_Bin1, Bin2, <<_,_/binary>>, <<>>) -> Bin2;
min(Bin1, _Bin2, <<>>, <<_,_/binary>>) -> Bin1;
min(Bin1, _Bin2, <<>>, <<>>) -> Bin1.

%% @doc Compare 2 binaries returning 'equal' if they are the same, 'first' if first binary is the bigest one or 'second' if second is the bigest one.
compare(<<H1,_/binary>>, <<H2,_/binary>>) when H1 > H2 -> first;
compare(<<H1,_/binary>>, <<H2,_/binary>>) when H1 < H2 -> second;
compare(<<_,T1/binary>>, <<_,T2/binary>>) -> compare(T1, T2);
compare(<<_,_/binary>>, <<>>) -> first;
compare(<<>>, <<_,_/binary>>) -> second;
compare(<<>>, <<>>) -> equal.

%% @doc Order the elements in a binaries list ascending
order_ascending(BinList) -> lists:sort(fun(A, B) ->
  case compare(A, B) of
    second -> false;
    _ -> true
  end
end, BinList).

%% @doc Order 2 binaries ascending
order_ascending(Bin1, Bin2) -> order_ascending(Bin1, Bin2, Bin1, Bin2).
order_ascending(Bin1, Bin2, <<H1,_/binary>>, <<H2,_/binary>>) when H1 < H2 -> {Bin1, Bin2};
order_ascending(Bin1, Bin2, <<H1,_/binary>>, <<H2,_/binary>>) when H1 > H2 -> {Bin2, Bin1};
order_ascending(Bin1, Bin2, <<_,T1/binary>>, <<_,T2/binary>>) -> order_ascending(Bin1, Bin2, T1, T2);
order_ascending(Bin1, Bin2, <<_,_/binary>>, <<>>) -> {Bin2, Bin1};
order_ascending(Bin1, Bin2, <<>>, <<_,_/binary>>) -> {Bin1, Bin2};
order_ascending(Bin1, Bin2, <<>>, <<>>) -> {Bin1, Bin2}.

%% @doc Order the elements in a binaries list descending
order_descending(BinList) -> lists:sort(fun(A, B) ->
  case compare(A, B) of
    second -> true;
    _ -> false
  end
end, BinList).

%% @doc Order 2 binaries descending
order_descending(Bin1, Bin2) -> order_descending(Bin1, Bin2, Bin1, Bin2).
order_descending(Bin1, Bin2, <<H1,_/binary>>, <<H2,_/binary>>) when H1 > H2 -> {Bin1, Bin2};
order_descending(Bin1, Bin2, <<H1,_/binary>>, <<H2,_/binary>>) when H1 < H2 -> {Bin2, Bin1};
order_descending(Bin1, Bin2, <<_,T1/binary>>, <<_,T2/binary>>) -> order_descending(Bin1, Bin2, T1, T2);
order_descending(Bin1, Bin2, <<_,_/binary>>, <<>>) -> {Bin1, Bin2};
order_descending(Bin1, Bin2, <<>>, <<_,_/binary>>) -> {Bin2, Bin1};
order_descending(Bin1, Bin2, <<>>, <<>>) -> {Bin1, Bin2}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% convert
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Convert each byte from the specified binary to upper case hexadecimal
convert_to_hex(Binary) -> convert_to_hex_upper(Binary, <<>>).

%% @doc Convert each byte from the specified binary to upper or lower case hexadecimal
convert_to_hex(Binary, upper) -> convert_to_hex_upper(Binary, <<>>);
convert_to_hex(Binary, lower) -> convert_to_hex_lower(Binary, <<>>).

convert_to_hex_upper(<<>>, Acumulator) -> Acumulator;
convert_to_hex_upper(<<A:8, B:8, C:8, D:8, E:8, F:8, G:8, H:8, Rest/binary>>, Acumulator) ->
  convert_to_hex_upper(Rest, <<Acumulator/binary,
  ?Q_LIB_BINARIES_HEX_UPPER_BYTE(A),
  ?Q_LIB_BINARIES_HEX_UPPER_BYTE(B),
  ?Q_LIB_BINARIES_HEX_UPPER_BYTE(C),
  ?Q_LIB_BINARIES_HEX_UPPER_BYTE(D),
  ?Q_LIB_BINARIES_HEX_UPPER_BYTE(E),
  ?Q_LIB_BINARIES_HEX_UPPER_BYTE(F),
  ?Q_LIB_BINARIES_HEX_UPPER_BYTE(G),
  ?Q_LIB_BINARIES_HEX_UPPER_BYTE(H)>>);
convert_to_hex_upper(<<X:8, Rest/binary>>, Acumulator) -> convert_to_hex_upper(Rest, <<Acumulator/binary, ?Q_LIB_BINARIES_HEX_UPPER_BYTE(X)>>).

convert_to_hex_lower(<<>>, Acumulator) -> Acumulator;
convert_to_hex_lower(<<A:8, B:8, C:8, D:8, E:8, F:8, G:8, H:8, Rest/binary>>, Acumulator) ->
  convert_to_hex_upper(Rest, <<Acumulator/binary,
  ?Q_LIB_BINARIES_HEX_UPPER_BYTE(A),
  ?Q_LIB_BINARIES_HEX_UPPER_BYTE(B),
  ?Q_LIB_BINARIES_HEX_UPPER_BYTE(C),
  ?Q_LIB_BINARIES_HEX_UPPER_BYTE(D),
  ?Q_LIB_BINARIES_HEX_UPPER_BYTE(E),
  ?Q_LIB_BINARIES_HEX_UPPER_BYTE(F),
  ?Q_LIB_BINARIES_HEX_UPPER_BYTE(G),
  ?Q_LIB_BINARIES_HEX_UPPER_BYTE(H)>>);
convert_to_hex_lower(<<X:8, Rest/binary>>, Acumulator) -> convert_to_hex_lower(Rest, <<Acumulator/binary, ?Q_LIB_BINARIES_HEX_LOWER_BYTE(X)>>).

-compile({inline, [hex_upper_byte/1]}).
-compile({inline, [hex_lower_byte/1]}).

hex_upper_byte(X) ->
  element(
    X+1, {<<"00">>,<<"01">>,<<"02">>,<<"03">>,<<"04">>,<<"05">>,<<"06">>,<<"07">>,
      <<"08">>,<<"09">>,<<"0A">>,<<"0B">>,<<"0C">>,<<"0D">>,<<"0E">>,<<"0F">>,
      <<"10">>,<<"11">>,<<"12">>,<<"13">>,<<"14">>,<<"15">>,<<"16">>,<<"17">>,
      <<"18">>,<<"19">>,<<"1A">>,<<"1B">>,<<"1C">>,<<"1D">>,<<"1E">>,<<"1F">>,
      <<"20">>,<<"21">>,<<"22">>,<<"23">>,<<"24">>,<<"25">>,<<"26">>,<<"27">>,
      <<"28">>,<<"29">>,<<"2A">>,<<"2B">>,<<"2C">>,<<"2D">>,<<"2E">>,<<"2F">>,
      <<"30">>,<<"31">>,<<"32">>,<<"33">>,<<"34">>,<<"35">>,<<"36">>,<<"37">>,
      <<"38">>,<<"39">>,<<"3A">>,<<"3B">>,<<"3C">>,<<"3D">>,<<"3E">>,<<"3F">>,
      <<"40">>,<<"41">>,<<"42">>,<<"43">>,<<"44">>,<<"45">>,<<"46">>,<<"47">>,
      <<"48">>,<<"49">>,<<"4A">>,<<"4B">>,<<"4C">>,<<"4D">>,<<"4E">>,<<"4F">>,
      <<"50">>,<<"51">>,<<"52">>,<<"53">>,<<"54">>,<<"55">>,<<"56">>,<<"57">>,
      <<"58">>,<<"59">>,<<"5A">>,<<"5B">>,<<"5C">>,<<"5D">>,<<"5E">>,<<"5F">>,
      <<"60">>,<<"61">>,<<"62">>,<<"63">>,<<"64">>,<<"65">>,<<"66">>,<<"67">>,
      <<"68">>,<<"69">>,<<"6A">>,<<"6B">>,<<"6C">>,<<"6D">>,<<"6E">>,<<"6F">>,
      <<"70">>,<<"71">>,<<"72">>,<<"73">>,<<"74">>,<<"75">>,<<"76">>,<<"77">>,
      <<"78">>,<<"79">>,<<"7A">>,<<"7B">>,<<"7C">>,<<"7D">>,<<"7E">>,<<"7F">>,
      <<"80">>,<<"81">>,<<"82">>,<<"83">>,<<"84">>,<<"85">>,<<"86">>,<<"87">>,
      <<"88">>,<<"89">>,<<"8A">>,<<"8B">>,<<"8C">>,<<"8D">>,<<"8E">>,<<"8F">>,
      <<"90">>,<<"91">>,<<"92">>,<<"93">>,<<"94">>,<<"95">>,<<"96">>,<<"97">>,
      <<"98">>,<<"99">>,<<"9A">>,<<"9B">>,<<"9C">>,<<"9D">>,<<"9E">>,<<"9F">>,
      <<"A0">>,<<"A1">>,<<"A2">>,<<"A3">>,<<"A4">>,<<"A5">>,<<"A6">>,<<"A7">>,
      <<"A8">>,<<"A9">>,<<"AA">>,<<"AB">>,<<"AC">>,<<"AD">>,<<"AE">>,<<"AF">>,
      <<"B0">>,<<"B1">>,<<"B2">>,<<"B3">>,<<"B4">>,<<"B5">>,<<"B6">>,<<"B7">>,
      <<"B8">>,<<"B9">>,<<"BA">>,<<"BB">>,<<"BC">>,<<"BD">>,<<"BE">>,<<"BF">>,
      <<"C0">>,<<"C1">>,<<"C2">>,<<"C3">>,<<"C4">>,<<"C5">>,<<"C6">>,<<"C7">>,
      <<"C8">>,<<"C9">>,<<"CA">>,<<"CB">>,<<"CC">>,<<"CD">>,<<"CE">>,<<"CF">>,
      <<"D0">>,<<"D1">>,<<"D2">>,<<"D3">>,<<"D4">>,<<"D5">>,<<"D6">>,<<"D7">>,
      <<"D8">>,<<"D9">>,<<"DA">>,<<"DB">>,<<"DC">>,<<"DD">>,<<"DE">>,<<"DF">>,
      <<"E0">>,<<"E1">>,<<"E2">>,<<"E3">>,<<"E4">>,<<"E5">>,<<"E6">>,<<"E7">>,
      <<"E8">>,<<"E9">>,<<"EA">>,<<"EB">>,<<"EC">>,<<"ED">>,<<"EE">>,<<"EF">>,
      <<"F0">>,<<"F1">>,<<"F2">>,<<"F3">>,<<"F4">>,<<"F5">>,<<"F6">>,<<"F7">>,
      <<"F8">>,<<"F9">>,<<"FA">>,<<"FB">>,<<"FC">>,<<"FD">>,<<"FE">>,<<"FF">>}).
hex_lower_byte(X) ->
  element(
    X+1, {<<"00">>,<<"01">>,<<"02">>,<<"03">>,<<"04">>,<<"05">>,<<"06">>,<<"07">>,
      <<"08">>,<<"09">>,<<"0a">>,<<"0b">>,<<"0c">>,<<"0d">>,<<"0e">>,<<"0f">>,
      <<"10">>,<<"11">>,<<"12">>,<<"13">>,<<"14">>,<<"15">>,<<"16">>,<<"17">>,
      <<"18">>,<<"19">>,<<"1a">>,<<"1b">>,<<"1c">>,<<"1d">>,<<"1e">>,<<"1f">>,
      <<"20">>,<<"21">>,<<"22">>,<<"23">>,<<"24">>,<<"25">>,<<"26">>,<<"27">>,
      <<"28">>,<<"29">>,<<"2a">>,<<"2b">>,<<"2c">>,<<"2d">>,<<"2e">>,<<"2f">>,
      <<"30">>,<<"31">>,<<"32">>,<<"33">>,<<"34">>,<<"35">>,<<"36">>,<<"37">>,
      <<"38">>,<<"39">>,<<"3a">>,<<"3b">>,<<"3c">>,<<"3d">>,<<"3e">>,<<"3f">>,
      <<"40">>,<<"41">>,<<"42">>,<<"43">>,<<"44">>,<<"45">>,<<"46">>,<<"47">>,
      <<"48">>,<<"49">>,<<"4a">>,<<"4b">>,<<"4c">>,<<"4d">>,<<"4e">>,<<"4f">>,
      <<"50">>,<<"51">>,<<"52">>,<<"53">>,<<"54">>,<<"55">>,<<"56">>,<<"57">>,
      <<"58">>,<<"59">>,<<"5a">>,<<"5b">>,<<"5c">>,<<"5d">>,<<"5e">>,<<"5f">>,
      <<"60">>,<<"61">>,<<"62">>,<<"63">>,<<"64">>,<<"65">>,<<"66">>,<<"67">>,
      <<"68">>,<<"69">>,<<"6a">>,<<"6b">>,<<"6c">>,<<"6d">>,<<"6e">>,<<"6f">>,
      <<"70">>,<<"71">>,<<"72">>,<<"73">>,<<"74">>,<<"75">>,<<"76">>,<<"77">>,
      <<"78">>,<<"79">>,<<"7a">>,<<"7b">>,<<"7c">>,<<"7d">>,<<"7e">>,<<"7f">>,
      <<"80">>,<<"81">>,<<"82">>,<<"83">>,<<"84">>,<<"85">>,<<"86">>,<<"87">>,
      <<"88">>,<<"89">>,<<"8a">>,<<"8b">>,<<"8c">>,<<"8d">>,<<"8e">>,<<"8f">>,
      <<"90">>,<<"91">>,<<"92">>,<<"93">>,<<"94">>,<<"95">>,<<"96">>,<<"97">>,
      <<"98">>,<<"99">>,<<"9a">>,<<"9b">>,<<"9c">>,<<"9d">>,<<"9e">>,<<"9f">>,
      <<"a0">>,<<"a1">>,<<"a2">>,<<"a3">>,<<"a4">>,<<"a5">>,<<"a6">>,<<"a7">>,
      <<"a8">>,<<"a9">>,<<"aa">>,<<"ab">>,<<"ac">>,<<"ad">>,<<"ae">>,<<"af">>,
      <<"b0">>,<<"b1">>,<<"b2">>,<<"b3">>,<<"b4">>,<<"b5">>,<<"b6">>,<<"b7">>,
      <<"b8">>,<<"b9">>,<<"ba">>,<<"bb">>,<<"bc">>,<<"bd">>,<<"be">>,<<"bf">>,
      <<"c0">>,<<"c1">>,<<"c2">>,<<"c3">>,<<"c4">>,<<"c5">>,<<"c6">>,<<"c7">>,
      <<"c8">>,<<"c9">>,<<"ca">>,<<"cb">>,<<"cc">>,<<"cd">>,<<"ce">>,<<"cf">>,
      <<"d0">>,<<"d1">>,<<"d2">>,<<"d3">>,<<"d4">>,<<"d5">>,<<"d6">>,<<"d7">>,
      <<"d8">>,<<"d9">>,<<"da">>,<<"db">>,<<"dc">>,<<"dd">>,<<"de">>,<<"df">>,
      <<"e0">>,<<"e1">>,<<"e2">>,<<"e3">>,<<"e4">>,<<"e5">>,<<"e6">>,<<"e7">>,
      <<"e8">>,<<"e9">>,<<"ea">>,<<"eb">>,<<"ec">>,<<"ed">>,<<"ee">>,<<"ef">>,
      <<"f0">>,<<"f1">>,<<"f2">>,<<"f3">>,<<"f4">>,<<"f5">>,<<"f6">>,<<"f7">>,
      <<"f8">>,<<"f9">>,<<"fa">>,<<"fb">>,<<"fc">>,<<"fd">>,<<"fe">>,<<"ff">>}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% test/benchmark
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Test module functions
test() -> ok.

%% @doc Benchmark module functions
benchmark() ->

    RandomBinary = crypto:strong_rand_bytes(8192),

    q_utils_benchmark:function(?MODULE, reverse, [RandomBinary], 100),
    q_utils_benchmark:function(?MODULE, reverse_recursive, [RandomBinary], 100),

    ok.