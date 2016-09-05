-module(zipng).
-compile(export_all).

%%========================================
%% Constants
%%========================================

%% SIGNATURE
-define(SIGN, hex_to_bin(
                ["89", "50", "4e", "47",
                 "0d", "0a", "1a", "0a"])).

%% IEND <<LEN:32, NAME:32, CRC:32>>
-define(IEND, hex_to_bin(
                ["00", "00", "00", "00",
                 "49", "45", "4e", "44",
                 "ae", "42", "60", "82"])).


%%========================================
%% API
%%========================================
load_png(Fname) ->
    {ok, FileBin} = file:read_file(Fname),
    FileBin.

check_sign(BinPNG) ->
    <<Signature:64, Rest/binary>> = BinPNG,
    BinSignature = binary:encode_unsigned(Signature),
    {(BinSignature =:= ?SIGN), Rest}.

split_into_chunks(BinPNG) ->
    %% BinPNG must not contain SIGNATURE
    <<DataLen:32, Name:32, Rest/binary>> = BinPNG.


%%========================================
%% test methods
%%========================================

read_test(Fname) ->
    FileBin = load_png(Fname),
    Hex = bin_to_hex(FileBin),
    Bin = hex_to_bin(Hex),
    io:format("Original:~n~p~n~n", [FileBin]),
    io:format("ConvertedBinary(Orig->Hex):~n~p~n~n", [Hex]),
    io:format("ConvertedBinary(Hex->Int):~n~p~n~n", [Bin]).


%%========================================
%% private methods
%%========================================

hex_to_dec(Hex) ->
    erlang:list_to_integer(Hex, 16).

dec_to_hex(Int) ->
    erlang:integer_to_list(Int, 16).

hex_to_bin(HexList) ->
    << << (hex_to_dec(Hex)) >> || Hex <- HexList >>.

bin_to_hex(Binary) ->
    [ dec_to_hex(Int) || << Int:8 >> <= Binary].
