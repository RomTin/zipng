-module(clone_dot).
-export([do/0]).

%% SIGNATURE
-define(SIGN, hex_to_bin(
                ["89", "50", "4e", "47",
                 "0d", "0a", "1a", "0a"])).
%% IHDR of dot.png
-define(IHDR_sample, hex_to_bin(
                       ["00", "00", "00", "0d",
                        "49", "48", "44", "52",
                        "00", "00", "00", "01",
                        "00", "00", "00", "01",
                        "08", "02", "00", "00",
                        "00", "90", "77", "53",
                        "de"])).
%% IDAT of dot.png
-define(IDAT_sample, hex_to_bin(
                       ["00", "00", "00", "0c",
                        "49", "44", "41", "54",
                        "08", "d7", "63", "60",
                        "60", "60", "00", "00",
                        "00", "04", "00", "01",
                        "27", "34", "27", "0a"])).
%% IEND <<LEN:32, NAME:32, CRC:32>>
-define(IEND, hex_to_bin(
                ["00", "00", "00", "00",
                 "49", "45", "4e", "44",
                 "ae", "42", "60", "82"])).

%% API
do() ->
    {ok, Fp} = file:open("./imgs/hoge_byZipng.png", [write]),
    file:write(Fp, ?SIGN),
    file:write(Fp, ?IHDR_sample),
    file:write(Fp, ?IDAT_sample),
    file:write(Fp, ?IEND),
    file:close(Fp).

%% private methods
hex_to_dec(Hex) ->
    erlang:list_to_integer(Hex, 16).

hex_to_bin(HexList) ->
    << << (hex_to_dec(Hex)) >> || Hex <- HexList >>.
