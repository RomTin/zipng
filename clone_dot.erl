#!/usr/bin/env escript
%% -*- erlang -*-

-include("./lib/zipng.hrl").

%%========================================
%% Constants
%%========================================

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


%%========================================
%% main
%%========================================
main(_) ->
    {ok, Fp} = file:open("./imgs/dot_byZipng.png", [write]),
    file:write(Fp, ?SIGN),
    file:write(Fp, ?IHDR_sample),
    file:write(Fp, ?IDAT_sample),
    file:write(Fp, ?IEND),
    file:close(Fp).
