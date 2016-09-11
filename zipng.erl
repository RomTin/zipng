#!/usr/bin/env escript
%% -*- erlang -*-

-include("./lib/zipng.hrl").

%%========================================
%% main
%%========================================
main(Arguments) when length(Arguments) >= 2 ->
    ZipList = tl(Arguments),
    PNG = hd(Arguments),
    io:format(">>> ~p will be hidden in ~p as zip arhive.~n", [ZipList, PNG]),
    {Status, PNGBin} = check_sign(load_png(PNG)),
    case Status of
        false ->
            io:format(">>> an error occurred.~n"),
            halt();
        true ->
            PNGBinList = lists:droplast([Bin || {_, Bin} <- split_into_chunks(PNGBin)]),
            ZipBin = make_zTXT_chunk(make_archive(ZipList)),
            NewBin = list_to_binary([?SIGN | PNGBinList] ++ [?IEND, ZipBin]),
            {ok, Fp} = file:open("./out_" ++ filename:basename(PNG), write),
            file:write(Fp, NewBin),
            file:close(Fp),
            io:format(">>> new png file was successfully generated!~n")
    end;
main(_) ->
    io:format(">>> You must specify ONE PNG file and ONE ZIP file~n").
