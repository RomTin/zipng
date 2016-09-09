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
            Hoge = make_zTXT_chunk(make_archive(ZipList)),
            NewBin = list_to_binary([?SIGN | PNGBinList ++ [Hoge, ?IEND]]),
            {ok, Fp} = file:open("arc.zip", write),
            %file:write(Fp, NewBin),
            file:write(Fp, make_archive(ZipList)),
            file:close(Fp)
    end;
main(_) ->
    io:format(">>> You must specify ONE PNG file and ONE ZIP file~n").
