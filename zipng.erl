#!/usr/bin/env escript
%% -*- erlang -*-

-include("./lib/zipng.hrl").

%%========================================
%% main
%%========================================
main([PNG, ZIP]) ->
    io:format(">>> ~p will be combined with ~p.~n", [ZIP, PNG]),
    {Status, BinPNG} = check_sign(load_png(PNG)),
    case Status of
        false ->
            io:format(">>> an error occurred.~n"),
            halt();
        true ->
            split_into_chunks(BinPNG)
    end;
main(_) ->
    io:format(">>> You must specify ONE PNG file and ONE ZIP file~n").
