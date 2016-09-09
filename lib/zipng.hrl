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

split_into_chunks(<<"">>) ->
    [];
split_into_chunks(BinPNG) ->
    %% BinPNG must not contain SIGNATURE
    <<DataLen:32, Name:32, _Rest/binary>> = BinPNG,
    LenInBit = DataLen * 8,
    <<Data:LenInBit, CRC32:32, Rest/binary>> = _Rest,

    CrcBody = <<Name:32, Data:LenInBit>>,
    %% chunk output
    io:format("Name: ~p | Size: ~10B | CRC32: ~10B | valid: ~p~n", [binary:encode_unsigned(Name), DataLen, CRC32, CRC32 =:= erlang:crc32(CrcBody)]),

    case binary:encode_unsigned(Name) of
        <<"IEND">> ->
            [{binary_to_list(binary:encode_unsigned(Name))
              ,<<DataLen:32,
                 Name:32,
                 Data:LenInBit,
                 CRC32:32>>}];
        _ ->
            [{binary_to_list(binary:encode_unsigned(Name))
              ,<<DataLen:32,
                 Name:32,
                 Data:LenInBit,
                 CRC32:32>>}
             | split_into_chunks(Rest)]
    end.

make_archive(FileNameList) ->
    MessFiles = [{Fname, file:read_file(Fname)}
                 || Fname <- FileNameList],
    Files = [{"./archive/" ++ filename:basename(Fname), Fbin}
             || {Fname, {Status, Fbin}} <- MessFiles, Status =:= ok],
    {ok, {_, ZipBin}} = zip:zip("_buf.zip", Files, [memory]),
    ZipBin.

make_zTXT_chunk(ZipBin) ->
    BinSize = 3 + length(binary_to_list(ZipBin)),
    BinSizeInBit = BinSize * 8,
    OtherData = <<"zTXT", " ">>,
    ChunkData = << OtherData/binary, 0:8, 0:8, ZipBin/binary >>,
    CRC32 = erlang:crc32(ChunkData),
    Ret = <<BinSize:32, ChunkData/binary, CRC32:32>>,
    split_into_chunks(Ret),
    Ret.

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
