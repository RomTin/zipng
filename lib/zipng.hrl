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

%% Local File Header Signature
-define(LOCAL, hex_to_bin(
                 ["04", "03", "4b", "50"])).

%% Central Directory Header Signature
-define(CENTRAL, hex_to_bin(
                   ["02", "01", "4b", "50"])).

%% End Of Central Directory Record
-define(EOCENTRAL, hex_to_bin(
                     ["06", "05", "4b", "50"])).


%%========================================
%% API ZIP
%%========================================

split_into_dir(<<"">>) ->
    [];
split_into_dir(ZipBin) ->
    <<Signature:32, Rest/binary>> = ZipBin,
    <<SignatureL:32>> = <<Signature:32/little-unsigned-integer>>,
    case binary:encode_unsigned(SignatureL) of
        ?LOCAL ->
            <<Other1:112, Size:32, Other2:32, FNLen:16, EFLen:16,
              Rest2/binary>> = Rest,
            <<SizeL:32>> = <<Size:32/little-unsigned-integer>>,
            <<FNLenL:16>> = <<FNLen:16/little-unsigned-integer>>,
            <<EFLenL:16>> = <<EFLen:16/little-unsigned-integer>>,
            <<FileName:FNLenL, ExtraField:EFLenL, Data:SizeL, Rest3/binary>> = Rest2,
            [{local,
              <<Signature:32, Other1:112, Size:32, Other2:32,
                FNLen:16, EFLen:16, FileName:FNLenL, ExtraField:EFLenL, Data:SizeL>>}
             | split_into_dir(Rest3)];
        ?CENTRAL ->
            <<Other1:192, FNLen:16, EFLen:16, FCLen:16, Other2:64, LCOffset:32,
              Rest2/binary>> = Rest,
            <<FNLenL:16>> = <<FNLen:16/little-unsigned-integer>>,
            <<EFLenL:16>> = <<EFLen:16/little-unsigned-integer>>,
            <<FCLenL:16>> = <<FCLen:16/little-unsigned-integer>>,
            <<FileName:FNLenL, ExtraField:EFLenL, FileComment:FCLenL, Rest3/binary>> = Rest2,
            [{central,
              <<Signature:32, Other1:192, FNLen:16, FCLen:16, Other2:64, LCOffset:32,
               FileName:FNLenL, ExtraField:EFLenL, FileComment:FCLenL>>}
             | split_into_dir(Rest3)];
        ?EOCENTRAL ->
            <<Other:96, CDOffset:32, FCLen:16, Rest2/binary>> = Rest,
            <<FCLenL:16>> = <<FCLen:16/little-unsigned-integer>>,
            <<FileComment:FCLenL, Rest3/binary>> = Rest2,
            [{central,
            <<Signature:32, Other:96, CDOffset:32, FCLen:16, FileComment:FCLenL>>}
            | split_into_dir(Rest3)]
    end.

add_offset(Binary, OFFSET) ->
    <<Value:32/little-unsigned-integer>> = Binary,
    <<Ret:32/little-unsigned-integer>> = << (Value + OFFSET):32>>,
    <<Ret:32>>.

%%========================================
%% API PNG
%%========================================
load_png(Fname) ->
    {ok, FileBin} = file:read_file(Fname),
    FileBin.

check_sign(PNGBin) ->
    <<Signature:64, Rest/binary>> = PNGBin,
    BinSignature = binary:encode_unsigned(Signature),
    {(BinSignature =:= ?SIGN), Rest}.

split_into_chunks(<<"">>) ->
    [];
split_into_chunks(PNGBin) ->
    %% PNGBin must not contain SIGNATURE
    <<DataLen:32, Name:32, _Rest/binary>> = PNGBin,
    LenInBit = DataLen * 8,
    <<Data:LenInBit, CRC32:32, Rest/binary>> = _Rest,

    CrcBody = <<Name:32, Data:LenInBit>>,
    %% chunk output
    io:format("Name: ~p | Size: ~7B | CRC32: ~10B | valid: ~p~n", [binary:encode_unsigned(Name), DataLen, CRC32, CRC32 =:= erlang:crc32(CrcBody)]),

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
    OtherData = <<"zTXT", " ">>,
    ChunkData = << OtherData/binary, 0:8, 0:8, ZipBin/binary >>,
    CRC32 = erlang:crc32(ChunkData),
    <<BinSize:32, ChunkData/binary, CRC32:32>>.

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
