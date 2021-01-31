-module(ys_utils).

-export([default_file_continuation/1, default_file_continuation/2]).
-export([trancoding_file_continuation/1, trancoding_file_continuation/2]).
-export([external_file_reader/2]).

default_file_continuation(Filename) -> default_file_continuation(Filename, 16384).

default_file_continuation(Filename, Size) ->
    case file:open(Filename, [raw, {read_ahead, Size}, read, binary]) of
        {ok, FD} ->
            {
                fun
                    (close) ->
                        file:close(FD);
                    (_) ->
                        case file:read(FD, Size) of
                            eof ->
                                eof;
                            {ok, Bin} ->
                                {Bin, nil};
                            {error, _} = Err ->
                                Err
                        end
                end,
                <<>>
            };
        {error, _} = OpenErr ->
            OpenErr
    end.

% F.1 Detection Without External Encoding Information
% Because each XML entity not accompanied by external encoding information and not in UTF-8 or
% UTF-16 encoding must begin with an XML encoding declaration, in which the first characters must
% be '<?xml', any conforming processor can detect, after two to four octets of input, which of the
% following cases apply. In reading this list, it may help to know that in UCS-4, '<' is
% " #x0000003C " and '?' is " #x0000003F ", and the Byte Order Mark required of UTF-16 data
% streams is " #xFEFF ". The notation ## is used to denote any byte value except that two
% consecutive ##s cannot be both 00.
trancoding_file_continuation(Filename) -> trancoding_file_continuation(Filename, 16384).

trancoding_file_continuation(Filename, Size) ->
    case file:open(Filename, [raw, {read_ahead, Size}, read, binary]) of
        {ok, FD} -> tfc_open(FD, Size);
        {error, _} = OpenErr -> OpenErr
    end.

tfc_bom(Bin) ->
    case unicode:bom_to_encoding(Bin) of
        {latin1, 0} ->
            case Bin of
                <<0, 0, 0, 16#3C, _/bitstring>> -> {{utf32, big}, 0};
                <<16#3C, 0, 0, 0, _/bitstring>> -> {{utf32, little}, 0};
                <<0, 16#3C, 0, 16#3F, _/bitstring>> -> {{utf16, big}, 0};
                <<16#3C, 0, 16#3F, 0, _/bitstring>> -> {{utf16, little}, 0};
                _ -> {utf8, 0}
            end;
        Other ->
            Other
    end.

tfc_enc(utf8, FD, Size, Stream) ->
    Cont = fun
        (close) ->
            file:close(FD);
        (_) ->
            case file:read(FD, Size) of
                eof ->
                    eof;
                {ok, ReadBin} ->
                    {ReadBin, none};
                {error, _} = Err ->
                    Err
            end
    end,
    {Cont, Stream};
tfc_enc(Enc, FD, 1, Stream) ->
    tfc_enc(Enc, FD, 2, Stream);
tfc_enc(Enc, FD, Size, Stream) ->
    EncInit = unicode:characters_to_binary(Stream, Enc),
    Cont = fun
        (close) ->
            file:close(FD);
        (ContState) ->
            case file:read(FD, Size) of
                eof ->
                    eof;
                {ok, ReadBin} when is_binary(ContState) ->
                    Raw = <<ContState/bitstring, ReadBin/bitstring>>,
                    case unicode:characters_to_binary(Raw, Enc) of
                        Encoded when is_binary(Encoded) ->
                            {Encoded, none};
                        {incomplete, Encoded, Leftovers} ->
                            {Encoded, Leftovers};
                        Error ->
                            Error
                    end;
                {ok, ReadBin} ->
                    case unicode:characters_to_binary(ReadBin, Enc) of
                        Encoded when is_binary(Encoded) ->
                            {Encoded, none};
                        {incomplete, Encoded, Leftovers} ->
                            {Encoded, Leftovers};
                        Error ->
                            Error
                    end;
                {error, _} = Err ->
                    Err
            end
    end,
    {Cont, EncInit}.

tfc_open(FD, Size) when Size < 4 ->
    case file:read(FD, 4) of
        eof ->
            file:close(FD),
            {fun(_) -> eof end, none};
        {ok, Bin} ->
            {Enc, BomLen} = tfc_bom(Bin),
            <<_:BomLen/binary, InitialStream/bitstring>> = Bin,
            tfc_enc(Enc, FD, Size, InitialStream);
        {error, _} = Err ->
            Err
    end;
tfc_open(FD, Size) ->
    case file:read(FD, Size) of
        eof ->
            file:close(FD),
            {fun(_) -> eof end, none};
        {ok, Bin} ->
            {Enc, BomLen} = tfc_bom(Bin),
            <<_:BomLen/binary, InitialStream/bitstring>> = Bin,
            tfc_enc(Enc, FD, Size, InitialStream);
        {error, _} = Err ->
            Err
    end.

external_file_reader({_Pub, Sys}, Base) ->
    Filename = filename:join([Base, Sys]),
    NewBase = filename:dirname(Filename),
    io:format("~p~n", [{NewBase, Filename}]),
    case file:read_file(Filename) of
        {ok, Bin} ->
            {Enc, BomLen} = tfc_bom(Bin),
            <<_:BomLen/binary, InitialStream/bitstring>> = Bin,
            {NewBase, unicode:characters_to_binary(InitialStream, Enc)};
        {error, Reason} ->
            io:format("~p~n", [{Reason, Base, Sys}]),
            {NewBase, <<>>}
    end.
