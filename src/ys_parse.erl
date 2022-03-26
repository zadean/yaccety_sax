-module(ys_parse).

-export([
    parse_doctypedecl/2,
    parse_content/2,
    parse_Misc/2,
    parse_element/2,
    parse_XMLDecl/2
]).

% -define(LINE_NUMBERS, true).
% -ifdef(LINE_NUMBERS).
% -define(bl(State), bump_line(State)).
% bump_line(#{line := L} = State) -> State#{line := L + 1}.
% -else.
% -define(bl(State), State).
% -endif.

-include("yaccety_sax.hrl").

-define(APPEND(Thing, Acc), append(Thing, Acc)).
-define(ACC(Stream, Pos, Len, Acc), ?APPEND(binary_part(Stream, Pos, Len), Acc)).

-define(MATCH, Bytes, Stream, Pos, State).
-define(MATCH1, Bytes1, Stream1, Pos1, State1).
-define(MATCH2, Bytes2, Stream2, Pos2, State2).
-define(MATCH3, Bytes3, Stream3, Pos3, State3).
-define(MATCH4, Bytes4, Stream4, Pos4, State4).
-define(MATCH5, Bytes5, Stream5, Pos5, State5).
-define(MATCH6, Bytes6, Stream6, Pos6, State6).
-define(MATCH7, Bytes7, Stream7, Pos7, State7).

%-define(dbg(_), ok).
%-define(dbg(Thing), io:format("~p~n", [{?LINE, Thing}])).

-define(XML_NS, <<"http://www.w3.org/XML/1998/namespace">>).
-define(XMLNS_NS, <<"http://www.w3.org/2000/xmlns/">>).

-define(WS(Char), (Char == 16#20 orelse Char == 16#9 orelse Char == 16#A orelse Char == 16#D)).

-compile({inline, [append/2, set_state_pos/2, to_binary/1, fatal_error/2]}).

append(<<>>, Acc) -> Acc;
append(Thing, []) -> Thing;
append(Thing, Acc) -> [Acc, Thing].

cf(#ys_state{continuation = undefined} = State) ->
    {no_bytes, State};
cf(#ys_state{continuation = {CF, replacement}, tags = Tags}) ->
    {Bytes, State} = CF(replacement),
    {Bytes, State#ys_state{tags = Tags}};
cf(#ys_state{continuation = {CF, CS}} = State) ->
    case CF(CS) of
        {Bin, CS1} when is_binary(Bin) ->
            {Bin, State#ys_state{continuation = {CF, CS1}}};
        eof ->
            _ = CF(close),
            {<<>>, State#ys_state{continuation = undefined}};
        {error, Err} ->
            {error, {Err, State}}
    end.

cf(_, #ys_state{continuation = undefined} = State, _) ->
    {no_bytes, State};
cf(Part, #ys_state{continuation = {CF, CS}} = State, {partial, BytesNeeded}) ->
    case CF(CS) of
        {Bin, CS1} when is_binary(Bin), byte_size(Bin) >= BytesNeeded ->
            RestBytes = binary_part(Bin, 0, BytesNeeded),
            <<CP/utf8>> = <<Part/bitstring, RestBytes/bitstring>>,
            {<<CP/utf8>>, Bin, BytesNeeded, State#ys_state{
                continuation = {CF, CS1}
            }};
        {Bin, CS1} when is_binary(Bin), byte_size(Bin) >= 0 ->
            cf(
                <<Part/bitstring, Bin/bitstring>>,
                State#ys_state{continuation = {CF, CS1}},
                {partial, BytesNeeded - byte_size(Bin)}
            );
        eof ->
            _ = CF(close),
            {<<>>, <<>>, 0, State#ys_state{continuation = undefined}};
        {error, Err} ->
            {error, {Err, State}}
    end.

-define(CHARPARTFUN(D),
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    {Char, Stream1, Pos1, State1} = cf(Bytes, State, {partial, D}),
    case Char of
        <<16#FFFE/utf8>> ->
            fatal_error({invalid_character, [Char]}, State1);
        <<16#FFFF/utf8>> ->
            fatal_error({invalid_character, [Char]}, State1);
        _ ->
            Acc2 = ?APPEND(Char, Acc1),
            <<_:Pos1/binary, Rest/bitstring>> = Stream1,
            ?FUNCTION_NAME(Rest, Stream1, Pos1, 0, State1, Acc2)
    end
).

-define(CHARPARTFUNX(D),
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    {Char, Stream1, Pos1, State1} = cf(Bytes, State, {partial, D}),
    case Char of
        <<16#FFFE/utf8>> ->
            fatal_error({invalid_character, [Char]}, State1);
        <<16#FFFF/utf8>> ->
            fatal_error({invalid_character, [Char]}, State1);
        <<_/utf8>> ->
            Acc2 = ?APPEND(Char, Acc1),
            <<_:Pos1/binary, Rest/bitstring>> = Stream1,
            ?FUNCTION_NAME(Rest, Stream1, Pos1, 0, State1, Acc2, DTD, External)
    end
).

-define(CHARPARTFUND(D),
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    {Char, Stream1, Pos1, State1} = cf(Bytes, State, {partial, D}),
    case Char of
        <<16#FFFE/utf8>> ->
            fatal_error({invalid_character, [Char]}, State1);
        <<16#FFFF/utf8>> ->
            fatal_error({invalid_character, [Char]}, State1);
        _ ->
            Acc2 = ?APPEND(Char, Acc1),
            <<_:Pos1/binary, Rest/bitstring>> = Stream1,
            ?FUNCTION_NAME(Rest, Stream1, Pos1, 0, State1, Acc2, DTD)
    end
).

-define(NAMECHARPARTFUN(D),
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    {CodeChar, Stream1, Pos1, State1} = cf(Bytes, State, {partial, D}),
    case CodeChar of
        <<Char/utf8>> when
            Char == 16#B7;
            Char >= 16#C0, Char =< 16#D6;
            Char >= 16#D8, Char =< 16#F6;
            Char >= 16#F8, Char =< 16#37D;
            Char >= 16#37F, Char =< 16#7FF;
            Char >= 16#800, Char =< 16#1FFF;
            Char >= 16#200C, Char =< 16#200D;
            Char >= 16#203F, Char =< 16#2040;
            Char >= 16#2070, Char =< 16#218F;
            Char >= 16#2C00, Char =< 16#2FEF;
            Char >= 16#3001, Char =< 16#D7FF;
            Char >= 16#F900, Char =< 16#FDCF;
            Char >= 16#FDF0, Char =< 16#FFFD;
            Char >= 16#10000, Char =< 16#EFFFF
        ->
            Acc2 = ?APPEND(CodeChar, Acc1),
            <<_:Pos1/binary, Rest/bitstring>> = Stream1,
            ?FUNCTION_NAME(Rest, Stream1, Pos1, 0, State1, Acc2);
        _ ->
            fatal_error({invalid_character, [CodeChar]}, State1)
    end
).

-define(FSTNAMECHARPARTFUN(D, Next),
    {CodeChar, Stream1, Pos1, State1} = cf(Bytes, State, {partial, D}),
    case CodeChar of
        <<Char/utf8>> when
            Char >= 16#C0, Char =< 16#D6;
            Char >= 16#D8, Char =< 16#F6;
            Char >= 16#F8, Char =< 16#2FF;
            Char >= 16#370, Char =< 16#37D;
            Char >= 16#37F, Char =< 16#7FF;
            Char >= 16#800, Char =< 16#1FFF;
            Char >= 16#200C, Char =< 16#200D;
            Char >= 16#2070, Char =< 16#218F;
            Char >= 16#2C00, Char =< 16#2FEF;
            Char >= 16#3001, Char =< 16#D7FF;
            Char >= 16#F900, Char =< 16#FDCF;
            Char >= 16#FDF0, Char =< 16#FFFD;
            Char >= 16#10000, Char =< 16#EFFFF
        ->
            <<_:Pos1/binary, Rest/bitstring>> = Stream1,
            Next(Rest, Stream1, Pos1, 0, State1, CodeChar);
        _ ->
            fatal_error({invalid_character, [CodeChar]}, State1)
    end
).

-define(GET_PE(Func),
    Func(<<"%", Bytes/bitstring>>, Stream, Pos, State, DTD, true) ->
        {PERef, Bytes1, _, _, State1} = parse_PEReference(Bytes, Stream, Pos + 1, State),
        case resolve_parameter_entity(PERef, DTD, State1, true, true) of
            % Stripping whitespace added in replacement
            {replacement_text, <<" ", ReplacementText/bitstring>>} ->
                RTState = replacement_text_state(Bytes1, State1),
                {_, ?MATCH2} = maybe_consume_s(ReplacementText, ReplacementText, 0, RTState),
                Func(?MATCH2, DTD, true)
        end
).

-define(GET_PE_ACC(Func),
    Func(<<"%", Bytes/bitstring>>, Stream, Pos, State, Acc, DTD, true) ->
        {PERef, Bytes1, _, _, State1} = parse_PEReference(Bytes, Stream, Pos + 1, State),
        case resolve_parameter_entity(PERef, DTD, State1, true, true) of
            % Stripping whitespace added in replacement
            {replacement_text, <<" ", ReplacementText/bitstring>>} ->
                RTState = replacement_text_state(Bytes1, State1),
                {_, ?MATCH2} = maybe_consume_s(ReplacementText, ReplacementText, 0, RTState),
                Func(?MATCH2, Acc, DTD, true)
        end
).
%%----------------------------------------------------------------------
%% XML character range
%% [2] Char ::= #x9 | #xA | #xD | [#x20-#xD7FF] | [#xE000-#xFFFD] |
%%              [#x10000-#x10FFFF]
%% any Unicode character, excluding the surrogate blocks, FFFE, and FFFF.
%%----------------------------------------------------------------------
-define(ONECHAR,
?FUNCTION_NAME(<<16#9/utf8, Rest/bitstring>>, Stream, Pos, Len, State, Acc) ->
    ?FUNCTION_NAME(Rest, Stream, Pos, Len + 1, State, Acc);
?FUNCTION_NAME(<<16#A/utf8, Rest/bitstring>>, Stream, Pos, Len, State, Acc) ->
    ?FUNCTION_NAME(Rest, Stream, Pos, Len + 1, State, Acc);
?FUNCTION_NAME(<<16#FFFE/utf8, _Rest/bitstring>>, _, _, _, State, _Acc) ->
    fatal_error({invalid_character, {?LINE, [16#FFFE]}}, State);
?FUNCTION_NAME(<<16#FFFF/utf8, _Rest/bitstring>>, _, _, _, State, _Acc) ->
    fatal_error({invalid_character, {?LINE, [16#FFFF]}}, State);
?FUNCTION_NAME(<<Char/utf8, Rest/bitstring>>, Stream, Pos, Len, State, Acc)  ->
    if
        Char < 16#20 ->
            fatal_error({invalid_character, {?LINE, [Char]}}, State);
        Char < 16#80 ->
            ?FUNCTION_NAME(Rest, Stream, Pos, Len + 1, State, Acc);
        Char < 16#800 ->
            ?FUNCTION_NAME(Rest, Stream, Pos, Len + 2, State, Acc);
        Char < 16#10000 ->
            ?FUNCTION_NAME(Rest, Stream, Pos, Len + 3, State, Acc);
        true ->
            ?FUNCTION_NAME(Rest, Stream, Pos, Len + 4, State, Acc)
    end;
%%
?FUNCTION_NAME(<<>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    {Stream1, State1} = cf(State),
    ?FUNCTION_NAME(Stream1, Stream1, 0, 0, State1, Acc1);
?FUNCTION_NAME(Bytes = <<30:5, _:3>>, Stream, Pos, Len, State, Acc) -> ?CHARPARTFUN(3);
?FUNCTION_NAME(Bytes = <<30:5, _:3, 2:2, _:6>>, Stream, Pos, Len, State, Acc) -> ?CHARPARTFUN(2);
?FUNCTION_NAME(Bytes = <<14:4, _:4>>, Stream, Pos, Len, State, Acc) -> ?CHARPARTFUN(2);
?FUNCTION_NAME(Bytes = <<30:5, _:3, 2:2, _:6, 2:2, _:6>>, Stream, Pos, Len, State, Acc) -> ?CHARPARTFUN(1);
?FUNCTION_NAME(Bytes = <<14:4, _:4, 2:2, _:6>>, Stream, Pos, Len, State, Acc) -> ?CHARPARTFUN(1);
?FUNCTION_NAME(Bytes = <<6:3, _:5>>, Stream, Pos, Len, State, Acc) -> ?CHARPARTFUN(1);
?FUNCTION_NAME(Bytes, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    {to_binary(Acc1), Bytes, Stream, Pos + Len, State}
).

-define(ONECHARX,
?FUNCTION_NAME(<<16#9/utf8, Rest/bitstring>>, Stream, Pos, Len, State, Acc, DTD, External) ->
    ?FUNCTION_NAME(Rest, Stream, Pos, Len + 1, State, Acc, DTD, External);
?FUNCTION_NAME(<<16#A/utf8, Rest/bitstring>>, Stream, Pos, Len, State, Acc, DTD, External) ->
    ?FUNCTION_NAME(Rest, Stream, Pos, Len + 1, State, Acc, DTD, External);
?FUNCTION_NAME(<<16#FFFE/utf8, _Rest/bitstring>>, _, _, _, State, _Acc, _DTD, _External) ->
    fatal_error({invalid_character, [16#FFFE]}, State) ;
?FUNCTION_NAME(<<16#FFFF/utf8, _Rest/bitstring>>, _, _, _, State, _Acc, _DTD, _External) ->
    fatal_error({invalid_character, [16#FFFF]}, State) ;
?FUNCTION_NAME(<<Char/utf8, Rest/bitstring>>, Stream, Pos, Len, State, Acc, DTD, External)  ->
    if
        Char < 16#20 ->
            fatal_error({invalid_character, [Char]}, State);
        Char < 16#80 ->
            ?FUNCTION_NAME(Rest, Stream, Pos, Len + 1, State, Acc, DTD, External);
        Char < 16#800 ->
            ?FUNCTION_NAME(Rest, Stream, Pos, Len + 2, State, Acc, DTD, External);
        Char < 16#10000 ->
            ?FUNCTION_NAME(Rest, Stream, Pos, Len + 3, State, Acc, DTD, External);
        true ->
            ?FUNCTION_NAME(Rest, Stream, Pos, Len + 4, State, Acc, DTD, External)
    end;
?FUNCTION_NAME(<<>>, Stream, Pos, Len, State, Acc, DTD, External) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    {Stream1, State1} = cf(State),
    ?FUNCTION_NAME(Stream1, Stream1, 0, 0, State1, Acc1, DTD, External);
?FUNCTION_NAME(Bytes = <<30:5, _:3>>, Stream, Pos, Len, State, Acc, DTD, External) -> ?CHARPARTFUNX(3);
?FUNCTION_NAME(Bytes = <<30:5, _:3, 2:2, _:6>>, Stream, Pos, Len, State, Acc, DTD, External) -> ?CHARPARTFUNX(2);
?FUNCTION_NAME(Bytes = <<14:4, _:4>>, Stream, Pos, Len, State, Acc, DTD, External) -> ?CHARPARTFUNX(2);
?FUNCTION_NAME(Bytes = <<30:5, _:3, 2:2, _:6, 2:2, _:6>>, Stream, Pos, Len, State, Acc, DTD, External) -> ?CHARPARTFUNX(1);
?FUNCTION_NAME(Bytes = <<14:4, _:4, 2:2, _:6>>, Stream, Pos, Len, State, Acc, DTD, External) -> ?CHARPARTFUNX(1);
?FUNCTION_NAME(Bytes = <<6:3, _:5>>, Stream, Pos, Len, State, Acc, DTD, External) -> ?CHARPARTFUNX(1);
?FUNCTION_NAME(Bytes, Stream, Pos, Len, State, Acc, DTD, External) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    {to_binary(Acc1), Bytes, Stream, Pos + Len, State, DTD, External}
).

-define(ONECHARD,
?FUNCTION_NAME(<<16#9/utf8, Rest/bitstring>>, Stream, Pos, Len, State, Acc, DTD) ->
    ?FUNCTION_NAME(Rest, Stream, Pos, Len + 1, State, Acc, DTD);
?FUNCTION_NAME(<<16#A/utf8, Rest/bitstring>>, Stream, Pos, Len, State, Acc, DTD) ->
    ?FUNCTION_NAME(Rest, Stream, Pos, Len + 1, State, Acc, DTD);
?FUNCTION_NAME(<<16#FFFE/utf8, _Rest/bitstring>>, _, _, _, State, _Acc, _DTD) ->
    fatal_error({invalid_character, [16#FFFE]}, State) ;
?FUNCTION_NAME(<<16#FFFF/utf8, _Rest/bitstring>>, _, _, _, State, _Acc, _DTD) ->
    fatal_error({invalid_character, [16#FFFF]}, State) ;
?FUNCTION_NAME(<<Char/utf8, Rest/bitstring>>, Stream, Pos, Len, State, Acc, DTD)  ->
    if
        Char < 16#20 ->
            fatal_error({invalid_character, [Char]}, State);
        Char < 16#80 ->
            ?FUNCTION_NAME(Rest, Stream, Pos, Len + 1, State, Acc, DTD);
        Char < 16#800 ->
            ?FUNCTION_NAME(Rest, Stream, Pos, Len + 2, State, Acc, DTD);
        Char < 16#10000 ->
            ?FUNCTION_NAME(Rest, Stream, Pos, Len + 3, State, Acc, DTD);
        true ->
            ?FUNCTION_NAME(Rest, Stream, Pos, Len + 4, State, Acc, DTD)
    end;
?FUNCTION_NAME(<<>>, Stream, Pos, Len, State, Acc, DTD) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    {Stream1, State1} = cf(State),
    ?FUNCTION_NAME(Stream1, Stream1, 0, 0, State1, Acc1, DTD);
?FUNCTION_NAME(Bytes = <<30:5, _:3>>, Stream, Pos, Len, State, Acc, DTD) -> ?CHARPARTFUND(3);
?FUNCTION_NAME(Bytes = <<30:5, _:3, 2:2, _:6>>, Stream, Pos, Len, State, Acc, DTD) -> ?CHARPARTFUND(2);
?FUNCTION_NAME(Bytes = <<14:4, _:4>>, Stream, Pos, Len, State, Acc, DTD) -> ?CHARPARTFUND(2);
?FUNCTION_NAME(Bytes = <<30:5, _:3, 2:2, _:6, 2:2, _:6>>, Stream, Pos, Len, State, Acc, DTD) -> ?CHARPARTFUND(1);
?FUNCTION_NAME(Bytes = <<14:4, _:4, 2:2, _:6>>, Stream, Pos, Len, State, Acc, DTD) -> ?CHARPARTFUND(1);
?FUNCTION_NAME(Bytes = <<6:3, _:5>>, Stream, Pos, Len, State, Acc, DTD) -> ?CHARPARTFUND(1);
?FUNCTION_NAME(Bytes, Stream, Pos, Len, State, Acc, DTD) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    {to_binary(Acc1), Bytes, Stream, Pos + Len, State, DTD}
).

%%----------------------------------------------------------------------
%% [4a] NameChar
%%      NameStartChar | "-" | "." | [0-9] | #xB7 | [#x0300-#x036F] | [#x203F-#x2040]
%%----------------------------------------------------------------------
-define(ONENAMECHAR,
?FUNCTION_NAME(<<Char/utf8, Rest/bitstring>>, Stream, Pos, Len, State, Acc)
    when
    Char == $-;
    Char == $.;
    Char >= $0, Char =< $9;
    Char == $:;
    Char >= $A, Char =< $Z;
    Char == $_;
    Char >= $a, Char =< $z

    ->
    ?FUNCTION_NAME(Rest, Stream, Pos, Len + 1, State, Acc);
?FUNCTION_NAME(<<Char/utf8, Rest/bitstring>>, Stream, Pos, Len, State, Acc)
    when
    Char == 16#B7;
    Char >= 16#C0, Char =< 16#D6;
    Char >= 16#D8, Char =< 16#F6;
    Char >= 16#F8, Char =< 16#37D;
    Char >= 16#37F, Char =< 16#7FF
    ->
    ?FUNCTION_NAME(Rest, Stream, Pos, Len + 2, State, Acc);
?FUNCTION_NAME(<<Char/utf8, Rest/bitstring>>, Stream, Pos, Len, State, Acc)
    when
    Char >= 16#800, Char =< 16#1FFF;
    Char >= 16#200C, Char =< 16#200D;
    Char >= 16#203F, Char =< 16#2040;
    Char >= 16#2070, Char =< 16#218F;
    Char >= 16#2C00, Char =< 16#2FEF;
    Char >= 16#3001, Char =< 16#D7FF;
    Char >= 16#F900, Char =< 16#FDCF;
    Char >= 16#FDF0, Char =< 16#FFFD
    ->
    ?FUNCTION_NAME(Rest, Stream, Pos, Len + 3, State, Acc);
?FUNCTION_NAME(<<Char/utf8, Rest/bitstring>>, Stream, Pos, Len, State, Acc)
    when Char >= 16#10000, Char =< 16#EFFFF ->
    ?FUNCTION_NAME(Rest, Stream, Pos, Len + 4, State, Acc);
?FUNCTION_NAME(Bytes = <<30:5, _:3>>, Stream, Pos, Len, State, Acc) -> ?NAMECHARPARTFUN(3);
?FUNCTION_NAME(Bytes = <<30:5, _:3, 2:2, _:6>>, Stream, Pos, Len, State, Acc) -> ?NAMECHARPARTFUN(2);
?FUNCTION_NAME(Bytes = <<14:4, _:4>>, Stream, Pos, Len, State, Acc) -> ?NAMECHARPARTFUN(2);
?FUNCTION_NAME(Bytes = <<30:5, _:3, 2:2, _:6, 2:2, _:6>>, Stream, Pos, Len, State, Acc) -> ?NAMECHARPARTFUN(1);
?FUNCTION_NAME(Bytes = <<14:4, _:4, 2:2, _:6>>, Stream, Pos, Len, State, Acc) -> ?NAMECHARPARTFUN(1);
?FUNCTION_NAME(Bytes = <<6:3, _:5>>, Stream, Pos, Len, State, Acc) -> ?NAMECHARPARTFUN(1);
?FUNCTION_NAME(Bytes, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    {to_binary(Acc1), Bytes, Stream, Pos + Len, State}
).

%%----------------------------------------------------------------------
%% [13] PubidChar ::= #x20 | #xD | #xA | [a-zA-Z0-9] | [-'()+,./:=?;!*#@$_%]
%%----------------------------------------------------------------------
-define(ONEPUBIDCHAR,
?FUNCTION_NAME(<<Char/utf8, Rest/bitstring>>, Stream, Pos, Len, State, Acc)
    when
    Char == 16#A;
    Char == 16#D;
    Char == 16#20;
    Char == 16#21;
    Char >= 16#23, Char =< 16#25;
    Char >= 16#27, Char =< 16#3B;
    Char == 16#3D;
    Char >= 16#3F, Char =< 16#5A;
    Char == 16#5F;
    Char >= 16#61, Char =< 16#7A
    ->
    ?FUNCTION_NAME(Rest, Stream, Pos, Len + 1, State, Acc);
?FUNCTION_NAME(<<Char/utf8, _/bitstring>>, _, _, _, _State, _) ->
    fatal_error(invalid_pubid_character, Char)
).

%%----------------------------------------------------------------------
%% Consume whitespace characters
%% params:  State
%% returns: {NewPos, NewState} | {error, non_whitespace}
%% [3] S ::= (#x20 | #x9 | #xD | #xA)+
%%----------------------------------------------------------------------
consume_s(<<Char/utf8, Rest/bitstring>>, Stream, Pos, State) when
    Char == 16#20; Char == 16#9; Char == 16#D; Char == 16#A
->
    maybe_consume_s(Rest, Stream, Pos + 1, State, true);
consume_s(<<>>, _, _, State) ->
    {Stream1, State1} = cf(State),
    consume_s(Stream1, Stream1, 0, State1);
consume_s(_, _, _, State) ->
    fatal_error(missing_whitespace, State).

consume_s(<<"%", Bytes/bitstring>>, Stream, Pos, State, DTD) ->
    {PERef, Bytes1, _, _, State1} = parse_PEReference(Bytes, Stream, Pos + 1, State),
    case resolve_parameter_entity(PERef, DTD, State1, true, true) of
        {replacement_text, ReplacementText} ->
            RTState = replacement_text_state(Bytes1, State1),
            consume_s(ReplacementText, ReplacementText, 0, RTState, DTD)
    end;
consume_s(?MATCH, _DTD) ->
    consume_s(?MATCH).

maybe_consume_s(?MATCH) -> maybe_consume_s(?MATCH, false).

maybe_consume_s(<<Char/utf8, Rest/bitstring>>, Stream, Pos, State, _) when
    Char == 16#20; Char == 16#9; Char == 16#D; Char == 16#A
->
    maybe_consume_s(Rest, Stream, Pos + 1, State, true);
maybe_consume_s(<<>>, Stream, Pos, State, Found) ->
    case cf(State) of
        {no_bytes, State1} -> {Found, <<>>, Stream, Pos, State1};
        {Stream1, State1} -> maybe_consume_s(Stream1, Stream1, 0, State1, Found)
    end;
maybe_consume_s(Part, Stream, Pos, State, Found) ->
    {Found, Part, Stream, Pos, State}.

maybe_consume_dtd_s(?MATCH, DTD) -> maybe_consume_dtd_s(?MATCH, DTD, false).

maybe_consume_dtd_s(<<Char/utf8, Rest/bitstring>>, Stream, Pos, State, DTD, _) when
    Char == 16#20; Char == 16#9; Char == 16#D; Char == 16#A
->
    maybe_consume_dtd_s(Rest, Stream, Pos + 1, State, DTD, true);
maybe_consume_dtd_s(<<"%", Bytes/bitstring>>, Stream, Pos, State, DTD, Found) ->
    {PERef, Bytes1, _, _, State1} = parse_PEReference(Bytes, Stream, Pos + 1, State),
    case resolve_parameter_entity(PERef, DTD, State1, true, true) of
        {replacement_text, ReplacementText} ->
            RTState = replacement_text_state(Bytes1, State1),
            maybe_consume_dtd_s(ReplacementText, ReplacementText, 0, RTState, DTD, Found)
    end;
maybe_consume_dtd_s(<<>>, Stream, Pos, State, DTD, Found) ->
    case cf(State) of
        {no_bytes, State1} -> {Found, <<>>, Stream, Pos, State1};
        {Stream1, State1} -> maybe_consume_dtd_s(Stream1, Stream1, 0, State1, DTD, Found)
    end;
maybe_consume_dtd_s(Part, Stream, Pos, State, _DTD, Found) ->
    {Found, Part, Stream, Pos, State}.

%%----------------------------------------------------------------------
%% Parse Name (does not split prefix and local parts like NCName)
%% params:  State
%% returns: {Name, NewState}
%% [4] NameStartChar
%%     ":" | [A-Z] | "_" | [a-z] | [#xC0-#xD6] | [#xD8-#xF6] | [#xF8-#x2FF] | [#x370-#x37D]
%%     | [#x37F-#x1FFF] | [#x200C-#x200D] | [#x2070-#x218F] | [#x2C00-#x2FEF]
%%     | [#x3001-#xD7FF] | [#xF900-#xFDCF] | [#xFDF0-#xFFFD] | [#x10000-#xEFFFF]
%% [5] Name ::= NameStartChar (NameChar)*
%%----------------------------------------------------------------------

parse_Name(<<Char/utf8, Rest/bitstring>>, Stream, Pos, State) ->
    if
        Char == $:; Char >= $A, Char =< $Z; Char == $_; Char >= $a, Char =< $z ->
            parse_Name(Rest, Stream, Pos, 1, State, []);
        Char >= 16#C0, Char =< 16#D6;
        Char >= 16#D8, Char =< 16#F6;
        Char >= 16#F8, Char =< 16#2FF;
        Char >= 16#370, Char =< 16#37D;
        Char >= 16#37F, Char =< 16#7FF ->
            parse_Name(Rest, Stream, Pos, 2, State, []);
        Char >= 16#800, Char =< 16#1FFF;
        Char >= 16#200C, Char =< 16#200D;
        Char >= 16#2070, Char =< 16#218F;
        Char >= 16#2C00, Char =< 16#2FEF;
        Char >= 16#3001, Char =< 16#D7FF;
        Char >= 16#F900, Char =< 16#FDCF;
        Char >= 16#FDF0, Char =< 16#FFFD ->
            parse_Name(Rest, Stream, Pos, 3, State, []);
        Char >= 16#10000, Char =< 16#EFFFF ->
            parse_Name(Rest, Stream, Pos, 4, State, []);
        true ->
            fatal_error(bad_name, {[Char], State})
    end;
parse_Name(<<>>, _, _, State) ->
    {Stream1, State1} = cf(State),
    parse_Name(Stream1, Stream1, 0, State1);
parse_Name(Bytes = <<30:5, _:3>>, _, _, State) ->
    ?FSTNAMECHARPARTFUN(3, parse_Name);
parse_Name(Bytes = <<30:5, _:3, 2:2, _:6>>, _, _, State) ->
    ?FSTNAMECHARPARTFUN(2, parse_Name);
parse_Name(Bytes = <<14:4, _:4>>, _, _, State) ->
    ?FSTNAMECHARPARTFUN(2, parse_Name);
parse_Name(Bytes = <<30:5, _:3, 2:2, _:6, 2:2, _:6>>, _, _, State) ->
    ?FSTNAMECHARPARTFUN(1, parse_Name);
parse_Name(Bytes = <<14:4, _:4, 2:2, _:6>>, _, _, State) ->
    ?FSTNAMECHARPARTFUN(1, parse_Name);
parse_Name(Bytes = <<6:3, _:5>>, _, _, State) ->
    ?FSTNAMECHARPARTFUN(1, parse_Name);
parse_Name(Bytes, _, _, State) ->
    fatal_error(bad_name, {Bytes, State}).

parse_Name(<<>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    {Stream1, State1} = cf(State),
    parse_Name(Stream1, Stream1, 0, 0, State1, Acc1);
?ONENAMECHAR.

%%----------------------------------------------------------------------
%% [67] Reference ::= EntityRef | CharRef
%% & char is removed
%% [66] CharRef ::= '&#' [0-9]+ ';' | '&#x' [0-9a-fA-F]+ ';'
%% [WFC: Legal Character]
%% [68] EntityRef ::= '&' Name ';'
%% [WFC: Entity Declared]
%% [WFC: Parsed Entity]
%% [WFC: No Recursion]
%% can be EntityRef | CharRef
%%----------------------------------------------------------------------
parse_Reference(<<>>, _, _, #ys_state{in_replacement = true} = State, _Type) ->
    fatal_error(unbalanced_reference, State);
parse_Reference(<<>>, _, _, State, Type) ->
    {Stream1, State1} = cf(State),
    parse_Reference(Stream1, Stream1, 0, State1, Type);
parse_Reference(<<$#>>, _, _, State, Type) ->
    {Stream1, State1} = cf(State),
    parse_Reference_lb(Stream1, Stream1, State1, Type);
parse_Reference(<<$#, $x, Rest/bitstring>>, Stream, Pos, State, _Type) ->
    parse_Reference_hex(Rest, Stream, Pos + 2, 0, State, []);
parse_Reference(<<$#, Rest/bitstring>>, Stream, Pos, State, _Type) ->
    parse_Reference_dec(Rest, Stream, Pos + 1, 0, State, []);
parse_Reference(?MATCH, Type) ->
    parse_Reference_name(?MATCH, Type).

parse_Reference_lb(<<>>, _, #ys_state{in_replacement = true} = State, _Type) ->
    fatal_error(unbalanced_reference, State);
parse_Reference_lb(<<>>, _, State, Type) ->
    {Stream1, State1} = cf(State),
    parse_Reference_lb(Stream1, Stream1, State1, Type);
parse_Reference_lb(<<$x, Rest/bitstring>>, Stream, State, _Type) ->
    parse_Reference_hex(Rest, Stream, 1, 0, State, []);
parse_Reference_lb(Bytes, Stream, State, _Type) ->
    parse_Reference_dec(Bytes, Stream, 0, 0, State, []).

% hex parse until ';' return char
parse_Reference_hex(<<>>, _, _, _, #ys_state{in_replacement = true} = State, _) ->
    fatal_error(unbalanced_reference, State);
parse_Reference_hex(<<>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    {Stream1, State1} = cf(State),
    parse_Reference_hex(Stream1, Stream1, 0, 0, State1, Acc1);
parse_Reference_hex(<<$;, Rest/bitstring>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    try
        case binary_to_integer(to_binary(Acc1), 16) of
            Int when
                Int == 16#9;
                Int == 16#A;
                Int == 16#D;
                Int >= 16#20, Int =< 16#D7FF;
                Int >= 16#E000, Int =< 16#FFFD;
                Int >= 16#10000, Int =< 16#10FFFF
            ->
                {{hex, <<Int/utf8>>}, Rest, Stream, Pos + Len + 1, State}
        end
    catch
        _:_ ->
            fatal_error(bad_charref, State)
    end;
parse_Reference_hex(<<Char/utf8, Rest/bitstring>>, Stream, Pos, Len, State, Acc) when
    Char >= $0, Char =< $9; Char >= $a, Char =< $f; Char >= $A, Char =< $F
->
    parse_Reference_hex(Rest, Stream, Pos, Len + 1, State, Acc);
parse_Reference_hex(_, _, _, _, State, _) ->
    fatal_error(bad_charref, State).

% decimal parse until ';' return char
parse_Reference_dec(<<>>, _, _, _, #ys_state{in_replacement = true} = State, _) ->
    fatal_error(unbalanced_reference, State);
parse_Reference_dec(<<>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    {Stream1, State1} = cf(State),
    parse_Reference_dec(Stream1, Stream1, 0, 0, State1, Acc1);
parse_Reference_dec(<<$;/utf8, Rest/bitstring>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    try
        case binary_to_integer(to_binary(Acc1)) of
            Int when
                Int == 16#9;
                Int == 16#A;
                Int == 16#D;
                Int >= 16#20, Int =< 16#D7FF;
                Int >= 16#E000, Int =< 16#FFFD;
                Int >= 16#10000, Int =< 16#10FFFF
            ->
                {{dec, <<Int/utf8>>}, Rest, Stream, Pos + Len + 1, State}
        end
    catch
        _:_ ->
            fatal_error(bad_charref, {Acc1, State})
    end;
parse_Reference_dec(<<Char/utf8, Rest/bitstring>>, Stream, Pos, Len, State, Acc) when
    Char >= $0, Char =< $9
->
    parse_Reference_dec(Rest, Stream, Pos, Len + 1, State, Acc);
parse_Reference_dec(_, _, _, _, State, _) ->
    fatal_error(bad_charref, State).

% Name parse until ';' return Name
parse_Reference_name(?MATCH, Type) ->
    case parse_Name(?MATCH) of
        {Name, <<$;/utf8, Bytes1/bitstring>>, Stream1, Pos1, State1} ->
            NameValue = resolve_general_entity(Name, State1, Type),
            {NameValue, Bytes1, Stream1, Pos1 + 1, State1};
        {_, _, _, _, State1} ->
            fatal_error(bad_charref, State1)
    end.

%%----------------------------------------------------------------------
%% Parse a Comment, leading '<!--' already removed
%% params:  State
%% returns: {Comment, NewState} | NewState (when ignoring)
%% [15] Comment ::= '<!--' ((Char - '-') | ('-' (Char - '-')))* '-->'
%%----------------------------------------------------------------------
parse_Comment(?MATCH) ->
    {Text, ?MATCH1} = parse_Comment(Bytes, Stream, Pos, 0, State, []),
    case State#ys_state.comments of
        _ when hd(State#ys_state.position) == ?dtd -> {{comment, Text}, ?MATCH1};
        false -> State1;
        true -> yaccety_sax:event_comment(Text, set_state_pos(State1, Bytes1))
    end.

consume_Comment(?MATCH) ->
    {_, ?MATCH1} = parse_Comment(Bytes, Stream, Pos, 0, State, []),
    {?MATCH1}.

parse_Comment(<<$\r, $\n, Rest/bitstring>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    Acc2 = ?APPEND(<<$\n>>, Acc1),
    ?FUNCTION_NAME(Rest, Stream, Pos + Len + 2, 0, State, Acc2);
?FUNCTION_NAME(<<$\r>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    Acc2 = ?APPEND(<<$\n>>, Acc1),
    case cf(State) of
        {<<$\n, Bytes1/bitstring>> = Stream1, State1} ->
            ?FUNCTION_NAME(Bytes1, Stream1, 1, 0, State1, Acc2);
        {Stream1, State1} ->
            ?FUNCTION_NAME(Stream1, Stream1, 0, 0, State1, Acc2)
    end;
parse_Comment(<<$\r, Rest/bitstring>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    Acc2 = ?APPEND(<<$\n>>, Acc1),
    ?FUNCTION_NAME(Rest, Stream, Pos + Len + 1, 0, State, Acc2);
parse_Comment(<<>>, _, _, _, #ys_state{in_replacement = true} = State, _) ->
    fatal_error(unbalanced_reference, State);
parse_Comment(<<>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    {Stream1, State1} = cf(State),
    parse_Comment(Stream1, Stream1, 0, 0, State1, Acc1);
parse_Comment(<<$-/utf8, $-/utf8, $>/utf8, Rest/bitstring>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    {Acc1, Rest, Stream, Pos + Len + 3, State};
parse_Comment(<<$-/utf8, $-/utf8>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    {Stream1, State1} = cf(State),
    parse_Comment_dashdash(Stream1, Stream1, State1, Acc1);
parse_Comment(<<$-/utf8, $-/utf8, _/bitstring>>, _, _, _, State, _Acc) ->
    fatal_error(bad_comment, State);
parse_Comment(<<$-/utf8>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    {Stream1, State1} = cf(State),
    parse_Comment_dash(Stream1, Stream1, State1, Acc1);
?ONECHAR.

parse_Comment_dashdash(<<$>/utf8, Rest/bitstring>>, Stream, State, Acc) ->
    {Acc, Rest, Stream, 1, State};
parse_Comment_dashdash(_, _, State, _) ->
    fatal_error(bad_comment, State).

parse_Comment_dash(<<$-/utf8, $>/utf8, Rest/bitstring>>, Stream, State, Acc) ->
    {Acc, Rest, Stream, 2, State};
parse_Comment_dash(<<$-/utf8>>, _, State, Acc) ->
    {Stream1, State1} = cf(State),
    parse_Comment_dashdash(Stream1, Stream1, State1, Acc);
parse_Comment_dash(<<$-/utf8, _/bitstring>>, _, State, _Acc) ->
    fatal_error(bad_comment, State);
parse_Comment_dash(Bytes, Stream, State, Acc) ->
    Acc1 = ?APPEND(<<$-/utf8>>, Acc),
    parse_Comment(Bytes, Stream, 0, 0, State, Acc1).

%%----------------------------------------------------------------------
%% Parse a processing-instruction, leading '<?' already removed
%% params:  State
%% returns: {PI, NewState} | NewState (when ignoring)
%% [16] PI       ::= '<?' PITarget (S (Char* - (Char* '?>' Char*)))? '?>'
%% [17] PITarget ::= Name - (('X' | 'x') ('M' | 'm') ('L' | 'l'))
%%----------------------------------------------------------------------

check_pi_name(<<X, M, L>>, State) when
    X == $x orelse X == $X, M == $m orelse M == $M, L == $l orelse L == $L
->
    fatal_error(bad_pi, State);
check_pi_name(_, _) ->
    ok.

parse_PI(?MATCH) ->
    {Name, ?MATCH1} = parse_NCName(?MATCH),
    ok = check_pi_name(Name, State1),
    {IsWs, ?MATCH2} = maybe_consume_s(?MATCH1),
    {Data, ?MATCH3} = parse_PI_data(Bytes2, Stream2, Pos2, 0, State2, []),
    case IsWs of
        false when Data =/= <<>> -> fatal_error(bad_pi, State);
        _ when State#ys_state.proc_inst == false -> State3;
        _ when hd(State#ys_state.position) == ?dtd -> {{pi, Name, Data}, ?MATCH3};
        _ -> yaccety_sax:event_processingInstruction(Name, Data, set_state_pos(State3, Bytes3))
    end.

parse_PI_data(<<$\r, $\n, Rest/bitstring>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    Acc2 = ?APPEND(<<$\n>>, Acc1),
    ?FUNCTION_NAME(Rest, Stream, Pos + Len + 2, 0, State, Acc2);
?FUNCTION_NAME(<<$\r>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    Acc2 = ?APPEND(<<$\n>>, Acc1),
    case cf(State) of
        {<<$\n, Bytes1/bitstring>> = Stream1, State1} ->
            ?FUNCTION_NAME(Bytes1, Stream1, 1, 0, State1, Acc2);
        {Stream1, State1} ->
            ?FUNCTION_NAME(Stream1, Stream1, 0, 0, State1, Acc2)
    end;
parse_PI_data(<<$\r, Rest/bitstring>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    Acc2 = ?APPEND(<<$\n>>, Acc1),
    ?FUNCTION_NAME(Rest, Stream, Pos + Len + 1, 0, State, Acc2);
parse_PI_data(<<>>, _Stream, _Pos, _Len, #ys_state{in_replacement = true} = State, _Acc) ->
    fatal_error(unbalanced_reference, State);
parse_PI_data(<<>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    {Stream1, State1} = cf(State),
    parse_PI_data(Stream1, Stream1, 0, 0, State1, Acc1);
parse_PI_data(<<$?, $>, Rest/bitstring>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    {to_binary(Acc1), Rest, Stream, Pos + 2 + Len, State};
parse_PI_data(<<$?>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    {Stream1, State1} = cf(State),
    parse_PI_data_qm(Stream1, Stream1, State1, Acc1);
?ONECHAR.

parse_PI_data_qm(<<$>/utf8, Rest/bitstring>>, Stream, State, Acc) ->
    {to_binary(Acc), Rest, Stream, 1, State};
parse_PI_data_qm(Bytes, Stream, State, Acc) ->
    Acc1 = ?APPEND(<<$?>>, Acc),
    parse_PI_data(Bytes, Stream, 0, 0, State, Acc1).

%%----------------------------------------------------------------------
%% Parse character data. In content, everything that is not
%%   (element | Reference | CDSect | PI | Comment)
%% params:  State
%% returns: {CharData, IsWs, NewState}
%% [14] CharData ::= [^<&]* - ([^<&]* ']]>' [^<&]*)
%%----------------------------------------------------------------------
parse_CharData(?MATCH) ->
    {{IsWs, Text}, Bytes1, _, _, State1} = parse_CharData_ws(Bytes, Stream, Pos, 0, State, []),
    IsIgnorable = false,
    IsCData = false,
    case State#ys_state.whitespace of
        false when IsWs == true ->
            set_state_pos(State1, Bytes1);
        _ ->
            yaccety_sax:event_characters(
                to_binary(Text),
                IsCData,
                IsIgnorable,
                IsWs,
                set_state_pos(State1, Bytes1)
            )
    end.

parse_CharData(?MATCH, Acc) ->
    {{IsWs, Text}, Bytes1, _, _, State1} = parse_CharData(Bytes, Stream, Pos, 0, State, Acc),
    IsIgnorable = false,
    IsCData = false,
    yaccety_sax:event_characters(
        to_binary(Text), IsCData, IsIgnorable, IsWs, set_state_pos(State1, Bytes1)
    ).

parse_CharData(
    <<$\r, $\n, Rest/bitstring>>,
    Stream,
    Pos,
    Len,
    #ys_state{in_replacement = false} = State,
    Acc
) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    Acc2 = ?APPEND(<<$\n>>, Acc1),
    parse_CharData(Rest, Stream, Pos + Len + 2, 0, State, Acc2);
?FUNCTION_NAME(<<$\r>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    Acc2 = ?APPEND(<<$\n>>, Acc1),
    case cf(State) of
        {<<$\n, Bytes1/bitstring>> = Stream1, State1} ->
            ?FUNCTION_NAME(Bytes1, Stream1, 1, 0, State1, Acc2);
        {Stream1, State1} ->
            ?FUNCTION_NAME(Stream1, Stream1, 0, 0, State1, Acc2)
    end;
parse_CharData(
    <<$\r, Rest/bitstring>>,
    Stream,
    Pos,
    Len,
    #ys_state{in_replacement = false} = State,
    Acc
) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    Acc2 = ?APPEND(<<$\n>>, Acc1),
    parse_CharData(Rest, Stream, Pos + Len + 1, 0, State, Acc2);
parse_CharData(<<$\r, Rest/bitstring>>, Stream, Pos, Len, State, Acc) ->
    parse_CharData(Rest, Stream, Pos, Len + 1, State, Acc);
parse_CharData(Bytes = <<$</utf8, _/bitstring>>, Stream, Pos, Len, State, Acc) ->
    Text = ?ACC(Stream, Pos, Len, Acc),
    {{false, Text}, Bytes, Stream, Pos + Len, State};
parse_CharData(<<$&/utf8, Rest/bitstring>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    case parse_Reference(Rest, Stream, Pos + Len + 1, State, text) of
        {{Name, {replacement_text, Ref}}, Bytes1, _, _, State1} ->
            RTState = replacement_text_state(Bytes1, State1, Name),
            ?FUNCTION_NAME(Ref, Ref, 0, 0, RTState, Acc1);
        {{Type, Ref}, ?MATCH1} when Type == gen; Type == dec; Type == hex ->
            Acc2 = ?APPEND(Ref, Acc1),
            ?FUNCTION_NAME(Bytes1, Stream1, Pos1, 0, State1, Acc2);
        {{Name, Ref}, Bytes1, _, _, State1} ->
            RTState = replacement_text_state(Bytes1, State1, Name),
            ?FUNCTION_NAME(Ref, Ref, 0, 0, RTState, Acc1)
    end;
parse_CharData(<<"]]>"/utf8, _/bitstring>>, _, _, _, State, _Acc) ->
    fatal_error(bad_char_data, State);
parse_CharData(<<"]]"/utf8>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    parse_CharData_bb(cf(State), Acc1);
parse_CharData(<<"]"/utf8>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    parse_CharData_b(cf(State), Acc1);
?ONECHAR.

parse_CharData_bb({<<$>, _/bitstring>>, State}, _Acc) ->
    fatal_error(bad_char_data, State);
parse_CharData_bb({<<"]>", _/bitstring>>, State}, _Acc) ->
    fatal_error(bad_char_data, State);
parse_CharData_bb({<<"]">>, State}, Acc) ->
    Acc1 = ?APPEND(<<"]">>, Acc),
    parse_CharData_bb(cf(State), Acc1);
parse_CharData_bb({Bytes, State}, Acc) ->
    Acc1 = ?APPEND(<<"]]">>, Acc),
    parse_CharData(Bytes, Bytes, 0, 0, State, Acc1).

parse_CharData_b({<<"]>", _/bitstring>>, State}, _Acc) ->
    fatal_error(bad_char_data, State);
parse_CharData_b({<<"]">>, State}, Acc) ->
    parse_CharData_bb(cf(State), Acc);
parse_CharData_b({Bytes, State}, Acc) ->
    Acc1 = ?APPEND(<<"]">>, Acc),
    parse_CharData(Bytes, Bytes, 0, 0, State, Acc1).

parse_CharData_ws(<<$\s, $\s, $\s, $\s, Rest/bitstring>>, Stream, Pos, Len, State, Acc) ->
    parse_CharData_ws(Rest, Stream, Pos, Len + 4, State, Acc);
parse_CharData_ws(<<$\t, $\t, $\t, $\t, Rest/bitstring>>, Stream, Pos, Len, State, Acc) ->
    parse_CharData_ws(Rest, Stream, Pos, Len + 4, State, Acc);
parse_CharData_ws(<<$\s, $\s, $\s, Rest/bitstring>>, Stream, Pos, Len, State, Acc) ->
    parse_CharData_ws(Rest, Stream, Pos, Len + 3, State, Acc);
parse_CharData_ws(<<$\t, $\t, $\t, Rest/bitstring>>, Stream, Pos, Len, State, Acc) ->
    parse_CharData_ws(Rest, Stream, Pos, Len + 3, State, Acc);
parse_CharData_ws(<<$\s, $\s, Rest/bitstring>>, Stream, Pos, Len, State, Acc) ->
    parse_CharData_ws(Rest, Stream, Pos, Len + 2, State, Acc);
parse_CharData_ws(<<$\t, $\t, Rest/bitstring>>, Stream, Pos, Len, State, Acc) ->
    parse_CharData_ws(Rest, Stream, Pos, Len + 2, State, Acc);
parse_CharData_ws(
    <<$\r, $\n, Rest/bitstring>>,
    Stream,
    Pos,
    Len,
    #ys_state{in_replacement = false} = State,
    Acc
) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    Acc2 = ?APPEND(<<$\n>>, Acc1),
    parse_CharData_ws(Rest, Stream, Pos + Len + 2, 0, State, Acc2);
?FUNCTION_NAME(<<$\r>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    Acc2 = ?APPEND(<<$\n>>, Acc1),
    case cf(State) of
        {<<$\n, Bytes1/bitstring>> = Stream1, State1} ->
            ?FUNCTION_NAME(Bytes1, Stream1, 1, 0, State1, Acc2);
        {Stream1, State1} ->
            ?FUNCTION_NAME(Stream1, Stream1, 0, 0, State1, Acc2)
    end;
parse_CharData_ws(
    <<$\r, Rest/bitstring>>,
    Stream,
    Pos,
    Len,
    #ys_state{in_replacement = false} = State,
    Acc
) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    Acc2 = ?APPEND(<<$\n>>, Acc1),
    parse_CharData_ws(Rest, Stream, Pos + 1, 0, State, Acc2);
parse_CharData_ws(<<$\r, Rest/bitstring>>, Stream, Pos, Len, State, Acc) ->
    parse_CharData_ws(Rest, Stream, Pos, Len + 1, State, Acc);
parse_CharData_ws(<<Char, Rest/bitstring>>, Stream, Pos, Len, State, Acc) when ?WS(Char) ->
    parse_CharData_ws(Rest, Stream, Pos, Len + 1, State, Acc);
parse_CharData_ws(Bytes = <<$<, _/bitstring>>, Stream, Pos, Len, State, Acc) ->
    Text = ?ACC(Stream, Pos, Len, Acc),
    {{true, Text}, Bytes, Stream, Pos + Len, State};
parse_CharData_ws(<<$&, Rest/bitstring>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    case parse_Reference(Rest, Stream, Pos + Len + 1, State, text) of
        {{Name, {replacement_text, Ref}}, Bytes1, _, _, State1} ->
            RTState = replacement_text_state(Bytes1, State1, Name),
            ?FUNCTION_NAME(Ref, Ref, 0, 0, RTState, Acc1);
        {{Type, <<Char>> = Ref}, ?MATCH1} when Type == dec orelse Type == hex, ?WS(Char) ->
            Acc2 = ?APPEND(Ref, Acc1),
            ?FUNCTION_NAME(Bytes1, Stream1, Pos1, 0, State1, Acc2);
        {{Type, Ref}, ?MATCH1} when Type == dec orelse Type == hex ->
            Acc2 = ?APPEND(Ref, Acc1),
            ?FUNCTION_NAME(Bytes1, Stream1, Pos1, 0, State1, Acc2);
        {{gen, Ref}, ?MATCH1} ->
            Acc2 = ?APPEND(Ref, Acc1),
            parse_CharData(Bytes1, Stream1, Pos1, 0, State1, Acc2);
        {{Name, Ref}, Bytes1, _, _, State1} ->
            RTState = replacement_text_state(Bytes1, State1, Name),
            ?FUNCTION_NAME(Ref, Ref, 0, 0, RTState, Acc1)
    end;
parse_CharData_ws(<<>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    {Stream1, State1} = cf(State),
    parse_CharData_ws(Stream1, Stream1, 0, 0, State1, Acc1);
parse_CharData_ws(Bytes, Stream, Pos, Len, State, Acc) ->
    parse_CharData(Bytes, Stream, Pos, Len, State, Acc).

%%----------------------------------------------------------------------
%% [18] CDSect  ::= CDStart CData CDEnd
%% [19] CDStart ::= '<![CDATA['
%% [20] CData   ::= (Char* - (Char* ']]>' Char*))
%% [21] CDEnd   ::= ']]>'
%% Parse CDATA Section. '<![' is already matched.
%% params:  State
%% returns: {CharData, IsWs, NewState}
%%----------------------------------------------------------------------
parse_CDSect(<<"CDATA["/utf8, Rest/bitstring>>, Stream, Pos, State) ->
    parse_CData(Rest, Stream, Pos + 6, State);
parse_CDSect(<<"CDATA"/utf8>>, _, _, State) ->
    {Stream1, State1} = cf(State),
    parse_CDSect_bcdata(Stream1, State1);
parse_CDSect(<<"CDAT"/utf8>>, _, _, State) ->
    {Stream1, State1} = cf(State),
    parse_CDSect_bcdat(Stream1, State1);
parse_CDSect(<<"CDA"/utf8>>, _, _, State) ->
    {Stream1, State1} = cf(State),
    parse_CDSect_bcda(Stream1, State1);
parse_CDSect(<<"CD"/utf8>>, _, _, State) ->
    {Stream1, State1} = cf(State),
    parse_CDSect_bcd(Stream1, State1);
parse_CDSect(<<"C"/utf8>>, _, _, State) ->
    {Stream1, State1} = cf(State),
    parse_CDSect_bc(Stream1, State1);
parse_CDSect(<<>>, _, _, State) ->
    {Stream1, State1} = cf(State),
    parse_CDSect(Stream1, Stream1, 0, State1);
parse_CDSect(_, _, _, State) ->
    fatal_error(bad_cdata, State).

parse_CDSect_bc(<<"DATA["/utf8, Rest/bitstring>> = Stream, State) ->
    parse_CData(Rest, Stream, 5, State);
parse_CDSect_bc(<<"DATA"/utf8>>, State) ->
    {Stream1, State1} = cf(State),
    parse_CDSect_bcdata(Stream1, State1);
parse_CDSect_bc(<<"DAT"/utf8>>, State) ->
    {Stream1, State1} = cf(State),
    parse_CDSect_bcdat(Stream1, State1);
parse_CDSect_bc(<<"DA"/utf8>>, State) ->
    {Stream1, State1} = cf(State),
    parse_CDSect_bcda(Stream1, State1);
parse_CDSect_bc(<<"D"/utf8>>, State) ->
    {Stream1, State1} = cf(State),
    parse_CDSect_bcd(Stream1, State1);
parse_CDSect_bc(_, State) ->
    fatal_error(bad_cdata, State).

parse_CDSect_bcd(<<"ATA["/utf8, Rest/bitstring>> = Stream, State) ->
    parse_CData(Rest, Stream, 4, State);
parse_CDSect_bcd(<<"ATA"/utf8>>, State) ->
    {Stream1, State1} = cf(State),
    parse_CDSect_bcdata(Stream1, State1);
parse_CDSect_bcd(<<"AT"/utf8>>, State) ->
    {Stream1, State1} = cf(State),
    parse_CDSect_bcdat(Stream1, State1);
parse_CDSect_bcd(<<"A"/utf8>>, State) ->
    {Stream1, State1} = cf(State),
    parse_CDSect_bcda(Stream1, State1);
parse_CDSect_bcd(_, State) ->
    fatal_error(bad_cdata, State).

parse_CDSect_bcda(<<"TA["/utf8, Rest/bitstring>> = Stream, State) ->
    parse_CData(Rest, Stream, 3, State);
parse_CDSect_bcda(<<"TA"/utf8>>, State) ->
    {Stream1, State1} = cf(State),
    parse_CDSect_bcdata(Stream1, State1);
parse_CDSect_bcda(<<"T"/utf8>>, State) ->
    {Stream1, State1} = cf(State),
    parse_CDSect_bcdat(Stream1, State1);
parse_CDSect_bcda(_, State) ->
    fatal_error(bad_cdata, State).

parse_CDSect_bcdat(<<"A["/utf8, Rest/bitstring>> = Stream, State) ->
    parse_CData(Rest, Stream, 2, State);
parse_CDSect_bcdat(<<"A"/utf8>>, State) ->
    {Stream1, State1} = cf(State),
    parse_CDSect_bcdata(Stream1, State1);
parse_CDSect_bcdat(_, State) ->
    fatal_error(bad_cdata, State).

parse_CDSect_bcdata(<<"["/utf8, Rest/bitstring>> = Stream, State) ->
    parse_CData(Rest, Stream, 1, State);
parse_CDSect_bcdata(_, State) ->
    fatal_error(bad_cdata, State).

parse_CData(Rest, Stream, Pos, State) ->
    {Text, Bytes1, _, _, State1} = parse_CData(Rest, Stream, Pos, 0, State, []),
    yaccety_sax:event_characters(
        to_binary(Text), true, false, false, set_state_pos(State1, Bytes1)
    ).

parse_CData(<<$\r, $\n, Rest/bitstring>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    Acc2 = ?APPEND(<<$\n>>, Acc1),
    ?FUNCTION_NAME(Rest, Stream, Pos + Len + 2, 0, State, Acc2);
?FUNCTION_NAME(<<$\r>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    Acc2 = ?APPEND(<<$\n>>, Acc1),
    case cf(State) of
        {<<$\n, Bytes1/bitstring>> = Stream1, State1} ->
            ?FUNCTION_NAME(Bytes1, Stream1, 1, 0, State1, Acc2);
        {Stream1, State1} ->
            ?FUNCTION_NAME(Stream1, Stream1, 0, 0, State1, Acc2)
    end;
parse_CData(<<$\r, Rest/bitstring>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    Acc2 = ?APPEND(<<$\n>>, Acc1),
    ?FUNCTION_NAME(Rest, Stream, Pos + Len + 1, 0, State, Acc2);
parse_CData(<<"]]>"/utf8, Rest/bitstring>>, Stream, Pos, Len, State, Acc) ->
    Text = ?ACC(Stream, Pos, Len, Acc),
    {Text, Rest, Stream, Pos + Len + 3, State};
parse_CData(<<"]]"/utf8>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    {Stream1, State1} = cf(State),
    parse_CData_bb(Stream1, State1, Acc1);
parse_CData(<<"]"/utf8>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    {Stream1, State1} = cf(State),
    parse_CData_b(Stream1, State1, Acc1);
parse_CData(<<>>, _, _, _, #ys_state{in_replacement = true} = State, _) ->
    fatal_error(unbalanced_reference, State);
?ONECHAR.

parse_CData_b(<<"]>"/utf8, Rest/bitstring>> = Stream, State, Acc) ->
    {Acc, Rest, Stream, 2, State};
parse_CData_b(<<"]"/utf8>>, State, Acc) ->
    {Stream1, State1} = cf(State),
    parse_CData_bb(Stream1, State1, Acc);
parse_CData_b(<<>>, State, Acc) ->
    {Stream1, State1} = cf(State),
    parse_CData_b(Stream1, State1, Acc);
parse_CData_b(Stream, State, Acc) ->
    Acc1 = ?APPEND(<<"]"/utf8>>, Acc),
    parse_CData(Stream, Stream, 0, 0, State, Acc1).

parse_CData_bb(<<"]"/utf8, Rest/bitstring>>, State, Acc) ->
    Acc1 = ?APPEND(<<"]"/utf8>>, Acc),
    parse_CData_bb(Rest, State, Acc1);
parse_CData_bb(<<">"/utf8, Rest/bitstring>> = Stream, State, Acc) ->
    {Acc, Rest, Stream, 1, State};
parse_CData_bb(<<>>, State, Acc) ->
    {Stream1, State1} = cf(State),
    parse_CData_bb(Stream1, State1, Acc);
parse_CData_bb(Stream, State, Acc) ->
    Acc1 = ?APPEND(<<"]]"/utf8>>, Acc),
    parse_CData(Stream, Stream, 0, 0, State, Acc1).

%%----------------------------------------------------------------------
%% [80] EncodingDecl ::= S 'encoding' Eq ('"' EncName '"' | "'" EncName "'" )
%%----------------------------------------------------------------------
parse_EncodingDecl(?MATCH) ->
    parse_EncodingDeclS(?MATCH).

parse_EncodingDeclS(<<"encoding"/utf8, Rest/bitstring>>, Stream, Pos, State) ->
    parse_EncodingDecl_encoding(Rest, Stream, Pos + 8, State);
parse_EncodingDeclS(<<"encodin"/utf8>>, _, _, State) ->
    parse_EncodingDecl_encodin(cf(State));
parse_EncodingDeclS(<<"encodi"/utf8>>, _, _, State) ->
    parse_EncodingDecl_encodi(cf(State));
parse_EncodingDeclS(<<"encod"/utf8>>, _, _, State) ->
    parse_EncodingDecl_encod(cf(State));
parse_EncodingDeclS(<<"enco"/utf8>>, _, _, State) ->
    parse_EncodingDecl_enco(cf(State));
parse_EncodingDeclS(<<"enc"/utf8>>, _, _, State) ->
    parse_EncodingDecl_enc(cf(State));
parse_EncodingDeclS(<<"en"/utf8>>, _, _, State) ->
    parse_EncodingDecl_en(cf(State));
parse_EncodingDeclS(<<"e"/utf8>>, _, _, State) ->
    parse_EncodingDecl_e(cf(State));
parse_EncodingDeclS(<<>>, _, _, State) ->
    {Stream1, State1} = cf(State),
    parse_EncodingDeclS(Stream1, Stream1, 0, State1);
parse_EncodingDeclS(?MATCH) ->
    % no encoding
    {{<<"UTF-8">>, false}, ?MATCH}.

parse_EncodingDecl_e({<<"ncoding"/utf8, Rest/bitstring>> = Stream, State}) ->
    parse_EncodingDecl_encoding(Rest, Stream, 7, State);
parse_EncodingDecl_e({<<"ncodin"/utf8>>, State}) ->
    parse_EncodingDecl_encodin(cf(State));
parse_EncodingDecl_e({<<"ncodi"/utf8>>, State}) ->
    parse_EncodingDecl_encodi(cf(State));
parse_EncodingDecl_e({<<"ncod"/utf8>>, State}) ->
    parse_EncodingDecl_encod(cf(State));
parse_EncodingDecl_e({<<"nco"/utf8>>, State}) ->
    parse_EncodingDecl_enco(cf(State));
parse_EncodingDecl_e({<<"nc"/utf8>>, State}) ->
    parse_EncodingDecl_enc(cf(State));
parse_EncodingDecl_e({<<"n"/utf8>>, State}) ->
    parse_EncodingDecl_en(cf(State));
parse_EncodingDecl_e({_, State}) ->
    fatal_error(bad_encoding, State).

parse_EncodingDecl_en({<<"coding"/utf8, Rest/bitstring>> = Stream, State}) ->
    parse_EncodingDecl_encoding(Rest, Stream, 6, State);
parse_EncodingDecl_en({<<"codin"/utf8>>, State}) ->
    parse_EncodingDecl_encodin(cf(State));
parse_EncodingDecl_en({<<"codi"/utf8>>, State}) ->
    parse_EncodingDecl_encodi(cf(State));
parse_EncodingDecl_en({<<"cod"/utf8>>, State}) ->
    parse_EncodingDecl_encod(cf(State));
parse_EncodingDecl_en({<<"co"/utf8>>, State}) ->
    parse_EncodingDecl_enco(cf(State));
parse_EncodingDecl_en({<<"c"/utf8>>, State}) ->
    parse_EncodingDecl_enc(cf(State));
parse_EncodingDecl_en({_, State}) ->
    fatal_error(bad_encoding, State).

parse_EncodingDecl_enc({<<"oding"/utf8, Rest/bitstring>> = Stream, State}) ->
    parse_EncodingDecl_encoding(Rest, Stream, 5, State);
parse_EncodingDecl_enc({<<"odin"/utf8>>, State}) ->
    parse_EncodingDecl_encodin(cf(State));
parse_EncodingDecl_enc({<<"odi"/utf8>>, State}) ->
    parse_EncodingDecl_encodi(cf(State));
parse_EncodingDecl_enc({<<"od"/utf8>>, State}) ->
    parse_EncodingDecl_encod(cf(State));
parse_EncodingDecl_enc({<<"o"/utf8>>, State}) ->
    parse_EncodingDecl_enco(cf(State));
parse_EncodingDecl_enc({_, State}) ->
    fatal_error(bad_encoding, State).

parse_EncodingDecl_enco({<<"ding"/utf8, Rest/bitstring>> = Stream, State}) ->
    parse_EncodingDecl_encoding(Rest, Stream, 4, State);
parse_EncodingDecl_enco({<<"din"/utf8>>, State}) ->
    parse_EncodingDecl_encodin(cf(State));
parse_EncodingDecl_enco({<<"di"/utf8>>, State}) ->
    parse_EncodingDecl_encodi(cf(State));
parse_EncodingDecl_enco({<<"d"/utf8>>, State}) ->
    parse_EncodingDecl_encod(cf(State));
parse_EncodingDecl_enco({_, State}) ->
    fatal_error(bad_encoding, State).

parse_EncodingDecl_encod({<<"ing"/utf8, Rest/bitstring>> = Stream, State}) ->
    parse_EncodingDecl_encoding(Rest, Stream, 3, State);
parse_EncodingDecl_encod({<<"in"/utf8>>, State}) ->
    parse_EncodingDecl_encodin(cf(State));
parse_EncodingDecl_encod({<<"i"/utf8>>, State}) ->
    parse_EncodingDecl_encodi(cf(State));
parse_EncodingDecl_encod({_, State}) ->
    fatal_error(bad_encoding, State).

parse_EncodingDecl_encodi({<<"ng"/utf8, Rest/bitstring>> = Stream, State}) ->
    parse_EncodingDecl_encoding(Rest, Stream, 2, State);
parse_EncodingDecl_encodi({<<"n"/utf8>>, State}) ->
    parse_EncodingDecl_encodin(cf(State));
parse_EncodingDecl_encodi({_, State}) ->
    fatal_error(bad_encoding, State).

parse_EncodingDecl_encodin({<<"g"/utf8, Rest/bitstring>> = Stream, State}) ->
    parse_EncodingDecl_encoding(Rest, Stream, 1, State);
parse_EncodingDecl_encodin({_, State}) ->
    fatal_error(bad_encoding, State).

parse_EncodingDecl_encoding(?MATCH) ->
    {?MATCH1} = parse_Eq(?MATCH),
    parse_EncodingDecl_EncName(?MATCH1).

parse_EncodingDecl_EncName(<<$'/utf8, Rest/bitstring>>, Stream, Pos, State) ->
    parse_EncodingDecl_EncName_sq(Rest, Stream, Pos + 1, State);
parse_EncodingDecl_EncName(<<$"/utf8, Rest/bitstring>>, Stream, Pos, State) ->
    parse_EncodingDecl_EncName_dq(Rest, Stream, Pos + 1, State);
parse_EncodingDecl_EncName(_, _, _, State) ->
    fatal_error(bad_encoding, State).

parse_EncodingDecl_EncName_sq(?MATCH) ->
    case parse_EncodingDecl_EncName_name(?MATCH) of
        {Name, <<$'/utf8, Rest/bitstring>>, Stream1, Pos1, State1} ->
            {{Name, true}, Rest, Stream1, Pos1 + 1, State1};
        {_, _, _, _, State1} ->
            fatal_error(bad_encoding, State1)
    end.

parse_EncodingDecl_EncName_dq(?MATCH) ->
    case parse_EncodingDecl_EncName_name(?MATCH) of
        {Name, <<$"/utf8, Rest/bitstring>>, Stream1, Pos1, State1} ->
            {{Name, true}, Rest, Stream1, Pos1 + 1, State1};
        {_, _, _, _, State1} ->
            fatal_error(bad_encoding, State1)
    end.

%%----------------------------------------------------------------------
%% [81] EncName ::= [A-Za-z] ([A-Za-z0-9._] | '-')*
%% /* Encoding name contains only Latin characters */
%%----------------------------------------------------------------------
parse_EncodingDecl_EncName_name(<<>>, _, _, State) ->
    {Stream1, State1} = cf(State),
    parse_EncodingDecl_EncName_name(Stream1, Stream1, 0, State1);
parse_EncodingDecl_EncName_name(<<Char/utf8, Rest/bitstring>>, Stream, Pos, State) when
    Char >= $A andalso Char =< $Z; Char >= $a andalso Char =< $z
->
    parse_EncodingDecl_EncName_name_1(Rest, Stream, Pos, 1, State, []);
parse_EncodingDecl_EncName_name(_, _, _, State) ->
    fatal_error(bad_char, State).

parse_EncodingDecl_EncName_name_1(<<>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    {Stream1, State1} = cf(State),
    parse_EncodingDecl_EncName_name_1(Stream1, Stream1, 0, 0, State1, Acc1);
parse_EncodingDecl_EncName_name_1(<<Char/utf8, Rest/bitstring>>, Stream, Pos, Len, State, Acc) when
    Char >= $A andalso Char =< $Z;
    Char >= $a andalso Char =< $z;
    Char >= $0 andalso Char =< $9;
    Char == $.;
    Char == $_;
    Char == $-
->
    parse_EncodingDecl_EncName_name_1(Rest, Stream, Pos, Len + 1, State, Acc);
parse_EncodingDecl_EncName_name_1(Bytes, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    {Acc1, Bytes, Stream, Pos + Len, State}.

%%----------------------------------------------------------------------
%% [25] Eq ::= S? '=' S?
%%----------------------------------------------------------------------
parse_Eq(?MATCH) ->
    {_Found, ?MATCH1} = maybe_consume_s(?MATCH),
    parse_Eq_1(?MATCH1).

parse_Eq_1(<<$=/utf8, Rest/bitstring>>, Stream, Pos, State) ->
    {_Found, ?MATCH1} = maybe_consume_s(Rest, Stream, Pos + 1, State),
    {?MATCH1};
parse_Eq_1(_, _, _, State) ->
    fatal_error(bad_eq, State).

%%----------------------------------------------------------------------
%% [26] VersionNum ::= '1.' [0-9]+
%%----------------------------------------------------------------------
parse_VersionNum_sq(<<$1/utf8, $./utf8, Rest/bitstring>>, Stream, Pos, State) ->
    parse_VersionNum_digit_sq(Rest, Stream, Pos + 2, 0, State, []);
parse_VersionNum_sq(<<$1/utf8>>, _, _, State) ->
    {Stream1, State1} = cf(State),
    parse_VersionNum_1_sq(Stream1, State1);
parse_VersionNum_sq(<<>>, _, _, State) ->
    {Stream1, State1} = cf(State),
    parse_VersionNum_sq(Stream1, Stream1, 0, State1);
parse_VersionNum_sq(_, _, _, State) ->
    fatal_error(bad_version_num, State).

parse_VersionNum_1_sq(<<$./utf8, Rest/bitstring>> = Stream, State) ->
    parse_VersionNum_digit_sq(Rest, Stream, 1, 0, State, []);
parse_VersionNum_1_sq(_, State) ->
    fatal_error(bad_version_num, State).

parse_VersionNum_digit_sq(<<>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    {Stream1, State1} = cf(State),
    parse_VersionNum_digit_sq(Stream1, Stream1, 0, 0, State1, Acc1);
parse_VersionNum_digit_sq(<<Char/utf8, Rest/bitstring>>, Stream, Pos, Len, State, Acc) when
    Char >= $0 andalso Char =< $9
->
    parse_VersionNum_digit_sq(Rest, Stream, Pos, Len + 1, State, Acc);
parse_VersionNum_digit_sq(<<$'/utf8, Rest/bitstring>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    {{version_to_number(Acc1, State), true}, Rest, Stream, Pos + Len + 1, State};
parse_VersionNum_digit_sq(_, _, _, _, State, _) ->
    fatal_error(bad_version_num, State).

parse_VersionNum_dq(<<$1/utf8, $./utf8, Rest/bitstring>>, Stream, Pos, State) ->
    parse_VersionNum_digit_dq(Rest, Stream, Pos + 2, 0, State, []);
parse_VersionNum_dq(<<$1/utf8>>, _, _, State) ->
    {Stream1, State1} = cf(State),
    parse_VersionNum_1_dq(Stream1, State1);
parse_VersionNum_dq(<<>>, _, _, State) ->
    {Stream1, State1} = cf(State),
    parse_VersionNum_dq(Stream1, Stream1, 0, State1);
parse_VersionNum_dq(_, _, _, State) ->
    fatal_error(bad_version_num, State).

parse_VersionNum_1_dq(<<$./utf8, Rest/bitstring>> = Stream, State) ->
    parse_VersionNum_digit_dq(Rest, Stream, 1, 0, State, []);
parse_VersionNum_1_dq(_, State) ->
    fatal_error(bad_version_num, State).

parse_VersionNum_digit_dq(<<>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    {Stream1, State1} = cf(State),
    parse_VersionNum_digit_dq(Stream1, Stream1, 0, 0, State1, Acc1);
parse_VersionNum_digit_dq(<<Char/utf8, Rest/bitstring>>, Stream, Pos, Len, State, Acc) when
    Char >= $0 andalso Char =< $9
->
    parse_VersionNum_digit_dq(Rest, Stream, Pos, Len + 1, State, Acc);
parse_VersionNum_digit_dq(<<$\"/utf8, Rest/bitstring>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    {{version_to_number(Acc1, State), true}, Rest, Stream, Pos + Len + 1, State};
parse_VersionNum_digit_dq(_, _, _, _, State, _) ->
    fatal_error(bad_version_num, State).

version_to_number(Acc, State) ->
    case to_binary([<<"1."/utf8>> | Acc]) of
        <<"1."/utf8>> -> fatal_error(bad_version_num, State);
        Bin -> Bin
    end.

%%----------------------------------------------------------------------
%% [24] VersionInfo ::= S 'version' Eq ("'" VersionNum "'" | '"' VersionNum '"')
%%----------------------------------------------------------------------
parse_VersionInfo(?MATCH) ->
    {Found, ?MATCH1} = maybe_consume_s(?MATCH),
    case Found of
        true ->
            parse_VersionInfoS(?MATCH1);
        false ->
            {{<<>>, false}, ?MATCH1}
    end.

parse_VersionInfo_version(?MATCH) ->
    case parse_Eq(?MATCH) of
        {<<$'/utf8, Bytes1/bitstring>>, Stream1, Pos1, State1} ->
            parse_VersionNum_sq(Bytes1, Stream1, Pos1 + 1, State1);
        {<<$\"/utf8, Bytes1/bitstring>>, Stream1, Pos1, State1} ->
            parse_VersionNum_dq(Bytes1, Stream1, Pos1 + 1, State1);
        {_, _, _, State1} ->
            fatal_error(bad_version, State1)
    end.

parse_VersionInfoS(<<"version"/utf8, Rest/bitstring>>, Stream, Pos, State) ->
    parse_VersionInfo_version(Rest, Stream, Pos + 7, State);
parse_VersionInfoS(<<"versio"/utf8>>, _, _, State) ->
    parse_VersionInfo_versio(cf(State));
parse_VersionInfoS(<<"versi"/utf8>>, _, _, State) ->
    parse_VersionInfo_versi(cf(State));
parse_VersionInfoS(<<"vers"/utf8>>, _, _, State) ->
    parse_VersionInfo_vers(cf(State));
parse_VersionInfoS(<<"ver"/utf8>>, _, _, State) ->
    parse_VersionInfo_ver(cf(State));
parse_VersionInfoS(<<"ve"/utf8>>, _, _, State) ->
    parse_VersionInfo_ve(cf(State));
parse_VersionInfoS(<<"v"/utf8>>, _, _, State) ->
    parse_VersionInfo_v(cf(State));
parse_VersionInfoS(<<>>, _, _, State) ->
    {Stream1, State1} = cf(State),
    parse_VersionInfoS(Stream1, Stream1, 0, State1);
parse_VersionInfoS(?MATCH) ->
    {{<<>>, false}, ?MATCH}.

parse_VersionInfo_v({<<"ersion"/utf8, Rest/bitstring>> = Stream, State}) ->
    parse_VersionInfo_version(Rest, Stream, 6, State);
parse_VersionInfo_v({<<"ersio"/utf8>>, State}) ->
    parse_VersionInfo_versio(cf(State));
parse_VersionInfo_v({<<"ersi"/utf8>>, State}) ->
    parse_VersionInfo_versi(cf(State));
parse_VersionInfo_v({<<"ers"/utf8>>, State}) ->
    parse_VersionInfo_vers(cf(State));
parse_VersionInfo_v({<<"er"/utf8>>, State}) ->
    parse_VersionInfo_ver(cf(State));
parse_VersionInfo_v({<<"e"/utf8>>, State}) ->
    parse_VersionInfo_ve(cf(State));
parse_VersionInfo_v({_, State}) ->
    fatal_error(bad_version_num, State).

parse_VersionInfo_ve({<<"rsion"/utf8, Rest/bitstring>> = Stream, State}) ->
    parse_VersionInfo_version(Rest, Stream, 5, State);
parse_VersionInfo_ve({<<"rsio"/utf8>>, State}) ->
    parse_VersionInfo_versio(cf(State));
parse_VersionInfo_ve({<<"rsi"/utf8>>, State}) ->
    parse_VersionInfo_versi(cf(State));
parse_VersionInfo_ve({<<"rs"/utf8>>, State}) ->
    parse_VersionInfo_vers(cf(State));
parse_VersionInfo_ve({<<"r"/utf8>>, State}) ->
    parse_VersionInfo_ver(cf(State));
parse_VersionInfo_ve({_, State}) ->
    fatal_error(bad_version_num, State).

parse_VersionInfo_ver({<<"sion"/utf8, Rest/bitstring>> = Stream, State}) ->
    parse_VersionInfo_version(Rest, Stream, 4, State);
parse_VersionInfo_ver({<<"sio"/utf8>>, State}) ->
    parse_VersionInfo_versio(cf(State));
parse_VersionInfo_ver({<<"si"/utf8>>, State}) ->
    parse_VersionInfo_versi(cf(State));
parse_VersionInfo_ver({<<"s"/utf8>>, State}) ->
    parse_VersionInfo_vers(cf(State));
parse_VersionInfo_ver({_, State}) ->
    fatal_error(bad_version_num, State).

parse_VersionInfo_vers({<<"ion"/utf8, Rest/bitstring>> = Stream, State}) ->
    parse_VersionInfo_version(Rest, Stream, 3, State);
parse_VersionInfo_vers({<<"io"/utf8>>, State}) ->
    parse_VersionInfo_versio(cf(State));
parse_VersionInfo_vers({<<"i"/utf8>>, State}) ->
    parse_VersionInfo_versi(cf(State));
parse_VersionInfo_vers({_, State}) ->
    fatal_error(bad_version_num, State).

parse_VersionInfo_versi({<<"on"/utf8, Rest/bitstring>> = Stream, State}) ->
    parse_VersionInfo_version(Rest, Stream, 2, State);
parse_VersionInfo_versi({<<"o"/utf8>>, State}) ->
    parse_VersionInfo_versio(cf(State));
parse_VersionInfo_versi({_, State}) ->
    fatal_error(bad_version_num, State).

parse_VersionInfo_versio({<<"n"/utf8, Rest/bitstring>> = Stream, State}) ->
    parse_VersionInfo_version(Rest, Stream, 1, State);
parse_VersionInfo_versio({_, State}) ->
    fatal_error(bad_version_num, State).

%%----------------------------------------------------------------------
%% [32] SDDecl ::= S 'standalone' Eq (("'" ('yes' | 'no') "'") | ('"' ('yes' | 'no') '"'))
%% leading space is trimmed already
%%----------------------------------------------------------------------
parse_SDDecl_standalone(?MATCH) ->
    {?MATCH1} = parse_Eq(?MATCH),
    parse_SDDecl_standalone_yesno(?MATCH1).

parse_SDDecl_standalone_yesno(<<$'/utf8, Rest/bitstring>>, Stream, Pos, State) ->
    parse_SDDecl_standalone_yesno_sq(Rest, Stream, Pos + 1, State);
parse_SDDecl_standalone_yesno(<<$"/utf8, Rest/bitstring>>, Stream, Pos, State) ->
    parse_SDDecl_standalone_yesno_dq(Rest, Stream, Pos + 1, State);
parse_SDDecl_standalone_yesno(<<>>, _, _, State) ->
    {Stream1, State1} = cf(State),
    parse_SDDecl_standalone_yesno(Stream1, Stream1, 0, State1);
parse_SDDecl_standalone_yesno(_, _, _, State) ->
    fatal_error(bad_standalone, State).

parse_SDDecl_standalone_yesno_sq(<<"no'"/utf8, Rest/bitstring>>, Stream, Pos, State) ->
    {{false, true}, Rest, Stream, Pos + 3, State};
parse_SDDecl_standalone_yesno_sq(<<"no"/utf8>>, _, _, State) ->
    parse_SDDecl_standalone_yesno_sq_no(cf(State));
parse_SDDecl_standalone_yesno_sq(<<"n"/utf8>>, _, _, State) ->
    parse_SDDecl_standalone_yesno_sq_n(cf(State));
parse_SDDecl_standalone_yesno_sq(<<"yes'"/utf8, Rest/bitstring>>, Stream, Pos, State) ->
    {{true, true}, Rest, Stream, Pos + 4, State};
parse_SDDecl_standalone_yesno_sq(<<"yes"/utf8>>, _, _, State) ->
    parse_SDDecl_standalone_yesno_sq_yes(cf(State));
parse_SDDecl_standalone_yesno_sq(<<"ye"/utf8>>, _, _, State) ->
    parse_SDDecl_standalone_yesno_sq_ye(cf(State));
parse_SDDecl_standalone_yesno_sq(<<"y"/utf8>>, _, _, State) ->
    parse_SDDecl_standalone_yesno_sq_y(cf(State));
parse_SDDecl_standalone_yesno_sq(<<>>, _, _, State) ->
    {Stream1, State1} = cf(State),
    parse_SDDecl_standalone_yesno_sq(Stream1, Stream1, 0, State1).

parse_SDDecl_standalone_yesno_sq_y({<<"es'"/utf8, Rest/bitstring>> = Stream, State}) ->
    {{true, true}, Rest, Stream, 3, State};
parse_SDDecl_standalone_yesno_sq_y({<<"es"/utf8>>, State}) ->
    parse_SDDecl_standalone_yesno_sq_yes(cf(State));
parse_SDDecl_standalone_yesno_sq_y({<<"e"/utf8>>, State}) ->
    parse_SDDecl_standalone_yesno_sq_ye(cf(State));
parse_SDDecl_standalone_yesno_sq_y({_, State}) ->
    fatal_error(bad_standalone, State).

parse_SDDecl_standalone_yesno_sq_ye({<<"s'"/utf8, Rest/bitstring>> = Stream, State}) ->
    {{true, true}, Rest, Stream, 2, State};
parse_SDDecl_standalone_yesno_sq_ye({<<"s"/utf8>>, State}) ->
    parse_SDDecl_standalone_yesno_sq_yes(cf(State));
parse_SDDecl_standalone_yesno_sq_ye({_, State}) ->
    fatal_error(bad_standalone, State).

parse_SDDecl_standalone_yesno_sq_yes({<<"'"/utf8, Rest/bitstring>> = Stream, State}) ->
    {{true, true}, Rest, Stream, 1, State};
parse_SDDecl_standalone_yesno_sq_yes({_, State}) ->
    fatal_error(bad_standalone, State).

parse_SDDecl_standalone_yesno_dq(<<"no\""/utf8, Rest/bitstring>>, Stream, Pos, State) ->
    {{false, true}, Rest, Stream, Pos + 3, State};
parse_SDDecl_standalone_yesno_dq(<<"no"/utf8>>, _, _, State) ->
    parse_SDDecl_standalone_yesno_dq_no(cf(State));
parse_SDDecl_standalone_yesno_dq(<<"n"/utf8>>, _, _, State) ->
    parse_SDDecl_standalone_yesno_dq_n(cf(State));
parse_SDDecl_standalone_yesno_dq(<<"yes\""/utf8, Rest/bitstring>>, Stream, Pos, State) ->
    {{true, true}, Rest, Stream, Pos + 4, State};
parse_SDDecl_standalone_yesno_dq(<<"yes"/utf8>>, _, _, State) ->
    parse_SDDecl_standalone_yesno_dq_yes(cf(State));
parse_SDDecl_standalone_yesno_dq(<<"ye"/utf8>>, _, _, State) ->
    parse_SDDecl_standalone_yesno_dq_ye(cf(State));
parse_SDDecl_standalone_yesno_dq(<<"y"/utf8>>, _, _, State) ->
    parse_SDDecl_standalone_yesno_dq_y(cf(State));
parse_SDDecl_standalone_yesno_dq(<<>>, _, _, State) ->
    {Stream1, State1} = cf(State),
    parse_SDDecl_standalone_yesno_dq(Stream1, Stream1, 0, State1).

parse_SDDecl_standalone_yesno_sq_n({<<"o'"/utf8, Rest/bitstring>> = Stream, State}) ->
    {{false, true}, Rest, Stream, 2, State};
parse_SDDecl_standalone_yesno_sq_n({<<"o"/utf8>>, State}) ->
    parse_SDDecl_standalone_yesno_sq_no(cf(State));
parse_SDDecl_standalone_yesno_sq_n({_, State}) ->
    fatal_error(bad_standalone, State).

parse_SDDecl_standalone_yesno_sq_no({<<"'"/utf8, Rest/bitstring>> = Stream, State}) ->
    {{false, true}, Rest, Stream, 1, State};
parse_SDDecl_standalone_yesno_sq_no({_, State}) ->
    fatal_error(bad_standalone, State).

parse_SDDecl_standalone_yesno_dq_n({<<"o\""/utf8, Rest/bitstring>> = Stream, State}) ->
    {{false, true}, Rest, Stream, 2, State};
parse_SDDecl_standalone_yesno_dq_n({<<"o"/utf8>>, State}) ->
    parse_SDDecl_standalone_yesno_dq_no(cf(State));
parse_SDDecl_standalone_yesno_dq_n({_, State}) ->
    fatal_error(bad_standalone, State).

parse_SDDecl_standalone_yesno_dq_no({<<"\""/utf8, Rest/bitstring>> = Stream, State}) ->
    {{false, true}, Rest, Stream, 1, State};
parse_SDDecl_standalone_yesno_dq_no({_, State}) ->
    fatal_error(bad_standalone, State).

parse_SDDecl_standalone_yesno_dq_y({<<"es\""/utf8, Rest/bitstring>> = Stream, State}) ->
    {{true, true}, Rest, Stream, 3, State};
parse_SDDecl_standalone_yesno_dq_y({<<"es"/utf8>>, State}) ->
    parse_SDDecl_standalone_yesno_dq_yes(cf(State));
parse_SDDecl_standalone_yesno_dq_y({<<"e"/utf8>>, State}) ->
    parse_SDDecl_standalone_yesno_dq_ye(cf(State));
parse_SDDecl_standalone_yesno_dq_y({_, State}) ->
    fatal_error(bad_standalone, State).

parse_SDDecl_standalone_yesno_dq_ye({<<"s\""/utf8, Rest/bitstring>> = Stream, State}) ->
    {{true, true}, Rest, Stream, 2, State};
parse_SDDecl_standalone_yesno_dq_ye({<<"s"/utf8>>, State}) ->
    parse_SDDecl_standalone_yesno_dq_yes(cf(State));
parse_SDDecl_standalone_yesno_dq_ye({_, State}) ->
    fatal_error(bad_standalone, State).

parse_SDDecl_standalone_yesno_dq_yes({<<"\""/utf8, Rest/bitstring>> = Stream, State}) ->
    {{true, true}, Rest, Stream, 1, State};
parse_SDDecl_standalone_yesno_dq_yes({_, State}) ->
    fatal_error(bad_standalone, State).

parse_SDDecl(<<"standalone"/utf8, Rest/bitstring>>, Stream, Pos, State) ->
    parse_SDDecl_standalone(Rest, Stream, Pos + 10, State);
parse_SDDecl(<<"standalon"/utf8>>, _, _, State) ->
    parse_SDDecl_standalon(cf(State));
parse_SDDecl(<<"standalo"/utf8>>, _, _, State) ->
    parse_SDDecl_standalo(cf(State));
parse_SDDecl(<<"standal"/utf8>>, _, _, State) ->
    parse_SDDecl_standal(cf(State));
parse_SDDecl(<<"standa"/utf8>>, _, _, State) ->
    parse_SDDecl_standa(cf(State));
parse_SDDecl(<<"stand"/utf8>>, _, _, State) ->
    parse_SDDecl_stand(cf(State));
parse_SDDecl(<<"stan"/utf8>>, _, _, State) ->
    parse_SDDecl_stan(cf(State));
parse_SDDecl(<<"sta"/utf8>>, _, _, State) ->
    parse_SDDecl_sta(cf(State));
parse_SDDecl(<<"st"/utf8>>, _, _, State) ->
    parse_SDDecl_st(cf(State));
parse_SDDecl(<<"s"/utf8>>, _, _, State) ->
    parse_SDDecl_s(cf(State));
parse_SDDecl(<<>>, _, _, State) ->
    {Stream1, State1} = cf(State),
    parse_SDDecl(Stream1, Stream1, 0, State1);
parse_SDDecl(?MATCH) ->
    {{false, false}, ?MATCH}.

parse_SDDecl_s({<<"tandalone"/utf8, Rest/bitstring>> = Stream, State}) ->
    parse_SDDecl_standalone(Rest, Stream, 9, State);
parse_SDDecl_s({<<"tandalon"/utf8>>, State}) ->
    parse_SDDecl_standalon(cf(State));
parse_SDDecl_s({<<"tandalo"/utf8>>, State}) ->
    parse_SDDecl_standalo(cf(State));
parse_SDDecl_s({<<"tandal"/utf8>>, State}) ->
    parse_SDDecl_standal(cf(State));
parse_SDDecl_s({<<"tanda"/utf8>>, State}) ->
    parse_SDDecl_standa(cf(State));
parse_SDDecl_s({<<"tand"/utf8>>, State}) ->
    parse_SDDecl_stand(cf(State));
parse_SDDecl_s({<<"tan"/utf8>>, State}) ->
    parse_SDDecl_stan(cf(State));
parse_SDDecl_s({<<"ta"/utf8>>, State}) ->
    parse_SDDecl_sta(cf(State));
parse_SDDecl_s({<<"t"/utf8>>, State}) ->
    parse_SDDecl_st(cf(State));
parse_SDDecl_s({_, State}) ->
    fatal_error(bad_standalone, State).

parse_SDDecl_st({<<"andalone"/utf8, Rest/bitstring>> = Stream, State}) ->
    parse_SDDecl_standalone(Rest, Stream, 8, State);
parse_SDDecl_st({<<"andalon"/utf8>>, State}) ->
    parse_SDDecl_standalon(cf(State));
parse_SDDecl_st({<<"andalo"/utf8>>, State}) ->
    parse_SDDecl_standalo(cf(State));
parse_SDDecl_st({<<"andal"/utf8>>, State}) ->
    parse_SDDecl_standal(cf(State));
parse_SDDecl_st({<<"anda"/utf8>>, State}) ->
    parse_SDDecl_standa(cf(State));
parse_SDDecl_st({<<"and"/utf8>>, State}) ->
    parse_SDDecl_stand(cf(State));
parse_SDDecl_st({<<"an"/utf8>>, State}) ->
    parse_SDDecl_stan(cf(State));
parse_SDDecl_st({<<"a"/utf8>>, State}) ->
    parse_SDDecl_sta(cf(State));
parse_SDDecl_st({_, State}) ->
    fatal_error(bad_standalone, State).

parse_SDDecl_sta({<<"ndalone"/utf8, Rest/bitstring>> = Stream, State}) ->
    parse_SDDecl_standalone(Rest, Stream, 7, State);
parse_SDDecl_sta({<<"ndalon"/utf8>>, State}) ->
    parse_SDDecl_standalon(cf(State));
parse_SDDecl_sta({<<"ndalo"/utf8>>, State}) ->
    parse_SDDecl_standalo(cf(State));
parse_SDDecl_sta({<<"ndal"/utf8>>, State}) ->
    parse_SDDecl_standal(cf(State));
parse_SDDecl_sta({<<"nda"/utf8>>, State}) ->
    parse_SDDecl_standa(cf(State));
parse_SDDecl_sta({<<"nd"/utf8>>, State}) ->
    parse_SDDecl_stand(cf(State));
parse_SDDecl_sta({<<"n"/utf8>>, State}) ->
    parse_SDDecl_stan(cf(State));
parse_SDDecl_sta({_, State}) ->
    fatal_error(bad_standalone, State).

parse_SDDecl_stan({<<"dalone"/utf8, Rest/bitstring>> = Stream, State}) ->
    parse_SDDecl_standalone(Rest, Stream, 6, State);
parse_SDDecl_stan({<<"dalon"/utf8>>, State}) ->
    parse_SDDecl_standalon(cf(State));
parse_SDDecl_stan({<<"dalo"/utf8>>, State}) ->
    parse_SDDecl_standalo(cf(State));
parse_SDDecl_stan({<<"dal"/utf8>>, State}) ->
    parse_SDDecl_standal(cf(State));
parse_SDDecl_stan({<<"da"/utf8>>, State}) ->
    parse_SDDecl_standa(cf(State));
parse_SDDecl_stan({<<"d"/utf8>>, State}) ->
    parse_SDDecl_stand(cf(State));
parse_SDDecl_stan({_, State}) ->
    fatal_error(bad_standalone, State).

parse_SDDecl_stand({<<"alone"/utf8, Rest/bitstring>> = Stream, State}) ->
    parse_SDDecl_standalone(Rest, Stream, 5, State);
parse_SDDecl_stand({<<"alon"/utf8>>, State}) ->
    parse_SDDecl_standalon(cf(State));
parse_SDDecl_stand({<<"alo"/utf8>>, State}) ->
    parse_SDDecl_standalo(cf(State));
parse_SDDecl_stand({<<"al"/utf8>>, State}) ->
    parse_SDDecl_standal(cf(State));
parse_SDDecl_stand({<<"a"/utf8>>, State}) ->
    parse_SDDecl_standa(cf(State));
parse_SDDecl_stand({_, State}) ->
    fatal_error(bad_standalone, State).

parse_SDDecl_standa({<<"lone"/utf8, Rest/bitstring>> = Stream, State}) ->
    parse_SDDecl_standalone(Rest, Stream, 4, State);
parse_SDDecl_standa({<<"lon"/utf8>>, State}) ->
    parse_SDDecl_standalon(cf(State));
parse_SDDecl_standa({<<"lo"/utf8>>, State}) ->
    parse_SDDecl_standalo(cf(State));
parse_SDDecl_standa({<<"l"/utf8>>, State}) ->
    parse_SDDecl_standal(cf(State));
parse_SDDecl_standa({_, State}) ->
    fatal_error(bad_standalone, State).

parse_SDDecl_standal({<<"one"/utf8, Rest/bitstring>> = Stream, State}) ->
    parse_SDDecl_standalone(Rest, Stream, 3, State);
parse_SDDecl_standal({<<"on"/utf8>>, State}) ->
    parse_SDDecl_standalon(cf(State));
parse_SDDecl_standal({<<"o"/utf8>>, State}) ->
    parse_SDDecl_standalo(cf(State));
parse_SDDecl_standal({_, State}) ->
    fatal_error(bad_standalone, State).

parse_SDDecl_standalo({<<"ne"/utf8, Rest/bitstring>> = Stream, State}) ->
    parse_SDDecl_standalone(Rest, Stream, 2, State);
parse_SDDecl_standalo({<<"n"/utf8>>, State}) ->
    parse_SDDecl_standalon(cf(State));
parse_SDDecl_standalo({_, State}) ->
    fatal_error(bad_standalone, State).

parse_SDDecl_standalon({<<"e"/utf8, Rest/bitstring>> = Stream, State}) ->
    parse_SDDecl_standalone(Rest, Stream, 1, State);
parse_SDDecl_standalon({_, State}) ->
    fatal_error(bad_standalone, State).

%%----------------------------------------------------------------------
%% [23] XMLDecl ::= '<?xml' VersionInfo EncodingDecl? SDDecl? S? '?>'
%%----------------------------------------------------------------------
parse_XMLDecl_ltqxml(?MATCH) ->
    {{Version, true}, ?MATCH1} = parse_VersionInfo(?MATCH),
    {IsWs2, ?MATCH2} = maybe_consume_s(?MATCH1),
    {{Encoding, EncSet}, ?MATCH3} = parse_EncodingDecl(?MATCH2),
    {{Standalone1, SaSet1}, ?MATCH6} =
        case IsWs2 of
            true when EncSet ->
                {IsWs4, ?MATCH4} = maybe_consume_s(?MATCH3),
                {{Standalone, SaSet}, ?MATCH5} = parse_SDDecl(?MATCH4),
                case IsWs4 of
                    false when SaSet -> fatal_error(missing_ws, State);
                    _ -> {{Standalone, SaSet}, ?MATCH5}
                end;
            true ->
                parse_SDDecl(?MATCH3);
            false when EncSet ->
                fatal_error(missing_ws, State);
            false ->
                {IsWs4, ?MATCH4} = maybe_consume_s(?MATCH3),
                {{Standalone, SaSet}, ?MATCH5} = parse_SDDecl(?MATCH4),
                case IsWs4 of
                    false when SaSet -> fatal_error(missing_ws, State);
                    _ -> {{Standalone, SaSet}, ?MATCH5}
                end
        end,
    {_, ?MATCH7} = maybe_consume_s(?MATCH6),
    {Bytes8, _, _, State8} = parse_XMLDecl_end(?MATCH7),
    State9 = set_next_parser_position(?misc_pre_dtd, State8),
    yaccety_sax:event_startDocument(
        Version,
        Encoding,
        EncSet,
        Standalone1,
        SaSet1,
        set_state_pos(State9, Bytes8)
    ).

parse_XMLDecl_end(<<"?>"/utf8, Rest/bitstring>>, Stream, Pos, State) ->
    {Rest, Stream, Pos + 2, State};
parse_XMLDecl_end(<<"?"/utf8>>, _, _, State) ->
    parse_XMLDecl_end_q(cf(State));
parse_XMLDecl_end(Bytes, _, _, State) ->
    fatal_error(bad_xmldecl, {Bytes, State}).

parse_XMLDecl_end_q({<<$>/utf8, Rest/bitstring>> = Stream, State}) ->
    {Rest, Stream, 1, State};
parse_XMLDecl_end_q({_, State}) ->
    fatal_error(bad_xmldecl, State).

parse_XMLDecl(Stream, State) -> parse_XMLDecl(Stream, Stream, 0, State).

parse_XMLDecl(<<"<?xml"/utf8, Rest/bitstring>>, Stream, Pos, State) ->
    parse_XMLDecl_ltqxml(Rest, Stream, Pos + 5, State);
parse_XMLDecl(<<"<?xm"/utf8>>, _, _, State) ->
    parse_XMLDecl_ltqxm(cf(State));
parse_XMLDecl(<<"<?x"/utf8>>, _, _, State) ->
    parse_XMLDecl_ltqx(cf(State));
parse_XMLDecl(<<"<?"/utf8>>, _, _, State) ->
    parse_XMLDecl_ltq(cf(State));
parse_XMLDecl(<<"<">>, _, _, State) ->
    parse_XMLDecl_lt(cf(State));
parse_XMLDecl(<<>>, _, _, State) ->
    {Stream1, State1} = cf(State),
    parse_XMLDecl(Stream1, Stream1, 0, State1);
parse_XMLDecl(Bytes, _, _, State) ->
    % default declaration
    State1 = set_next_parser_position(?misc_pre_dtd, State),
    yaccety_sax:event_startDocument(
        <<"1.0">>,
        <<"UTF-8">>,
        false,
        false,
        false,
        set_state_pos(State1, Bytes)
    ).

parse_XMLDecl_lt({<<"?xml"/utf8, Rest/bitstring>> = Stream, State}) ->
    parse_XMLDecl_ltqxml(Rest, Stream, 4, State);
parse_XMLDecl_lt({<<"?xm"/utf8>>, State}) ->
    parse_XMLDecl_ltqxm(cf(State));
parse_XMLDecl_lt({<<"?x"/utf8>>, State}) ->
    parse_XMLDecl_ltqx(cf(State));
parse_XMLDecl_lt({<<"?"/utf8>>, State}) ->
    parse_XMLDecl_ltq(cf(State));
parse_XMLDecl_lt({Bytes, State}) ->
    % default declaration, came in with one byte, so assuming a prepend is okay here.
    State1 = set_next_parser_position(?misc_pre_dtd, State),
    yaccety_sax:event_startDocument(
        <<"1.0">>,
        <<"UTF-8">>,
        false,
        false,
        false,
        set_state_pos(State1, <<"<", Bytes/bitstring>>)
    ).

parse_XMLDecl_ltq({<<"xml"/utf8, Rest/bitstring>> = Stream, State}) ->
    parse_XMLDecl_ltqxml(Rest, Stream, 3, State);
parse_XMLDecl_ltq({<<"xm"/utf8>>, State}) ->
    parse_XMLDecl_ltqxm(cf(State));
parse_XMLDecl_ltq({<<"x"/utf8>>, State}) ->
    parse_XMLDecl_ltqx(cf(State));
parse_XMLDecl_ltq({Bytes, State}) ->
    % default declaration, came in with one byte, so assuming a prepend is okay here.
    State1 = set_next_parser_position(?misc_pre_dtd, State),
    yaccety_sax:event_startDocument(
        <<"1.0">>,
        <<"UTF-8">>,
        false,
        false,
        false,
        set_state_pos(State1, <<"<?", Bytes/bitstring>>)
    ).

parse_XMLDecl_ltqx({<<"ml"/utf8, Rest/bitstring>> = Stream, State}) ->
    parse_XMLDecl_ltqxml(Rest, Stream, 2, State);
parse_XMLDecl_ltqx({<<"m"/utf8>>, State}) ->
    parse_XMLDecl_ltqxm(cf(State));
parse_XMLDecl_ltqx({_, State}) ->
    fatal_error(bad_xmldecl, State).

parse_XMLDecl_ltqxm({<<"l"/utf8, Rest/bitstring>> = Stream, State}) ->
    parse_XMLDecl_ltqxml(Rest, Stream, 1, State);
parse_XMLDecl_ltqxm({_, State}) ->
    fatal_error(bad_xmldecl, State).

%%----------------------------------------------------------------------
%% [27] Misc ::= Comment | PI | S
%%----------------------------------------------------------------------
parse_Misc(Stream, State) -> parse_Misc(Stream, Stream, 0, State).

parse_Misc(<<"<!--"/utf8, Rest/bitstring>>, Stream, Pos, State) ->
    parse_Comment(Rest, Stream, Pos + 4, State);
parse_Misc(<<"<?"/utf8, Rest/bitstring>>, Stream, Pos, State) ->
    parse_PI(Rest, Stream, Pos + 2, State);
parse_Misc(<<"<!-"/utf8>>, _, _, State) ->
    parse_Misc_ltbangdash(cf(State));
parse_Misc(<<"<!"/utf8>>, _, _, State) ->
    parse_Misc_ltbang(cf(State));
parse_Misc(<<"<"/utf8>>, _, _, State) ->
    parse_Misc_lt(cf(State));
parse_Misc(no_bytes, _, _, State) ->
    {no_bytes, State};
parse_Misc(<<>>, _, _, State) ->
    case cf(State) of
        {no_bytes, State1} -> {no_bytes, State1};
        {Stream1, State1} -> parse_Misc(Stream1, Stream1, 0, State1)
    end;
parse_Misc(?MATCH) ->
    case maybe_consume_s(?MATCH) of
        {true, ?MATCH1} ->
            parse_Misc(?MATCH1);
        {false, Bytes1, _, _, State1} ->
            set_state_pos(State1, Bytes1)
    end.

parse_Misc_lt({<<"!--"/utf8, Rest/bitstring>> = Stream, State}) ->
    parse_Comment(Rest, Stream, 3, State);
parse_Misc_lt({<<"!-"/utf8>>, State}) ->
    parse_Misc_ltbangdash(cf(State));
parse_Misc_lt({<<"!"/utf8>>, State}) ->
    parse_Misc_ltbang(cf(State));
parse_Misc_lt({<<"?"/utf8, Rest/bitstring>> = Stream, State}) ->
    parse_PI(Rest, Stream, 1, State);
parse_Misc_lt({Bytes, State}) ->
    set_state_pos(State, <<"<", Bytes/bitstring>>).

parse_Misc_ltbang({<<"--"/utf8, Rest/bitstring>> = Stream, State}) ->
    parse_Comment(Rest, Stream, 2, State);
parse_Misc_ltbang({<<"-"/utf8>>, State}) ->
    parse_Misc_ltbangdash(cf(State));
parse_Misc_ltbang({Bytes, State}) ->
    set_state_pos(State, <<"<!", Bytes/bitstring>>).

parse_Misc_ltbangdash({<<"-"/utf8, Rest/bitstring>> = Stream, State}) ->
    parse_Comment(Rest, Stream, 1, State);
parse_Misc_ltbangdash({_, State}) ->
    fatal_error(bad_misc, State).

%%----------------------------------------------------------------------
%% [43] content ::= CharData? ((element | Reference | CDSect | PI | Comment) CharData?)*
%%----------------------------------------------------------------------
parse_content(Stream, State) -> parse_content(Stream, Stream, 0, State).

parse_content(
    <<"</"/utf8, Rest/bitstring>>,
    Stream,
    Pos,
    #ys_state{in_replacement = false} = State
) ->
    case State#ys_state.namespace_aware of
        true ->
            parse_ETag(Rest, Stream, Pos + 2, State);
        false ->
            parse_ETag_no_ns(Rest, Stream, Pos + 2, State)
    end;
parse_content(<<"<!--"/utf8, Rest/bitstring>>, Stream, Pos, State) ->
    parse_Comment(Rest, Stream, Pos + 4, State);
parse_content(<<"<!-"/utf8>>, _, _, State) ->
    parse_content_ltbangdash(cf(State));
parse_content(<<"<!["/utf8, Rest/bitstring>>, Stream, Pos, State) ->
    parse_CDSect(Rest, Stream, Pos + 3, State);
parse_content(<<"<!"/utf8>>, _, _, State) ->
    parse_content_ltbang(cf(State));
parse_content(<<"<?"/utf8, Rest/bitstring>>, Stream, Pos, State) ->
    parse_PI(Rest, Stream, Pos + 2, State);
parse_content(<<"<"/utf8>>, _, _, State) ->
    parse_content_lt(cf(State));
parse_content(<<"<"/utf8, Rest/bitstring>>, Stream, Pos, State) ->
    case State#ys_state.namespace_aware of
        true ->
            parse_element_lt(Rest, Stream, Pos + 1, State);
        false ->
            parse_element_lt_no_ns(Rest, Stream, Pos + 1, State)
    end;
parse_content(<<"&"/utf8, Rest/bitstring>>, Stream, Pos, State) ->
    case parse_Reference(Rest, Stream, Pos + 1, State, content) of
        {{_, {replacement_text, <<>>}}, ?MATCH1} ->
            ?FUNCTION_NAME(?MATCH1);
        {{Name, {replacement_text, Ref}}, Bytes1, _, _, State1} ->
            RTState = replacement_text_state(Bytes1, State1, Name),
            ?FUNCTION_NAME(Ref, Ref, 0, RTState);
        {{gen, Ref}, ?MATCH1} ->
            parse_CharData(?MATCH1, Ref);
        {{Type, Ref}, ?MATCH1} when Type == hex; Type == dec ->
            parse_CharData(?MATCH1, Ref);
        {{Name, Ref}, Bytes1, _, _, State1} ->
            RTState = replacement_text_state(Bytes1, State1, Name),
            ?FUNCTION_NAME(Ref, Ref, 0, RTState)
    end;
parse_content(<<>>, _, _, State) ->
    {Stream1, State1} = cf(State),
    parse_content(Stream1, Stream1, 0, State1);
parse_content(?MATCH) ->
    parse_CharData(?MATCH).

parse_content_lt({<<"!--"/utf8, Rest/bitstring>> = Stream, State}) ->
    parse_Comment(Rest, Stream, 3, State);
parse_content_lt({<<"!-"/utf8>>, State}) ->
    parse_content_ltbangdash(cf(State));
parse_content_lt({<<"!["/utf8, Rest/bitstring>> = Stream, State}) ->
    parse_CDSect(Rest, Stream, 2, State);
parse_content_lt({<<"/"/utf8, Rest/bitstring>> = Stream, State}) ->
    case State#ys_state.namespace_aware of
        true ->
            parse_ETag(Rest, Stream, 1, State);
        false ->
            parse_ETag_no_ns(Rest, Stream, 1, State)
    end;
parse_content_lt({<<"!"/utf8>>, State}) ->
    parse_content_ltbang(cf(State));
parse_content_lt({<<"?"/utf8, Rest/bitstring>> = Stream, State}) ->
    parse_PI(Rest, Stream, 1, State);
parse_content_lt({<<>>, State}) ->
    parse_content_lt(cf(State));
parse_content_lt({Stream, State}) ->
    case State#ys_state.namespace_aware of
        true ->
            parse_element_lt(Stream, Stream, 0, State);
        false ->
            parse_element_lt_no_ns(Stream, Stream, 0, State)
    end.

parse_content_ltbang({<<"--"/utf8, Rest/bitstring>> = Stream, State}) ->
    parse_Comment(Rest, Stream, 2, State);
parse_content_ltbang({<<"-"/utf8>>, State}) ->
    parse_content_ltbangdash(cf(State));
parse_content_ltbang({<<"["/utf8, Rest/bitstring>> = Stream, State}) ->
    parse_CDSect(Rest, Stream, 1, State);
parse_content_ltbang({_, State}) ->
    fatal_error(bad_content, State).

parse_content_ltbangdash({<<"-"/utf8, Rest/bitstring>> = Stream, State}) ->
    parse_Comment(Rest, Stream, 1, State);
parse_content_ltbangdash({_, State}) ->
    fatal_error(bad_content, State).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%
%%                                                                                                                   %%
%%                                            Namespace Stuff                                                        %%
%%                                                                                                                   %%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%

%%----------------------------------------------------------------------
%% [39] element ::= EmptyElemTag | STag content ETag
%%    [WFC: Element Type Match]
%% [40] STag    ::= '<' Name (S Attribute)* S? '>'
%%    [WFC: Unique Att Spec]
%% [12ns] STag                ::=       '<' QName (S Attribute)* S? '>'
%%    [NSC: Prefix Declared]
%%    [WFC: Unique Att Spec]
%% [14ns] EmptyElemTag        ::=       '<' QName (S Attribute)* S? '/>'
%%    [NSC: Prefix Declared]
%%    [WFC: Unique Att Spec]
%%----------------------------------------------------------------------
parse_element(Stream, State) -> parse_element(Stream, Stream, 0, State).

parse_element(<<$</utf8, Rest/bitstring>>, Stream, Pos, State) ->
    case State#ys_state.namespace_aware of
        true ->
            parse_element_lt(Rest, Stream, Pos + 1, State);
        false ->
            parse_element_lt_no_ns(Rest, Stream, Pos + 1, State)
    end;
parse_element(<<>>, _, _, State) ->
    {Stream1, State1} = cf(State),
    parse_element(Stream1, Stream1, 0, State1);
parse_element(B, _, _, State) ->
    fatal_error(non_element, {B, State}).

parse_element_lt_no_ns(?MATCH) ->
    #ys_state{
        tags = Tags,
        position = P
    } = State,
    {{NameP, NameL}, ?MATCH1} = parse_QName(?MATCH),
    QName = {<<>>, NameP, NameL},
    case Bytes1 of
        <<$>/utf8, Bytes2/bitstring>> ->
            State2 = State1#ys_state{
                position = [?content | P],
                tags = [QName | Tags],
                rest_stream = Bytes2
            },
            yaccety_sax:event_startElement(QName, [], [], State2);
        <<$//utf8, Rest/bitstring>> ->
            parse_element_empty_no_ns(
                Rest,
                Stream1,
                Pos1 + 1,
                State1,
                QName,
                [],
                P,
                Tags
            );
        _ ->
            {As, ?MATCH2} = parse_attributes_no_ns(?MATCH1, []),
            case Bytes2 of
                <<$>/utf8, Bytes3/bitstring>> ->
                    State3 = State2#ys_state{
                        position = [?content | P],
                        tags = [QName | Tags],
                        rest_stream = Bytes3
                    },
                    yaccety_sax:event_startElement(QName, As, [], State3);
                <<$//utf8, Rest/bitstring>> ->
                    parse_element_empty_no_ns(
                        Rest,
                        Stream2,
                        Pos2 + 1,
                        State2,
                        QName,
                        As,
                        P,
                        Tags
                    );
                _ ->
                    fatal_error(bad_element, State2)
            end
    end.

parse_element_lt(
    Bytes,
    Stream,
    Pos,
    #ys_state{
        tags = Tags,
        inscope_ns = [LastNss | _] = Nss,
        position = P,
        dtd = undefined
    } = State
) ->
    {{NameP, NameL}, ?MATCH1} = parse_QName(?MATCH),
    case Bytes1 of
        <<$>/utf8, Bytes2/bitstring>> ->
            QName = expand_qname(NameP, NameL, LastNss),
            State2 = State1#ys_state{
                position = [?content | P],
                tags = [QName | Tags],
                inscope_ns = [LastNss | Nss],
                rest_stream = Bytes2
            },
            yaccety_sax:event_startElement(QName, [], [], State2);
        <<$//utf8, Rest/bitstring>> ->
            QName = expand_qname(NameP, NameL, LastNss),
            parse_element_empty(
                Rest,
                Stream1,
                Pos1 + 1,
                State1,
                QName,
                [],
                [],
                P,
                Tags
            );
        _ ->
            {{Ns, As}, ?MATCH2} = parse_attributes(
                ?MATCH1,
                [],
                [],
                {NameP, NameL},
                []
            ),
            NewNsMap = namespace_map_from_list(Ns),
            NamespaceMap = merge_namespaces(LastNss, NewNsMap),
            QName = expand_qname(NameP, NameL, NamespaceMap),
            As1 = qualify_attribute_names(As, NamespaceMap),
            ok = check_attributes(As1, State2),
            case Bytes2 of
                <<$>/utf8, Bytes3/bitstring>> ->
                    State3 = State2#ys_state{
                        position = [?content | P],
                        tags = [QName | Tags],
                        inscope_ns = [NamespaceMap | Nss],
                        rest_stream = Bytes3
                    },
                    yaccety_sax:event_startElement(QName, As1, Ns, State3);
                <<$//utf8, Rest/bitstring>> ->
                    parse_element_empty(
                        Rest,
                        Stream2,
                        Pos2 + 1,
                        State2,
                        QName,
                        As1,
                        Ns,
                        P,
                        Tags
                    );
                _ ->
                    fatal_error(bad_element, State2)
            end
    end;
parse_element_lt(?MATCH) ->
    #ys_state{
        tags = Tags,
        inscope_ns = [LastNss | _] = Nss,
        position = P,
        dtd = DTD
    } = State,
    {{NameP, NameL}, ?MATCH1} = parse_QName(?MATCH),
    DTDAtts =
        case DTD of
            #{atts := #{{NameP, NameL} := AttList}} -> AttList;
            _ -> []
        end,
    {{Ns, As}, ?MATCH2} = parse_attributes(
        ?MATCH1,
        [],
        [],
        {NameP, NameL},
        DTDAtts
    ),
    NewNsMap = namespace_map_from_list(Ns),
    NamespaceMap = merge_namespaces(LastNss, NewNsMap),
    QName = expand_qname(NameP, NameL, NamespaceMap),
    As1 = qualify_attribute_names(As, NamespaceMap),
    ok = check_attributes(As1, State2),
    case Bytes2 of
        <<$>/utf8, Bytes3/bitstring>> ->
            State3 = State2#ys_state{
                position = [?content | P],
                tags = [QName | Tags],
                inscope_ns = [NamespaceMap | Nss],
                rest_stream = Bytes3,
                in_replacement = false
            },
            yaccety_sax:event_startElement(QName, As1, Ns, State3);
        <<$//utf8, Rest/bitstring>> ->
            parse_element_empty(
                Rest,
                Stream2,
                Pos2 + 1,
                State2,
                QName,
                As1,
                Ns,
                P,
                Tags
            );
        _ ->
            fatal_error(bad_element, State2)
    end.

check_attributes([], _State) -> ok;
check_attributes(Atts, State) -> check_attributes(Atts, #{}, State).

check_attributes([{{?XML_NS, <<"xml">>, <<"space">>}, Value} | _], _, State) when
    Value =/= <<"preserve">>, Value =/= <<"default">>
->
    fatal_error(bad_xml_space, State);
check_attributes([{{Ns, _, Ln}, _} | _], Acc, State) when is_map_key({Ns, Ln}, Acc) ->
    fatal_error(duplicate_attribute, {{Ns, Ln}, State});
check_attributes([{{Ns, _, Ln}, _} | Atts], Acc, State) ->
    check_attributes(Atts, Acc#{{Ns, Ln} => []}, State);
check_attributes([], _, _) ->
    ok.

parse_element_empty_no_ns(<<$>, Bytes/bitstring>>, _, _, State, QName, Ats, P, Tags) ->
    Pss =
        case P of
            % Empty root element
            [?element | Ps] -> [?empty, ?misc_post_element | Ps];
            _ -> [?empty | P]
        end,
    State1 = State#ys_state{
        position = Pss,
        tags = [QName | Tags],
        rest_stream = Bytes
    },
    yaccety_sax:event_startElement(QName, Ats, [], State1);
parse_element_empty_no_ns(<<>>, _, _, State, Name, Ats, P, Tags) ->
    {Stream1, State1} = cf(State),
    parse_element_empty_no_ns(Stream1, Stream1, 0, State1, Name, Ats, P, Tags);
parse_element_empty_no_ns(_, _, _, State, _, _, _, _) ->
    fatal_error(bad_element, State).

% misc_post_element
parse_element_empty(<<$>, Bytes/bitstring>>, _, _, State, QName, Ats, Nss, P, Tags) ->
    Pss =
        case P of
            % Empty root element
            [?element | Ps] -> [?empty, ?misc_post_element | Ps];
            _ -> [?empty | P]
        end,
    State1 = State#ys_state{
        position = Pss,
        tags = [QName | Tags],
        rest_stream = Bytes
    },
    yaccety_sax:event_startElement(QName, Ats, Nss, State1);
parse_element_empty(<<>>, _, _, State, Name, Ats, Nss, P, Tags) ->
    {Stream1, State1} = cf(State),
    parse_element_empty(Stream1, Stream1, 0, State1, Name, Ats, Nss, P, Tags);
parse_element_empty(_, _, _, State, _, _, _, _, _) ->
    fatal_error(bad_element, State).

%%----------------------------------------------------------------------
%% [41] Attribute ::= Name Eq AttValue
%% [WFC: No External Entity References]
%% [WFC: No < in Attribute Values]
%% Initial S is trimmed, first character is not ">" or "/"
%% [1ns]  NSAttName           ::=       PrefixedAttName | DefaultAttName
%% [2ns]  PrefixedAttName     ::=       'xmlns:' NCName
%%    [NSC: Reserved Prefixes and Namespace Names]
%% [3ns]  DefaultAttName      ::=       'xmlns'
%% [15ns] Attribute           ::=       NSAttName Eq AttValue | QName Eq AttValue
%%    [NSC: Prefix Declared]
%%    [NSC: No Prefix Undeclaring]
%%    [NSC: Attributes Unique]
%%    [VC: Attribute Value Type]
%%----------------------------------------------------------------------
parse_Attribute(?MATCH) ->
    {{P, L}, ?MATCH1} = parse_QName(?MATCH),
    {Bytes2, Stream2, Pos2, State2} = parse_Eq(?MATCH1),
    {Value, ?MATCH3} = parse_AttValue(?MATCH2),
    {{P, L, Value}, ?MATCH3}.

parse_attributes_no_ns(Bytes = <<$>/utf8, _/bitstring>>, Stream, Pos, State, Atts) ->
    {Atts, ?MATCH};
parse_attributes_no_ns(?MATCH, Atts) ->
    case maybe_consume_s(?MATCH) of
        {false, ?MATCH1} ->
            {Atts, ?MATCH1};
        {true, <<C/utf8, _/bitstring>> = ?MATCH1} when C == $>; C == $/ ->
            {Atts, ?MATCH1};
        {true, ?MATCH1} ->
            {{P, L, Value}, ?MATCH2} = parse_Attribute(?MATCH1),
            parse_attributes_no_ns(?MATCH2, [{{<<>>, P, L}, Value} | Atts])
    end.

fill_default_attributes(Atts, [], _) ->
    Atts;
fill_default_attributes(Atts, DAtts, State) ->
    [
        {Px, Ln,
            case Type of
                cdata -> Fixed2;
                _ -> normalize_attribute_value(Fixed2)
            end}
     || {{Px, Ln}, {Type, {DefType, Fixed}}} <- maps:to_list(DAtts),
        Px =/= <<"xmlns">>,
        DefType == value orelse DefType == fixed,
        not lists:any(fun({AttPx, AttLn, _AttVal}) -> {AttPx, AttLn} == {Px, Ln} end, Atts),
        Fixed2 <- [parse_AttValue_x(Fixed, Fixed, 0, 0, State, [])]
    ] ++ Atts.

fill_default_namespaces(Nss, [], _) ->
    Nss;
fill_default_namespaces(Nss, DAtts, State) ->
    [
        {
            case Type of
                cdata -> Fixed2;
                _ -> normalize_attribute_value(Fixed2)
            end,
            NsPx
        }
     || {{Xmlns, NsPx}, {Type, {DefType, Fixed}}} <- maps:to_list(DAtts),
        Xmlns == <<"xmlns">>,
        DefType == value orelse DefType == fixed,
        not lists:any(fun({_Uri, NsPx1}) -> NsPx1 == NsPx end, Nss),
        Fixed2 <- [parse_AttValue_x(Fixed, Fixed, 0, 0, State, [])]
    ] ++ Nss.

parse_attributes(Bytes = <<$>/utf8, _/bitstring>>, Stream, Pos, State, Nss, Atts, _, AttList) ->
    NewAtts = fill_default_attributes(Atts, AttList, State),
    NewNss = fill_default_namespaces(Nss, AttList, State),
    {{NewNss, NewAtts}, ?MATCH};
parse_attributes(?MATCH, Nss, Atts, EName, AttList) ->
    case maybe_consume_s(?MATCH) of
        {false, ?MATCH1} ->
            NewAtts = fill_default_attributes(Atts, AttList, State1),
            NewNss = fill_default_namespaces(Nss, AttList, State),
            {{NewNss, NewAtts}, ?MATCH1};
        {true, <<C/utf8, _/bitstring>> = ?MATCH1} when C == $>; C == $/ ->
            NewAtts = fill_default_attributes(Atts, AttList, State1),
            NewNss = fill_default_namespaces(Nss, AttList, State),
            {{NewNss, NewAtts}, ?MATCH1};
        {true, ?MATCH1} ->
            {Att, ?MATCH2} = parse_Attribute(?MATCH1),
            parse_attributes_(Att, ?MATCH2, Nss, Atts, EName, AttList)
    end.

parse_attributes_({<<>>, <<"xmlns">>, ?XML_NS}, _, _, _, State, _, _, _, _) ->
    fatal_error(rebinding_prefix, {<<"xml">>, State});
parse_attributes_({<<>>, <<"xmlns">>, ?XMLNS_NS}, _, _, _, State, _, _, _, _) ->
    fatal_error(rebinding_prefix, {<<"xmlns">>, State});
parse_attributes_({<<>>, <<"xmlns">>, Val}, ?MATCH, Nss, Atts, EName, AttList) ->
    parse_attributes(?MATCH, [{normalize_attribute_value(Val), <<>>} | Nss], Atts, EName, AttList);
parse_attributes_({<<"xmlns">>, Px, <<>>}, _, _, _, State, _, _, _, _) ->
    fatal_error(unbinding_prefix, {Px, State});
parse_attributes_({<<"xmlns">>, <<"xml">>, Val}, _, _, _, State, _, _, _, _) when Val =/= ?XML_NS ->
    fatal_error(rebinding_prefix, {<<"xml">>, State});
parse_attributes_({<<"xmlns">>, Px, ?XML_NS}, _, _, _, State, _, _, _, _) when Px =/= <<"xml">> ->
    fatal_error(rebinding_prefix, {<<"xml">>, State});
parse_attributes_({<<"xmlns">>, _, ?XMLNS_NS}, _, _, _, State, _, _, _, _) ->
    fatal_error(rebinding_prefix, {<<"xmlns">>, State});
parse_attributes_({<<"xmlns">>, <<"xmlns">>, _}, _, _, _, State, _, _, _, _) ->
    fatal_error(rebinding_prefix, {<<"xmlns">>, State});
parse_attributes_({<<"xmlns">>, Px, Val}, ?MATCH, Nss, Atts, EName, AttList) ->
    parse_attributes(?MATCH, [{normalize_attribute_value(Val), Px} | Nss], Atts, EName, AttList);
parse_attributes_(Other, ?MATCH, Nss, Atts, EName, []) ->
    parse_attributes(?MATCH, Nss, [Other | Atts], EName, []);
parse_attributes_({Px, Ln, Val}, ?MATCH, Nss, Atts, EName, AttList) ->
    {Norm, AttList1} =
        case AttList of
            #{{Px, Ln} := {_, cdata, _}} ->
                {Val, maps:remove({Px, Ln}, AttList)};
            #{{Px, Ln} := {cdata, _}} ->
                {Val, maps:remove({Px, Ln}, AttList)};
            #{{Px, Ln} := _} ->
                {normalize_attribute_value(Val), maps:remove({Px, Ln}, AttList)};
            _ ->
                {Val, AttList}
        end,
    parse_attributes(?MATCH, Nss, [{Px, Ln, Norm} | Atts], EName, AttList1).

%%----------------------------------------------------------------------
%% [10] AttValue ::= '"' ([^<&"] | Reference)* '"' |  "'" ([^<&'] | Reference)* "'"
%%----------------------------------------------------------------------
parse_AttValue(<<$'/utf8, Rest/bitstring>>, Stream, Pos, State) ->
    parse_AttValue_sq(Rest, Stream, Pos + 1, 0, State, []);
parse_AttValue(<<$"/utf8, Rest/bitstring>>, Stream, Pos, State) ->
    parse_AttValue_dq(Rest, Stream, Pos + 1, 0, State, []);
parse_AttValue(<<>>, _, _, State) ->
    {Stream1, State1} = cf(State),
    parse_AttValue(Stream1, Stream1, 0, State1);
parse_AttValue(_, _, _, State) ->
    fatal_error(bad_attval, State).

parse_AttValue_x(
    <<$\r, $\n, Rest/bitstring>>,
    Stream,
    Pos,
    Len,
    #ys_state{in_replacement = false} = State,
    Acc
) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    Acc2 = ?APPEND(<<$\s>>, Acc1),
    ?FUNCTION_NAME(Rest, Stream, Pos + Len + 2, 0, State, Acc2);
parse_AttValue_x(<<$</utf8, _/bitstring>>, _, _, _, State, _Acc) ->
    fatal_error(bad_attval, State);
parse_AttValue_x(<<$&/utf8, Rest/bitstring>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    case parse_Reference(Rest, Stream, Pos + Len + 1, State, attribute) of
        {{Name, {replacement_text, Ref}}, Bytes1, _, _, State1} ->
            RTState = replacement_text_state(Bytes1, State1, Name),
            ?FUNCTION_NAME(Ref, Ref, 0, 0, RTState, Acc1);
        {{gen, Ref}, ?MATCH1} ->
            Acc2 = ?APPEND(Ref, Acc1),
            ?FUNCTION_NAME(Bytes1, Stream1, Pos1, 0, State1, Acc2);
        {{Type, Ref}, ?MATCH1} when Type == hex; Type == dec ->
            Acc2 = ?APPEND(Ref, Acc1),
            ?FUNCTION_NAME(Bytes1, Stream1, Pos1, 0, State1, Acc2);
        {{Name, Ref}, Bytes1, _, _, State1} ->
            RTState = replacement_text_state(Bytes1, State1, Name),
            ?FUNCTION_NAME(Ref, Ref, 0, 0, RTState, Acc1)
    end;
parse_AttValue_x(<<>>, Stream, Pos, Len, _State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    to_binary(Acc1);
parse_AttValue_x(<<Char, Rest/bitstring>>, Stream, Pos, Len, State, Acc) when ?WS(Char) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    Acc2 = ?APPEND(<<$\s>>, Acc1),
    ?FUNCTION_NAME(Rest, Stream, Pos + Len + 1, 0, State, Acc2);
?ONECHAR.

parse_AttValue_sq(
    <<$', Rest/bitstring>>,
    Stream,
    Pos,
    Len,
    #ys_state{in_replacement = true} = State,
    Acc
) ->
    ?FUNCTION_NAME(Rest, Stream, Pos, Len + 1, State, Acc);
parse_AttValue_sq(
    <<$\r, $\n, Rest/bitstring>>,
    Stream,
    Pos,
    Len,
    #ys_state{in_replacement = false} = State,
    Acc
) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    Acc2 = ?APPEND(<<$\s>>, Acc1),
    ?FUNCTION_NAME(Rest, Stream, Pos + Len + 2, 0, State, Acc2);
parse_AttValue_sq(<<$\r>>, Stream, Pos, Len, #ys_state{in_replacement = false} = State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    case cf(State) of
        {<<$\n, Rest/bitstring>> = Stream1, State1} ->
            Acc2 = ?APPEND(<<$\s>>, Acc1),
            ?FUNCTION_NAME(Rest, Stream1, 1, 0, State1, Acc2);
        {Stream1, State1} ->
            Acc2 = ?APPEND(<<$\s>>, Acc1),
            ?FUNCTION_NAME(Stream1, Stream1, 0, 0, State1, Acc2)
    end;
parse_AttValue_sq(<<$'/utf8, Rest/bitstring>>, Stream, Pos, Len, State, Acc) ->
    Value = ?ACC(Stream, Pos, Len, Acc),
    {to_binary(Value), Rest, Stream, Pos + Len + 1, State};
parse_AttValue_sq(<<$</utf8, _/bitstring>>, _, _, _, State, _Acc) ->
    fatal_error(bad_attval, State);
parse_AttValue_sq(<<$&/utf8, Rest/bitstring>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    case parse_Reference(Rest, Stream, Pos + Len + 1, State, attribute) of
        {{Name, {replacement_text, Ref}}, Bytes1, _, _, State1} ->
            RTState = replacement_text_state(Bytes1, State1, Name),
            ?FUNCTION_NAME(Ref, Ref, 0, 0, RTState, Acc1);
        {{gen, Ref}, ?MATCH1} ->
            Acc2 = ?APPEND(Ref, Acc1),
            ?FUNCTION_NAME(Bytes1, Stream1, Pos1, 0, State1, Acc2);
        {{Type, Ref}, ?MATCH1} when Type == hex; Type == dec ->
            Acc2 = ?APPEND(Ref, Acc1),
            ?FUNCTION_NAME(Bytes1, Stream1, Pos1, 0, State1, Acc2);
        {{Name, Ref}, Bytes1, _, _, State1} ->
            RTState = replacement_text_state(Bytes1, State1, Name),
            ?FUNCTION_NAME(Ref, Ref, 0, 0, RTState, Acc1)
    end;
parse_AttValue_sq(<<>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    {Stream1, State1} = cf(State),
    ?FUNCTION_NAME(Stream1, Stream1, 0, 0, State1, Acc1);
parse_AttValue_sq(<<Char, Rest/bitstring>>, Stream, Pos, Len, State, Acc) when ?WS(Char) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    Acc2 = ?APPEND(<<$\s>>, Acc1),
    ?FUNCTION_NAME(Rest, Stream, Pos + Len + 1, 0, State, Acc2);
?ONECHAR.

parse_AttValue_dq(
    <<$", Rest/bitstring>>, Stream, Pos, Len, #ys_state{in_replacement = true} = State, Acc
) ->
    ?FUNCTION_NAME(Rest, Stream, Pos, Len + 1, State, Acc);
parse_AttValue_dq(
    <<$\r, $\n, Rest/bitstring>>, Stream, Pos, Len, #ys_state{in_replacement = false} = State, Acc
) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    Acc2 = ?APPEND(<<$\s>>, Acc1),
    ?FUNCTION_NAME(Rest, Stream, Pos + Len + 2, 0, State, Acc2);
parse_AttValue_dq(<<$\r>>, Stream, Pos, Len, #ys_state{in_replacement = false} = State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    case cf(State) of
        {<<$\n, Rest/bitstring>> = Stream1, State1} ->
            Acc2 = ?APPEND(<<$\s>>, Acc1),
            ?FUNCTION_NAME(Rest, Stream1, 1, 0, State1, Acc2);
        {Stream1, State1} ->
            Acc2 = ?APPEND(<<$\s>>, Acc1),
            ?FUNCTION_NAME(Stream1, Stream1, 0, 0, State1, Acc2)
    end;
parse_AttValue_dq(<<$"/utf8, Rest/bitstring>>, Stream, Pos, Len, State, Acc) ->
    Value = ?ACC(Stream, Pos, Len, Acc),
    {to_binary(Value), Rest, Stream, Pos + Len + 1, State};
parse_AttValue_dq(<<$</utf8, _/bitstring>>, _, _, _, State, _Acc) ->
    fatal_error(bad_attval, State);
parse_AttValue_dq(<<$&/utf8, Rest/bitstring>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    case parse_Reference(Rest, Stream, Pos + Len + 1, State, attribute) of
        {{Name, {replacement_text, Ref}}, Bytes1, _, _, State1} ->
            RTState = replacement_text_state(Bytes1, State1, Name),
            ?FUNCTION_NAME(Ref, Ref, 0, 0, RTState, Acc1);
        {{gen, Ref}, ?MATCH1} ->
            Acc2 = ?APPEND(Ref, Acc1),
            ?FUNCTION_NAME(Bytes1, Stream1, Pos1, 0, State1, Acc2);
        {{Type, Ref}, ?MATCH1} when Type == hex; Type == dec ->
            Acc2 = ?APPEND(Ref, Acc1),
            ?FUNCTION_NAME(Bytes1, Stream1, Pos1, 0, State1, Acc2);
        {{Name, Ref}, Bytes1, _, _, State1} ->
            RTState = replacement_text_state(Bytes1, State1, Name),
            ?FUNCTION_NAME(Ref, Ref, 0, 0, RTState, Acc1)
    end;
parse_AttValue_dq(<<>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    {Stream1, State1} = cf(State),
    ?FUNCTION_NAME(Stream1, Stream1, 0, 0, State1, Acc1);
parse_AttValue_dq(<<Char, Rest/bitstring>>, Stream, Pos, Len, State, Acc) when ?WS(Char) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    Acc2 = ?APPEND(<<$\s>>, Acc1),
    ?FUNCTION_NAME(Rest, Stream, Pos + Len + 1, 0, State, Acc2);
?ONECHAR.

parse_AttValue(<<$'/utf8, Rest/bitstring>>, Stream, Pos, State, DTD, External) ->
    parse_AttValue_sq(Rest, Stream, Pos + 1, 0, State, [], DTD, External);
parse_AttValue(<<$"/utf8, Rest/bitstring>>, Stream, Pos, State, DTD, External) ->
    parse_AttValue_dq(Rest, Stream, Pos + 1, 0, State, [], DTD, External);
parse_AttValue(<<>>, _, _, State, DTD, External) ->
    {Stream1, State1} = cf(State),
    parse_AttValue(Stream1, Stream1, 0, State1, DTD, External);
parse_AttValue(_, _, _, State, _DTD, _External) ->
    fatal_error(bad_attval, State).

parse_AttValue_sq(
    <<$', Rest/bitstring>>,
    Stream,
    Pos,
    Len,
    #ys_state{in_replacement = true} = State,
    Acc,
    DTD,
    External
) ->
    ?FUNCTION_NAME(Rest, Stream, Pos, Len + 1, State, Acc, DTD, External);
parse_AttValue_sq(<<$'/utf8, Rest/bitstring>>, Stream, Pos, Len, State, Acc, _DTD, _External) ->
    Value = ?ACC(Stream, Pos, Len, Acc),
    {to_binary(Value), Rest, Stream, Pos + Len + 1, State};
parse_AttValue_sq(<<$</utf8, _/bitstring>>, _, _, _, State, _Acc, _, _) ->
    fatal_error(bad_attval, State);
parse_AttValue_sq(<<$&/utf8, Rest/bitstring>>, Stream, Pos, Len, State, Acc, DTD, External) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    case parse_Reference(Rest, Stream, Pos + Len + 1, State#ys_state{dtd = DTD}, attribute) of
        {{Name, {replacement_text, _Ref}}, ?MATCH1} ->
            Acc2 = ?APPEND([$&, Name, $;], Acc1),
            ?FUNCTION_NAME(Bytes1, Stream1, Pos1, 0, State1, Acc2, DTD, External);
        {{gen, Ref}, ?MATCH1} ->
            Acc2 = ?APPEND(Ref, Acc1),
            ?FUNCTION_NAME(Bytes1, Stream1, Pos1, 0, State1, Acc2, DTD, External);
        {{Type, Ref}, ?MATCH1} when Type == hex; Type == dec ->
            Acc2 = ?APPEND(Ref, Acc1),
            ?FUNCTION_NAME(Bytes1, Stream1, Pos1, 0, State1, Acc2, DTD, External);
        {{Name, _Ref}, ?MATCH1} ->
            Acc2 = ?APPEND([$&, Name, $;], Acc1),
            ?FUNCTION_NAME(Bytes1, Stream1, Pos1, 0, State1, Acc2, DTD, External)
    end;
parse_AttValue_sq(<<>>, Stream, Pos, Len, State, Acc, DTD, External) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    {Stream1, State1} = cf(State),
    ?FUNCTION_NAME(Stream1, Stream1, 0, 0, State1, Acc1, DTD, External);
parse_AttValue_sq(<<Char, Rest/bitstring>>, Stream, Pos, Len, State, Acc, DTD, External) when
    ?WS(Char)
->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    Acc2 = ?APPEND(<<$\s>>, Acc1),
    ?FUNCTION_NAME(Rest, Stream, Pos + Len + 1, 0, State, Acc2, DTD, External);
?ONECHARX.

parse_AttValue_dq(
    <<$", Rest/bitstring>>,
    Stream,
    Pos,
    Len,
    #ys_state{in_replacement = true} = State,
    Acc,
    DTD,
    External
) ->
    ?FUNCTION_NAME(Rest, Stream, Pos, Len + 1, State, Acc, DTD, External);
parse_AttValue_dq(<<$"/utf8, Rest/bitstring>>, Stream, Pos, Len, State, Acc, _DTD, _External) ->
    Value = ?ACC(Stream, Pos, Len, Acc),
    {to_binary(Value), Rest, Stream, Pos + Len + 1, State};
parse_AttValue_dq(<<$</utf8, _/bitstring>>, _, _, _, State, _Acc, _, _) ->
    fatal_error(bad_attval, State);
parse_AttValue_dq(<<$&/utf8, Rest/bitstring>>, Stream, Pos, Len, State, Acc, DTD, External) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    case parse_Reference(Rest, Stream, Pos + Len + 1, State#ys_state{dtd = DTD}, attribute) of
        {{Name, {replacement_text, _Ref}}, ?MATCH1} ->
            Acc2 = ?APPEND([$&, Name, $;], Acc1),
            ?FUNCTION_NAME(Bytes1, Stream1, Pos1, 0, State1, Acc2, DTD, External);
        {{gen, Ref}, ?MATCH1} ->
            Acc2 = ?APPEND(Ref, Acc1),
            ?FUNCTION_NAME(Bytes1, Stream1, Pos1, 0, State1, Acc2, DTD, External);
        {{Type, Ref}, ?MATCH1} when Type == hex; Type == dec ->
            Acc2 = ?APPEND(Ref, Acc1),
            ?FUNCTION_NAME(Bytes1, Stream1, Pos1, 0, State1, Acc2, DTD, External);
        {{Name, _Ref}, ?MATCH1} ->
            Acc2 = ?APPEND([$&, Name, $;], Acc1),
            ?FUNCTION_NAME(Bytes1, Stream1, Pos1, 0, State1, Acc2, DTD, External)
    end;
parse_AttValue_dq(<<>>, Stream, Pos, Len, State, Acc, DTD, External) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    {Stream1, State1} = cf(State),
    ?FUNCTION_NAME(Stream1, Stream1, 0, 0, State1, Acc1, DTD, External);
parse_AttValue_dq(<<Char, Rest/bitstring>>, Stream, Pos, Len, State, Acc, DTD, External) when
    ?WS(Char)
->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    Acc2 = ?APPEND(<<$\s>>, Acc1),
    ?FUNCTION_NAME(Rest, Stream, Pos + Len + 1, 0, State, Acc2, DTD, External);
?ONECHARX.

normalize_attribute_value(Value) ->
    Tokens = get_tokens(Value, Value, 0),
    Comb = combine_tokens(Tokens),
    to_binary(Comb).

get_tokens(<<>>, _, _) -> [];
get_tokens(<<$\s, Rest/bitstring>>, Value, Pos) -> get_tokens(Rest, Value, Pos + 1);
get_tokens(Bytes, Value, Pos) -> get_tokens(Bytes, Value, Pos, 0, []).

get_tokens(<<$\s, Rest/bitstring>>, Value, Pos, 0, Acc) ->
    get_tokens(Rest, Value, Pos + 1, 0, Acc);
get_tokens(<<$\s, Rest/bitstring>>, Value, Pos, Len, Acc) ->
    get_tokens(Rest, Value, Pos + Len + 1, 0, [binary_part(Value, Pos, Len) | Acc]);
get_tokens(<<_, Rest/bitstring>>, Value, Pos, Len, Acc) ->
    get_tokens(Rest, Value, Pos, Len + 1, Acc);
get_tokens(<<>>, _, _, 0, Acc) ->
    Acc;
get_tokens(<<>>, Value, Pos, Len, Acc) ->
    [binary_part(Value, Pos, Len) | Acc].

combine_tokens([]) -> <<>>;
combine_tokens([Token]) -> Token;
combine_tokens([Token | Rest]) -> combine_tokens(Rest, [Token]).

combine_tokens([], Acc) -> Acc;
combine_tokens([Token | Tokens], Acc) -> combine_tokens(Tokens, [Token | [<<$\s>> | Acc]]).

%%----------------------------------------------------------------------
%% -[42] ETag ::= '</' Name S? '>'
%% +[13ns] ETag ::= '</' QName S? '>'  [NSC: Prefix Declared]
%% '</' is already trimmed
%%----------------------------------------------------------------------
parse_ETag(
    Bytes,
    Stream,
    Pos,
    #ys_state{inscope_ns = [Ns | Nss], position = [_, ?element | _Ps1], tags = [Tag | Ts]} = State
) ->
    {{NameP, NameL}, ?MATCH1} = parse_QName(?MATCH),
    {_, Bytes2, _, _, State2} = maybe_consume_s(?MATCH1),
    case Bytes2 of
        <<$>/utf8, Bytes3/bitstring>> ->
            QName = expand_qname(NameP, NameL, Ns),
            State3 = State2#ys_state{inscope_ns = Nss, position = [?misc_post_element], tags = Ts},
            case QName of
                Tag ->
                    yaccety_sax:event_endElement(QName, set_state_pos(State3, Bytes3));
                _ ->
                    fatal_error(unmatched_tag, {Tag, QName})
            end;
        _ ->
            fatal_error(bad_endtag, State2)
    end;
parse_ETag(
    Bytes,
    Stream,
    Pos,
    #ys_state{inscope_ns = [Ns | Nss], position = [_ | Ps], tags = [Tag | Ts]} = State
) ->
    {{NameP, NameL}, ?MATCH1} = parse_QName(?MATCH),
    case Bytes1 of
        <<$>/utf8, Bytes2/bitstring>> ->
            QName = expand_qname(NameP, NameL, Ns),
            State2 = State1#ys_state{inscope_ns = Nss, position = Ps, tags = Ts},
            case QName of
                Tag ->
                    yaccety_sax:event_endElement(QName, set_state_pos(State2, Bytes2));
                _ ->
                    fatal_error(unmatched_tag, {Tag, QName})
            end;
        _ ->
            {_, Bytes2, _, _, State2} = maybe_consume_s(?MATCH1),
            case Bytes2 of
                <<$>/utf8, Bytes3/bitstring>> ->
                    QName = expand_qname(NameP, NameL, Ns),
                    State3 = State2#ys_state{inscope_ns = Nss, position = Ps, tags = Ts},
                    case QName of
                        Tag ->
                            yaccety_sax:event_endElement(
                                QName,
                                set_state_pos(State3, Bytes3)
                            );
                        _ ->
                            fatal_error(unmatched_tag, {Tag, QName})
                    end;
                _ ->
                    fatal_error(bad_endtag, State2)
            end
    end.

parse_ETag_no_ns(
    Bytes,
    Stream,
    Pos,
    #ys_state{position = [_, ?element | _Ps1], tags = [Tag | Ts]} = State
) ->
    {{NameP, NameL}, ?MATCH1} = parse_QName(?MATCH),
    {_, Bytes2, _, _, State2} = maybe_consume_s(?MATCH1),
    case Bytes2 of
        <<$>/utf8, Bytes3/bitstring>> ->
            QName = {<<>>, NameP, NameL},
            State3 = State2#ys_state{position = [?misc_post_element], tags = Ts},
            case QName of
                Tag ->
                    yaccety_sax:event_endElement(QName, set_state_pos(State3, Bytes3));
                _ ->
                    fatal_error(unmatched_tag, {Tag, QName})
            end;
        _ ->
            fatal_error(bad_endtag, State2)
    end;
parse_ETag_no_ns(
    Bytes,
    Stream,
    Pos,
    #ys_state{position = [_ | Ps], tags = [Tag | Ts]} = State
) ->
    {{NameP, NameL}, ?MATCH1} = parse_QName(?MATCH),
    case Bytes1 of
        <<$>/utf8, Bytes2/bitstring>> ->
            QName = {<<>>, NameP, NameL},
            State2 = State1#ys_state{position = Ps, tags = Ts},
            case QName of
                Tag ->
                    yaccety_sax:event_endElement(QName, set_state_pos(State2, Bytes2));
                _ ->
                    fatal_error(unmatched_tag, {Tag, QName})
            end;
        _ ->
            {_, Bytes2, _, _, State2} = maybe_consume_s(?MATCH1),
            case Bytes2 of
                <<$>/utf8, Bytes3/bitstring>> ->
                    QName = {<<>>, NameP, NameL},
                    State3 = State2#ys_state{position = Ps, tags = Ts},
                    case QName of
                        Tag ->
                            yaccety_sax:event_endElement(
                                QName,
                                set_state_pos(State3, Bytes3)
                            );
                        _ ->
                            fatal_error(unmatched_tag, {Tag, QName})
                    end;
                _ ->
                    fatal_error(bad_endtag, State2)
            end
    end.

%%----------------------------------------------------------------------
%% [4ns]  NCName ::= Name - (Char* ':' Char*)  /* An XML Name, minus the ":" */
%%----------------------------------------------------------------------
parse_NCName(<<Char/utf8, Rest/bitstring>>, Stream, Pos, State) when
    Char >= $a, Char =< $z; Char >= $A, Char =< $Z; Char == $_
->
    parse_NCName(Rest, Stream, Pos, 1, State, []);
parse_NCName(<<Char/utf8, Rest/bitstring>>, Stream, Pos, State) when
    Char >= 16#C0, Char =< 16#D6;
    Char >= 16#D8, Char =< 16#F6;
    Char >= 16#F8, Char =< 16#2FF;
    Char >= 16#370, Char =< 16#37D;
    Char >= 16#37F, Char =< 16#7FF
->
    parse_NCName(Rest, Stream, Pos, 2, State, []);
parse_NCName(<<Char/utf8, Rest/bitstring>>, Stream, Pos, State) when
    Char >= 16#800, Char =< 16#1FFF;
    Char >= 16#200C, Char =< 16#200D;
    Char >= 16#2070, Char =< 16#218F;
    Char >= 16#2C00, Char =< 16#2FEF;
    Char >= 16#3001, Char =< 16#D7FF;
    Char >= 16#F900, Char =< 16#FDCF;
    Char >= 16#FDF0, Char =< 16#FFFD
->
    parse_NCName(Rest, Stream, Pos, 3, State, []);
parse_NCName(<<Char/utf8, Rest/bitstring>>, Stream, Pos, State) when
    Char >= 16#10000, Char =< 16#EFFFF
->
    parse_NCName(Rest, Stream, Pos, 4, State, []);
parse_NCName(<<Char/utf8, _Rest/bitstring>>, _, _, _) ->
    fatal_error(bad_name, Char);
parse_NCName(<<>>, _, _, State) ->
    {Stream1, State1} = cf(State),
    parse_NCName(Stream1, Stream1, 0, State1);
parse_NCName(Bytes = <<30:5, _:3>>, _, _, State) ->
    ?FSTNAMECHARPARTFUN(3, parse_Name);
parse_NCName(Bytes = <<30:5, _:3, 2:2, _:6>>, _, _, State) ->
    ?FSTNAMECHARPARTFUN(2, parse_Name);
parse_NCName(Bytes = <<14:4, _:4>>, _, _, State) ->
    ?FSTNAMECHARPARTFUN(2, parse_Name);
parse_NCName(Bytes = <<30:5, _:3, 2:2, _:6, 2:2, _:6>>, _, _, State) ->
    ?FSTNAMECHARPARTFUN(1, parse_Name);
parse_NCName(Bytes = <<14:4, _:4, 2:2, _:6>>, _, _, State) ->
    ?FSTNAMECHARPARTFUN(1, parse_Name);
parse_NCName(Bytes = <<6:3, _:5>>, _, _, State) ->
    ?FSTNAMECHARPARTFUN(1, parse_Name);
parse_NCName(_, _, _, State) ->
    fatal_error(bad_name, State).

-define(NCNameChar(Char),
    ((Char >= $a andalso Char =< $z) orelse
        (Char >= $A andalso Char =< $Z) orelse
        Char == $_ orelse
        Char == $- orelse
        (Char >= $0 andalso Char =< $9) orelse
        Char == $.)
).

parse_NCName(<<Char, Char1, Char2, Char3, Rest/bitstring>>, Stream, Pos, Len, State, Acc) when
    ?NCNameChar(Char) andalso
        ?NCNameChar(Char1) andalso ?NCNameChar(Char2) andalso ?NCNameChar(Char3)
->
    parse_NCName(Rest, Stream, Pos, Len + 4, State, Acc);
parse_NCName(<<Char, Char1, Char2, Rest/bitstring>>, Stream, Pos, Len, State, Acc) when
    ?NCNameChar(Char) andalso ?NCNameChar(Char1) andalso ?NCNameChar(Char2)
->
    parse_NCName(Rest, Stream, Pos, Len + 3, State, Acc);
parse_NCName(<<Char, Char1, Rest/bitstring>>, Stream, Pos, Len, State, Acc) when
    ?NCNameChar(Char) andalso ?NCNameChar(Char1)
->
    parse_NCName(Rest, Stream, Pos, Len + 2, State, Acc);
parse_NCName(<<Char, Rest/bitstring>>, Stream, Pos, Len, State, Acc) when ?NCNameChar(Char) ->
    parse_NCName(Rest, Stream, Pos, Len + 1, State, Acc);
parse_NCName(<<Char, _/bitstring>> = Bytes, Stream, Pos, Len, State, Acc) when Char < 16#80 ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    {to_binary(Acc1), Bytes, Stream, Pos + Len, State};
parse_NCName(<<Char/utf8, Rest/bitstring>> = Bytes, Stream, Pos, Len, State, Acc) ->
    if
        Char == 16#B7;
        Char >= 16#C0, Char =< 16#D6;
        Char >= 16#D8, Char =< 16#F6;
        Char >= 16#F8, Char =< 16#37D;
        Char >= 16#37F, Char =< 16#7FF ->
            parse_NCName(Rest, Stream, Pos, Len + 2, State, Acc);
        Char < 16#800 ->
            Acc1 = ?ACC(Stream, Pos, Len, Acc),
            {Acc1, Bytes, Stream, Pos + Len, State};
        Char >= 16#800, Char =< 16#1FFF;
        Char >= 16#200C, Char =< 16#200D;
        Char >= 16#203F, Char =< 16#2040;
        Char >= 16#2070, Char =< 16#218F;
        Char >= 16#2C00, Char =< 16#2FEF;
        Char >= 16#3001, Char =< 16#D7FF;
        Char >= 16#F900, Char =< 16#FDCF;
        Char >= 16#FDF0, Char =< 16#FFFD ->
            parse_NCName(Rest, Stream, Pos, Len + 3, State, Acc);
        Char >= 16#10000, Char =< 16#EFFFF ->
            parse_NCName(Rest, Stream, Pos, Len + 4, State, Acc);
        true ->
            Acc1 = ?ACC(Stream, Pos, Len, Acc),
            {to_binary(Acc1), Bytes, Stream, Pos + Len, State}
    end;
parse_NCName(<<>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    {Stream1, State1} = cf(State),
    parse_NCName(Stream1, Stream1, 0, 0, State1, Acc1);
parse_NCName(Bytes = <<30:5, _:3>>, Stream, Pos, Len, State, Acc) ->
    ?NAMECHARPARTFUN(3);
parse_NCName(Bytes = <<30:5, _:3, 2:2, _:6>>, Stream, Pos, Len, State, Acc) ->
    ?NAMECHARPARTFUN(2);
parse_NCName(Bytes = <<14:4, _:4>>, Stream, Pos, Len, State, Acc) ->
    ?NAMECHARPARTFUN(2);
parse_NCName(Bytes = <<30:5, _:3, 2:2, _:6, 2:2, _:6>>, Stream, Pos, Len, State, Acc) ->
    ?NAMECHARPARTFUN(1);
parse_NCName(Bytes = <<14:4, _:4, 2:2, _:6>>, Stream, Pos, Len, State, Acc) ->
    ?NAMECHARPARTFUN(1);
parse_NCName(Bytes = <<6:3, _:5>>, Stream, Pos, Len, State, Acc) ->
    ?NAMECHARPARTFUN(1);
parse_NCName(Bytes, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    {to_binary(Acc1), Bytes, Stream, Pos + Len, State}.

%%----------------------------------------------------------------------
%% [7ns]  QName               ::=       PrefixedName | UnprefixedName
%% [8ns]  PrefixedName        ::=       Prefix ':' LocalPart
%% [9ns]  UnprefixedName      ::=       LocalPart
%% [10ns] Prefix              ::=       NCName
%% [11ns] LocalPart           ::=       NCName
%%----------------------------------------------------------------------
parse_QName(?MATCH) ->
    {Name1, ?MATCH1} = parse_NCName(?MATCH),
    case Bytes1 of
        <<$:/utf8, Rest/bitstring>> ->
            {Name2, ?MATCH2} = parse_NCName(Rest, Stream1, Pos1 + 1, State1),
            {{to_binary(Name1), to_binary(Name2)}, ?MATCH2};
        _ ->
            {{<<>>, to_binary(Name1)}, ?MATCH1}
    end.

parse_QName(<<"%", Bytes/bitstring>>, Stream, Pos, State, DTD) ->
    {PERef, Bytes1, _, _, State1} = parse_PEReference(Bytes, Stream, Pos + 1, State),
    case resolve_parameter_entity(PERef, DTD, State1, false, true) of
        {replacement_text, ReplacementText} ->
            RTState = replacement_text_state(Bytes1, State1),
            {_, ?MATCH2} = maybe_consume_s(ReplacementText, ReplacementText, 0, RTState),
            parse_QName(?MATCH2, DTD)
    end;
parse_QName(?MATCH, _) ->
    parse_QName(?MATCH).

qualify_attribute_names([], _) -> [];
qualify_attribute_names(Atts, Nss) -> [{expand_attribute_qname(P, L, Nss), V} || {P, L, V} <- Atts].

expand_qname(<<>>, L, none) ->
    {<<>>, <<>>, L};
expand_qname(<<"xml">>, L, _) ->
    {?XML_NS, <<"xml">>, L};
expand_qname(P, L, Nss) ->
    case Nss of
        #{P := N} ->
            {N, P, L};
        _ ->
            fatal_error(unknown_prefix, P)
    end.

expand_attribute_qname(<<>>, L, _) -> {<<>>, <<>>, L};
expand_attribute_qname(P, L, Nss) -> expand_qname(P, L, Nss).

set_next_parser_position(Pos, #ys_state{position = S} = State) ->
    State#ys_state{position = [Pos | S]}.

namespace_map_from_list([]) -> none;
namespace_map_from_list(List) -> namespace_map_from_list(List, #{}).

namespace_map_from_list([{_, P} | _], Map) when is_map_key(Map, P) ->
    fatal_error(duplicate_prefix, P);
namespace_map_from_list([{U, P} | T], Map) ->
    namespace_map_from_list(T, Map#{P => U});
namespace_map_from_list([], Map) ->
    Map.

merge_namespaces(none, none) -> none;
merge_namespaces(none, NewNsMap) -> maps:merge(#{<<>> => <<>>}, NewNsMap);
merge_namespaces(LastNss, none) -> LastNss;
merge_namespaces(LastNss, NewNsMap) -> maps:merge(LastNss, NewNsMap).

fatal_error(Reason, State) -> error(Reason, [State]).

set_state_pos(State, Bytes) -> State#ys_state{rest_stream = Bytes}.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%
%%                                                                                                                   %%
%%                                                  DTD Stuff                                                        %%
%%                                                                                                                   %%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%

%%----------------------------------------------------------------------
%% [16ns] doctypedecl ::= '<!DOCTYPE' S QName (S ExternalID)? S? ('[' (markupdecl | PEReference | S)* ']' S?)? '>'  [VC: Root Element Type]
%%----------------------------------------------------------------------
parse_doctypedecl(Stream, #ys_state{dtd = undefined} = State) ->
    parse_doctypedecl(Stream, Stream, 0, State);
parse_doctypedecl(Stream, #ys_state{dtd = DTD} = State) ->
    {#{pi_comments := PiCms} = DTD1, Bytes1, _, _, State1} = parse_intSubset(
        Stream, Stream, 0, State#ys_state{dtd = undefined}, DTD
    ),
    yaccety_sax:event_dtd(
        DTD1,
        set_state_pos(State1#ys_state{dtd = DTD1#{pi_comments := lists:reverse(PiCms)}}, Bytes1)
    ).

parse_doctypedecl(<<"<!DOCTYPE"/utf8, Rest/bitstring>>, Stream, Pos, State) ->
    parse_doctypedecl_ltexDOCTYPE(Rest, Stream, Pos + 9, State);
parse_doctypedecl(<<"<!DOCTYP"/utf8>>, _, _, State) ->
    parse_doctypedecl_ltexDOCTYP(cf(State));
parse_doctypedecl(<<"<!DOCTY"/utf8>>, _, _, State) ->
    parse_doctypedecl_ltexDOCTY(cf(State));
parse_doctypedecl(<<"<!DOCT"/utf8>>, _, _, State) ->
    parse_doctypedecl_ltexDOCT(cf(State));
parse_doctypedecl(<<"<!DOC"/utf8>>, _, _, State) ->
    parse_doctypedecl_ltexDOC(cf(State));
parse_doctypedecl(<<"<!DO"/utf8>>, _, _, State) ->
    parse_doctypedecl_ltexDO(cf(State));
parse_doctypedecl(<<"<!D"/utf8>>, _, _, State) ->
    parse_doctypedecl_ltexD(cf(State));
parse_doctypedecl(<<"<!"/utf8>>, _, _, State) ->
    parse_doctypedecl_ltex(cf(State));
parse_doctypedecl(<<"<"/utf8>>, _, _, State) ->
    parse_doctypedecl_lt(cf(State));
parse_doctypedecl(_, _, _, State) ->
    State.

parse_doctypedecl_ltexDOCTYPE(?MATCH) ->
    {_, ?MATCH1} = consume_s(?MATCH),
    {QName, ?MATCH2} = parse_QName(?MATCH1),
    DTD = empty_proc_dtd(),
    DTD1 = set_dtd_name(DTD, QName),
    case maybe_consume_s(?MATCH2) of
        % Empty DTD
        {_, <<$>/utf8, Bytes3/bitstring>>, _, _, State3} ->
            State4 = State3#ys_state{dtd = DTD1},
            yaccety_sax:event_dtd(DTD1, set_state_pos(State4, Bytes3));
        % No ExternalID
        {_, <<$[/utf8, Bytes3/bitstring>>, Stream3, Pos3, State3} ->
            {DTD2, Bytes4, _, _, State4} = parse_intSubset(Bytes3, Stream3, Pos3 + 1, State3, DTD1),
            State5 = State4#ys_state{dtd = DTD2},
            yaccety_sax:event_dtd(DTD2, set_state_pos(State5, Bytes4));
        % Is ExternalID
        {true, ?MATCH3} ->
            {PubSys, ?MATCH4} = parse_ExternalID(
                Bytes3,
                Stream3,
                Pos3,
                State3
            ),
            {_, ?MATCH5} = maybe_consume_s(?MATCH4),
            DTD2 = set_dtd_external_id(DTD1, PubSys),
            case Bytes5 of
                <<$>/utf8, Bytes6/bitstring>> ->
                    DTD3 = maybe_parse_external_subset(State5, PubSys, DTD2),
                    State6 = State5#ys_state{dtd = DTD3},
                    yaccety_sax:event_dtd(DTD3, set_state_pos(State6, Bytes6));
                <<$[/utf8, Bytes5a/bitstring>> ->
                    {DTD3, Bytes6, _, _, State6} = parse_intSubset(
                        Bytes5a, Stream5, Pos5 + 1, State5, DTD2
                    ),
                    DTD4 = maybe_parse_external_subset(State6, PubSys, DTD3),
                    State7 = State6#ys_state{dtd = DTD4},
                    yaccety_sax:event_dtd(DTD4, set_state_pos(State7, Bytes6));
                _ ->
                    fatal_error(bad_dtd, {Bytes5, State5})
            end
    end.

% TODO: Make this lazy and only called if needed.
maybe_parse_external_subset(#ys_state{external = undefined}, _PubSys, DTD) ->
    DTD;
maybe_parse_external_subset(#ys_state{external = EntResolver, base = Base} = State, PubSys, DTD) ->
    {NewBase, Stream} = EntResolver(PubSys, Base),
    % Make sure the continuation is not called.
    {_, ?MATCH1} = parse_TextDecl(Stream, State#ys_state{continuation = undefined, base = NewBase}),
    {DTD1, Bytes2, _, _, State2} = parse_extSubsetDecl(?MATCH1, DTD),
    case Bytes2 of
        <<>> -> DTD1;
        _ -> fatal_error(bad_dtd, State2)
    end.

%%----------------------------------------------------------------------
%% [75] ExternalID ::= 'SYSTEM' S SystemLiteral | 'PUBLIC' S PubidLiteral S SystemLiteral
%% First byte is in stream.
%% return {Pub, Sys}
%%----------------------------------------------------------------------
parse_ExternalID(<<"SYSTEM", Rest/bitstring>>, Stream, Pos, State) ->
    parse_ExternalID_system(Rest, Stream, Pos + 6, State);
parse_ExternalID(<<"PUBLIC", Rest/bitstring>>, Stream, Pos, State) ->
    parse_ExternalID_public(Rest, Stream, Pos + 6, State);
parse_ExternalID(<<"SYSTE">>, _, _, State) ->
    parse_ExternalID_syste(cf(State));
parse_ExternalID(<<"SYST">>, _, _, State) ->
    parse_ExternalID_syst(cf(State));
parse_ExternalID(<<"SYS">>, _, _, State) ->
    parse_ExternalID_sys(cf(State));
parse_ExternalID(<<"SY">>, _, _, State) ->
    parse_ExternalID_sy(cf(State));
parse_ExternalID(<<"S">>, _, _, State) ->
    parse_ExternalID_s(cf(State));
parse_ExternalID(<<"PUBLI">>, _, _, State) ->
    parse_ExternalID_publi(cf(State));
parse_ExternalID(<<"PUBL">>, _, _, State) ->
    parse_ExternalID_publ(cf(State));
parse_ExternalID(<<"PUB">>, _, _, State) ->
    parse_ExternalID_pub(cf(State));
parse_ExternalID(<<"PU">>, _, _, State) ->
    parse_ExternalID_pu(cf(State));
parse_ExternalID(<<"P">>, _, _, State) ->
    parse_ExternalID_p(cf(State));
parse_ExternalID(?MATCH) ->
    {{<<>>, <<>>}, ?MATCH}.

parse_ExternalID_system(?MATCH) ->
    {_, ?MATCH1} = consume_s(?MATCH),
    {Sys, ?MATCH2} = parse_SystemLiteral(?MATCH1),
    {{<<>>, to_binary(Sys)}, ?MATCH2}.

parse_ExternalID_public(?MATCH) ->
    {_, ?MATCH1} = consume_s(?MATCH),
    {Pub, ?MATCH2} = parse_PubidLiteral(?MATCH1),
    {{_, Sys}, ?MATCH3} = parse_ExternalID_system(?MATCH2),
    {{Pub, Sys}, ?MATCH3}.

%%----------------------------------------------------------------------
%% [11] SystemLiteral ::= ('"' [^"]* '"') | ("'" [^']* "'")
%%----------------------------------------------------------------------
parse_SystemLiteral(<<>>, _, _, State) ->
    {Stream1, State1} = cf(State),
    parse_SystemLiteral(Stream1, Stream1, 0, State1);
parse_SystemLiteral(<<$'/utf8, Rest/bitstring>>, Stream, Pos, State) ->
    parse_SystemLiteral_sq(Rest, Stream, Pos + 1, 0, State, []);
parse_SystemLiteral(<<$"/utf8, Rest/bitstring>>, Stream, Pos, State) ->
    parse_SystemLiteral_dq(Rest, Stream, Pos + 1, 0, State, []);
parse_SystemLiteral(Bytes, _, _, State) ->
    fatal_error(bad_sysid, {Bytes, State}).

parse_SystemLiteral_sq(<<$\r, $\n, Rest/bitstring>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    Acc2 = ?APPEND(<<$\n>>, Acc1),
    ?FUNCTION_NAME(Rest, Stream, Pos + Len + 2, 0, State, Acc2);
?FUNCTION_NAME(<<$\r>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    Acc2 = ?APPEND(<<$\n>>, Acc1),
    case cf(State) of
        {<<$\n, Bytes1/bitstring>> = Stream1, State1} ->
            ?FUNCTION_NAME(Bytes1, Stream1, 1, 0, State1, Acc2);
        {Stream1, State1} ->
            ?FUNCTION_NAME(Stream1, Stream1, 0, 0, State1, Acc2)
    end;
parse_SystemLiteral_sq(<<$\r, Rest/bitstring>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    Acc2 = ?APPEND(<<$\n>>, Acc1),
    ?FUNCTION_NAME(Rest, Stream, Pos + Len + 1, 0, State, Acc2);
parse_SystemLiteral_sq(<<$'>>, Stream, Pos, Len, State, Acc) ->
    Text = ?ACC(Stream, Pos, Len, Acc),
    {Stream1, State1} = cf(State),
    {Text, Stream1, Stream1, 0, State1};
parse_SystemLiteral_sq(<<$'/utf8, Rest/bitstring>>, Stream, Pos, Len, State, Acc) ->
    Text = ?ACC(Stream, Pos, Len, Acc),
    {Text, Rest, Stream, Pos + Len + 1, State};
?ONECHAR.

parse_SystemLiteral_dq(<<$\r, $\n, Rest/bitstring>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    Acc2 = ?APPEND(<<$\n>>, Acc1),
    ?FUNCTION_NAME(Rest, Stream, Pos + Len + 2, 0, State, Acc2);
?FUNCTION_NAME(<<$\r>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    Acc2 = ?APPEND(<<$\n>>, Acc1),
    case cf(State) of
        {<<$\n, Bytes1/bitstring>> = Stream1, State1} ->
            ?FUNCTION_NAME(Bytes1, Stream1, 1, 0, State1, Acc2);
        {Stream1, State1} ->
            ?FUNCTION_NAME(Stream1, Stream1, 0, 0, State1, Acc2)
    end;
parse_SystemLiteral_dq(<<$\r, Rest/bitstring>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    Acc2 = ?APPEND(<<$\n>>, Acc1),
    ?FUNCTION_NAME(Rest, Stream, Pos + Len + 1, 0, State, Acc2);
parse_SystemLiteral_dq(<<$">>, Stream, Pos, Len, State, Acc) ->
    Text = ?ACC(Stream, Pos, Len, Acc),
    {Stream1, State1} = cf(State),
    {Text, Stream1, Stream1, 0, State1};
parse_SystemLiteral_dq(<<$"/utf8, Rest/bitstring>>, Stream, Pos, Len, State, Acc) ->
    Text = ?ACC(Stream, Pos, Len, Acc),
    {Text, Rest, Stream, Pos + Len + 1, State};
?ONECHAR.

%%----------------------------------------------------------------------
%% [12] PubidLiteral ::= '"' PubidChar* '"' | "'" (PubidChar - "'")* "'"
%%----------------------------------------------------------------------
parse_PubidLiteral(<<>>, _, _, State) ->
    {Stream1, State1} = cf(State),
    parse_PubidLiteral(Stream1, Stream1, 0, State1);
parse_PubidLiteral(<<$'/utf8, Rest/bitstring>>, Stream, Pos, State) ->
    parse_PubidLiteral_sq(Rest, Stream, Pos + 1, 0, State, []);
parse_PubidLiteral(<<$"/utf8, Rest/bitstring>>, Stream, Pos, State) ->
    parse_PubidLiteral_dq(Rest, Stream, Pos + 1, 0, State, []);
parse_PubidLiteral(_, _, _, State) ->
    fatal_error(bad_pubid, State).

parse_PubidLiteral_sq(<<>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    {Stream1, State1} = cf(State),
    parse_PubidLiteral_sq(Stream1, Stream1, 0, 0, State1, Acc1);
parse_PubidLiteral_sq(<<$'/utf8, Rest/bitstring>>, Stream, Pos, Len, State, Acc) ->
    Text = ?ACC(Stream, Pos, Len, Acc),
    {normalize_PubidLiteral(Text), Rest, Stream, Pos + Len + 1, State};
?ONEPUBIDCHAR.

parse_PubidLiteral_dq(<<>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    {Stream1, State1} = cf(State),
    parse_PubidLiteral_dq(Stream1, Stream1, 0, 0, State1, Acc1);
parse_PubidLiteral_dq(<<$"/utf8, Rest/bitstring>>, Stream, Pos, Len, State, Acc) ->
    Text = ?ACC(Stream, Pos, Len, Acc),
    {normalize_PubidLiteral(Text), Rest, Stream, Pos + Len + 1, State};
?ONEPUBIDCHAR.

% (markupdecl | PEReference | S)* ']' S?)? '>'
%%----------------------------------------------------------------------
%%  [WFC: PE Between Declarations]
%% [28b] intSubset  ::= (markupdecl | PEReference | S)*
%% [29]  markupdecl ::= elementdecl | AttlistDecl | EntityDecl | NotationDecl | PI | Comment
%%  [VC: Proper Declaration/PE Nesting]
%% [69] PEReference ::= '%' Name ';'
%%  [WFC: PEs in Internal Subset]
%%----------------------------------------------------------------------
parse_intSubset(<<"<!ELEMENT", Bytes/bitstring>>, Stream, Pos, State, DTD) ->
    {DTD1, ?MATCH1} = parse_elementdecl(Bytes, Stream, Pos + 9, State, DTD, false),
    parse_intSubset(?MATCH1, DTD1);
parse_intSubset(<<"<!ELEMEN">>, _, _, State, DTD) ->
    parse_intSubset_ltexELEMEN(cf(State), DTD);
parse_intSubset(<<"<!ELEME">>, _, _, State, DTD) ->
    parse_intSubset_ltexELEME(cf(State), DTD);
parse_intSubset(<<"<!ELEM">>, _, _, State, DTD) ->
    parse_intSubset_ltexELEM(cf(State), DTD);
parse_intSubset(<<"<!ELE">>, _, _, State, DTD) ->
    parse_intSubset_ltexELE(cf(State), DTD);
parse_intSubset(<<"<!EL">>, _, _, State, DTD) ->
    parse_intSubset_ltexEL(cf(State), DTD);
parse_intSubset(<<"<!E">>, _, _, State, DTD) ->
    parse_intSubset_ltexE(cf(State), DTD);
parse_intSubset(<<"<!-">>, _, _, State, DTD) ->
    parse_intSubset_ltexdash(cf(State), DTD);
parse_intSubset(<<"<!">>, _, _, State, DTD) ->
    parse_intSubset_ltex(cf(State), DTD);
parse_intSubset(<<"<">>, _, _, State, DTD) ->
    parse_intSubset_lt(cf(State), DTD);
parse_intSubset(<<"<!ATTLIST", Bytes/bitstring>>, Stream, Pos, State, DTD) ->
    {DTD1, ?MATCH1} = parse_AttlistDecl(Bytes, Stream, Pos + 9, State, DTD, false),
    parse_intSubset(?MATCH1, DTD1);
parse_intSubset(<<"<!ATTLIS">>, _, _, State, DTD) ->
    parse_intSubset_ltexATTLIS(cf(State), DTD);
parse_intSubset(<<"<!ATTLI">>, _, _, State, DTD) ->
    parse_intSubset_ltexATTLI(cf(State), DTD);
parse_intSubset(<<"<!ATTL">>, _, _, State, DTD) ->
    parse_intSubset_ltexATTL(cf(State), DTD);
parse_intSubset(<<"<!ATT">>, _, _, State, DTD) ->
    parse_intSubset_ltexATT(cf(State), DTD);
parse_intSubset(<<"<!AT">>, _, _, State, DTD) ->
    parse_intSubset_ltexAT(cf(State), DTD);
parse_intSubset(<<"<!A">>, _, _, State, DTD) ->
    parse_intSubset_ltexA(cf(State), DTD);
parse_intSubset(<<"<!ENTITY", Bytes/bitstring>>, Stream, Pos, State, DTD) ->
    {DTD1, ?MATCH1} = parse_EntityDecl(Bytes, Stream, Pos + 8, State, DTD, false),
    parse_intSubset(?MATCH1, DTD1);
parse_intSubset(<<"<!ENTIT">>, _, _, State, DTD) ->
    parse_intSubset_ltexENTIT(cf(State), DTD);
parse_intSubset(<<"<!ENTI">>, _, _, State, DTD) ->
    parse_intSubset_ltexENTI(cf(State), DTD);
parse_intSubset(<<"<!ENT">>, _, _, State, DTD) ->
    parse_intSubset_ltexENT(cf(State), DTD);
parse_intSubset(<<"<!EN">>, _, _, State, DTD) ->
    parse_intSubset_ltexEN(cf(State), DTD);
parse_intSubset(<<"<!NOTATION", Bytes/bitstring>>, Stream, Pos, State, DTD) ->
    {DTD1, ?MATCH1} = parse_NotationDecl(Bytes, Stream, Pos + 10, State, DTD),
    parse_intSubset(?MATCH1, DTD1);
parse_intSubset(<<"<!NOTATIO">>, _, _, State, DTD) ->
    parse_intSubset_ltexNOTATIO(cf(State), DTD);
parse_intSubset(<<"<!NOTATI">>, _, _, State, DTD) ->
    parse_intSubset_ltexNOTATI(cf(State), DTD);
parse_intSubset(<<"<!NOTAT">>, _, _, State, DTD) ->
    parse_intSubset_ltexNOTAT(cf(State), DTD);
parse_intSubset(<<"<!NOTA">>, _, _, State, DTD) ->
    parse_intSubset_ltexNOTA(cf(State), DTD);
parse_intSubset(<<"<!NOT">>, _, _, State, DTD) ->
    parse_intSubset_ltexNOT(cf(State), DTD);
parse_intSubset(<<"<!NO">>, _, _, State, DTD) ->
    parse_intSubset_ltexNO(cf(State), DTD);
parse_intSubset(<<"<!N">>, _, _, State, DTD) ->
    parse_intSubset_ltexN(cf(State), DTD);
parse_intSubset(<<"%", Bytes/bitstring>>, Stream, Pos, State, DTD) ->
    % Reset the replacement text flag since this should all be one piece
    {PERef, ?MATCH1} = parse_PEReference(Bytes, Stream, Pos + 1, State),
    case resolve_parameter_entity(PERef, DTD, State1, false, false) of
        {replacement_text, ReplacementText} ->
            RTState = replacement_text_state(Bytes1, State1),
            parse_intSubset(
                ReplacementText, ReplacementText, 0, RTState#ys_state{in_replacement = false}, DTD
            );
        {dtd, DTD1} ->
            parse_intSubset(?MATCH1, DTD1)
    end;
parse_intSubset(<<"]>", Bytes/bitstring>>, Stream, Pos, State, DTD) ->
    {DTD, Bytes, Stream, Pos + 2, State};
parse_intSubset(<<"]", Bytes/bitstring>>, Stream, Pos, State, DTD) ->
    case maybe_consume_s(Bytes, Stream, Pos + 1, State) of
        {_, <<">", Bytes1/bitstring>>, Stream1, Pos1, State1} ->
            {DTD, Bytes1, Stream1, Pos1 + 1, State1};
        {_, _, _, _, State1} ->
            fatal_error(bad_dtd, State1)
    end;
parse_intSubset(<<Char/utf8, Bytes/bitstring>>, Stream, Pos, State, DTD) when
    Char == 16#20; Char == 16#9; Char == 16#A; Char == 16#D
->
    {_, ?MATCH1} = maybe_consume_s(Bytes, Stream, Pos + 1, State),
    parse_intSubset(?MATCH1, DTD);
parse_intSubset(<<"<!--", Bytes/bitstring>>, Stream, Pos, State, DTD) ->
    {Comment, ?MATCH1} = parse_Comment(Bytes, Stream, Pos + 4, State),
    DTD1 = add_pi_comment_to_dtd(Comment, DTD),
    parse_intSubset(?MATCH1, DTD1);
parse_intSubset(<<"<?", Bytes/bitstring>>, Stream, Pos, State, DTD) ->
    {PI, ?MATCH1} = parse_PI(Bytes, Stream, Pos + 2, State),
    DTD1 = add_pi_comment_to_dtd(PI, DTD),
    parse_intSubset(?MATCH1, DTD1);
parse_intSubset(<<>>, _, _, State, DTD) ->
    {Bytes1, State1} = cf(State),
    parse_intSubset(Bytes1, Bytes1, 0, State1, DTD);
parse_intSubset(Bytes, _, _, State, _DTD) ->
    fatal_error(bad_dtd, {Bytes, State}).

%%----------------------------------------------------------------------
%% [17ns] elementdecl ::= '<!ELEMENT' S QName S contentspec S? '>' [VC: Unique Element Type Declaration]
%% [46]   contentspec ::= 'EMPTY' | 'ANY' | Mixed | children
%% [19ns] Mixed ::= '(' S? '#PCDATA' (S? '|' S? QName)* S? ')*' | '(' S? '#PCDATA' S? ')'
%% [47]   children ::= (choice | seq) ('?' | '*' | '+')?
%% [49]   choice ::= '(' S? cp ( S? '|' S? cp )+ S? ')'    [VC: Proper Group/PE Nesting]
%% [50]   seq ::= '(' S? cp ( S? ',' S? cp )* S? ')'    [VC: Proper Group/PE Nesting]
%% [18ns] cp ::= (QName | choice | seq) ('?' | '*' | '+')?
%%----------------------------------------------------------------------
parse_elementdecl(?MATCH, DTD, External) ->
    {_, ?MATCH1} = consume_s(?MATCH),
    {QName, ?MATCH2} = parse_QName(?MATCH1),
    {_, ?MATCH3} = consume_s(?MATCH2),
    % TODO: E Deterministic Content Models
    % Each model must be deterministic
    {ContentSpec, ?MATCH4} = parse_contentspec(?MATCH3, DTD, External),
    AfterWs =
        if
            External -> maybe_consume_dtd_s(?MATCH4, DTD);
            true -> maybe_consume_s(?MATCH4)
        end,
    case AfterWs of
        {_, <<$>, Bytes5/bitstring>>, Stream5, Pos5, State5} ->
            DTD1 = add_element_to_dtd({QName, ContentSpec}, DTD),
            {DTD1, Bytes5, Stream5, Pos5 + 1, State5};
        {_, Bytes5, _Stream5, _Pos5, State5} ->
            fatal_error(invalid_elementdecl, {Bytes5, State5})
    end.

parse_contentspec(<<"EMPTY", Rest/bitstring>>, Stream, Pos, State, _, _) ->
    {empty, Rest, Stream, Pos + 5, State};
parse_contentspec(<<"EMPT">>, _, _, State, _, _) ->
    parse_contentspec_EMPT(cf(State));
parse_contentspec(<<"EMP">>, _, _, State, _, _) ->
    parse_contentspec_EMP(cf(State));
parse_contentspec(<<"EM">>, _, _, State, _, _) ->
    parse_contentspec_EM(cf(State));
parse_contentspec(<<"E">>, _, _, State, _, _) ->
    parse_contentspec_E(cf(State));
parse_contentspec(<<"ANY", Rest/bitstring>>, Stream, Pos, State, _, _) ->
    {any, Rest, Stream, Pos + 3, State};
parse_contentspec(<<"AN">>, _, _, State, _, _) ->
    parse_contentspec_AN(cf(State));
parse_contentspec(<<"A">>, _, _, State, _, _) ->
    parse_contentspec_A(cf(State));
parse_contentspec(<<"(", Rest/bitstring>>, Stream, Pos, State, DTD, External) ->
    parse_contentspec_Mixed_or_children(Rest, Stream, Pos + 1, State, DTD, External);
?GET_PE(parse_contentspec).

parse_contentspec_Mixed_or_children(?MATCH, DTD, External) ->
    case maybe_consume_s(?MATCH) of
        {_, <<$#, Bytes1/bitstring>>, Stream1, Pos1, State1} ->
            parse_Mixed(Bytes1, Stream1, Pos1 + 1, State1, DTD, External);
        {_, <<"%", Bytes1/bitstring>>, Stream1, Pos1, State1} when External == true ->
            {PERef, Bytes2, _, _, State2} = parse_PEReference(Bytes1, Stream1, Pos1 + 1, State1),
            case resolve_parameter_entity(PERef, DTD, State2, true, true) of
                {replacement_text, <<" ", ReplacementText/bitstring>>} ->
                    RTState = replacement_text_state(Bytes2, State2),
                    {_, ?MATCH3} = maybe_consume_s(ReplacementText, ReplacementText, 0, RTState),
                    ?FUNCTION_NAME(?MATCH3, DTD, true)
            end;
        {_, ?MATCH1} ->
            parse_children(?MATCH1, DTD, External)
    end.

parse_Mixed(<<"PCDATA", Rest/bitstring>>, Stream, Pos, State, DTD, External) ->
    parse_Mixed_(Rest, Stream, Pos + 6, State, DTD, External);
parse_Mixed(<<"PCDAT">>, _, _, State, DTD, External) ->
    parse_Mixed_PCDAT(cf(State), DTD, External);
parse_Mixed(<<"PCDA">>, _, _, State, DTD, External) ->
    parse_Mixed_PCDA(cf(State), DTD, External);
parse_Mixed(<<"PCD">>, _, _, State, DTD, External) ->
    parse_Mixed_PCD(cf(State), DTD, External);
parse_Mixed(<<"PC">>, _, _, State, DTD, External) ->
    parse_Mixed_PC(cf(State), DTD, External);
parse_Mixed(<<"P">>, _, _, State, DTD, External) ->
    parse_Mixed_P(cf(State), DTD, External);
parse_Mixed(<<>>, _, _, State, DTD, External) ->
    {Stream1, State1} = cf(State),
    parse_Mixed(Stream1, Stream1, 0, State1, DTD, External).

parse_Mixed_(?MATCH, DTD, External) ->
    case maybe_consume_s(?MATCH) of
        {_, <<$), $*, Bytes1/bitstring>>, Stream1, Pos1, State1} ->
            {{mixed, []}, Bytes1, Stream1, Pos1 + 2, State1};
        {_, <<$)>>, _, _, State1} ->
            case cf(State1) of
                {<<$*, Bytes2/bitstring>> = Stream2, State2} ->
                    {{mixed, []}, Bytes2, Stream2, 1, State2};
                {Bytes2, State2} ->
                    {{mixed, []}, Bytes2, Bytes2, 0, State2}
            end;
        {_, <<$), Bytes1/bitstring>>, Stream1, Pos1, State1} ->
            {{mixed, []}, Bytes1, Stream1, Pos1 + 1, State1};
        {_, <<$|, _/bitstring>> = ?MATCH1} ->
            parse_Mixed_names(?MATCH1, [], DTD, External);
        {_, <<$%, Bytes1/bitstring>>, Stream1, Pos1, State1} when External == true ->
            {PERef, Bytes2, _, _, State2} = parse_PEReference(Bytes1, Stream1, Pos1 + 1, State1),
            case resolve_parameter_entity(PERef, DTD, State2, true, true) of
                {replacement_text, ReplacementText} ->
                    RTState = replacement_text_state(Bytes2, State2),
                    {_, ?MATCH3} = maybe_consume_s(ReplacementText, ReplacementText, 0, RTState),
                    ?FUNCTION_NAME(?MATCH3, DTD, External)
            end
    end.

parse_Mixed_names(<<")*", Rest/bitstring>>, Stream, Pos, State, Acc, _DTD, _External) ->
    {{mixed, Acc}, Rest, Stream, Pos + 2, State};
parse_Mixed_names(<<$|, Rest/bitstring>>, Stream, Pos, State, Acc, DTD, false) ->
    {_, ?MATCH1} = maybe_consume_s(Rest, Stream, Pos + 1, State),
    {Name, ?MATCH2} = parse_QName(?MATCH1),
    {_, ?MATCH3} = maybe_consume_s(?MATCH2),
    parse_Mixed_names(?MATCH3, [Name | Acc], DTD, false);
parse_Mixed_names(<<$|, Rest/bitstring>>, Stream, Pos, State, Acc, DTD, External) ->
    {_, ?MATCH1} = maybe_consume_s(Rest, Stream, Pos + 1, State),
    {Name, ?MATCH2} = parse_QName(?MATCH1, DTD),
    {_, ?MATCH3} = maybe_consume_s(?MATCH2),
    parse_Mixed_names(?MATCH3, [Name | Acc], DTD, External);
parse_Mixed_names(<<")">>, _, _, State, Acc, _DTD, _External) ->
    {<<$*, Bytes1/bitstring>> = Stream1, State1} = cf(State),
    {{mixed, Acc}, Bytes1, Stream1, 1, State1};
parse_Mixed_names(<<>>, _, _, State, Acc, DTD, External) ->
    {Stream1, State1} = cf(State),
    parse_Mixed_names(Stream1, Stream1, 0, State1, Acc, DTD, External).

parse_children(?MATCH, DTD, External) ->
    {Seq, ?MATCH2} =
        case parse_choice_or_seq(?MATCH, DTD, External) of
            {CorS, <<$?, Bytes1/bitstring>>, Stream1, Pos1, State1} ->
                {{CorS, '?'}, Bytes1, Stream1, Pos1 + 1, State1};
            {CorS, <<$*, Bytes1/bitstring>>, Stream1, Pos1, State1} ->
                {{CorS, '*'}, Bytes1, Stream1, Pos1 + 1, State1};
            {CorS, <<$+, Bytes1/bitstring>>, Stream1, Pos1, State1} ->
                {{CorS, '+'}, Bytes1, Stream1, Pos1 + 1, State1};
            {CorS, ?MATCH1} ->
                {{CorS, 'one'}, ?MATCH1}
        end,
    {_, ?MATCH3} = maybe_consume_s(?MATCH2),
    {Seq, ?MATCH3}.

?GET_PE(parse_choice_or_seq);
parse_choice_or_seq(?MATCH, DTD, External) ->
    {CP, ?MATCH1} = parse_cp(?MATCH, DTD, External),
    case maybe_consume_s(?MATCH1) of
        {_, <<$|, _/bitstring>> = ?MATCH2} ->
            parse_choice(?MATCH2, [CP], DTD, External);
        {_, <<$,, _/bitstring>> = ?MATCH2} ->
            parse_seq(?MATCH2, DTD, External, [CP]);
        {_, <<$)>>, _Stream2, _Pos2, State2} ->
            {Stream3, State3} = cf(State2),
            {{seq, [CP]}, Stream3, Stream3, 0, State3};
        {_, <<$), Bytes2/bitstring>>, Stream2, Pos2, State2} ->
            {{seq, [CP]}, Bytes2, Stream2, Pos2 + 1, State2}
    end.

?GET_PE(parse_cp);
parse_cp(<<>>, _Stream, _Pos, State, DTD, External) ->
    {Stream1, State1} = cf(State),
    ?FUNCTION_NAME(Stream1, Stream1, 0, State1, DTD, External);
parse_cp(<<$(, Rest/bitstring>>, Stream, Pos, State, DTD, External) ->
    {_, ?MATCH1} = maybe_consume_s(Rest, Stream, Pos + 1, State),
    case parse_choice_or_seq(?MATCH1, DTD, External) of
        {CorS, <<$?, Bytes2/bitstring>>, Stream2, Pos2, State2} ->
            {{CorS, '?'}, Bytes2, Stream2, Pos2 + 1, State2};
        {CorS, <<$*, Bytes2/bitstring>>, Stream2, Pos2, State2} ->
            {{CorS, '*'}, Bytes2, Stream2, Pos2 + 1, State2};
        {CorS, <<$+, Bytes2/bitstring>>, Stream2, Pos2, State2} ->
            {{CorS, '+'}, Bytes2, Stream2, Pos2 + 1, State2};
        {CorS, ?MATCH2} ->
            {{CorS, 'one'}, ?MATCH2}
    end;
parse_cp(?MATCH, _, _) ->
    {_, ?MATCH1} = maybe_consume_s(?MATCH),
    case parse_QName(?MATCH1) of
        {QName, <<$?, Bytes2/bitstring>>, Stream2, Pos2, State2} ->
            {{QName, '?'}, Bytes2, Stream2, Pos2 + 1, State2};
        {QName, <<$*, Bytes2/bitstring>>, Stream2, Pos2, State2} ->
            {{QName, '*'}, Bytes2, Stream2, Pos2 + 1, State2};
        {QName, <<$+, Bytes2/bitstring>>, Stream2, Pos2, State2} ->
            {{QName, '+'}, Bytes2, Stream2, Pos2 + 1, State2};
        {QName, ?MATCH2} ->
            {{QName, 'one'}, ?MATCH2}
    end.

parse_choice(<<$|, Rest/bitstring>>, Stream, Pos, State, Acc, DTD, External) ->
    {_, ?MATCH1} = maybe_consume_s(Rest, Stream, Pos + 1, State),
    {CP, ?MATCH2} = parse_cp(?MATCH1, DTD, External),
    {_, ?MATCH3} = maybe_consume_s(?MATCH2),
    parse_choice(?MATCH3, [CP | Acc], DTD, External);
parse_choice(<<$)>>, _Stream, _Pos, State, Acc, _DTD, _External) ->
    {Stream1, State1} = cf(State),
    Choice = lists:reverse(Acc),
    case model_is_deterministic(Choice) of
        true -> {{choice, Choice}, Stream1, Stream1, 0, State1};
        false -> fatal_error(non_deterministic_model, State)
    end;
parse_choice(<<$), Rest/bitstring>>, Stream, Pos, State, Acc, _, _) ->
    Choice = lists:reverse(Acc),
    case model_is_deterministic(Choice) of
        true -> {{choice, Choice}, Rest, Stream, Pos + 1, State};
        false -> fatal_error(non_deterministic_model, State)
    end;
?GET_PE_ACC(parse_choice).

parse_seq(<<$,, Rest/bitstring>>, Stream, Pos, State, DTD, External, Acc) ->
    {_, ?MATCH1} = maybe_consume_s(Rest, Stream, Pos + 1, State),
    {CP, ?MATCH2} = parse_cp(?MATCH1, DTD, External),
    {_, ?MATCH3} = maybe_consume_s(?MATCH2),
    parse_seq(?MATCH3, DTD, External, [CP | Acc]);
parse_seq(<<$)>>, _Stream, _Pos, State, _DTD, _External, Acc) ->
    {Stream1, State1} = cf(State),
    {{seq, lists:reverse(Acc)}, Stream1, Stream1, 0, State1};
parse_seq(<<$), Rest/bitstring>>, Stream, Pos, State, _, _, Acc) ->
    {{seq, lists:reverse(Acc)}, Rest, Stream, Pos + 1, State};
parse_seq(Bytes, _, _, _, _, _, _) ->
    fatal_error(non_deterministic_model, Bytes).

model_is_deterministic(Model) ->
    Names = [Name || {Name, _} <- Model],
    lists:sort(Names) == lists:usort(Names).

%%----------------------------------------------------------------------
%% [20ns] AttlistDecl ::= '<!ATTLIST' S QName AttDef* S? '>'
%% [21ns] AttDef ::= S (QName | NSAttName) S AttType S DefaultDecl
%% [1ns]  NSAttName ::= PrefixedAttName | DefaultAttName
%% [2ns]  PrefixedAttName ::= 'xmlns:' NCName	[NSC: Reserved Prefixes and Namespace Names]
%% [3ns]  DefaultAttName ::= 'xmlns'
%% [54]   AttType ::= StringType | TokenizedType | EnumeratedType
%% [55]   StringType ::= 'CDATA'
%% [56]   TokenizedType ::= 'ID' | 'IDREF' | 'IDREFS' | 'ENTITY' | 'ENTITIES' | 'NMTOKEN' | 'NMTOKENS'
%% [57]   EnumeratedType ::= NotationType | Enumeration
%% [58]   NotationType ::= 'NOTATION' S '(' S? Name (S? '|' S? Name)* S? ')'     [VC: Notation Attributes]
%% [59]   Enumeration ::= '(' S? Nmtoken (S? '|' S? Nmtoken)* S? ')'    [VC: Enumeration]
%% [60]   DefaultDecl ::= '#REQUIRED' | '#IMPLIED' | (('#FIXED' S)? AttValue)
%% [10]   AttValue ::= '"' ([^<&"] | Reference)* '"' |  "'" ([^<&'] | Reference)* "'"
%%----------------------------------------------------------------------
parse_AttlistDecl(<<"%", Bytes/bitstring>>, Stream, Pos, State, DTD, true) ->
    {PERef, Bytes1, _, _, State1} = parse_PEReference(Bytes, Stream, Pos + 1, State),
    case resolve_parameter_entity(PERef, DTD, State1, true, true) of
        {replacement_text, ReplacementText} ->
            RTState = replacement_text_state(Bytes1, State1),
            parse_AttlistDecl(ReplacementText, ReplacementText, 0, RTState, DTD, true)
    end;
parse_AttlistDecl(?MATCH, DTD, true) ->
    {_, ?MATCH1} = consume_s(Bytes, Stream, Pos, State),
    {ElemName, ?MATCH2} = parse_QName(?MATCH1, DTD),
    {AttDefs, ?MATCH3} = parse_AttDef(?MATCH2, #{}, DTD, true),
    DTD1 = add_attlist_to_dtd({ElemName, AttDefs}, DTD),
    {DTD1, ?MATCH3};
parse_AttlistDecl(?MATCH, DTD, External) ->
    {_, ?MATCH1} = consume_s(Bytes, Stream, Pos, State),
    {ElemName, ?MATCH2} = parse_QName(?MATCH1),
    {AttDefs, ?MATCH3} = parse_AttDef(?MATCH2, #{}, DTD, External),
    DTD1 = add_attlist_to_dtd({ElemName, AttDefs}, DTD),
    {DTD1, ?MATCH3}.

parse_AttDef(?MATCH, Acc, DTD, External) ->
    case maybe_consume_s(?MATCH) of
        {_, <<$>, Bytes1/bitstring>>, Stream1, Pos1, State1} ->
            {Acc, Bytes1, Stream1, Pos1 + 1, State1};
        {true, ?MATCH1} when External == true ->
            {QName, ?MATCH2} = parse_QName(?MATCH1, DTD),
            {_, ?MATCH3} = consume_s(?MATCH2),
            {AttType, ?MATCH4} = parse_AttType(?MATCH3),
            {_, ?MATCH5} = consume_s(?MATCH4, DTD),
            {DefaultDecl, ?MATCH6} = parse_DefaultDecl(?MATCH5, DTD, External),
            parse_AttDef(?MATCH6, Acc#{QName => {AttType, DefaultDecl}}, DTD, External);
        {true, ?MATCH1} ->
            {QName, ?MATCH2} = parse_QName(?MATCH1),
            {_, ?MATCH3} = consume_s(?MATCH2),
            {AttType, ?MATCH4} = parse_AttType(?MATCH3),
            {_, ?MATCH5} = consume_s(?MATCH4),
            {DefaultDecl, ?MATCH6} = parse_DefaultDecl(?MATCH5, DTD, External),
            parse_AttDef(?MATCH6, Acc#{QName => {AttType, DefaultDecl}}, DTD, External)
    end.

parse_DefaultDecl_hREQUIRED(?MATCH) -> {required, ?MATCH}.

parse_DefaultDecl_hIMPLIED(?MATCH) -> {implied, ?MATCH}.

parse_DefaultDecl_hFIXED(?MATCH, DTD, External) ->
    {_, ?MATCH1} = consume_s(?MATCH),
    State0 = State1#ys_state{in_replacement = false},
    {AttValue, ?MATCH2} = parse_AttValue(Bytes1, Stream1, Pos1, State0, DTD, External),
    {{fixed, AttValue}, ?MATCH2}.

parse_DefaultDecl(<<"#REQUIRED", Rest/bitstring>>, Stream, Pos, State, _, _) ->
    parse_DefaultDecl_hREQUIRED(Rest, Stream, Pos + 9, State);
parse_DefaultDecl(<<"#REQUIRE">>, _, _, State, _, _) ->
    parse_DefaultDecl_hREQUIRE(cf(State));
parse_DefaultDecl(<<"#REQUIR">>, _, _, State, _, _) ->
    parse_DefaultDecl_hREQUIR(cf(State));
parse_DefaultDecl(<<"#REQUI">>, _, _, State, _, _) ->
    parse_DefaultDecl_hREQUI(cf(State));
parse_DefaultDecl(<<"#REQU">>, _, _, State, _, _) ->
    parse_DefaultDecl_hREQU(cf(State));
parse_DefaultDecl(<<"#REQ">>, _, _, State, _, _) ->
    parse_DefaultDecl_hREQ(cf(State));
parse_DefaultDecl(<<"#RE">>, _, _, State, _, _) ->
    parse_DefaultDecl_hRE(cf(State));
parse_DefaultDecl(<<"#R">>, _, _, State, _, _) ->
    parse_DefaultDecl_hR(cf(State));
parse_DefaultDecl(<<"#">>, _, _, State, DTD, _) ->
    parse_DefaultDecl_h(cf(State), DTD);
parse_DefaultDecl(<<"#IMPLIED", Rest/bitstring>>, Stream, Pos, State, _, _) ->
    parse_DefaultDecl_hIMPLIED(Rest, Stream, Pos + 8, State);
parse_DefaultDecl(<<"#IMPLIE">>, _, _, State, _, _) ->
    parse_DefaultDecl_hIMPLIE(cf(State));
parse_DefaultDecl(<<"#IMPLI">>, _, _, State, _, _) ->
    parse_DefaultDecl_hIMPLI(cf(State));
parse_DefaultDecl(<<"#IMPL">>, _, _, State, _, _) ->
    parse_DefaultDecl_hIMPL(cf(State));
parse_DefaultDecl(<<"#IMP">>, _, _, State, _, _) ->
    parse_DefaultDecl_hIMP(cf(State));
parse_DefaultDecl(<<"#IM">>, _, _, State, _, _) ->
    parse_DefaultDecl_hIM(cf(State));
parse_DefaultDecl(<<"#I">>, _, _, State, _, _) ->
    parse_DefaultDecl_hI(cf(State));
parse_DefaultDecl(<<"#FIXED", Rest/bitstring>>, Stream, Pos, State, DTD, External) ->
    parse_DefaultDecl_hFIXED(Rest, Stream, Pos + 6, State, DTD, External);
parse_DefaultDecl(<<"#FIXE">>, _, _, State, DTD, _) ->
    parse_DefaultDecl_hFIXE(cf(State), DTD);
parse_DefaultDecl(<<"#FIX">>, _, _, State, DTD, _) ->
    parse_DefaultDecl_hFIX(cf(State), DTD);
parse_DefaultDecl(<<"#FI">>, _, _, State, DTD, _) ->
    parse_DefaultDecl_hFI(cf(State), DTD);
parse_DefaultDecl(<<"#F">>, _, _, State, DTD, _) ->
    parse_DefaultDecl_hF(cf(State), DTD);
?GET_PE(parse_DefaultDecl);
parse_DefaultDecl(?MATCH, DTD, External) ->
    % Faking not in replacement to block single quote characters in value
    State0 = State#ys_state{in_replacement = false},
    {AttValue, ?MATCH1} = parse_AttValue(Bytes, Stream, Pos, State0, DTD, External),
    {{value, AttValue}, ?MATCH1}.

%% [55]   StringType ::= 'CDATA'
%% [56]   TokenizedType ::= 'ID' | 'IDREF' | 'IDREFS' | 'ENTITY' | 'ENTITIES' | 'NMTOKEN' | 'NMTOKENS'
%% [57]   EnumeratedType ::= NotationType | Enumeration
%% [58]   NotationType ::= 'NOTATION' S '(' S? Name (S? '|' S? Name)* S? ')'     [VC: Notation Attributes]
%% [59]   Enumeration ::= '(' S? Nmtoken (S? '|' S? Nmtoken)* S? ')'    [VC: Enumeration]
parse_AttType(<<"CDATA", Rest/bitstring>>, Stream, Pos, State) ->
    {cdata, Rest, Stream, Pos + 5, State};
parse_AttType(<<"CDAT">>, _, _, State) ->
    parse_AttType_CDAT(cf(State));
parse_AttType(<<"CDA">>, _, _, State) ->
    parse_AttType_CDA(cf(State));
parse_AttType(<<"CD">>, _, _, State) ->
    parse_AttType_CD(cf(State));
parse_AttType(<<"C">>, _, _, State) ->
    parse_AttType_C(cf(State));
parse_AttType(<<$(, Rest/bitstring>>, Stream, Pos, State) ->
    {_, ?MATCH1} = maybe_consume_s(Rest, Stream, Pos + 1, State),
    {Nmtoken, ?MATCH2} = parse_Nmtoken(?MATCH1),
    {_, ?MATCH3} = maybe_consume_s(?MATCH2),
    parse_AttType_Enumeration(?MATCH3, [Nmtoken]);
parse_AttType(<<"NOTATION", Rest/bitstring>>, Stream, Pos, State) ->
    parse_AttType_NOTATION(Rest, Stream, Pos + 8, State);
parse_AttType(<<"NOTATIO">>, _, _, State) ->
    parse_AttType_NOTATIO(cf(State));
parse_AttType(<<"NOTATI">>, _, _, State) ->
    parse_AttType_NOTATI(cf(State));
parse_AttType(<<"NOTAT">>, _, _, State) ->
    parse_AttType_NOTAT(cf(State));
parse_AttType(<<"NOTA">>, _, _, State) ->
    parse_AttType_NOTA(cf(State));
parse_AttType(<<"NOT">>, _, _, State) ->
    parse_AttType_NOT(cf(State));
parse_AttType(<<"NO">>, _, _, State) ->
    parse_AttType_NO(cf(State));
parse_AttType(<<"NMTOKENS", Rest/bitstring>>, Stream, Pos, State) ->
    {nmtokens, Rest, Stream, Pos + 8, State};
parse_AttType(<<"NMTOKEN">>, _, _, State) ->
    parse_AttType_NMTOKEN(cf(State));
parse_AttType(<<"NMTOKE">>, _, _, State) ->
    parse_AttType_NMTOKE(cf(State));
parse_AttType(<<"NMTOK">>, _, _, State) ->
    parse_AttType_NMTOK(cf(State));
parse_AttType(<<"NMTO">>, _, _, State) ->
    parse_AttType_NMTO(cf(State));
parse_AttType(<<"NMT">>, _, _, State) ->
    parse_AttType_NMT(cf(State));
parse_AttType(<<"NM">>, _, _, State) ->
    parse_AttType_NM(cf(State));
parse_AttType(<<"N">>, _, _, State) ->
    parse_AttType_N(cf(State));
parse_AttType(<<"NMTOKEN", Rest/bitstring>>, Stream, Pos, State) ->
    {nmtoken, Rest, Stream, Pos + 7, State};
parse_AttType(<<"ENTITIES", Rest/bitstring>>, Stream, Pos, State) ->
    {entities, Rest, Stream, Pos + 8, State};
parse_AttType(<<"ENTITIE">>, _, _, State) ->
    parse_AttType_ENTITIE(cf(State));
parse_AttType(<<"ENTITI">>, _, _, State) ->
    parse_AttType_ENTITI(cf(State));
parse_AttType(<<"ENTIT">>, _, _, State) ->
    parse_AttType_ENTIT(cf(State));
parse_AttType(<<"ENTI">>, _, _, State) ->
    parse_AttType_ENTI(cf(State));
parse_AttType(<<"ENT">>, _, _, State) ->
    parse_AttType_ENT(cf(State));
parse_AttType(<<"EN">>, _, _, State) ->
    parse_AttType_EN(cf(State));
parse_AttType(<<"E">>, _, _, State) ->
    parse_AttType_E(cf(State));
parse_AttType(<<"ENTITY", Rest/bitstring>>, Stream, Pos, State) ->
    {entity, Rest, Stream, Pos + 6, State};
parse_AttType(<<"IDREFS", Rest/bitstring>>, Stream, Pos, State) ->
    {idrefs, Rest, Stream, Pos + 6, State};
parse_AttType(<<"IDREF">>, _, _, State) ->
    parse_AttType_IDREF(cf(State));
parse_AttType(<<"IDRE">>, _, _, State) ->
    parse_AttType_IDRE(cf(State));
parse_AttType(<<"IDR">>, _, _, State) ->
    parse_AttType_IDR(cf(State));
parse_AttType(<<"IDREF", Rest/bitstring>>, Stream, Pos, State) ->
    {idref, Rest, Stream, Pos + 5, State};
parse_AttType(<<"ID">>, _, _, State) ->
    parse_AttType_ID(cf(State));
parse_AttType(<<"ID", Rest/bitstring>>, Stream, Pos, State) ->
    {id, Rest, Stream, Pos + 2, State};
parse_AttType(<<"I">>, _, _, State) ->
    parse_AttType_I(cf(State)).

%% [58]   NotationType ::= 'NOTATION' S '(' S? Name (S? '|' S? Name)* S? ')'     [VC: Notation Attributes]
parse_AttType_NOTATION(?MATCH) ->
    {_, <<$(, Bytes1/bitstring>>, Stream1, Pos1, State1} = consume_s(?MATCH),
    {_, ?MATCH2} = maybe_consume_s(Bytes1, Stream1, Pos1 + 1, State1),
    {Name, ?MATCH3} = parse_Name(?MATCH2),
    {_, ?MATCH4} = maybe_consume_s(?MATCH3),
    parse_AttType_NOTATION_names(?MATCH4, [Name]).

parse_AttType_NOTATION_names(<<$), Rest/bitstring>>, Stream, Pos, State, Acc) ->
    {{notation, lists:reverse(Acc)}, Rest, Stream, Pos + 1, State};
parse_AttType_NOTATION_names(<<$|, Rest/bitstring>>, Stream, Pos, State, Acc) ->
    {_, ?MATCH1} = maybe_consume_s(Rest, Stream, Pos + 1, State),
    {Name, ?MATCH2} = parse_Name(?MATCH1),
    {_, ?MATCH3} = maybe_consume_s(?MATCH2),
    parse_AttType_NOTATION_names(?MATCH3, [Name | Acc]).

%% [59]   Enumeration ::= '(' S? Nmtoken (S? '|' S? Nmtoken)* S? ')'    [VC: Enumeration]
parse_AttType_Enumeration(<<$), Rest/bitstring>>, Stream, Pos, State, Acc) ->
    {{enum, lists:reverse(Acc)}, Rest, Stream, Pos + 1, State};
parse_AttType_Enumeration(<<$|, Rest/bitstring>>, Stream, Pos, State, Acc) ->
    {_, ?MATCH1} = maybe_consume_s(Rest, Stream, Pos + 1, State),
    {Nmtoken, ?MATCH2} = parse_Nmtoken(?MATCH1),
    {_, ?MATCH3} = maybe_consume_s(?MATCH2),
    parse_AttType_Enumeration(?MATCH3, [Nmtoken | Acc]).

%%----------------------------------------------------------------------
%% [70]   EntityDecl ::= GEDecl | PEDecl
%% [71]   GEDecl ::= '<!ENTITY' S Name S EntityDef S? '>'
%% [73]   EntityDef ::= EntityValue | (ExternalID NDataDecl?)
%% [9]    EntityValue ::= '"' ([^%&"] | PEReference | Reference)* '"' |  "'" ([^%&'] | PEReference | Reference)* "'"
%% [76]   NDataDecl ::= S 'NDATA' S Name     [VC: Notation Declared]
%% [72]   PEDecl ::= '<!ENTITY' S '%' S Name S PEDef S? '>'
%% [74]   PEDef ::= EntityValue | ExternalID
%%----------------------------------------------------------------------
parse_EntityDecl(?MATCH, DTD, External) ->
    case consume_s(?MATCH) of
        {_, <<$%, Bytes1/bitstring>>, Stream1, Pos1, State1} ->
            %% [72]   PEDecl ::= '<!ENTITY' S '%' S Name S PEDef S? '>'
            {_, ?MATCH2} = consume_s(Bytes1, Stream1, Pos1 + 1, State1),
            {Name, ?MATCH3} = parse_NCName(?MATCH2),
            {_, ?MATCH4} = consume_s(?MATCH3),
            {{InEx, EntityValue, DTD1}, ?MATCH5} = parse_PEDef(?MATCH4, DTD, External),
            {_, <<$>, Bytes6/bitstring>>, Stream6, Pos6, State6} = maybe_consume_s(?MATCH5),
            DTD2 = add_entity_to_dtd({Name, {parameter, InEx, EntityValue}}, DTD1),
            {DTD2, Bytes6, Stream6, Pos6 + 1, State6};
        {_, ?MATCH1} ->
            %% [71]   GEDecl ::= '<!ENTITY' S Name S EntityDef S? '>'
            {Name, ?MATCH2} = parse_NCName(?MATCH1),
            {_, ?MATCH3} = consume_s(?MATCH2),
            {{EntityType, EntityValue, DTD1}, ?MATCH4} = parse_EntityDef(?MATCH3, DTD, External),
            {_, <<$>, Bytes5/bitstring>>, Stream5, Pos5, State5} = maybe_consume_s(?MATCH4),
            DTD2 = add_entity_to_dtd({Name, {general, EntityType, EntityValue}}, DTD1),
            {DTD2, Bytes5, Stream5, Pos5 + 1, State5}
    end.

% quote == EntityValue, P or S == ExternalID
parse_PEDef(<<Char, _/bitstring>> = ?MATCH, DTD, _) when Char == $P; Char == $S ->
    {Val, ?MATCH1} = parse_ExternalID(?MATCH),
    {{external, Val, DTD}, ?MATCH1};
parse_PEDef(?MATCH, DTD, External) ->
    case parse_EntityValue(?MATCH, DTD, External) of
        {{Val, DTD1}, ?MATCH1} -> {{internal, Val, DTD1}, ?MATCH1};
        {Val, ?MATCH1} -> {{internal, Val, DTD}, ?MATCH1}
    end.

parse_EntityDef(<<Char, _/bitstring>> = ?MATCH, DTD, _) when Char == $P; Char == $S ->
    {{Pub, Sys}, ?MATCH1} = parse_ExternalID(?MATCH),
    case maybe_consume_s(?MATCH1) of
        {true, <<$N, Bytes2/bitstring>>, Stream2, Pos2, State2} ->
            {Name, ?MATCH3} = parse_NDataDecl(Bytes2, Stream2, Pos2 + 1, State2),
            {{external, {Pub, Sys, Name}, DTD}, ?MATCH3};
        {_, ?MATCH2} ->
            {{external, {Pub, Sys}, DTD}, ?MATCH2}
    end;
parse_EntityDef(?MATCH, DTD, External) ->
    case parse_EntityValue(?MATCH, DTD, External) of
        {{Val, DTD1}, ?MATCH1} -> {{internal, Val, DTD1}, ?MATCH1};
        {Val, ?MATCH1} -> {{internal, Val, DTD}, ?MATCH1}
    end.

parse_NDataDecl(<<"DATA", Rest/bitstring>>, Stream, Pos, State) ->
    parse_NDataDecl_NDATA(Rest, Stream, Pos + 4, State);
parse_NDataDecl(<<"DAT">>, _, _, State) ->
    parse_NDataDecl_NDAT(cf(State));
parse_NDataDecl(<<"DA">>, _, _, State) ->
    parse_NDataDecl_NDA(cf(State));
parse_NDataDecl(<<"D">>, _, _, State) ->
    parse_NDataDecl_ND(cf(State));
parse_NDataDecl(<<>>, _, _, State) ->
    {Bytes1, State1} = cf(State),
    parse_NDataDecl(Bytes1, Bytes1, 0, State1).

parse_NDataDecl_ND({<<"ATA", Rest/bitstring>> = Stream, State}) ->
    parse_NDataDecl_NDATA(Rest, Stream, 3, State);
parse_NDataDecl_ND({<<"AT">>, State}) ->
    parse_NDataDecl_NDAT(cf(State));
parse_NDataDecl_ND({<<"A">>, State}) ->
    parse_NDataDecl_NDA(cf(State));
?FUNCTION_NAME({<<>>, State}) ->
    ?FUNCTION_NAME(cf(State)).

parse_NDataDecl_NDA({<<"TA", Rest/bitstring>> = Stream, State}) ->
    parse_NDataDecl_NDATA(Rest, Stream, 2, State);
parse_NDataDecl_NDA({<<"T">>, State}) ->
    parse_NDataDecl_NDAT(cf(State));
?FUNCTION_NAME({<<>>, State}) ->
    ?FUNCTION_NAME(cf(State)).

parse_NDataDecl_NDAT({<<"A", Rest/bitstring>> = Stream, State}) ->
    parse_NDataDecl_NDATA(Rest, Stream, 1, State);
?FUNCTION_NAME({<<>>, State}) ->
    ?FUNCTION_NAME(cf(State)).

%% [76]   NDataDecl ::= S 'NDATA' S Name
parse_NDataDecl_NDATA(?MATCH) ->
    {_, ?MATCH1} = consume_s(?MATCH),
    parse_Name(?MATCH1).

%% [9]    EntityValue ::= '"' ([^%&"] | PEReference | Reference)* '"' |  "'" ([^%&'] | PEReference | Reference)* "'"
parse_EntityValue(<<$', Rest/bitstring>>, Stream, Pos, State, DTD, External) ->
    parse_EntityValue_sq(Rest, Stream, Pos + 1, 0, State, [], DTD, External);
parse_EntityValue(<<$", Rest/bitstring>>, Stream, Pos, State, DTD, External) ->
    parse_EntityValue_dq(Rest, Stream, Pos + 1, 0, State, [], DTD, External).

parse_EntityValue_sq(<<$\r, Rest/bitstring>>, Stream, Pos, Len, State, Acc, DTD, External) ->
    ?FUNCTION_NAME(Rest, Stream, Pos, Len + 1, State, Acc, DTD, External);
parse_EntityValue_sq(
    <<$', Rest/bitstring>>,
    Stream,
    Pos,
    Len,
    #ys_state{in_replacement = false} = State,
    Acc,
    DTD,
    External
) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    if
        External ->
            {{to_binary(Acc1), DTD}, Rest, Stream, Pos + 1 + Len, State};
        true ->
            {to_binary(Acc1), Rest, Stream, Pos + 1 + Len, State}
    end;
parse_EntityValue_sq(<<$%, Rest/bitstring>>, Stream, Pos, Len, State, Acc, DTD, true) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    {PERef, ?MATCH1} = parse_PEReference(Rest, Stream, Pos + Len + 1, State),
    case resolve_parameter_entity(PERef, DTD, State1, false, true) of
        {replacement_text, ReplacementText} ->
            RTState = replacement_text_state(Bytes1, State1),
            ?FUNCTION_NAME(ReplacementText, ReplacementText, 0, 0, RTState, Acc1, DTD, true);
        {dtd, DTD1} ->
            ?FUNCTION_NAME(Bytes1, Stream1, Pos1, 0, State1, Acc1, DTD1, true)
    end;
parse_EntityValue_sq(<<$%, _Rest/bitstring>>, _Stream, _Pos, _Len, State, _Acc, _DTD, false) ->
    fatal_error(pe_in_markup, State);
parse_EntityValue_sq(<<$&, Rest/bitstring>>, Stream, Pos, Len, State, Acc, DTD, External) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    case parse_Reference(Rest, Stream, Pos + Len + 1, State, entity) of
        {{_Name, {replacement_text, Ref}}, ?MATCH1} ->
            Acc2 = ?APPEND(Ref, Acc1),
            ?FUNCTION_NAME(Bytes1, Stream1, Pos1, 0, State1, Acc2, DTD, External);
        {{gen, <<$<>>}, ?MATCH1} ->
            Acc2 = ?APPEND(<<"&lt;">>, Acc1),
            ?FUNCTION_NAME(Bytes1, Stream1, Pos1, 0, State1, Acc2, DTD, External);
        {{gen, <<$&>>}, ?MATCH1} ->
            Acc2 = ?APPEND(<<"&amp;">>, Acc1),
            ?FUNCTION_NAME(Bytes1, Stream1, Pos1, 0, State1, Acc2, DTD, External);
        {{Type, <<$&>>}, ?MATCH1} when Type == hex; Type == dec ->
            Acc2 = ?APPEND(<<"&#38;">>, Acc1),
            ?FUNCTION_NAME(Bytes1, Stream1, Pos1, 0, State1, Acc2, DTD, External);
        {{Type, <<$\r>>}, ?MATCH1} when Type == hex; Type == dec ->
            Acc2 = ?APPEND(<<"&#xD;">>, Acc1),
            ?FUNCTION_NAME(Bytes1, Stream1, Pos1, 0, State1, Acc2, DTD, External);
        {{_, Ref}, ?MATCH1} ->
            Acc2 = ?APPEND(Ref, Acc1),
            ?FUNCTION_NAME(Bytes1, Stream1, Pos1, 0, State1, Acc2, DTD, External)
    end;
?ONECHARX.

parse_EntityValue_dq(<<$\r, Rest/bitstring>>, Stream, Pos, Len, State, Acc, DTD, External) ->
    ?FUNCTION_NAME(Rest, Stream, Pos, Len + 1, State, Acc, DTD, External);
parse_EntityValue_dq(
    <<$", Rest/bitstring>>,
    Stream,
    Pos,
    Len,
    #ys_state{in_replacement = false} = State,
    Acc,
    DTD,
    External
) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    if
        External ->
            {{to_binary(Acc1), DTD}, Rest, Stream, Pos + 1 + Len, State};
        true ->
            {to_binary(Acc1), Rest, Stream, Pos + 1 + Len, State}
    end;
% Not allowed in internal, is allowed in external.
parse_EntityValue_dq(<<$%, Rest/bitstring>>, Stream, Pos, Len, State, Acc, DTD, true) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    {PERef, ?MATCH1} = parse_PEReference(Rest, Stream, Pos + Len + 1, State),
    case resolve_parameter_entity(PERef, DTD, State1, false, true) of
        {replacement_text, ReplacementText} ->
            RTState = replacement_text_state(Bytes1, State1),
            ?FUNCTION_NAME(ReplacementText, ReplacementText, 0, 0, RTState, Acc1, DTD, true);
        {dtd, DTD1} ->
            ?FUNCTION_NAME(Bytes1, Stream1, Pos1, 0, State1, Acc1, DTD1, true)
    end;
parse_EntityValue_dq(<<$%, _Rest/bitstring>>, _Stream, _Pos, _Len, State, _Acc, _DTD, false) ->
    fatal_error(pe_in_markup, State);
parse_EntityValue_dq(<<$&, Rest/bitstring>>, Stream, Pos, Len, State, Acc, DTD, External) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    case parse_Reference(Rest, Stream, Pos + Len + 1, State#ys_state{dtd = DTD}, entity) of
        {{_Name, {replacement_text, Ref}}, ?MATCH1} ->
            Acc2 = ?APPEND(Ref, Acc1),
            ?FUNCTION_NAME(Bytes1, Stream1, Pos1, 0, State1, Acc2, DTD, External);
        {{gen, <<$<>>}, ?MATCH1} ->
            Acc2 = ?APPEND(<<"&lt;">>, Acc1),
            ?FUNCTION_NAME(Bytes1, Stream1, Pos1, 0, State1, Acc2, DTD, External);
        {{gen, <<$&>>}, ?MATCH1} ->
            Acc2 = ?APPEND(<<"&amp;">>, Acc1),
            ?FUNCTION_NAME(Bytes1, Stream1, Pos1, 0, State1, Acc2, DTD, External);
        {{Type, <<$&>>}, ?MATCH1} when Type == hex; Type == dec ->
            Acc2 = ?APPEND(<<"&#38;">>, Acc1),
            ?FUNCTION_NAME(Bytes1, Stream1, Pos1, 0, State1, Acc2, DTD, External);
        {{Type, <<$\r>>}, ?MATCH1} when Type == hex; Type == dec ->
            Acc2 = ?APPEND(<<"&#xD;">>, Acc1),
            ?FUNCTION_NAME(Bytes1, Stream1, Pos1, 0, State1, Acc2, DTD, External);
        {{_, Ref}, ?MATCH1} ->
            Acc2 = ?APPEND(Ref, Acc1),
            ?FUNCTION_NAME(Bytes1, Stream1, Pos1, 0, State1, Acc2, DTD, External)
    end;
?ONECHARX.

%%----------------------------------------------------------------------
%% [82]   NotationDecl ::= '<!NOTATION' S Name S (ExternalID | PublicID) S? '>'    [VC: Unique Notation Name]
%% [83]   PublicID            ::=       'PUBLIC' S PubidLiteral
%%----------------------------------------------------------------------
parse_NotationDecl(?MATCH, DTD) ->
    {_, ?MATCH1} = consume_s(?MATCH),
    {Name, ?MATCH2} = parse_NCName(?MATCH1),
    {_, ?MATCH3} = consume_s(?MATCH2),
    {{Pub, Sys}, ?MATCH4} =
        case Bytes3 of
            <<$S, _/bitstring>> ->
                parse_ExternalID(?MATCH3);
            <<$P, _/bitstring>> ->
                parse_PublicID(?MATCH3)
        end,
    case Bytes4 of
        <<$>, Bytes5/bitstring>> ->
            DTD1 = add_notation_to_dtd({Name, Pub, Sys}, DTD),
            {DTD1, Bytes5, Stream4, Pos4 + 1, State4};
        _ ->
            {Sys1, <<$>, Bytes5/bitstring>>, Stream5, Pos5, State5} = parse_SystemLiteral(?MATCH4),
            DTD1 = add_notation_to_dtd({Name, Pub, Sys1}, DTD),
            {DTD1, Bytes5, Stream5, Pos5 + 1, State5}
    end.

parse_PublicID(<<"PUBLIC", Rest/bitstring>>, Stream, Pos, State) ->
    parse_PublicID_PUBLIC(Rest, Stream, Pos + 6, State);
parse_PublicID(<<"PUBLI">>, _, _, State) ->
    parse_PublicID_PUBLI(cf(State));
parse_PublicID(<<"PUBL">>, _, _, State) ->
    parse_PublicID_PUBL(cf(State));
parse_PublicID(<<"PUB">>, _, _, State) ->
    parse_PublicID_PUB(cf(State));
parse_PublicID(<<"PU">>, _, _, State) ->
    parse_PublicID_PU(cf(State));
parse_PublicID(<<"P">>, _, _, State) ->
    parse_PublicID_P(cf(State));
?FUNCTION_NAME(<<>>, _, _, State) ->
    {Stream1, State1} = cf(State),
    ?FUNCTION_NAME(Stream1, Stream1, 0, State1).

parse_PublicID_P({<<"UBLIC", Rest/bitstring>> = Stream, State}) ->
    parse_PublicID_PUBLIC(Rest, Stream, 5, State);
parse_PublicID_P({<<"UBLI">>, State}) ->
    parse_PublicID_PUBLI(cf(State));
parse_PublicID_P({<<"UBL">>, State}) ->
    parse_PublicID_PUBL(cf(State));
parse_PublicID_P({<<"UB">>, State}) ->
    parse_PublicID_PUB(cf(State));
parse_PublicID_P({<<"U">>, State}) ->
    parse_PublicID_PU(cf(State));
?FUNCTION_NAME({<<>>, State}) ->
    ?FUNCTION_NAME(cf(State)).

parse_PublicID_PU({<<"BLIC", Rest/bitstring>> = Stream, State}) ->
    parse_PublicID_PUBLIC(Rest, Stream, 4, State);
parse_PublicID_PU({<<"BLI">>, State}) ->
    parse_PublicID_PUBLI(cf(State));
parse_PublicID_PU({<<"BL">>, State}) ->
    parse_PublicID_PUBL(cf(State));
parse_PublicID_PU({<<"B">>, State}) ->
    parse_PublicID_PUB(cf(State));
?FUNCTION_NAME({<<>>, State}) ->
    ?FUNCTION_NAME(cf(State)).

parse_PublicID_PUB({<<"LIC", Rest/bitstring>> = Stream, State}) ->
    parse_PublicID_PUBLIC(Rest, Stream, 3, State);
parse_PublicID_PUB({<<"LI">>, State}) ->
    parse_PublicID_PUBLI(cf(State));
parse_PublicID_PUB({<<"L">>, State}) ->
    parse_PublicID_PUBL(cf(State));
?FUNCTION_NAME({<<>>, State}) ->
    ?FUNCTION_NAME(cf(State)).

parse_PublicID_PUBL({<<"IC", Rest/bitstring>> = Stream, State}) ->
    parse_PublicID_PUBLIC(Rest, Stream, 2, State);
parse_PublicID_PUBL({<<"I">>, State}) ->
    parse_PublicID_PUBLI(cf(State));
?FUNCTION_NAME({<<>>, State}) ->
    ?FUNCTION_NAME(cf(State)).

parse_PublicID_PUBLI({<<"C", Rest/bitstring>> = Stream, State}) ->
    parse_PublicID_PUBLIC(Rest, Stream, 1, State);
?FUNCTION_NAME({<<>>, State}) ->
    ?FUNCTION_NAME(cf(State)).

parse_PublicID_PUBLIC(?MATCH) ->
    {_, ?MATCH1} = consume_s(?MATCH),
    {Pub, ?MATCH2} = parse_PubidLiteral(?MATCH1),
    {_, ?MATCH3} = maybe_consume_s(?MATCH2),
    {{Pub, <<>>}, ?MATCH3}.

%%----------------------------------------------------------------------
%% Consume Nmtoken
%% [7] Nmtoken ::= (NameChar)+
%%----------------------------------------------------------------------
parse_Nmtoken(?MATCH) ->
    case parse_Name(Bytes, Stream, Pos, 0, State, []) of
        {<<>>, _, _, _, State1} -> fatal_error(bad_token, State1);
        Ret -> Ret
    end.

%%----------------------------------------------------------------------
%% [69] PEReference ::= '%' Name ';'
%% [WFC: No Recursion]
%% [WFC: In DTD]
%% % already trimmed
%% returns Name
%%----------------------------------------------------------------------
parse_PEReference(?MATCH) ->
    case parse_Name(?MATCH) of
        {Name, <<$;/utf8, Bytes1/bitstring>>, Stream1, Pos1, State1} ->
            {Name, Bytes1, Stream1, Pos1 + 1, State1};
        {_, Bytes1, _, _, State1} ->
            fatal_error(bad_peref, {Bytes1, State1})
    end.

%%----------------------------------------------------------------------
%% [28] doctypedecl ::= '<!DOCTYPE' S Name (S ExternalID)? S? ('[' intSubset ']' S?)? '>'
%% [16ns] doctypedecl ::= '<!DOCTYPE' S QName (S ExternalID)? S? ('[' (markupdecl | PEReference | S)* ']' S?)? '>'
%% [VC: Root Element Type]
%% [WFC: External Subset]
%%----------------------------------------------------------------------

parse_doctypedecl_lt({<<"!DOCTYPE"/utf8, Rest/bitstring>> = Stream, State}) ->
    parse_doctypedecl_ltexDOCTYPE(Rest, Stream, 8, State);
parse_doctypedecl_lt({<<"!DOCTYP"/utf8>>, State}) ->
    parse_doctypedecl_ltexDOCTYP(cf(State));
parse_doctypedecl_lt({<<"!DOCTY"/utf8>>, State}) ->
    parse_doctypedecl_ltexDOCTY(cf(State));
parse_doctypedecl_lt({<<"!DOCT"/utf8>>, State}) ->
    parse_doctypedecl_ltexDOCT(cf(State));
parse_doctypedecl_lt({<<"!DOC"/utf8>>, State}) ->
    parse_doctypedecl_ltexDOC(cf(State));
parse_doctypedecl_lt({<<"!DO"/utf8>>, State}) ->
    parse_doctypedecl_ltexDO(cf(State));
parse_doctypedecl_lt({<<"!D"/utf8>>, State}) ->
    parse_doctypedecl_ltexD(cf(State));
parse_doctypedecl_lt({<<"!"/utf8>>, State}) ->
    parse_doctypedecl_ltex(cf(State));
parse_doctypedecl_lt({<<>>, State}) ->
    parse_doctypedecl_lt(cf(State));
parse_doctypedecl_lt({_, State}) ->
    fatal_error(bad_doctype, State).

parse_doctypedecl_ltex({<<"DOCTYPE"/utf8, Rest/bitstring>> = Stream, State}) ->
    parse_doctypedecl_ltexDOCTYPE(Rest, Stream, 7, State);
parse_doctypedecl_ltex({<<"DOCTYP"/utf8>>, State}) ->
    parse_doctypedecl_ltexDOCTYP(cf(State));
parse_doctypedecl_ltex({<<"DOCTY"/utf8>>, State}) ->
    parse_doctypedecl_ltexDOCTY(cf(State));
parse_doctypedecl_ltex({<<"DOCT"/utf8>>, State}) ->
    parse_doctypedecl_ltexDOCT(cf(State));
parse_doctypedecl_ltex({<<"DOC"/utf8>>, State}) ->
    parse_doctypedecl_ltexDOC(cf(State));
parse_doctypedecl_ltex({<<"DO"/utf8>>, State}) ->
    parse_doctypedecl_ltexDO(cf(State));
parse_doctypedecl_ltex({<<"D"/utf8>>, State}) ->
    parse_doctypedecl_ltexD(cf(State));
parse_doctypedecl_ltex({<<>>, State}) ->
    parse_doctypedecl_ltex(cf(State));
parse_doctypedecl_ltex({_, State}) ->
    fatal_error(bad_doctype, State).

parse_doctypedecl_ltexD({<<"OCTYPE"/utf8, Rest/bitstring>> = Stream, State}) ->
    parse_doctypedecl_ltexDOCTYPE(Rest, Stream, 6, State);
parse_doctypedecl_ltexD({<<"OCTYP"/utf8>>, State}) ->
    parse_doctypedecl_ltexDOCTYP(cf(State));
parse_doctypedecl_ltexD({<<"OCTY"/utf8>>, State}) ->
    parse_doctypedecl_ltexDOCTY(cf(State));
parse_doctypedecl_ltexD({<<"OCT"/utf8>>, State}) ->
    parse_doctypedecl_ltexDOCT(cf(State));
parse_doctypedecl_ltexD({<<"OC"/utf8>>, State}) ->
    parse_doctypedecl_ltexDOC(cf(State));
parse_doctypedecl_ltexD({<<"O"/utf8>>, State}) ->
    parse_doctypedecl_ltexDO(cf(State));
parse_doctypedecl_ltexD({<<>>, State}) ->
    parse_doctypedecl_ltexD(cf(State));
parse_doctypedecl_ltexD({_, State}) ->
    fatal_error(bad_doctype, State).

parse_doctypedecl_ltexDO({<<"CTYPE"/utf8, Rest/bitstring>> = Stream, State}) ->
    parse_doctypedecl_ltexDOCTYPE(Rest, Stream, 5, State);
parse_doctypedecl_ltexDO({<<"CTYP"/utf8>>, State}) ->
    parse_doctypedecl_ltexDOCTYP(cf(State));
parse_doctypedecl_ltexDO({<<"CTY"/utf8>>, State}) ->
    parse_doctypedecl_ltexDOCTY(cf(State));
parse_doctypedecl_ltexDO({<<"CT"/utf8>>, State}) ->
    parse_doctypedecl_ltexDOCT(cf(State));
parse_doctypedecl_ltexDO({<<"C"/utf8>>, State}) ->
    parse_doctypedecl_ltexDOC(cf(State));
parse_doctypedecl_ltexDO({<<>>, State}) ->
    parse_doctypedecl_ltexDO(cf(State));
parse_doctypedecl_ltexDO({_, State}) ->
    fatal_error(bad_doctype, State).

parse_doctypedecl_ltexDOC({<<"TYPE"/utf8, Rest/bitstring>> = Stream, State}) ->
    parse_doctypedecl_ltexDOCTYPE(Rest, Stream, 4, State);
parse_doctypedecl_ltexDOC({<<"TYP"/utf8>>, State}) ->
    parse_doctypedecl_ltexDOCTYP(cf(State));
parse_doctypedecl_ltexDOC({<<"TY"/utf8>>, State}) ->
    parse_doctypedecl_ltexDOCTY(cf(State));
parse_doctypedecl_ltexDOC({<<"T"/utf8>>, State}) ->
    parse_doctypedecl_ltexDOCT(cf(State));
parse_doctypedecl_ltexDOC({<<>>, State}) ->
    parse_doctypedecl_ltexDOC(cf(State));
parse_doctypedecl_ltexDOC({_, State}) ->
    fatal_error(bad_doctype, State).

parse_doctypedecl_ltexDOCT({<<"YPE"/utf8, Rest/bitstring>> = Stream, State}) ->
    parse_doctypedecl_ltexDOCTYPE(Rest, Stream, 3, State);
parse_doctypedecl_ltexDOCT({<<"YP"/utf8>>, State}) ->
    parse_doctypedecl_ltexDOCTYP(cf(State));
parse_doctypedecl_ltexDOCT({<<"Y"/utf8>>, State}) ->
    parse_doctypedecl_ltexDOCTY(cf(State));
parse_doctypedecl_ltexDOCT({<<>>, State}) ->
    parse_doctypedecl_ltexDOCT(cf(State));
parse_doctypedecl_ltexDOCT({_, State}) ->
    fatal_error(bad_doctype, State).

parse_doctypedecl_ltexDOCTY({<<"PE"/utf8, Rest/bitstring>> = Stream, State}) ->
    parse_doctypedecl_ltexDOCTYPE(Rest, Stream, 2, State);
parse_doctypedecl_ltexDOCTY({<<"P"/utf8>>, State}) ->
    parse_doctypedecl_ltexDOCTYP(cf(State));
parse_doctypedecl_ltexDOCTY({<<>>, State}) ->
    parse_doctypedecl_ltexDOCTY(cf(State));
parse_doctypedecl_ltexDOCTY({_, State}) ->
    fatal_error(bad_doctype, State).

parse_doctypedecl_ltexDOCTYP({<<"E"/utf8, Rest/bitstring>> = Stream, State}) ->
    parse_doctypedecl_ltexDOCTYPE(Rest, Stream, 1, State);
parse_doctypedecl_ltexDOCTYP({<<>>, State}) ->
    parse_doctypedecl_ltexDOCTYP(cf(State));
parse_doctypedecl_ltexDOCTYP({_, State}) ->
    fatal_error(bad_doctype, State).

parse_ExternalID_syste({<<"M", Rest/bitstring>> = Stream, State}) ->
    parse_ExternalID_system(Rest, Stream, 1, State);
?FUNCTION_NAME({<<>>, State}) ->
    ?FUNCTION_NAME(cf(State));
parse_ExternalID_syste({_, State}) ->
    fatal_error(bad_system_id, State).

parse_ExternalID_syst({<<"EM", Rest/bitstring>> = Stream, State}) ->
    parse_ExternalID_system(Rest, Stream, 2, State);
parse_ExternalID_syst({<<"E">>, State}) ->
    parse_ExternalID_syste(cf(State));
?FUNCTION_NAME({<<>>, State}) ->
    ?FUNCTION_NAME(cf(State));
parse_ExternalID_syst({_, State}) ->
    fatal_error(bad_system_id, State).

parse_ExternalID_sys({<<"TEM", Rest/bitstring>> = Stream, State}) ->
    parse_ExternalID_system(Rest, Stream, 3, State);
parse_ExternalID_sys({<<"TE">>, State}) ->
    parse_ExternalID_syste(cf(State));
parse_ExternalID_sys({<<"T">>, State}) ->
    parse_ExternalID_syst(cf(State));
?FUNCTION_NAME({<<>>, State}) ->
    ?FUNCTION_NAME(cf(State));
parse_ExternalID_sys({_, State}) ->
    fatal_error(bad_system_id, State).

parse_ExternalID_sy({<<"STEM", Rest/bitstring>> = Stream, State}) ->
    parse_ExternalID_system(Rest, Stream, 4, State);
parse_ExternalID_sy({<<"STE">>, State}) ->
    parse_ExternalID_syste(cf(State));
parse_ExternalID_sy({<<"ST">>, State}) ->
    parse_ExternalID_syst(cf(State));
parse_ExternalID_sy({<<"S">>, State}) ->
    parse_ExternalID_sys(cf(State));
?FUNCTION_NAME({<<>>, State}) ->
    ?FUNCTION_NAME(cf(State));
parse_ExternalID_sy({_, State}) ->
    fatal_error(bad_system_id, State).

parse_ExternalID_s({<<"YSTEM", Rest/bitstring>> = Stream, State}) ->
    parse_ExternalID_system(Rest, Stream, 5, State);
parse_ExternalID_s({<<"YSTE">>, State}) ->
    parse_ExternalID_syste(cf(State));
parse_ExternalID_s({<<"YST">>, State}) ->
    parse_ExternalID_syst(cf(State));
parse_ExternalID_s({<<"YS">>, State}) ->
    parse_ExternalID_sys(cf(State));
parse_ExternalID_s({<<"Y">>, State}) ->
    parse_ExternalID_sy(cf(State));
?FUNCTION_NAME({<<>>, State}) ->
    ?FUNCTION_NAME(cf(State));
parse_ExternalID_s({_, State}) ->
    fatal_error(bad_system_id, State).

parse_ExternalID_publi({<<"C", Rest/bitstring>> = Stream, State}) ->
    parse_ExternalID_public(Rest, Stream, 1, State);
?FUNCTION_NAME({<<>>, State}) ->
    ?FUNCTION_NAME(cf(State));
parse_ExternalID_publi({_, State}) ->
    fatal_error(bad_public_id, State).

parse_ExternalID_publ({<<"IC", Rest/bitstring>> = Stream, State}) ->
    parse_ExternalID_public(Rest, Stream, 2, State);
parse_ExternalID_publ({<<"I">>, State}) ->
    parse_ExternalID_publi(cf(State));
?FUNCTION_NAME({<<>>, State}) ->
    ?FUNCTION_NAME(cf(State));
parse_ExternalID_publ({_, State}) ->
    fatal_error(bad_public_id, State).

parse_ExternalID_pub({<<"LIC", Rest/bitstring>> = Stream, State}) ->
    parse_ExternalID_public(Rest, Stream, 3, State);
parse_ExternalID_pub({<<"LI">>, State}) ->
    parse_ExternalID_publi(cf(State));
parse_ExternalID_pub({<<"L">>, State}) ->
    parse_ExternalID_publ(cf(State));
?FUNCTION_NAME({<<>>, State}) ->
    ?FUNCTION_NAME(cf(State));
parse_ExternalID_pub({_, State}) ->
    fatal_error(bad_public_id, State).

parse_ExternalID_pu({<<"BLIC", Rest/bitstring>> = Stream, State}) ->
    parse_ExternalID_public(Rest, Stream, 4, State);
parse_ExternalID_pu({<<"BLI">>, State}) ->
    parse_ExternalID_publi(cf(State));
parse_ExternalID_pu({<<"BL">>, State}) ->
    parse_ExternalID_publ(cf(State));
parse_ExternalID_pu({<<"B">>, State}) ->
    parse_ExternalID_pub(cf(State));
?FUNCTION_NAME({<<>>, State}) ->
    ?FUNCTION_NAME(cf(State));
parse_ExternalID_pu({_, State}) ->
    fatal_error(bad_public_id, State).

parse_ExternalID_p({<<"UBLIC", Rest/bitstring>> = Stream, State}) ->
    parse_ExternalID_public(Rest, Stream, 5, State);
parse_ExternalID_p({<<"UBLI">>, State}) ->
    parse_ExternalID_publi(cf(State));
parse_ExternalID_p({<<"UBL">>, State}) ->
    parse_ExternalID_publ(cf(State));
parse_ExternalID_p({<<"UB">>, State}) ->
    parse_ExternalID_pub(cf(State));
parse_ExternalID_p({<<"U">>, State}) ->
    parse_ExternalID_pu(cf(State));
?FUNCTION_NAME({<<>>, State}) ->
    ?FUNCTION_NAME(cf(State));
parse_ExternalID_p({_, State}) ->
    fatal_error(bad_public_id, State).

parse_intSubset_ltex({<<"--"/utf8, Bytes/bitstring>> = Stream, State}, DTD) ->
    {?MATCH1} = consume_Comment(Bytes, Stream, 2, State),
    parse_intSubset(?MATCH1, DTD);
parse_intSubset_ltex({<<"-"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexdash(cf(State), DTD);
parse_intSubset_ltex({<<"ELEMENT"/utf8, Bytes/bitstring>> = Stream, State}, DTD) ->
    {DTD1, ?MATCH1} = parse_elementdecl(Bytes, Stream, 7, State, DTD, false),
    parse_intSubset(?MATCH1, DTD1);
parse_intSubset_ltex({<<"ELEMEN"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexELEMEN(cf(State), DTD);
parse_intSubset_ltex({<<"ELEME"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexELEME(cf(State), DTD);
parse_intSubset_ltex({<<"ELEM"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexELEM(cf(State), DTD);
parse_intSubset_ltex({<<"ELE"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexELE(cf(State), DTD);
parse_intSubset_ltex({<<"EL"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexEL(cf(State), DTD);
parse_intSubset_ltex({<<"E"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexE(cf(State), DTD);
parse_intSubset_ltex({<<"ATTLIST"/utf8, Bytes/bitstring>> = Stream, State}, DTD) ->
    {DTD1, ?MATCH1} = parse_AttlistDecl(Bytes, Stream, 7, State, DTD, false),
    parse_intSubset(?MATCH1, DTD1);
parse_intSubset_ltex({<<"ATTLIS"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexATTLIS(cf(State), DTD);
parse_intSubset_ltex({<<"ATTLI"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexATTLI(cf(State), DTD);
parse_intSubset_ltex({<<"ATTL"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexATTL(cf(State), DTD);
parse_intSubset_ltex({<<"ATT"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexATT(cf(State), DTD);
parse_intSubset_ltex({<<"AT"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexAT(cf(State), DTD);
parse_intSubset_ltex({<<"A"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexA(cf(State), DTD);
parse_intSubset_ltex({<<"ENTITY"/utf8, Bytes/bitstring>> = Stream, State}, DTD) ->
    {DTD1, ?MATCH1} = parse_EntityDecl(Bytes, Stream, 6, State, DTD, false),
    parse_intSubset(?MATCH1, DTD1);
parse_intSubset_ltex({<<"ENTIT"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexENTIT(cf(State), DTD);
parse_intSubset_ltex({<<"ENTI"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexENTI(cf(State), DTD);
parse_intSubset_ltex({<<"ENT"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexENT(cf(State), DTD);
parse_intSubset_ltex({<<"EN"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexEN(cf(State), DTD);
parse_intSubset_ltex({<<"NOTATION"/utf8, Bytes/bitstring>> = Stream, State}, DTD) ->
    {DTD1, ?MATCH1} = parse_NotationDecl(Bytes, Stream, 8, State, DTD),
    parse_intSubset(?MATCH1, DTD1);
parse_intSubset_ltex({<<"NOTATIO"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexNOTATIO(cf(State), DTD);
parse_intSubset_ltex({<<"NOTATI"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexNOTATI(cf(State), DTD);
parse_intSubset_ltex({<<"NOTAT"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexNOTAT(cf(State), DTD);
parse_intSubset_ltex({<<"NOTA"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexNOTA(cf(State), DTD);
parse_intSubset_ltex({<<"NOT"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexNOT(cf(State), DTD);
parse_intSubset_ltex({<<"NO"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexNO(cf(State), DTD);
parse_intSubset_ltex({<<"N"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexN(cf(State), DTD);
?FUNCTION_NAME({<<>>, State}, DTD) ->
    ?FUNCTION_NAME(cf(State), DTD);
parse_intSubset_ltex({_, State}, _DTD) ->
    fatal_error(bad_dtd, State).

parse_intSubset_lt({<<"?"/utf8, Bytes/bitstring>> = Stream, State}, DTD) ->
    {PI, ?MATCH1} = parse_PI(Bytes, Stream, 1, State),
    DTD1 = add_pi_comment_to_dtd(PI, DTD),
    parse_intSubset(?MATCH1, DTD1);
parse_intSubset_lt({<<"!--"/utf8, Bytes/bitstring>> = Stream, State}, DTD) ->
    {?MATCH1} = consume_Comment(Bytes, Stream, 3, State),
    parse_intSubset(?MATCH1, DTD);
parse_intSubset_lt({<<"!-"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexdash(cf(State), DTD);
parse_intSubset_lt({<<"!ELEMENT"/utf8, Bytes/bitstring>> = Stream, State}, DTD) ->
    {DTD1, ?MATCH1} = parse_elementdecl(Bytes, Stream, 8, State, DTD, false),
    parse_intSubset(?MATCH1, DTD1);
parse_intSubset_lt({<<"!ELEMEN"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexELEMEN(cf(State), DTD);
parse_intSubset_lt({<<"!ELEME"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexELEME(cf(State), DTD);
parse_intSubset_lt({<<"!ELEM"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexELEM(cf(State), DTD);
parse_intSubset_lt({<<"!ELE"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexELE(cf(State), DTD);
parse_intSubset_lt({<<"!EL"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexEL(cf(State), DTD);
parse_intSubset_lt({<<"!E"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexE(cf(State), DTD);
parse_intSubset_lt({<<"!"/utf8>>, State}, DTD) ->
    parse_intSubset_ltex(cf(State), DTD);
parse_intSubset_lt({<<"!ATTLIST"/utf8, Bytes/bitstring>> = Stream, State}, DTD) ->
    {DTD1, ?MATCH1} = parse_AttlistDecl(Bytes, Stream, 8, State, DTD, false),
    parse_intSubset(?MATCH1, DTD1);
parse_intSubset_lt({<<"!ATTLIS"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexATTLIS(cf(State), DTD);
parse_intSubset_lt({<<"!ATTLI"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexATTLI(cf(State), DTD);
parse_intSubset_lt({<<"!ATTL"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexATTL(cf(State), DTD);
parse_intSubset_lt({<<"!ATT"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexATT(cf(State), DTD);
parse_intSubset_lt({<<"!AT"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexAT(cf(State), DTD);
parse_intSubset_lt({<<"!A"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexA(cf(State), DTD);
parse_intSubset_lt({<<"!ENTITY"/utf8, Bytes/bitstring>> = Stream, State}, DTD) ->
    {DTD1, ?MATCH1} = parse_EntityDecl(Bytes, Stream, 7, State, DTD, false),
    parse_intSubset(?MATCH1, DTD1);
parse_intSubset_lt({<<"!ENTIT"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexENTIT(cf(State), DTD);
parse_intSubset_lt({<<"!ENTI"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexENTI(cf(State), DTD);
parse_intSubset_lt({<<"!ENT"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexENT(cf(State), DTD);
parse_intSubset_lt({<<"!EN"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexEN(cf(State), DTD);
parse_intSubset_lt({<<"!NOTATION"/utf8, Bytes/bitstring>> = Stream, State}, DTD) ->
    {DTD1, ?MATCH1} = parse_NotationDecl(Bytes, Stream, 9, State, DTD),
    parse_intSubset(?MATCH1, DTD1);
parse_intSubset_lt({<<"!NOTATIO"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexNOTATIO(cf(State), DTD);
parse_intSubset_lt({<<"!NOTATI"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexNOTATI(cf(State), DTD);
parse_intSubset_lt({<<"!NOTAT"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexNOTAT(cf(State), DTD);
parse_intSubset_lt({<<"!NOTA"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexNOTA(cf(State), DTD);
parse_intSubset_lt({<<"!NOT"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexNOT(cf(State), DTD);
parse_intSubset_lt({<<"!NO"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexNO(cf(State), DTD);
parse_intSubset_lt({<<"!N"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexN(cf(State), DTD);
?FUNCTION_NAME({<<>>, State}, DTD) ->
    ?FUNCTION_NAME(cf(State), DTD);
parse_intSubset_lt({_, State}, _DTD) ->
    fatal_error(bad_dtd, State).

parse_intSubset_ltexN({<<"OTATION"/utf8, Bytes/bitstring>> = Stream, State}, DTD) ->
    {DTD1, ?MATCH1} = parse_NotationDecl(Bytes, Stream, 7, State, DTD),
    parse_intSubset(?MATCH1, DTD1);
parse_intSubset_ltexN({<<"OTATIO"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexNOTATIO(cf(State), DTD);
parse_intSubset_ltexN({<<"OTATI"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexNOTATI(cf(State), DTD);
parse_intSubset_ltexN({<<"OTAT"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexNOTAT(cf(State), DTD);
parse_intSubset_ltexN({<<"OTA"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexNOTA(cf(State), DTD);
parse_intSubset_ltexN({<<"OT"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexNOT(cf(State), DTD);
parse_intSubset_ltexN({<<"O"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexNO(cf(State), DTD);
?FUNCTION_NAME({<<>>, State}, DTD) ->
    ?FUNCTION_NAME(cf(State), DTD);
parse_intSubset_ltexN({_, State}, _DTD) ->
    fatal_error(bad_dtd, State).

parse_intSubset_ltexNO({<<"TATION"/utf8, Bytes/bitstring>> = Stream, State}, DTD) ->
    {DTD1, ?MATCH1} = parse_NotationDecl(Bytes, Stream, 6, State, DTD),
    parse_intSubset(?MATCH1, DTD1);
parse_intSubset_ltexNO({<<"TATIO"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexNOTATIO(cf(State), DTD);
parse_intSubset_ltexNO({<<"TATI"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexNOTATI(cf(State), DTD);
parse_intSubset_ltexNO({<<"TAT"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexNOTAT(cf(State), DTD);
parse_intSubset_ltexNO({<<"TA"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexNOTA(cf(State), DTD);
parse_intSubset_ltexNO({<<"T"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexNOT(cf(State), DTD);
?FUNCTION_NAME({<<>>, State}, DTD) ->
    ?FUNCTION_NAME(cf(State), DTD);
parse_intSubset_ltexNO({_, State}, _DTD) ->
    fatal_error(bad_dtd, State).

parse_intSubset_ltexNOT({<<"ATION"/utf8, Bytes/bitstring>> = Stream, State}, DTD) ->
    {DTD1, ?MATCH1} = parse_NotationDecl(Bytes, Stream, 5, State, DTD),
    parse_intSubset(?MATCH1, DTD1);
parse_intSubset_ltexNOT({<<"ATIO"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexNOTATIO(cf(State), DTD);
parse_intSubset_ltexNOT({<<"ATI"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexNOTATI(cf(State), DTD);
parse_intSubset_ltexNOT({<<"AT"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexNOTAT(cf(State), DTD);
parse_intSubset_ltexNOT({<<"A"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexNOTA(cf(State), DTD);
?FUNCTION_NAME({<<>>, State}, DTD) ->
    ?FUNCTION_NAME(cf(State), DTD);
parse_intSubset_ltexNOT({_, State}, _DTD) ->
    fatal_error(bad_dtd, State).

parse_intSubset_ltexNOTA({<<"TION"/utf8, Bytes/bitstring>> = Stream, State}, DTD) ->
    {DTD1, ?MATCH1} = parse_NotationDecl(Bytes, Stream, 4, State, DTD),
    parse_intSubset(?MATCH1, DTD1);
parse_intSubset_ltexNOTA({<<"TIO"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexNOTATIO(cf(State), DTD);
parse_intSubset_ltexNOTA({<<"TI"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexNOTATI(cf(State), DTD);
parse_intSubset_ltexNOTA({<<"T"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexNOTAT(cf(State), DTD);
?FUNCTION_NAME({<<>>, State}, DTD) ->
    ?FUNCTION_NAME(cf(State), DTD);
parse_intSubset_ltexNOTA({_, State}, _DTD) ->
    fatal_error(bad_dtd, State).

parse_intSubset_ltexNOTAT({<<"ION"/utf8, Bytes/bitstring>> = Stream, State}, DTD) ->
    {DTD1, ?MATCH1} = parse_NotationDecl(Bytes, Stream, 3, State, DTD),
    parse_intSubset(?MATCH1, DTD1);
parse_intSubset_ltexNOTAT({<<"IO"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexNOTATIO(cf(State), DTD);
parse_intSubset_ltexNOTAT({<<"I"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexNOTATI(cf(State), DTD);
?FUNCTION_NAME({<<>>, State}, DTD) ->
    ?FUNCTION_NAME(cf(State), DTD);
parse_intSubset_ltexNOTAT({_, State}, _DTD) ->
    fatal_error(bad_dtd, State).

parse_intSubset_ltexNOTATI({<<"ON"/utf8, Bytes/bitstring>> = Stream, State}, DTD) ->
    {DTD1, ?MATCH1} = parse_NotationDecl(Bytes, Stream, 2, State, DTD),
    parse_intSubset(?MATCH1, DTD1);
parse_intSubset_ltexNOTATI({<<"O"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexNOTATIO(cf(State), DTD);
?FUNCTION_NAME({<<>>, State}, DTD) ->
    ?FUNCTION_NAME(cf(State), DTD);
parse_intSubset_ltexNOTATI({_, State}, _DTD) ->
    fatal_error(bad_dtd, State).

parse_intSubset_ltexNOTATIO({<<"N"/utf8, Bytes/bitstring>> = Stream, State}, DTD) ->
    {DTD1, ?MATCH1} = parse_NotationDecl(Bytes, Stream, 1, State, DTD),
    parse_intSubset(?MATCH1, DTD1);
?FUNCTION_NAME({<<>>, State}, DTD) ->
    ?FUNCTION_NAME(cf(State), DTD);
parse_intSubset_ltexNOTATIO({_, State}, _DTD) ->
    fatal_error(bad_dtd, State).

parse_intSubset_ltexA({<<"TTLIST"/utf8, Bytes/bitstring>> = Stream, State}, DTD) ->
    {DTD1, ?MATCH1} = parse_AttlistDecl(Bytes, Stream, 6, State, DTD, false),
    parse_intSubset(?MATCH1, DTD1);
parse_intSubset_ltexA({<<"TTLIS"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexATTLIS(cf(State), DTD);
parse_intSubset_ltexA({<<"TTLI"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexATTLI(cf(State), DTD);
parse_intSubset_ltexA({<<"TTL"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexATTL(cf(State), DTD);
parse_intSubset_ltexA({<<"TT"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexATT(cf(State), DTD);
parse_intSubset_ltexA({<<"T"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexAT(cf(State), DTD);
?FUNCTION_NAME({<<>>, State}, DTD) ->
    ?FUNCTION_NAME(cf(State), DTD);
parse_intSubset_ltexA({_, State}, _DTD) ->
    fatal_error(bad_dtd, State).

parse_intSubset_ltexAT({<<"TLIST"/utf8, Bytes/bitstring>> = Stream, State}, DTD) ->
    {DTD1, ?MATCH1} = parse_AttlistDecl(Bytes, Stream, 5, State, DTD, false),
    parse_intSubset(?MATCH1, DTD1);
parse_intSubset_ltexAT({<<"TLIS"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexATTLIS(cf(State), DTD);
parse_intSubset_ltexAT({<<"TLI"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexATTLI(cf(State), DTD);
parse_intSubset_ltexAT({<<"TL"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexATTL(cf(State), DTD);
parse_intSubset_ltexAT({<<"T"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexATT(cf(State), DTD);
?FUNCTION_NAME({<<>>, State}, DTD) ->
    ?FUNCTION_NAME(cf(State), DTD);
parse_intSubset_ltexAT({_, State}, _DTD) ->
    fatal_error(bad_dtd, State).

parse_intSubset_ltexATT({<<"LIST"/utf8, Bytes/bitstring>> = Stream, State}, DTD) ->
    {DTD1, ?MATCH1} = parse_AttlistDecl(Bytes, Stream, 4, State, DTD, false),
    parse_intSubset(?MATCH1, DTD1);
parse_intSubset_ltexATT({<<"LIS"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexATTLIS(cf(State), DTD);
parse_intSubset_ltexATT({<<"LI"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexATTLI(cf(State), DTD);
parse_intSubset_ltexATT({<<"L"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexATTL(cf(State), DTD);
?FUNCTION_NAME({<<>>, State}, DTD) ->
    ?FUNCTION_NAME(cf(State), DTD);
parse_intSubset_ltexATT({_, State}, _DTD) ->
    fatal_error(bad_dtd, State).

parse_intSubset_ltexATTL({<<"IST"/utf8, Bytes/bitstring>> = Stream, State}, DTD) ->
    {DTD1, ?MATCH1} = parse_AttlistDecl(Bytes, Stream, 3, State, DTD, false),
    parse_intSubset(?MATCH1, DTD1);
parse_intSubset_ltexATTL({<<"IS"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexATTLIS(cf(State), DTD);
parse_intSubset_ltexATTL({<<"I"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexATTLI(cf(State), DTD);
?FUNCTION_NAME({<<>>, State}, DTD) ->
    ?FUNCTION_NAME(cf(State), DTD);
parse_intSubset_ltexATTL({_, State}, _DTD) ->
    fatal_error(bad_dtd, State).

parse_intSubset_ltexATTLI({<<"ST"/utf8, Bytes/bitstring>> = Stream, State}, DTD) ->
    {DTD1, ?MATCH1} = parse_AttlistDecl(Bytes, Stream, 2, State, DTD, false),
    parse_intSubset(?MATCH1, DTD1);
parse_intSubset_ltexATTLI({<<"S"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexATTLIS(cf(State), DTD);
?FUNCTION_NAME({<<>>, State}, DTD) ->
    ?FUNCTION_NAME(cf(State), DTD);
parse_intSubset_ltexATTLI({_, State}, _DTD) ->
    fatal_error(bad_dtd, State).

parse_intSubset_ltexATTLIS({<<"T"/utf8, Bytes/bitstring>> = Stream, State}, DTD) ->
    {DTD1, ?MATCH1} = parse_AttlistDecl(Bytes, Stream, 1, State, DTD, false),
    parse_intSubset(?MATCH1, DTD1);
?FUNCTION_NAME({<<>>, State}, DTD) ->
    ?FUNCTION_NAME(cf(State), DTD);
parse_intSubset_ltexATTLIS({_, State}, _DTD) ->
    fatal_error(bad_dtd, State).

parse_intSubset_ltexE({<<"LEMENT"/utf8, Bytes/bitstring>> = Stream, State}, DTD) ->
    {DTD1, ?MATCH1} = parse_elementdecl(Bytes, Stream, 6, State, DTD, false),
    parse_intSubset(?MATCH1, DTD1);
parse_intSubset_ltexE({<<"LEMEN"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexELEMEN(cf(State), DTD);
parse_intSubset_ltexE({<<"LEME"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexELEME(cf(State), DTD);
parse_intSubset_ltexE({<<"LEM"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexELEM(cf(State), DTD);
parse_intSubset_ltexE({<<"LE"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexELE(cf(State), DTD);
parse_intSubset_ltexE({<<"L"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexEL(cf(State), DTD);
parse_intSubset_ltexE({<<"NTITY"/utf8, Bytes/bitstring>> = Stream, State}, DTD) ->
    {DTD1, ?MATCH1} = parse_EntityDecl(Bytes, Stream, 5, State, DTD, false),
    parse_intSubset(?MATCH1, DTD1);
parse_intSubset_ltexE({<<"NTIT"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexENTIT(cf(State), DTD);
parse_intSubset_ltexE({<<"NTI"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexENTI(cf(State), DTD);
parse_intSubset_ltexE({<<"NT"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexENT(cf(State), DTD);
parse_intSubset_ltexE({<<"N"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexEN(cf(State), DTD);
?FUNCTION_NAME({<<>>, State}, DTD) ->
    ?FUNCTION_NAME(cf(State), DTD);
parse_intSubset_ltexE({_, State}, _DTD) ->
    fatal_error(bad_dtd, State).

parse_intSubset_ltexEL({<<"EMENT"/utf8, Bytes/bitstring>> = Stream, State}, DTD) ->
    {DTD1, ?MATCH1} = parse_elementdecl(Bytes, Stream, 5, State, DTD, false),
    parse_intSubset(?MATCH1, DTD1);
parse_intSubset_ltexEL({<<"EMEN"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexELEMEN(cf(State), DTD);
parse_intSubset_ltexEL({<<"EME"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexELEME(cf(State), DTD);
parse_intSubset_ltexEL({<<"EM"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexELEM(cf(State), DTD);
parse_intSubset_ltexEL({<<"E"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexELE(cf(State), DTD);
?FUNCTION_NAME({<<>>, State}, DTD) ->
    ?FUNCTION_NAME(cf(State), DTD);
parse_intSubset_ltexEL({_, State}, _DTD) ->
    fatal_error(bad_dtd, State).

parse_intSubset_ltexELE({<<"MENT"/utf8, Bytes/bitstring>> = Stream, State}, DTD) ->
    {DTD1, ?MATCH1} = parse_elementdecl(Bytes, Stream, 4, State, DTD, false),
    parse_intSubset(?MATCH1, DTD1);
parse_intSubset_ltexELE({<<"MEN"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexELEMEN(cf(State), DTD);
parse_intSubset_ltexELE({<<"ME"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexELEME(cf(State), DTD);
parse_intSubset_ltexELE({<<"M"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexELEM(cf(State), DTD);
?FUNCTION_NAME({<<>>, State}, DTD) ->
    ?FUNCTION_NAME(cf(State), DTD);
parse_intSubset_ltexELE({_, State}, _DTD) ->
    fatal_error(bad_dtd, State).

parse_intSubset_ltexELEM({<<"ENT"/utf8, Bytes/bitstring>> = Stream, State}, DTD) ->
    {DTD1, ?MATCH1} = parse_elementdecl(Bytes, Stream, 3, State, DTD, false),
    parse_intSubset(?MATCH1, DTD1);
parse_intSubset_ltexELEM({<<"EN"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexELEMEN(cf(State), DTD);
parse_intSubset_ltexELEM({<<"E"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexELEME(cf(State), DTD);
?FUNCTION_NAME({<<>>, State}, DTD) ->
    ?FUNCTION_NAME(cf(State), DTD);
parse_intSubset_ltexELEM({_, State}, _DTD) ->
    fatal_error(bad_dtd, State).

parse_intSubset_ltexELEME({<<"NT"/utf8, Bytes/bitstring>> = Stream, State}, DTD) ->
    {DTD1, ?MATCH1} = parse_elementdecl(Bytes, Stream, 2, State, DTD, false),
    parse_intSubset(?MATCH1, DTD1);
parse_intSubset_ltexELEME({<<"N"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexELEMEN(cf(State), DTD);
?FUNCTION_NAME({<<>>, State}, DTD) ->
    ?FUNCTION_NAME(cf(State), DTD);
parse_intSubset_ltexELEME({_, State}, _DTD) ->
    fatal_error(bad_dtd, State).

parse_intSubset_ltexELEMEN({<<"T"/utf8, Bytes/bitstring>> = Stream, State}, DTD) ->
    {DTD1, ?MATCH1} = parse_elementdecl(Bytes, Stream, 1, State, DTD, false),
    parse_intSubset(?MATCH1, DTD1);
?FUNCTION_NAME({<<>>, State}, DTD) ->
    ?FUNCTION_NAME(cf(State), DTD);
parse_intSubset_ltexELEMEN({_, State}, _DTD) ->
    fatal_error(bad_dtd, State).

parse_intSubset_ltexEN({<<"TITY"/utf8, Bytes/bitstring>> = Stream, State}, DTD) ->
    {DTD1, ?MATCH1} = parse_EntityDecl(Bytes, Stream, 4, State, DTD, false),
    parse_intSubset(?MATCH1, DTD1);
parse_intSubset_ltexEN({<<"TIT"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexENTIT(cf(State), DTD);
parse_intSubset_ltexEN({<<"TI"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexENTI(cf(State), DTD);
parse_intSubset_ltexEN({<<"T"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexENT(cf(State), DTD);
?FUNCTION_NAME({<<>>, State}, DTD) ->
    ?FUNCTION_NAME(cf(State), DTD);
parse_intSubset_ltexEN({_, State}, _DTD) ->
    fatal_error(bad_dtd, State).

parse_intSubset_ltexENT({<<"ITY"/utf8, Bytes/bitstring>> = Stream, State}, DTD) ->
    {DTD1, ?MATCH1} = parse_EntityDecl(Bytes, Stream, 3, State, DTD, false),
    parse_intSubset(?MATCH1, DTD1);
parse_intSubset_ltexENT({<<"IT"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexENTIT(cf(State), DTD);
parse_intSubset_ltexENT({<<"I"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexENTI(cf(State), DTD);
?FUNCTION_NAME({<<>>, State}, DTD) ->
    ?FUNCTION_NAME(cf(State), DTD);
parse_intSubset_ltexENT({_, State}, _DTD) ->
    fatal_error(bad_dtd, State).

parse_intSubset_ltexENTI({<<"TY"/utf8, Bytes/bitstring>> = Stream, State}, DTD) ->
    {DTD1, ?MATCH1} = parse_EntityDecl(Bytes, Stream, 2, State, DTD, false),
    parse_intSubset(?MATCH1, DTD1);
parse_intSubset_ltexENTI({<<"T"/utf8>>, State}, DTD) ->
    parse_intSubset_ltexENTIT(cf(State), DTD);
?FUNCTION_NAME({<<>>, State}, DTD) ->
    ?FUNCTION_NAME(cf(State), DTD);
parse_intSubset_ltexENTI({_, State}, _DTD) ->
    fatal_error(bad_dtd, State).

parse_intSubset_ltexENTIT({<<"Y"/utf8, Bytes/bitstring>> = Stream, State}, DTD) ->
    {DTD1, ?MATCH1} = parse_EntityDecl(Bytes, Stream, 1, State, DTD, false),
    parse_intSubset(?MATCH1, DTD1);
?FUNCTION_NAME({<<>>, State}, DTD) ->
    ?FUNCTION_NAME(cf(State), DTD);
parse_intSubset_ltexENTIT({_, State}, _DTD) ->
    fatal_error(bad_dtd, State).

parse_intSubset_ltexdash({<<"-"/utf8, Bytes/bitstring>> = Stream, State}, DTD) ->
    {?MATCH1} = consume_Comment(Bytes, Stream, 1, State),
    parse_intSubset(?MATCH1, DTD);
?FUNCTION_NAME({<<>>, State}, DTD) ->
    ?FUNCTION_NAME(cf(State), DTD);
parse_intSubset_ltexdash({_, State}, _DTD) ->
    fatal_error(bad_dtd, State).

parse_contentspec_E({<<"MPTY", Rest/bitstring>> = Stream, State}) ->
    {empty, Rest, Stream, 4, State};
parse_contentspec_E({<<"MPT">>, State}) ->
    parse_contentspec_EMPT(cf(State));
parse_contentspec_E({<<"MP">>, State}) ->
    parse_contentspec_EMP(cf(State));
parse_contentspec_E({<<"M">>, State}) ->
    parse_contentspec_EM(cf(State));
?FUNCTION_NAME({<<>>, State}) ->
    ?FUNCTION_NAME(cf(State)).

parse_contentspec_EM({<<"PTY", Rest/bitstring>> = Stream, State}) ->
    {empty, Rest, Stream, 3, State};
parse_contentspec_EM({<<"PT">>, State}) ->
    parse_contentspec_EMPT(cf(State));
parse_contentspec_EM({<<"P">>, State}) ->
    parse_contentspec_EMP(cf(State));
?FUNCTION_NAME({<<>>, State}) ->
    ?FUNCTION_NAME(cf(State)).

parse_contentspec_EMP({<<"TY", Rest/bitstring>> = Stream, State}) ->
    {empty, Rest, Stream, 2, State};
parse_contentspec_EMP({<<"T">>, State}) ->
    parse_contentspec_EMPT(cf(State));
?FUNCTION_NAME({<<>>, State}) ->
    ?FUNCTION_NAME(cf(State)).

parse_contentspec_EMPT({<<"Y", Rest/bitstring>> = Stream, State}) ->
    {empty, Rest, Stream, 1, State};
?FUNCTION_NAME({<<>>, State}) ->
    ?FUNCTION_NAME(cf(State)).

parse_contentspec_A({<<"NY", Rest/bitstring>> = Stream, State}) ->
    {any, Rest, Stream, 2, State};
parse_contentspec_A({<<"N">>, State}) ->
    parse_contentspec_AN(cf(State));
?FUNCTION_NAME({<<>>, State}) ->
    ?FUNCTION_NAME(cf(State)).

parse_contentspec_AN({<<"Y", Rest/bitstring>> = Stream, State}) ->
    {any, Rest, Stream, 1, State};
?FUNCTION_NAME({<<>>, State}) ->
    ?FUNCTION_NAME(cf(State)).

parse_Mixed_P({<<"CDATA", Rest/bitstring>> = Stream, State}, DTD, External) ->
    parse_Mixed_(Rest, Stream, 5, State, DTD, External);
parse_Mixed_P({<<"CDAT">>, State}, DTD, External) ->
    parse_Mixed_PCDAT(cf(State), DTD, External);
parse_Mixed_P({<<"CDA">>, State}, DTD, External) ->
    parse_Mixed_PCDA(cf(State), DTD, External);
parse_Mixed_P({<<"CD">>, State}, DTD, External) ->
    parse_Mixed_PCD(cf(State), DTD, External);
parse_Mixed_P({<<"C">>, State}, DTD, External) ->
    parse_Mixed_PC(cf(State), DTD, External);
?FUNCTION_NAME({<<>>, State}, DTD, External) ->
    ?FUNCTION_NAME(cf(State), DTD, External).

parse_Mixed_PC({<<"DATA", Rest/bitstring>> = Stream, State}, DTD, External) ->
    parse_Mixed_(Rest, Stream, 4, State, DTD, External);
parse_Mixed_PC({<<"DAT">>, State}, DTD, External) ->
    parse_Mixed_PCDAT(cf(State), DTD, External);
parse_Mixed_PC({<<"DA">>, State}, DTD, External) ->
    parse_Mixed_PCDA(cf(State), DTD, External);
parse_Mixed_PC({<<"D">>, State}, DTD, External) ->
    parse_Mixed_PCD(cf(State), DTD, External);
?FUNCTION_NAME({<<>>, State}, DTD, External) ->
    ?FUNCTION_NAME(cf(State), DTD, External).

parse_Mixed_PCD({<<"ATA", Rest/bitstring>> = Stream, State}, DTD, External) ->
    parse_Mixed_(Rest, Stream, 3, State, DTD, External);
parse_Mixed_PCD({<<"AT">>, State}, DTD, External) ->
    parse_Mixed_PCDAT(cf(State), DTD, External);
parse_Mixed_PCD({<<"A">>, State}, DTD, External) ->
    parse_Mixed_PCDA(cf(State), DTD, External);
?FUNCTION_NAME({<<>>, State}, DTD, External) ->
    ?FUNCTION_NAME(cf(State), DTD, External).

parse_Mixed_PCDA({<<"TA", Rest/bitstring>> = Stream, State}, DTD, External) ->
    parse_Mixed_(Rest, Stream, 2, State, DTD, External);
parse_Mixed_PCDA({<<"T">>, State}, DTD, External) ->
    parse_Mixed_PCDAT(cf(State), DTD, External);
?FUNCTION_NAME({<<>>, State}, DTD, External) ->
    ?FUNCTION_NAME(cf(State), DTD, External).

parse_Mixed_PCDAT({<<"A", Rest/bitstring>> = Stream, State}, DTD, External) ->
    parse_Mixed_(Rest, Stream, 1, State, DTD, External);
?FUNCTION_NAME({<<>>, State}, DTD, External) ->
    ?FUNCTION_NAME(cf(State), DTD, External).

parse_AttType_C({<<"DATA", Rest/bitstring>> = Stream, State}) -> {cdata, Rest, Stream, 4, State};
parse_AttType_C({<<"DAT">>, State}) -> parse_AttType_CDAT(cf(State));
parse_AttType_C({<<"DA">>, State}) -> parse_AttType_CDA(cf(State));
parse_AttType_C({<<"D">>, State}) -> parse_AttType_CD(cf(State));
?FUNCTION_NAME({<<>>, State}) -> ?FUNCTION_NAME(cf(State)).

parse_AttType_CD({<<"ATA", Rest/bitstring>> = Stream, State}) -> {cdata, Rest, Stream, 3, State};
parse_AttType_CD({<<"AT">>, State}) -> parse_AttType_CDAT(cf(State));
parse_AttType_CD({<<"A">>, State}) -> parse_AttType_CDA(cf(State));
?FUNCTION_NAME({<<>>, State}) -> ?FUNCTION_NAME(cf(State)).

parse_AttType_CDA({<<"TA", Rest/bitstring>> = Stream, State}) -> {cdata, Rest, Stream, 2, State};
parse_AttType_CDA({<<"T">>, State}) -> parse_AttType_CDAT(cf(State));
?FUNCTION_NAME({<<>>, State}) -> ?FUNCTION_NAME(cf(State)).

parse_AttType_CDAT({<<"A", Rest/bitstring>> = Stream, State}) -> {cdata, Rest, Stream, 1, State};
?FUNCTION_NAME({<<>>, State}) -> ?FUNCTION_NAME(cf(State)).

parse_AttType_N({<<"OTATION", Rest/bitstring>> = Stream, State}) ->
    parse_AttType_NOTATION(Rest, Stream, 7, State);
parse_AttType_N({<<"OTATIO">>, State}) ->
    parse_AttType_NOTATIO(cf(State));
parse_AttType_N({<<"OTATI">>, State}) ->
    parse_AttType_NOTATI(cf(State));
parse_AttType_N({<<"OTAT">>, State}) ->
    parse_AttType_NOTAT(cf(State));
parse_AttType_N({<<"OTA">>, State}) ->
    parse_AttType_NOTA(cf(State));
parse_AttType_N({<<"OT">>, State}) ->
    parse_AttType_NOT(cf(State));
parse_AttType_N({<<"O">>, State}) ->
    parse_AttType_NO(cf(State));
parse_AttType_N({<<"MTOKENS", Rest/bitstring>> = Stream, State}) ->
    {nmtokens, Rest, Stream, 7, State};
parse_AttType_N({<<"MTOKEN">>, State}) ->
    parse_AttType_NMTOKEN(cf(State));
parse_AttType_N({<<"MTOKEN", Rest/bitstring>> = Stream, State}) ->
    {nmtoken, Rest, Stream, 6, State};
parse_AttType_N({<<"MTOKE">>, State}) ->
    parse_AttType_NMTOKE(cf(State));
parse_AttType_N({<<"MTOK">>, State}) ->
    parse_AttType_NMTOK(cf(State));
parse_AttType_N({<<"MTO">>, State}) ->
    parse_AttType_NMTO(cf(State));
parse_AttType_N({<<"MT">>, State}) ->
    parse_AttType_NMT(cf(State));
parse_AttType_N({<<"M">>, State}) ->
    parse_AttType_NM(cf(State));
?FUNCTION_NAME({<<>>, State}) ->
    ?FUNCTION_NAME(cf(State)).

parse_AttType_NO({<<"TATION", Rest/bitstring>> = Stream, State}) ->
    parse_AttType_NOTATION(Rest, Stream, 6, State);
parse_AttType_NO({<<"TATIO">>, State}) ->
    parse_AttType_NOTATIO(cf(State));
parse_AttType_NO({<<"TATI">>, State}) ->
    parse_AttType_NOTATI(cf(State));
parse_AttType_NO({<<"TAT">>, State}) ->
    parse_AttType_NOTAT(cf(State));
parse_AttType_NO({<<"TA">>, State}) ->
    parse_AttType_NOTA(cf(State));
parse_AttType_NO({<<"T">>, State}) ->
    parse_AttType_NOT(cf(State));
?FUNCTION_NAME({<<>>, State}) ->
    ?FUNCTION_NAME(cf(State)).

parse_AttType_NOT({<<"ATION", Rest/bitstring>> = Stream, State}) ->
    parse_AttType_NOTATION(Rest, Stream, 5, State);
parse_AttType_NOT({<<"ATIO">>, State}) ->
    parse_AttType_NOTATIO(cf(State));
parse_AttType_NOT({<<"ATI">>, State}) ->
    parse_AttType_NOTATI(cf(State));
parse_AttType_NOT({<<"AT">>, State}) ->
    parse_AttType_NOTAT(cf(State));
parse_AttType_NOT({<<"A">>, State}) ->
    parse_AttType_NOTA(cf(State));
?FUNCTION_NAME({<<>>, State}) ->
    ?FUNCTION_NAME(cf(State)).

parse_AttType_NOTA({<<"TION", Rest/bitstring>> = Stream, State}) ->
    parse_AttType_NOTATION(Rest, Stream, 4, State);
parse_AttType_NOTA({<<"TIO">>, State}) ->
    parse_AttType_NOTATIO(cf(State));
parse_AttType_NOTA({<<"TI">>, State}) ->
    parse_AttType_NOTATI(cf(State));
parse_AttType_NOTA({<<"T">>, State}) ->
    parse_AttType_NOTAT(cf(State));
?FUNCTION_NAME({<<>>, State}) ->
    ?FUNCTION_NAME(cf(State)).

parse_AttType_NOTAT({<<"ION", Rest/bitstring>> = Stream, State}) ->
    parse_AttType_NOTATION(Rest, Stream, 3, State);
parse_AttType_NOTAT({<<"IO">>, State}) ->
    parse_AttType_NOTATIO(cf(State));
parse_AttType_NOTAT({<<"I">>, State}) ->
    parse_AttType_NOTATI(cf(State));
?FUNCTION_NAME({<<>>, State}) ->
    ?FUNCTION_NAME(cf(State)).

parse_AttType_NOTATI({<<"ON", Rest/bitstring>> = Stream, State}) ->
    parse_AttType_NOTATION(Rest, Stream, 2, State);
parse_AttType_NOTATI({<<"O">>, State}) ->
    parse_AttType_NOTATIO(cf(State));
?FUNCTION_NAME({<<>>, State}) ->
    ?FUNCTION_NAME(cf(State)).

parse_AttType_NOTATIO({<<"N", Rest/bitstring>> = Stream, State}) ->
    parse_AttType_NOTATION(Rest, Stream, 1, State);
?FUNCTION_NAME({<<>>, State}) ->
    ?FUNCTION_NAME(cf(State)).

parse_AttType_NM({<<"TOKENS", Rest/bitstring>> = Stream, State}) ->
    {nmtokens, Rest, Stream, 6, State};
parse_AttType_NM({<<"TOKEN">>, State}) ->
    parse_AttType_NMTOKEN(cf(State));
parse_AttType_NM({<<"TOKEN", Rest/bitstring>> = Stream, State}) ->
    {nmtoken, Rest, Stream, 5, State};
parse_AttType_NM({<<"TOKE">>, State}) ->
    parse_AttType_NMTOKE(cf(State));
parse_AttType_NM({<<"TOK">>, State}) ->
    parse_AttType_NMTOK(cf(State));
parse_AttType_NM({<<"TO">>, State}) ->
    parse_AttType_NMTO(cf(State));
parse_AttType_NM({<<"T">>, State}) ->
    parse_AttType_NMT(cf(State));
?FUNCTION_NAME({<<>>, State}) ->
    ?FUNCTION_NAME(cf(State)).

parse_AttType_NMT({<<"OKENS", Rest/bitstring>> = Stream, State}) ->
    {nmtokens, Rest, Stream, 5, State};
parse_AttType_NMT({<<"OKEN">>, State}) ->
    parse_AttType_NMTOKEN(cf(State));
parse_AttType_NMT({<<"OKEN", Rest/bitstring>> = Stream, State}) ->
    {nmtoken, Rest, Stream, 4, State};
parse_AttType_NMT({<<"OKE">>, State}) ->
    parse_AttType_NMTOKE(cf(State));
parse_AttType_NMT({<<"OK">>, State}) ->
    parse_AttType_NMTOK(cf(State));
parse_AttType_NMT({<<"O">>, State}) ->
    parse_AttType_NMTO(cf(State));
?FUNCTION_NAME({<<>>, State}) ->
    ?FUNCTION_NAME(cf(State)).

parse_AttType_NMTO({<<"KENS", Rest/bitstring>> = Stream, State}) ->
    {nmtokens, Rest, Stream, 4, State};
parse_AttType_NMTO({<<"KEN">>, State}) ->
    parse_AttType_NMTOKEN(cf(State));
parse_AttType_NMTO({<<"KEN", Rest/bitstring>> = Stream, State}) ->
    {nmtoken, Rest, Stream, 3, State};
parse_AttType_NMTO({<<"KE">>, State}) ->
    parse_AttType_NMTOKE(cf(State));
parse_AttType_NMTO({<<"K">>, State}) ->
    parse_AttType_NMTOK(cf(State));
?FUNCTION_NAME({<<>>, State}) ->
    ?FUNCTION_NAME(cf(State)).

parse_AttType_NMTOK({<<"ENS", Rest/bitstring>> = Stream, State}) ->
    {nmtokens, Rest, Stream, 3, State};
parse_AttType_NMTOK({<<"EN">>, State}) ->
    parse_AttType_NMTOKEN(cf(State));
parse_AttType_NMTOK({<<"EN", Rest/bitstring>> = Stream, State}) ->
    {nmtoken, Rest, Stream, 2, State};
parse_AttType_NMTOK({<<"E">>, State}) ->
    parse_AttType_NMTOKE(cf(State));
?FUNCTION_NAME({<<>>, State}) ->
    ?FUNCTION_NAME(cf(State)).

parse_AttType_NMTOKE({<<"NS", Rest/bitstring>> = Stream, State}) ->
    {nmtokens, Rest, Stream, 2, State};
parse_AttType_NMTOKE({<<"N">>, State}) ->
    parse_AttType_NMTOKEN(cf(State));
parse_AttType_NMTOKE({<<"N", Rest/bitstring>> = Stream, State}) ->
    {nmtoken, Rest, Stream, 1, State};
?FUNCTION_NAME({<<>>, State}) ->
    ?FUNCTION_NAME(cf(State)).

parse_AttType_NMTOKEN({<<"S", Rest/bitstring>> = Stream, State}) ->
    {nmtokens, Rest, Stream, 1, State};
?FUNCTION_NAME({<<>>, State}) ->
    ?FUNCTION_NAME(cf(State));
parse_AttType_NMTOKEN({Stream, State}) ->
    {nmtoken, Stream, Stream, 0, State}.

parse_AttType_E({<<"NTITIES", Rest/bitstring>> = Stream, State}) ->
    {entities, Rest, Stream, 7, State};
parse_AttType_E({<<"NTITY", Rest/bitstring>> = Stream, State}) ->
    {entity, Rest, Stream, 5, State};
parse_AttType_E({<<"NTITIE">>, State}) ->
    parse_AttType_ENTITIE(cf(State));
parse_AttType_E({<<"NTITI">>, State}) ->
    parse_AttType_ENTITI(cf(State));
parse_AttType_E({<<"NTIT">>, State}) ->
    parse_AttType_ENTIT(cf(State));
parse_AttType_E({<<"NTI">>, State}) ->
    parse_AttType_ENTI(cf(State));
parse_AttType_E({<<"NT">>, State}) ->
    parse_AttType_ENT(cf(State));
parse_AttType_E({<<"N">>, State}) ->
    parse_AttType_EN(cf(State));
?FUNCTION_NAME({<<>>, State}) ->
    ?FUNCTION_NAME(cf(State)).

parse_AttType_EN({<<"TITIES", Rest/bitstring>> = Stream, State}) ->
    {entities, Rest, Stream, 6, State};
parse_AttType_EN({<<"TITY", Rest/bitstring>> = Stream, State}) ->
    {entity, Rest, Stream, 4, State};
parse_AttType_EN({<<"TITIE">>, State}) ->
    parse_AttType_ENTITIE(cf(State));
parse_AttType_EN({<<"TITI">>, State}) ->
    parse_AttType_ENTITI(cf(State));
parse_AttType_EN({<<"TIT">>, State}) ->
    parse_AttType_ENTIT(cf(State));
parse_AttType_EN({<<"TI">>, State}) ->
    parse_AttType_ENTI(cf(State));
parse_AttType_EN({<<"T">>, State}) ->
    parse_AttType_ENT(cf(State));
?FUNCTION_NAME({<<>>, State}) ->
    ?FUNCTION_NAME(cf(State)).

parse_AttType_ENT({<<"ITIES", Rest/bitstring>> = Stream, State}) ->
    {entities, Rest, Stream, 5, State};
parse_AttType_ENT({<<"ITY", Rest/bitstring>> = Stream, State}) ->
    {entity, Rest, Stream, 3, State};
parse_AttType_ENT({<<"ITIE">>, State}) ->
    parse_AttType_ENTITIE(cf(State));
parse_AttType_ENT({<<"ITI">>, State}) ->
    parse_AttType_ENTITI(cf(State));
parse_AttType_ENT({<<"IT">>, State}) ->
    parse_AttType_ENTIT(cf(State));
parse_AttType_ENT({<<"I">>, State}) ->
    parse_AttType_ENTI(cf(State));
?FUNCTION_NAME({<<>>, State}) ->
    ?FUNCTION_NAME(cf(State)).

parse_AttType_ENTI({<<"TIES", Rest/bitstring>> = Stream, State}) ->
    {entities, Rest, Stream, 4, State};
parse_AttType_ENTI({<<"TY", Rest/bitstring>> = Stream, State}) ->
    {entity, Rest, Stream, 2, State};
parse_AttType_ENTI({<<"TIE">>, State}) ->
    parse_AttType_ENTITIE(cf(State));
parse_AttType_ENTI({<<"TI">>, State}) ->
    parse_AttType_ENTITI(cf(State));
parse_AttType_ENTI({<<"T">>, State}) ->
    parse_AttType_ENTIT(cf(State));
?FUNCTION_NAME({<<>>, State}) ->
    ?FUNCTION_NAME(cf(State)).

parse_AttType_ENTIT({<<"IES", Rest/bitstring>> = Stream, State}) ->
    {entities, Rest, Stream, 3, State};
parse_AttType_ENTIT({<<"Y", Rest/bitstring>> = Stream, State}) ->
    {entity, Rest, Stream, 1, State};
parse_AttType_ENTIT({<<"IE">>, State}) ->
    parse_AttType_ENTITIE(cf(State));
parse_AttType_ENTIT({<<"I">>, State}) ->
    parse_AttType_ENTITI(cf(State));
?FUNCTION_NAME({<<>>, State}) ->
    ?FUNCTION_NAME(cf(State)).

parse_AttType_ENTITI({<<"ES", Rest/bitstring>> = Stream, State}) ->
    {entities, Rest, Stream, 2, State};
parse_AttType_ENTITI({<<"E">>, State}) ->
    parse_AttType_ENTITIE(cf(State));
?FUNCTION_NAME({<<>>, State}) ->
    ?FUNCTION_NAME(cf(State)).

parse_AttType_ENTITIE({<<"S", Rest/bitstring>> = Stream, State}) ->
    {entities, Rest, Stream, 1, State};
?FUNCTION_NAME({<<>>, State}) ->
    ?FUNCTION_NAME(cf(State)).

parse_AttType_I({<<"DREFS", Rest/bitstring>> = Stream, State}) ->
    {idrefs, Rest, Stream, 5, State};
parse_AttType_I({<<"DREF">>, State}) ->
    parse_AttType_IDREF(cf(State));
parse_AttType_I({<<"DREF", Rest/bitstring>> = Stream, State}) ->
    {idref, Rest, Stream, 4, State};
parse_AttType_I({<<"DRE">>, State}) ->
    parse_AttType_IDRE(cf(State));
parse_AttType_I({<<"DR">>, State}) ->
    parse_AttType_IDR(cf(State));
parse_AttType_I({<<"D">>, State}) ->
    parse_AttType_ID(cf(State));
parse_AttType_I({<<"D", Bytes/bitstring>> = Stream, State}) ->
    {id, Bytes, Stream, 1, State};
?FUNCTION_NAME({<<>>, State}) ->
    ?FUNCTION_NAME(cf(State)).

parse_AttType_ID({<<"REFS", Rest/bitstring>> = Stream, State}) ->
    {idrefs, Rest, Stream, 4, State};
parse_AttType_ID({<<"REF">>, State}) ->
    parse_AttType_IDREF(cf(State));
parse_AttType_ID({<<"REF", Rest/bitstring>> = Stream, State}) ->
    {idref, Rest, Stream, 3, State};
parse_AttType_ID({<<"RE">>, State}) ->
    parse_AttType_IDRE(cf(State));
parse_AttType_ID({<<"R">>, State}) ->
    parse_AttType_IDR(cf(State));
?FUNCTION_NAME({<<>>, State}) ->
    ?FUNCTION_NAME(cf(State));
parse_AttType_ID({Bytes, State}) ->
    {id, Bytes, Bytes, 0, State}.

parse_AttType_IDR({<<"EFS", Rest/bitstring>> = Stream, State}) ->
    {idrefs, Rest, Stream, 3, State};
parse_AttType_IDR({<<"EF">>, State}) ->
    parse_AttType_IDREF(cf(State));
parse_AttType_IDR({<<"EF", Rest/bitstring>> = Stream, State}) ->
    {idref, Rest, Stream, 2, State};
parse_AttType_IDR({<<"E">>, State}) ->
    parse_AttType_IDRE(cf(State));
?FUNCTION_NAME({<<>>, State}) ->
    ?FUNCTION_NAME(cf(State)).

parse_AttType_IDRE({<<"FS", Rest/bitstring>> = Stream, State}) ->
    {idrefs, Rest, Stream, 2, State};
parse_AttType_IDRE({<<"F">>, State}) ->
    parse_AttType_IDREF(cf(State));
parse_AttType_IDRE({<<"F", Rest/bitstring>> = Stream, State}) ->
    {idref, Rest, Stream, 1, State};
?FUNCTION_NAME({<<>>, State}) ->
    ?FUNCTION_NAME(cf(State)).

parse_AttType_IDREF({<<"S", Rest/bitstring>> = Stream, State}) ->
    {idrefs, Rest, Stream, 1, State};
?FUNCTION_NAME({<<>>, State}) ->
    ?FUNCTION_NAME(cf(State));
parse_AttType_IDREF({Stream, State}) ->
    {idref, Stream, Stream, 0, State}.

parse_DefaultDecl_h({<<"REQUIRED", Rest/bitstring>> = Stream, State}, _) ->
    parse_DefaultDecl_hREQUIRED(Rest, Stream, 8, State);
parse_DefaultDecl_h({<<"REQUIRE">>, State}, _) ->
    parse_DefaultDecl_hREQUIRE(cf(State));
parse_DefaultDecl_h({<<"REQUIR">>, State}, _) ->
    parse_DefaultDecl_hREQUIR(cf(State));
parse_DefaultDecl_h({<<"REQUI">>, State}, _) ->
    parse_DefaultDecl_hREQUI(cf(State));
parse_DefaultDecl_h({<<"REQU">>, State}, _) ->
    parse_DefaultDecl_hREQU(cf(State));
parse_DefaultDecl_h({<<"REQ">>, State}, _) ->
    parse_DefaultDecl_hREQ(cf(State));
parse_DefaultDecl_h({<<"RE">>, State}, _) ->
    parse_DefaultDecl_hRE(cf(State));
parse_DefaultDecl_h({<<"R">>, State}, _) ->
    parse_DefaultDecl_hR(cf(State));
parse_DefaultDecl_h({<<"IMPLIED", Rest/bitstring>> = Stream, State}, _) ->
    parse_DefaultDecl_hIMPLIED(Rest, Stream, 7, State);
parse_DefaultDecl_h({<<"IMPLIE">>, State}, _) ->
    parse_DefaultDecl_hIMPLIE(cf(State));
parse_DefaultDecl_h({<<"IMPLI">>, State}, _) ->
    parse_DefaultDecl_hIMPLI(cf(State));
parse_DefaultDecl_h({<<"IMPL">>, State}, _) ->
    parse_DefaultDecl_hIMPL(cf(State));
parse_DefaultDecl_h({<<"IMP">>, State}, _) ->
    parse_DefaultDecl_hIMP(cf(State));
parse_DefaultDecl_h({<<"IM">>, State}, _) ->
    parse_DefaultDecl_hIM(cf(State));
parse_DefaultDecl_h({<<"I">>, State}, _) ->
    parse_DefaultDecl_hI(cf(State));
parse_DefaultDecl_h({<<"FIXED", Rest/bitstring>> = Stream, State}, DTD) ->
    parse_DefaultDecl_hFIXED(Rest, Stream, 5, State, DTD, false);
parse_DefaultDecl_h({<<"FIXE">>, State}, DTD) ->
    parse_DefaultDecl_hFIXE(cf(State), DTD);
parse_DefaultDecl_h({<<"FIX">>, State}, DTD) ->
    parse_DefaultDecl_hFIX(cf(State), DTD);
parse_DefaultDecl_h({<<"FI">>, State}, DTD) ->
    parse_DefaultDecl_hFI(cf(State), DTD);
parse_DefaultDecl_h({<<"F">>, State}, DTD) ->
    parse_DefaultDecl_hF(cf(State), DTD);
?FUNCTION_NAME({<<>>, State}, DTD) ->
    ?FUNCTION_NAME(cf(State), DTD).

parse_DefaultDecl_hR({<<"EQUIRED", Rest/bitstring>> = Stream, State}) ->
    parse_DefaultDecl_hREQUIRED(Rest, Stream, 7, State);
parse_DefaultDecl_hR({<<"EQUIRE">>, State}) ->
    parse_DefaultDecl_hREQUIRE(cf(State));
parse_DefaultDecl_hR({<<"EQUIR">>, State}) ->
    parse_DefaultDecl_hREQUIR(cf(State));
parse_DefaultDecl_hR({<<"EQUI">>, State}) ->
    parse_DefaultDecl_hREQUI(cf(State));
parse_DefaultDecl_hR({<<"EQU">>, State}) ->
    parse_DefaultDecl_hREQU(cf(State));
parse_DefaultDecl_hR({<<"EQ">>, State}) ->
    parse_DefaultDecl_hREQ(cf(State));
parse_DefaultDecl_hR({<<"E">>, State}) ->
    parse_DefaultDecl_hRE(cf(State));
?FUNCTION_NAME({<<>>, State}) ->
    ?FUNCTION_NAME(cf(State)).

parse_DefaultDecl_hRE({<<"QUIRED", Rest/bitstring>> = Stream, State}) ->
    parse_DefaultDecl_hREQUIRED(Rest, Stream, 6, State);
parse_DefaultDecl_hRE({<<"QUIRE">>, State}) ->
    parse_DefaultDecl_hREQUIRE(cf(State));
parse_DefaultDecl_hRE({<<"QUIR">>, State}) ->
    parse_DefaultDecl_hREQUIR(cf(State));
parse_DefaultDecl_hRE({<<"QUI">>, State}) ->
    parse_DefaultDecl_hREQUI(cf(State));
parse_DefaultDecl_hRE({<<"QU">>, State}) ->
    parse_DefaultDecl_hREQU(cf(State));
parse_DefaultDecl_hRE({<<"Q">>, State}) ->
    parse_DefaultDecl_hREQ(cf(State));
?FUNCTION_NAME({<<>>, State}) ->
    ?FUNCTION_NAME(cf(State)).

parse_DefaultDecl_hREQ({<<"UIRED", Rest/bitstring>> = Stream, State}) ->
    parse_DefaultDecl_hREQUIRED(Rest, Stream, 5, State);
parse_DefaultDecl_hREQ({<<"UIRE">>, State}) ->
    parse_DefaultDecl_hREQUIRE(cf(State));
parse_DefaultDecl_hREQ({<<"UIR">>, State}) ->
    parse_DefaultDecl_hREQUIR(cf(State));
parse_DefaultDecl_hREQ({<<"UI">>, State}) ->
    parse_DefaultDecl_hREQUI(cf(State));
parse_DefaultDecl_hREQ({<<"U">>, State}) ->
    parse_DefaultDecl_hREQU(cf(State));
?FUNCTION_NAME({<<>>, State}) ->
    ?FUNCTION_NAME(cf(State)).

parse_DefaultDecl_hREQU({<<"IRED", Rest/bitstring>> = Stream, State}) ->
    parse_DefaultDecl_hREQUIRED(Rest, Stream, 4, State);
parse_DefaultDecl_hREQU({<<"IRE">>, State}) ->
    parse_DefaultDecl_hREQUIRE(cf(State));
parse_DefaultDecl_hREQU({<<"IR">>, State}) ->
    parse_DefaultDecl_hREQUIR(cf(State));
parse_DefaultDecl_hREQU({<<"I">>, State}) ->
    parse_DefaultDecl_hREQUI(cf(State));
?FUNCTION_NAME({<<>>, State}) ->
    ?FUNCTION_NAME(cf(State)).

parse_DefaultDecl_hREQUI({<<"RED", Rest/bitstring>> = Stream, State}) ->
    parse_DefaultDecl_hREQUIRED(Rest, Stream, 3, State);
parse_DefaultDecl_hREQUI({<<"RE">>, State}) ->
    parse_DefaultDecl_hREQUIRE(cf(State));
parse_DefaultDecl_hREQUI({<<"R">>, State}) ->
    parse_DefaultDecl_hREQUIR(cf(State));
?FUNCTION_NAME({<<>>, State}) ->
    ?FUNCTION_NAME(cf(State)).

parse_DefaultDecl_hREQUIR({<<"ED", Rest/bitstring>> = Stream, State}) ->
    parse_DefaultDecl_hREQUIRED(Rest, Stream, 2, State);
parse_DefaultDecl_hREQUIR({<<"E">>, State}) ->
    parse_DefaultDecl_hREQUIRE(cf(State));
?FUNCTION_NAME({<<>>, State}) ->
    ?FUNCTION_NAME(cf(State)).

parse_DefaultDecl_hREQUIRE({<<"D", Rest/bitstring>> = Stream, State}) ->
    parse_DefaultDecl_hREQUIRED(Rest, Stream, 1, State);
?FUNCTION_NAME({<<>>, State}) ->
    ?FUNCTION_NAME(cf(State)).

parse_DefaultDecl_hI({<<"MPLIED", Rest/bitstring>> = Stream, State}) ->
    parse_DefaultDecl_hIMPLIED(Rest, Stream, 6, State);
parse_DefaultDecl_hI({<<"MPLIE">>, State}) ->
    parse_DefaultDecl_hIMPLIE(cf(State));
parse_DefaultDecl_hI({<<"MPLI">>, State}) ->
    parse_DefaultDecl_hIMPLI(cf(State));
parse_DefaultDecl_hI({<<"MPL">>, State}) ->
    parse_DefaultDecl_hIMPL(cf(State));
parse_DefaultDecl_hI({<<"MP">>, State}) ->
    parse_DefaultDecl_hIMP(cf(State));
parse_DefaultDecl_hI({<<"M">>, State}) ->
    parse_DefaultDecl_hIM(cf(State));
?FUNCTION_NAME({<<>>, State}) ->
    ?FUNCTION_NAME(cf(State)).

parse_DefaultDecl_hIM({<<"PLIED", Rest/bitstring>> = Stream, State}) ->
    parse_DefaultDecl_hIMPLIED(Rest, Stream, 5, State);
parse_DefaultDecl_hIM({<<"PLIE">>, State}) ->
    parse_DefaultDecl_hIMPLIE(cf(State));
parse_DefaultDecl_hIM({<<"PLI">>, State}) ->
    parse_DefaultDecl_hIMPLI(cf(State));
parse_DefaultDecl_hIM({<<"PL">>, State}) ->
    parse_DefaultDecl_hIMPL(cf(State));
parse_DefaultDecl_hIM({<<"P">>, State}) ->
    parse_DefaultDecl_hIMP(cf(State));
?FUNCTION_NAME({<<>>, State}) ->
    ?FUNCTION_NAME(cf(State)).

parse_DefaultDecl_hIMP({<<"LIED", Rest/bitstring>> = Stream, State}) ->
    parse_DefaultDecl_hIMPLIED(Rest, Stream, 4, State);
parse_DefaultDecl_hIMP({<<"LIE">>, State}) ->
    parse_DefaultDecl_hIMPLIE(cf(State));
parse_DefaultDecl_hIMP({<<"LI">>, State}) ->
    parse_DefaultDecl_hIMPLI(cf(State));
parse_DefaultDecl_hIMP({<<"L">>, State}) ->
    parse_DefaultDecl_hIMPL(cf(State));
?FUNCTION_NAME({<<>>, State}) ->
    ?FUNCTION_NAME(cf(State)).

parse_DefaultDecl_hIMPL({<<"IED", Rest/bitstring>> = Stream, State}) ->
    parse_DefaultDecl_hIMPLIED(Rest, Stream, 3, State);
parse_DefaultDecl_hIMPL({<<"IE">>, State}) ->
    parse_DefaultDecl_hIMPLIE(cf(State));
parse_DefaultDecl_hIMPL({<<"I">>, State}) ->
    parse_DefaultDecl_hIMPLI(cf(State));
?FUNCTION_NAME({<<>>, State}) ->
    ?FUNCTION_NAME(cf(State)).

parse_DefaultDecl_hIMPLI({<<"ED", Rest/bitstring>> = Stream, State}) ->
    parse_DefaultDecl_hIMPLIED(Rest, Stream, 2, State);
parse_DefaultDecl_hIMPLI({<<"E">>, State}) ->
    parse_DefaultDecl_hIMPLIE(cf(State));
?FUNCTION_NAME({<<>>, State}) ->
    ?FUNCTION_NAME(cf(State)).

parse_DefaultDecl_hIMPLIE({<<"D", Rest/bitstring>> = Stream, State}) ->
    parse_DefaultDecl_hIMPLIED(Rest, Stream, 1, State);
?FUNCTION_NAME({<<>>, State}) ->
    ?FUNCTION_NAME(cf(State)).

parse_DefaultDecl_hF({<<"IXED", Rest/bitstring>> = Stream, State}, DTD) ->
    parse_DefaultDecl_hFIXED(Rest, Stream, 4, State, DTD, false);
parse_DefaultDecl_hF({<<"IXE">>, State}, DTD) ->
    parse_DefaultDecl_hFIXE(cf(State), DTD);
parse_DefaultDecl_hF({<<"IX">>, State}, DTD) ->
    parse_DefaultDecl_hFIX(cf(State), DTD);
parse_DefaultDecl_hF({<<"I">>, State}, DTD) ->
    parse_DefaultDecl_hFI(cf(State), DTD);
?FUNCTION_NAME({<<>>, State}, DTD) ->
    ?FUNCTION_NAME(cf(State), DTD).

parse_DefaultDecl_hFI({<<"XED", Rest/bitstring>> = Stream, State}, DTD) ->
    parse_DefaultDecl_hFIXED(Rest, Stream, 3, State, DTD, false);
parse_DefaultDecl_hFI({<<"XE">>, State}, DTD) ->
    parse_DefaultDecl_hFIXE(cf(State), DTD);
parse_DefaultDecl_hFI({<<"X">>, State}, DTD) ->
    parse_DefaultDecl_hFIX(cf(State), DTD);
?FUNCTION_NAME({<<>>, State}, DTD) ->
    ?FUNCTION_NAME(cf(State), DTD).

parse_DefaultDecl_hFIX({<<"ED", Rest/bitstring>> = Stream, State}, DTD) ->
    parse_DefaultDecl_hFIXED(Rest, Stream, 2, State, DTD, false);
parse_DefaultDecl_hFIX({<<"E">>, State}, DTD) ->
    parse_DefaultDecl_hFIXE(cf(State), DTD);
?FUNCTION_NAME({<<>>, State}, DTD) ->
    ?FUNCTION_NAME(cf(State), DTD).

parse_DefaultDecl_hFIXE({<<"D", Rest/bitstring>> = Stream, State}, DTD) ->
    parse_DefaultDecl_hFIXED(Rest, Stream, 1, State, DTD, false);
?FUNCTION_NAME({<<>>, State}, DTD) ->
    ?FUNCTION_NAME(cf(State), DTD).

add_element_to_dtd({Name, Elem}, #{elems := Elems} = DTD) ->
    DTD#{elems := Elems#{Name => Elem}}.

add_attlist_to_dtd({ElemName, AttDefs}, #{atts := Atts} = DTD) ->
    case Atts of
        #{ElemName := OldAttDefs} ->
            Merged = maps:merge(AttDefs, OldAttDefs),
            DTD#{atts := Atts#{ElemName => Merged}};
        _ ->
            DTD#{atts := Atts#{ElemName => AttDefs}}
    end.

add_notation_to_dtd({Name, Pub, Sys}, #{nots := Nots} = DTD) ->
    DTD#{nots := Nots#{Name => {Pub, Sys}}}.

%{Name, {general|parameter, internal|external|unparsed, Value}}
add_entity_to_dtd({Name, {general, Type, Value}}, #{gen_ents := Ents} = DTD) ->
    case is_map_key(Name, Ents) of
        true -> DTD;
        false -> DTD#{gen_ents := Ents#{Name => {Type, Value}}}
    end;
add_entity_to_dtd({Name, {parameter, Type, Value}}, #{par_ents := Ents} = DTD) ->
    case is_map_key(Name, Ents) of
        true -> DTD;
        false -> DTD#{par_ents := Ents#{Name => {Type, Value}}}
    end.

add_pi_comment_to_dtd(CommentPi, #{pi_comments := CmPis} = DTD) ->
    DTD#{pi_comments := [CommentPi | CmPis]}.

set_dtd_name(DTD, Name) -> DTD#{name := Name}.

set_dtd_external_id(DTD, ExternalId) -> DTD#{external_id := ExternalId}.

resolve_parameter_entity(
    PERef,
    #{par_ents := Params} = DTD,
    #ys_state{external = ExtResolver, base = Base, standalone = Standalone} = State,
    External,
    AsText
) ->
    % Replacement text in DTDs needs leading and trailing spaces added to ensure
    % proper nesting.
    case Params of
        #{PERef := {external, _PubSys}} when Standalone == true ->
            fatal_error(external_entity_in_standalone, State);
        #{PERef := {external, _PubSys}} when ExtResolver == undefined ->
            {replacement_text, <<>>};
        #{PERef := {external, PubSys}} when AsText == true ->
            {NewBase, Stream} = ExtResolver(PubSys, Base),
            {_, Bytes1, _Stream1, _Pos1, _State1} = parse_TextDecl(Stream, State#ys_state{
                continuation = undefined, base = NewBase
            }),
            Stream2 =
                if
                    External -> <<" ", Bytes1/bitstring, " ">>;
                    true -> Bytes1
                end,
            {replacement_text, Stream2};
        #{PERef := {external, PubSys}} ->
            {NewBase, Stream} = ExtResolver(PubSys, Base),
            {_, Bytes1, _Stream1, _Pos1, State1} = parse_TextDecl(Stream, State#ys_state{
                continuation = undefined, base = NewBase
            }),
            Stream2 =
                if
                    External -> <<" ", Bytes1/bitstring, " ">>;
                    true -> Bytes1
                end,
            {DTD1, _, _, _, _State3} = parse_extSubsetDecl(Stream2, Stream2, 0, State1, DTD),
            {dtd, DTD1};
        #{PERef := {internal, Value}} when External ->
            {replacement_text, <<" ", Value/bitstring, " ">>};
        #{PERef := {internal, Value}} ->
            {replacement_text, Value};
        _ ->
            fatal_error(unknown_parameter_entity, PERef)
    end.

resolve_general_entity(<<"amp">>, _, _) ->
    {gen, <<"&">>};
resolve_general_entity(<<"lt">>, _, _) ->
    {gen, <<"<">>};
resolve_general_entity(<<"gt">>, _, _) ->
    {gen, <<">">>};
resolve_general_entity(<<"apos">>, _, _) ->
    {gen, <<"'">>};
resolve_general_entity(<<"quot">>, _, _) ->
    {gen, <<"\"">>};
resolve_general_entity(Name, #ys_state{dtd = undefined}, _) ->
    {Name, <<$&, Name/binary, $;>>};
resolve_general_entity(
    Name, #ys_state{external = ExtResolver, dtd = #{gen_ents := Ents}} = State, Type
) ->
    case Ents of
        #{Name := {internal, {_, _}}} ->
            {Name, <<>>};
        #{Name := {internal, Value}} when Type == attribute ->
            ExpValue = expand_att_char_refs(Value, Value, 0, 0, State, []),
            {Name, {replacement_text, ExpValue}};
        #{Name := {internal, Value}} ->
            ExpValue = expand_char_refs(Value, Value, 0, 0, State, []),
            {Name, {replacement_text, ExpValue}};
        #{Name := {external, {_, _, _}}} when Type =/= entity ->
            fatal_error(unparsed_entity_outside_entity, Name);
        #{Name := {external, _}} when Type == attribute ->
            fatal_error(external_entity_in_attribute, Name);
        #{Name := {external, {Pub, Sys, _Name}}} when Type == entity ->
            ExtResolver = State#ys_state.external,
            {NewBase, Stream} = ExtResolver({Pub, Sys}, State#ys_state.base),
            {_, Bytes1, _, _, _} = parse_TextDecl(Stream, State#ys_state{base = NewBase}),
            {Name, {replacement_text, Bytes1}};
        #{Name := {external, {Pub, Sys}}} when
            Type == content andalso ExtResolver =/= undefined;
            Type == text andalso ExtResolver =/= undefined;
            Type == entity andalso ExtResolver =/= undefined
        ->
            ExtResolver = State#ys_state.external,
            {NewBase, Stream} = ExtResolver({Pub, Sys}, State#ys_state.base),
            {_, Bytes1, _, _, _} = parse_TextDecl(Stream, State#ys_state{base = NewBase}),
            {Name, {replacement_text, normalize_eol_characters(Bytes1)}};
        %% TODO: resolve
        _ when is_map_key(Name, Ents) ->
            {Name, <<>>};
        _ when Type == entity ->
            {Name, <<$&, Name/binary, $;>>};
        _ when Type == attribute; Type == text; Type == content ->
            #ys_state{dtd = #{par_ents := PEnts}} = State,
            case maps:size(PEnts) of
                0 ->
                    fatal_error(unknown_entity_in_entity, Name);
                _ ->
                    % Validity error
                    {gen, <<$&, Name/binary, $;>>}
            end;
        _ ->
            {gen, <<$&, Name/binary, $;>>}
    end.

expand_char_refs(<<$\r, $\n, Rest/bitstring>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    Acc2 = ?APPEND(<<$\n>>, Acc1),
    ?FUNCTION_NAME(Rest, Stream, Pos + Len + 2, 0, State, Acc2);
?FUNCTION_NAME(<<$\r>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    Acc2 = ?APPEND(<<$\n>>, Acc1),
    case cf(State) of
        {<<$\n, Bytes1/bitstring>> = Stream1, State1} ->
            ?FUNCTION_NAME(Bytes1, Stream1, 1, 0, State1, Acc2);
        {Stream1, State1} ->
            ?FUNCTION_NAME(Stream1, Stream1, 0, 0, State1, Acc2)
    end;
expand_char_refs(<<$\r, Rest/bitstring>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    Acc2 = ?APPEND(<<$\n>>, Acc1),
    ?FUNCTION_NAME(Rest, Stream, Pos + Len + 1, 0, State, Acc2);
expand_char_refs(<<$&, $#, $x, Rest/bitstring>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    {{hex, Ref}, ?MATCH1} = parse_Reference_hex(Rest, Stream, Pos + Len + 3, 0, State, []),
    Acc2 =
        case Ref of
            <<16#D>> -> ?APPEND(<<$&, $#, $x, $D, $;>>, Acc1);
            _ -> ?APPEND(Ref, Acc1)
        end,
    ?FUNCTION_NAME(Bytes1, Stream1, Pos1, 0, State1, Acc2);
expand_char_refs(<<$&, $#, Rest/bitstring>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    {{dec, Ref}, ?MATCH1} = parse_Reference_dec(Rest, Stream, Pos + Len + 2, 0, State, []),
    Acc2 =
        case Ref of
            <<16#D>> -> ?APPEND(<<$&, $#, $1, $3, $;>>, Acc1);
            _ -> ?APPEND(Ref, Acc1)
        end,
    ?FUNCTION_NAME(Bytes1, Stream1, Pos1, 0, State1, Acc2);
expand_char_refs(<<>>, Stream, Pos, Len, _State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    to_binary(Acc1);
?ONECHAR.

expand_att_char_refs(<<$\r, $\n, Rest/bitstring>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    Acc2 = ?APPEND(<<$\s>>, Acc1),
    ?FUNCTION_NAME(Rest, Stream, Pos + Len + 2, 0, State, Acc2);
expand_att_char_refs(<<$\t, Rest/bitstring>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    Acc2 = ?APPEND(<<$\s>>, Acc1),
    ?FUNCTION_NAME(Rest, Stream, Pos + Len + 1, 0, State, Acc2);
expand_att_char_refs(<<$\n, Rest/bitstring>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    Acc2 = ?APPEND(<<$\s>>, Acc1),
    ?FUNCTION_NAME(Rest, Stream, Pos + Len + 1, 0, State, Acc2);
expand_att_char_refs(<<$\r, Rest/bitstring>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    Acc2 = ?APPEND(<<$\s>>, Acc1),
    ?FUNCTION_NAME(Rest, Stream, Pos + Len + 1, 0, State, Acc2);
expand_att_char_refs(<<$&, $#, $x, Rest/bitstring>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    {{hex, Ref}, ?MATCH1} = parse_Reference_hex(Rest, Stream, Pos + Len + 3, 0, State, []),
    Acc2 = ?APPEND(Ref, Acc1),
    ?FUNCTION_NAME(Bytes1, Stream1, Pos1, 0, State1, Acc2);
expand_att_char_refs(<<$&, $#, Rest/bitstring>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    {{dec, Ref}, ?MATCH1} = parse_Reference_dec(Rest, Stream, Pos + Len + 2, 0, State, []),
    Acc2 = ?APPEND(Ref, Acc1),
    ?FUNCTION_NAME(Bytes1, Stream1, Pos1, 0, State1, Acc2);
expand_att_char_refs(<<>>, Stream, Pos, Len, _State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    to_binary(Acc1);
?ONECHAR.

empty_proc_dtd() ->
    #{
        % Root element name
        name => undefined,
        % External DTD IDs
        external_id => undefined,
        % Element declarations
        elems => #{},
        % Attribute List declarations
        atts => #{},
        % Notation declarations
        nots => #{},
        % Entities used in the document content
        gen_ents => #{},
        % Entities used in the DTD
        par_ents => #{},
        % Stack of named references
        ref_stack => [],
        % Processing Instructions and comments from the DTD
        pi_comments => []
    }.

replacement_text_state(Bytes, #ys_state{dtd = #{ref_stack := RefStack} = DTD} = State, RefName) ->
    case
        lists:any(
            fun
                (A) when is_atom(A) -> false;
                (I) -> I == RefName
            end,
            RefStack
        )
    of
        true ->
            fatal_error(recursive_reference, {RefName, State});
        false ->
            DTD1 = DTD#{ref_stack := [RefName | RefStack]},
            State#ys_state{
                in_replacement = true,
                continuation = {fun(_) -> {Bytes, State} end, replacement},
                dtd = DTD1
            }
    end.

replacement_text_state(Bytes, #ys_state{in_replacement = InRepl} = State) ->
    State#ys_state{
        in_replacement = true,
        continuation = {fun(_) -> {Bytes, State#ys_state{in_replacement = InRepl}} end, replacement}
    }.

to_binary(Bin) when is_binary(Bin) -> Bin;
to_binary(IoList) -> iolist_to_binary(IoList).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%
%%                                                                                                                   %%
%%                                         External DTD Stuff                                                        %%
%%                                           NOT STREAMING                                                           %%
%%                                                                                                                   %%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%

% [78]   extParsedEnt        ::=       TextDecl? content
% [30]   extSubset           ::=       TextDecl? extSubsetDecl
% [77]   TextDecl            ::=       '<?xml' VersionInfo? EncodingDecl S? '?>'
parse_TextDecl(Stream, State) -> parse_TextDecl(Stream, Stream, 0, State).

parse_TextDecl(<<"<?xml"/utf8, Rest/bitstring>>, Stream, Pos, State) ->
    parse_TextDecl_ltqxml(Rest, Stream, Pos + 5, State);
parse_TextDecl(?MATCH) ->
    % default declaration
    {{<<"1.0">>, <<"UTF-8">>}, ?MATCH}.

parse_TextDecl_ltqxml(?MATCH) ->
    {{Version, _}, ?MATCH1} = parse_VersionInfo(?MATCH),
    {_, ?MATCH2} = maybe_consume_s(?MATCH1),
    {{Encoding, true}, ?MATCH3} = parse_EncodingDecl(?MATCH2),
    {_, ?MATCH4} = maybe_consume_s(?MATCH3),
    {?MATCH5} = parse_TextDecl_end(?MATCH4),
    {{Version, Encoding}, ?MATCH5}.

parse_TextDecl_end(<<"?>"/utf8, Rest/bitstring>>, Stream, Pos, State) ->
    {Rest, Stream, Pos + 2, State};
parse_TextDecl_end(Bytes, _, _, State) ->
    fatal_error(bad_textdecl, {Bytes, State}).

% [31]   extSubsetDecl       ::=       ( markupdecl | conditionalSect | DeclSep)*
% [61]   conditionalSect     ::=       includeSect | ignoreSect
% [62]   includeSect         ::=       '<![' S? 'INCLUDE' S? '[' extSubsetDecl ']]>'     [VC: Proper Conditional Section/PE Nesting]
% [63]   ignoreSect          ::=       '<![' S? 'IGNORE' S? '[' ignoreSectContents* ']]>'    [VC: Proper Conditional Section/PE Nesting]
% [64]   ignoreSectContents  ::=       Ignore ('<![' ignoreSectContents ']]>' Ignore)*
% [65]   Ignore              ::=       Char* - (Char* ('<![' | ']]>') Char*)
% [28a]  DeclSep             ::=       PEReference | S     [WFC: PE Between Declarations]
parse_ignoreSect(<<"]]>", Bytes/bitstring>>, Stream, Pos, State) ->
    {Bytes, Stream, Pos + 3, State};
parse_ignoreSect(<<"<![", Bytes/bitstring>>, Stream, Pos, State) ->
    {?MATCH1} = parse_ignoreSect(Bytes, Stream, Pos + 3, State),
    parse_ignoreSect(?MATCH1);
parse_ignoreSect(<<_/utf8, Bytes/bitstring>>, Stream, Pos, State) ->
    parse_ignoreSect(Bytes, Stream, Pos + 1, State);
parse_ignoreSect(<<>>, _, _, State) ->
    {Bytes1, State1} = cf(State),
    parse_ignoreSect(Bytes1, Bytes1, 0, State1).

parse_extSubsetDecl(<<"<![", Bytes/bitstring>>, Stream, Pos, State, DTD) ->
    {_, ?MATCH1} = maybe_consume_s(Bytes, Stream, Pos + 3, State),
    case Bytes1 of
        <<"IGNORE", Bytes2/bitstring>> ->
            {_, <<"[", Bytes3/bitstring>>, Stream3, Pos3, State3} = maybe_consume_s(
                Bytes2, Stream1, Pos1 + 6, State1
            ),
            {?MATCH4} = parse_ignoreSect(Bytes3, Stream3, Pos3 + 1, State3),
            parse_extSubsetDecl(?MATCH4, DTD);
        <<"INCLUDE", Bytes2/bitstring>> ->
            {_, <<"[", Bytes3/bitstring>>, Stream3, Pos3, State3} = maybe_consume_s(
                Bytes2, Stream1, Pos1 + 7, State1
            ),
            {DTD1, ?MATCH4} = parse_extSubsetDecl(Bytes3, Stream3, Pos3 + 1, State3, DTD),
            case Bytes4 of
                <<"]]>", Bytes5/bitstring>> ->
                    parse_extSubsetDecl(Bytes5, Stream4, Pos4 + 3, State4, DTD1);
                _ ->
                    fatal_error(unbalanced_include, State4)
            end;
        <<"%", Bytes2/bitstring>> ->
            {PERef, Bytes3, _Stream3, _Pos3, State3} = parse_PEReference(
                Bytes2, Stream1, Pos1 + 1, State1
            ),
            case resolve_parameter_entity(PERef, DTD, State1, true, true) of
                {replacement_text, ReplacementText} ->
                    RTState = replacement_text_state(Bytes3, State3),
                    ReplacementText1 = <<"<![", ReplacementText/bitstring>>,
                    parse_extSubsetDecl(ReplacementText1, ReplacementText1, 0, RTState, DTD)
            end;
        _ ->
            fatal_error(bad_dtd, State)
    end;
parse_extSubsetDecl(<<"<!ELEMENT", Bytes/bitstring>>, Stream, Pos, State, DTD) ->
    {DTD1, ?MATCH1} = parse_elementdecl(Bytes, Stream, Pos + 9, State, DTD, true),
    parse_extSubsetDecl(?MATCH1, DTD1);
parse_extSubsetDecl(<<"<!ATTLIST", Bytes/bitstring>>, Stream, Pos, State, DTD) ->
    {DTD1, ?MATCH1} = parse_AttlistDecl(Bytes, Stream, Pos + 9, State, DTD, true),
    parse_extSubsetDecl(?MATCH1, DTD1);
parse_extSubsetDecl(<<"<!ENTITY", Bytes/bitstring>>, Stream, Pos, State, DTD) ->
    {DTD1, ?MATCH1} = parse_EntityDecl(Bytes, Stream, Pos + 8, State, DTD, true),
    parse_extSubsetDecl(?MATCH1, DTD1);
parse_extSubsetDecl(<<"<!NOTATION", Bytes/bitstring>>, Stream, Pos, State, DTD) ->
    {DTD1, ?MATCH1} = parse_NotationDecl(Bytes, Stream, Pos + 10, State, DTD),
    parse_extSubsetDecl(?MATCH1, DTD1);
parse_extSubsetDecl(<<"%", Bytes/bitstring>>, Stream, Pos, State, DTD) ->
    {PERef, ?MATCH1} = parse_PEReference(Bytes, Stream, Pos + 1, State),
    case resolve_parameter_entity(PERef, DTD, State1, true, false) of
        {replacement_text, ReplacementText} ->
            RTState = replacement_text_state(Bytes1, State1),
            parse_extSubsetDecl(ReplacementText, ReplacementText, 0, RTState, DTD);
        {dtd, DTD1} ->
            parse_extSubsetDecl(?MATCH1, DTD1)
    end;
parse_extSubsetDecl(<<"]>", Bytes/bitstring>>, Stream, Pos, State, DTD) ->
    {DTD, Bytes, Stream, Pos + 2, State};
parse_extSubsetDecl(<<Char/utf8, Bytes/bitstring>>, Stream, Pos, State, DTD) when
    Char == 16#20; Char == 16#9; Char == 16#A; Char == 16#D
->
    {_, ?MATCH1} = maybe_consume_s(Bytes, Stream, Pos + 1, State),
    parse_extSubsetDecl(?MATCH1, DTD);
parse_extSubsetDecl(<<"<!--", Bytes/bitstring>>, Stream, Pos, State, DTD) ->
    {Comment, ?MATCH1} = parse_Comment(Bytes, Stream, Pos + 4, State),
    DTD1 = add_pi_comment_to_dtd(Comment, DTD),
    parse_extSubsetDecl(?MATCH1, DTD1);
parse_extSubsetDecl(<<"<?", Bytes/bitstring>>, Stream, Pos, State, DTD) ->
    {PI, ?MATCH1} = parse_PI(Bytes, Stream, Pos + 2, State),
    DTD1 = add_pi_comment_to_dtd(PI, DTD),
    parse_extSubsetDecl(?MATCH1, DTD1);
parse_extSubsetDecl(<<>>, Stream, Pos, State, DTD) ->
    {DTD, <<>>, Stream, Pos, State};
parse_extSubsetDecl(?MATCH, DTD) ->
    {DTD, ?MATCH}.

normalize_eol_characters(<<$\r, $\n, Rest/bitstring>>) ->
    <<$\n, (normalize_eol_characters(Rest))/bitstring>>;
normalize_eol_characters(<<$\r, Rest/bitstring>>) ->
    <<$\n, (normalize_eol_characters(Rest))/bitstring>>;
normalize_eol_characters(<<C, Rest/bitstring>>) ->
    <<C, (normalize_eol_characters(Rest))/bitstring>>;
normalize_eol_characters(<<>>) ->
    <<>>.

% Trim leading and trailing whitespace and shrink all consecutive whitespaces to a single space character.
% TODO: Trim trailing.
normalize_PubidLiteral(<<Char/utf8, Bytes/bitstring>>) when
    Char == 16#20; Char == 16#9; Char == 16#A; Char == 16#D
->
    normalize_PubidLiteral(Bytes);
normalize_PubidLiteral(<<>>) ->
    <<>>;
normalize_PubidLiteral(Bytes) when is_list(Bytes) ->
    normalize_PubidLiteral(iolist_to_binary(Bytes));
normalize_PubidLiteral(Bytes) ->
    normalize_PubidLiteral_1(Bytes).

normalize_PubidLiteral_1(<<Char1, Char2, Bytes/bitstring>>) when
    Char1 == 16#20 orelse Char1 == 16#9 orelse Char1 == 16#A orelse Char1 == 16#D,
    Char2 == 16#20 orelse Char2 == 16#9 orelse Char2 == 16#A orelse Char2 == 16#D
->
    normalize_PubidLiteral_1(<<" ", Bytes/bitstring>>);
normalize_PubidLiteral_1(<<Char1, Bytes/bitstring>>) when
    Char1 == 16#9 orelse Char1 == 16#A orelse Char1 == 16#D
->
    <<" ", (normalize_PubidLiteral_1(Bytes))/bitstring>>;
normalize_PubidLiteral_1(<<Char1, Bytes/bitstring>>) ->
    <<Char1, (normalize_PubidLiteral_1(Bytes))/bitstring>>;
normalize_PubidLiteral_1(<<>>) ->
    <<>>.
