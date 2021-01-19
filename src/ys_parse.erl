-module(ys_parse).

-export([
    % parse_doctypedecl/2,
    parse_content/4,
    parse_Misc/4,
    parse_element/4,
    parse_XMLDecl/4
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

-compile({inline, [append/2]}).

append(Thing, []) -> Thing;
append(Thing, Acc) -> [Acc, Thing].

cf(#ys_state{continuation = undefined} = State) ->
    {no_bytes, State};
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
            {<<Part/binary, RestBytes/binary>>, Bin, BytesNeeded, State#ys_state{
                continuation = {CF, CS1}
            }};
        {Bin, CS1} when is_binary(Bin), byte_size(Bin) >= 0 ->
            cf(
                <<Part/binary, Bin/binary>>,
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
            <<_:Pos1/binary, Rest/binary>> = Stream1,
            ?FUNCTION_NAME(Rest, Stream1, Pos1, 0, State1, Acc2)
    end
).

-define(NAMECHARPARTFUN(D),
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    {CodeChar, Stream1, Pos1, State1} = cf(Bytes, State, {partial, D}),
    case CodeChar of
        <<Char/utf8>> when
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
            <<_:Pos1/binary, Rest/binary>> = Stream1,
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
            <<_:Pos1/binary, Rest/binary>> = Stream1,
            Next(Rest, Stream1, Pos1, 0, State1, CodeChar);
        _ ->
            fatal_error({invalid_character, [CodeChar]}, State1)
    end
).

%%----------------------------------------------------------------------
%% XML character range
%% [2] Char ::= #x9 | #xA | #xD | [#x20-#xD7FF] | [#xE000-#xFFFD] |
%%              [#x10000-#x10FFFF]
%% any Unicode character, excluding the surrogate blocks, FFFE, and FFFF.
%%----------------------------------------------------------------------
-define(ONECHAR,
?FUNCTION_NAME(<<16#9/utf8, Rest/binary>>, Stream, Pos, Len, State, Acc) ->
    ?FUNCTION_NAME(Rest, Stream, Pos, Len + 1, State, Acc);
?FUNCTION_NAME(<<16#A/utf8, Rest/binary>>, Stream, Pos, Len, State, Acc) ->
    ?FUNCTION_NAME(Rest, Stream, Pos, Len + 1, State, Acc);
?FUNCTION_NAME(<<16#D/utf8, Rest/binary>>, Stream, Pos, Len, State, Acc) ->
    ?FUNCTION_NAME(Rest, Stream, Pos, Len + 1, State, Acc);
?FUNCTION_NAME(<<16#FFFE/utf8, _Rest/binary>>, _Stream, _Pos, _Len, State, _Acc) ->
    fatal_error({invalid_character, [16#FFFE]}, State) ;
?FUNCTION_NAME(<<16#FFFF/utf8, _Rest/binary>>, _Stream, _Pos, _Len, State, _Acc) ->
    fatal_error({invalid_character, [16#FFFF]}, State) ;
?FUNCTION_NAME(<<Char/utf8, Rest/binary>>, Stream, Pos, Len, State, Acc)  ->
    if
        Char < 16#20 ->
            fatal_error({invalid_character, [Char]}, State);
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
    {Acc1, Bytes, Stream, Pos + Len, State}
).

%%----------------------------------------------------------------------
%% [4a] NameChar
%%      NameStartChar | "-" | "." | [0-9] | #xB7 | [#x0300-#x036F] | [#x203F-#x2040]
%%----------------------------------------------------------------------
-define(ONENAMECHAR,
?FUNCTION_NAME(<<Char/utf8, Rest/binary>>, Stream, Pos, Len, State, Acc)
    when
    Char == $-;
    Char == $.;
    Char >= $0, Char =< $9;
    Char == $:;
    Char >= $A, Char =< $Z;
    Char == $_;
    Char >= $a, Char =< $z;
    Char == 16#B7
    ->
    ?FUNCTION_NAME(Rest, Stream, Pos, Len + 1, State, Acc);
?FUNCTION_NAME(<<Char/utf8, Rest/binary>>, Stream, Pos, Len, State, Acc)
    when
    Char >= 16#C0, Char =< 16#D6;
    Char >= 16#D8, Char =< 16#F6;
    Char >= 16#F8, Char =< 16#37D;
    Char >= 16#37F, Char =< 16#7FF
    ->
    ?FUNCTION_NAME(Rest, Stream, Pos, Len + 2, State, Acc);
?FUNCTION_NAME(<<Char/utf8, Rest/binary>>, Stream, Pos, Len, State, Acc)
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
?FUNCTION_NAME(<<Char/utf8, Rest/binary>>, Stream, Pos, Len, State, Acc)
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
    {Acc1, Bytes, Stream, Pos + Len, State}
).

%%----------------------------------------------------------------------
%% [13] PubidChar ::= #x20 | #xD | #xA | [a-zA-Z0-9] | [-'()+,./:=?;!*#@$_%]
%%----------------------------------------------------------------------
-define(ONEPUBIDCHAR,
?FUNCTION_NAME(<<Char/utf8, Rest/binary>>, Stream, Pos, Len, State, Acc)
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
?FUNCTION_NAME(_, _, _, _, State, _) ->
    fatal_error(invalid_pubid_character, State)
).

%%----------------------------------------------------------------------
%% Consume whitespace characters
%% params:  State
%% returns: {NewPos, NewState} | {error, non_whitespace}
%% [3] S ::= (#x20 | #x9 | #xD | #xA)+
%%----------------------------------------------------------------------
consume_s(<<Char/utf8, Rest/binary>>, Stream, Pos, State) when
    Char == 16#20; Char == 16#9; Char == 16#D; Char == 16#A
->
    maybe_consume_s(Rest, Stream, Pos + 1, State);
consume_s(<<>>, _Stream, _Pos, State) ->
    {Stream1, State1} = cf(State),
    consume_s(Stream1, Stream1, 0, State1);
consume_s(_, _Stream, _Pos, State) ->
    fatal_error(missing_whitespace, State).

maybe_consume_s(Bytes, Stream, Pos, State) -> maybe_consume_s(Bytes, Stream, Pos, State, false).

maybe_consume_s(<<Char/utf8, Rest/binary>>, Stream, Pos, State, _) when
    Char == 16#20; Char == 16#9; Char == 16#D; Char == 16#A
->
    maybe_consume_s(Rest, Stream, Pos + 1, State, true);
maybe_consume_s(<<>>, _Stream, _Pos, State, Found) ->
    {Stream1, State1} = cf(State),
    maybe_consume_s(Stream1, Stream1, 0, State1, Found);
maybe_consume_s(Part, Stream, Pos, State, Found) ->
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

parse_Name(<<Char/utf8, Rest/binary>>, Stream, Pos, State) ->
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
            fatal_error(bad_name, State)
    end;
parse_Name(<<>>, _Stream, _Pos, State) ->
    {Stream1, State1} = cf(State),
    parse_Name(Stream1, Stream1, 0, State1);
parse_Name(Bytes = <<30:5, _:3>>, _Stream, _Pos, State) ->
    ?FSTNAMECHARPARTFUN(3, parse_Name);
parse_Name(Bytes = <<30:5, _:3, 2:2, _:6>>, _Stream, _Pos, State) ->
    ?FSTNAMECHARPARTFUN(2, parse_Name);
parse_Name(Bytes = <<14:4, _:4>>, _Stream, _Pos, State) ->
    ?FSTNAMECHARPARTFUN(2, parse_Name);
parse_Name(Bytes = <<30:5, _:3, 2:2, _:6, 2:2, _:6>>, _Stream, _Pos, State) ->
    ?FSTNAMECHARPARTFUN(1, parse_Name);
parse_Name(Bytes = <<14:4, _:4, 2:2, _:6>>, _Stream, _Pos, State) ->
    ?FSTNAMECHARPARTFUN(1, parse_Name);
parse_Name(Bytes = <<6:3, _:5>>, _Stream, _Pos, State) ->
    ?FSTNAMECHARPARTFUN(1, parse_Name);
parse_Name(_, _, _, State) ->
    fatal_error(bad_name, State).

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
parse_Reference(<<>>, _Stream, _Pos, State) ->
    {Stream1, State1} = cf(State),
    parse_Reference(Stream1, Stream1, 0, State1);
parse_Reference(<<$#/utf8>>, _Stream, _Pos, State) ->
    {Stream1, State1} = cf(State),
    parse_Reference_lb(Stream1, Stream1, State1);
parse_Reference(<<$#/utf8, $x/utf8, Rest/binary>>, Stream, Pos, State) ->
    parse_Reference_hex(Rest, Stream, Pos + 2, 0, State, []);
parse_Reference(<<$#/utf8, Rest/binary>>, Stream, Pos, State) ->
    parse_Reference_dec(Rest, Stream, Pos + 1, 0, State, []);
parse_Reference(Bytes, Stream, Pos, State) ->
    parse_Reference_name(Bytes, Stream, Pos, State).

parse_Reference_lb(<<>>, _Stream, State) ->
    {Stream1, State1} = cf(State),
    parse_Reference_lb(Stream1, Stream1, State1);
parse_Reference_lb(<<$x/utf8, Rest/binary>>, Stream, State) ->
    parse_Reference_hex(Rest, Stream, 1, 0, State, []);
parse_Reference_lb(Bytes, Stream, State) ->
    parse_Reference_dec(Bytes, Stream, 0, 0, State, []).

% hex parse until ';' return char
parse_Reference_hex(<<>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    {Stream1, State1} = cf(State),
    parse_Reference_hex(Stream1, Stream1, 0, 0, State1, Acc1);
parse_Reference_hex(<<$;/utf8, Rest/binary>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    try
        Int = binary_to_integer(iolist_to_binary(Acc1), 16),
        {<<Int/utf8>>, Rest, Stream, Pos + Len + 1, State}
    catch
        _:_ ->
            fatal_error(bad_charref, State)
    end;
parse_Reference_hex(<<Char/utf8, Rest/binary>>, Stream, Pos, Len, State, Acc) when
    Char >= $0, Char =< $9; Char >= $a, Char =< $f; Char >= $A, Char =< $F
->
    parse_Reference_hex(Rest, Stream, Pos, Len + 1, State, Acc);
parse_Reference_hex(_, _, _, _, State, _) ->
    fatal_error(bad_charref, State).

% decimal parse until ';' return char
parse_Reference_dec(<<>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    {Stream1, State1} = cf(State),
    parse_Reference_dec(Stream1, Stream1, 0, 0, State1, Acc1);
parse_Reference_dec(<<$;/utf8, Rest/binary>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    try
        Int = binary_to_integer(iolist_to_binary(Acc1)),
        {<<Int/utf8>>, Rest, Stream, Pos + Len + 1, State}
    catch
        _:_ ->
            fatal_error(bad_charref, State)
    end;
parse_Reference_dec(<<Char/utf8, Rest/binary>>, Stream, Pos, Len, State, Acc) when
    Char >= $0, Char =< $9
->
    parse_Reference_dec(Rest, Stream, Pos, Len + 1, State, Acc);
parse_Reference_dec(_, _, _, _, State, _) ->
    fatal_error(bad_charref, State).

% Name parse until ';' return Name
parse_Reference_name(Bytes, Stream, Pos, State) ->
    case parse_Name(Bytes, Stream, Pos, State) of
        {Name, <<$;/utf8, Bytes1/binary>>, Stream1, Pos1, State1} ->
            {Name, Bytes1, Stream1, Pos1 + 1, State1};
        {_, _, _, _, State1} ->
            fatal_error(bad_charref, State1)
    end.

%%----------------------------------------------------------------------
%% Parse a Comment, leading '<!--' already removed
%% params:  State
%% returns: {Comment, NewState} | NewState (when ignoring)
%% [15] Comment ::= '<!--' ((Char - '-') | ('-' (Char - '-')))* '-->'
%%----------------------------------------------------------------------
parse_Comment(Bytes, Stream, Pos, State) ->
    parse_Comment(Bytes, Stream, Pos, 0, State, []).

parse_Comment(<<>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    {Stream1, State1} = cf(State),
    parse_Comment(Stream1, Stream1, 0, 0, State1, Acc1);
parse_Comment(<<$-/utf8, $-/utf8, $>/utf8, Rest/binary>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    {Acc1, Rest, Stream, Pos + 3, State};
parse_Comment(<<$-/utf8, $-/utf8>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    {Stream1, State1} = cf(State),
    parse_Comment_dashdash(Stream1, Stream1, State1, Acc1);
parse_Comment(<<$-/utf8>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    {Stream1, State1} = cf(State),
    parse_Comment_dash(Stream1, Stream1, State1, Acc1);
?ONECHAR.

parse_Comment_dashdash(<<$>/utf8, _Rest/binary>>, Stream, State, Acc) ->
    yaccety_sax:event_comment(Acc, set_state_pos(State, Stream, 1));
parse_Comment_dashdash(_, _, State, _) ->
    fatal_error(bad_comment, State).

parse_Comment_dash(<<$-/utf8, $>/utf8, _Rest/binary>>, Stream, State, Acc) ->
    yaccety_sax:event_comment(Acc, set_state_pos(State, Stream, 2));
parse_Comment_dash(<<$-/utf8>>, _Stream, State, Acc) ->
    {Stream1, State1} = cf(State),
    parse_Comment_dashdash(Stream1, Stream1, State1, Acc);
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

parse_PI(Bytes, Stream, Pos, State) ->
    {Name, Bytes1, Stream1, Pos1, State1} = parse_Name(Bytes, Stream, Pos, State),
    ok = check_pi_name(Name, State1),
    {_Found, Bytes2, Stream2, Pos2, State2} = maybe_consume_s(Bytes1, Stream1, Pos1, State1),
    {Data, _Bytes3, Stream3, Pos3, State3} = parse_PI_data(Bytes2, Stream2, Pos2, 0, State2, []),
    yaccety_sax:event_processingInstruction(Name, Data, set_state_pos(State3, Stream3, Pos3)).

parse_PI_data(<<>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    {Stream1, State1} = cf(State),
    parse_PI_data(Stream1, Stream1, 0, 0, State1, Acc1);
parse_PI_data(<<$?/utf8, $>/utf8, Rest/binary>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    {Acc1, Rest, Stream, Pos + 2, State};
parse_PI_data(<<$?/utf8>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    {Stream1, State1} = cf(State),
    parse_PI_data_qm(Stream1, Stream1, State1, Acc1);
?ONECHAR.

parse_PI_data_qm(<<$>/utf8, Rest/binary>>, Stream, State, Acc) ->
    {Acc, Rest, Stream, 1, State};
parse_PI_data_qm(Bytes, Stream, State, Acc) ->
    parse_PI_data(Bytes, Stream, 0, 0, State, Acc).

%%----------------------------------------------------------------------
%% Parse character data. In content, everything that is not
%%   (element | Reference | CDSect | PI | Comment)
%% params:  State
%% returns: {CharData, IsWs, NewState}
%% [14] CharData ::= [^<&]* - ([^<&]* ']]>' [^<&]*)
%%----------------------------------------------------------------------
parse_CharData(Bytes, Stream, Pos, State) ->
    {{IsWs, Text}, _Bytes1, Stream1, Pos1, State1} = parse_CharData_ws(
        Bytes,
        Stream,
        Pos,
        0,
        State,
        []
    ),
    IsIgnorable = false,
    IsCData = false,
    ok = check_chardata(Text, State),
    yaccety_sax:event_characters(
        Text,
        IsCData,
        IsIgnorable,
        IsWs,
        set_state_pos(State1, Stream1, Pos1)
    ).

parse_CharData(Bytes = <<$</utf8, _/binary>>, Stream, Pos, Len, State, Acc) ->
    Text = ?ACC(Stream, Pos, Len, Acc),
    {{false, Text}, Bytes, Stream, Pos + Len, State};
parse_CharData(Bytes = <<$&/utf8, _/binary>>, Stream, Pos, Len, State, Acc) ->
    Text = ?ACC(Stream, Pos, Len, Acc),
    {{false, Text}, Bytes, Stream, Pos + Len, State};
?ONECHAR.

parse_CharData_ws(<<Char/utf8, Rest/binary>>, Stream, Pos, Len, State, Acc) when
    Char == 16#20; Char == 16#9; Char == 16#A; Char == 16#D
->
    parse_CharData_ws(Rest, Stream, Pos + 1, Len, State, Acc);
parse_CharData_ws(Bytes = <<$</utf8, _/binary>>, Stream, Pos, Len, State, Acc) ->
    Text = ?ACC(Stream, Pos, Len, Acc),
    {{true, Text}, Bytes, Stream, Pos + Len, State};
parse_CharData_ws(Bytes = <<$&/utf8, _/binary>>, Stream, Pos, Len, State, Acc) ->
    Text = ?ACC(Stream, Pos, Len, Acc),
    {{true, Text}, Bytes, Stream, Pos + Len, State};
parse_CharData_ws(<<>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    {Stream1, State1} = cf(State),
    parse_CharData_ws(Stream1, Stream1, 0, 0, State1, Acc1);
parse_CharData_ws(Bytes, Stream, Pos, Len, State, Acc) ->
    parse_CharData(Bytes, Stream, Pos, Len, State, Acc).

% check_chardata(<<"]]>", _/binary>>, State) -> fatal_error(bad_chardata, State);
% check_chardata(<<_, Rest/binary>>, State) -> check_chardata(Rest, State);
% check_chardata(<<>>, _) -> ok.
check_chardata(_, _) -> ok.

%%----------------------------------------------------------------------
%% [18] CDSect  ::= CDStart CData CDEnd
%% [19] CDStart ::= '<![CDATA['
%% [20] CData   ::= (Char* - (Char* ']]>' Char*))
%% [21] CDEnd   ::= ']]>'
%% Parse CDATA Section. '<![' is already matched.
%% params:  State
%% returns: {CharData, IsWs, NewState}
%%----------------------------------------------------------------------
parse_CDSect(<<"CDATA["/utf8, Rest/binary>>, Stream, Pos, State) ->
    parse_CData(Rest, Stream, Pos + 6, State);
parse_CDSect(<<"CDATA"/utf8>>, _Stream, _Pos, State) ->
    {Stream1, State1} = cf(State),
    parse_CDSect_bcdata(Stream1, State1);
parse_CDSect(<<"CDAT"/utf8>>, _Stream, _Pos, State) ->
    {Stream1, State1} = cf(State),
    parse_CDSect_bcdat(Stream1, State1);
parse_CDSect(<<"CDA"/utf8>>, _Stream, _Pos, State) ->
    {Stream1, State1} = cf(State),
    parse_CDSect_bcda(Stream1, State1);
parse_CDSect(<<"CD"/utf8>>, _Stream, _Pos, State) ->
    {Stream1, State1} = cf(State),
    parse_CDSect_bcd(Stream1, State1);
parse_CDSect(<<"C"/utf8>>, _Stream, _Pos, State) ->
    {Stream1, State1} = cf(State),
    parse_CDSect_bc(Stream1, State1);
parse_CDSect(<<>>, _Stream, _Pos, State) ->
    {Stream1, State1} = cf(State),
    parse_CDSect(Stream1, Stream1, 0, State1);
parse_CDSect(_, _, _, State) ->
    fatal_error(bad_cdata, State).

parse_CDSect_bc(<<"DATA["/utf8, Rest/binary>> = Stream, State) ->
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

parse_CDSect_bcd(<<"ATA["/utf8, Rest/binary>> = Stream, State) ->
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

parse_CDSect_bcda(<<"TA["/utf8, Rest/binary>> = Stream, State) ->
    parse_CData(Rest, Stream, 3, State);
parse_CDSect_bcda(<<"TA"/utf8>>, State) ->
    {Stream1, State1} = cf(State),
    parse_CDSect_bcdata(Stream1, State1);
parse_CDSect_bcda(<<"T"/utf8>>, State) ->
    {Stream1, State1} = cf(State),
    parse_CDSect_bcdat(Stream1, State1);
parse_CDSect_bcda(_, State) ->
    fatal_error(bad_cdata, State).

parse_CDSect_bcdat(<<"A["/utf8, Rest/binary>> = Stream, State) ->
    parse_CData(Rest, Stream, 2, State);
parse_CDSect_bcdat(<<"A"/utf8>>, State) ->
    {Stream1, State1} = cf(State),
    parse_CDSect_bcdata(Stream1, State1);
parse_CDSect_bcdat(_, State) ->
    fatal_error(bad_cdata, State).

parse_CDSect_bcdata(<<"["/utf8, Rest/binary>> = Stream, State) ->
    parse_CData(Rest, Stream, 1, State);
parse_CDSect_bcdata(_, State) ->
    fatal_error(bad_cdata, State).

parse_CData(Rest, Stream, Pos, State) ->
    {Text, _Bytes1, Stream1, Pos1, State1} = parse_CData(Rest, Stream, Pos, 0, State, []),
    yaccety_sax:event_characters(Text, true, false, false, set_state_pos(State1, Stream1, Pos1)).

parse_CData(<<"]]>"/utf8, Rest/binary>>, Stream, Pos, Len, State, Acc) ->
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
?ONECHAR.

parse_CData_b(<<"]>"/utf8, Rest/binary>> = Stream, State, Acc) ->
    {Acc, Rest, Stream, 2, State};
parse_CData_b(<<"]"/utf8>>, State, Acc) ->
    {Stream1, State1} = cf(State),
    parse_CData_bb(Stream1, State1, Acc);
parse_CData_b(Stream, State, Acc) ->
    Acc1 = ?APPEND(<<"]"/utf8>>, Acc),
    parse_CData(Stream, Stream, 0, 0, State, Acc1).

parse_CData_bb(<<">"/utf8, Rest/binary>> = Stream, State, Acc) ->
    {Acc, Rest, Stream, 1, State};
parse_CData_bb(Stream, State, Acc) ->
    Acc1 = ?APPEND(<<"]]"/utf8>>, Acc),
    parse_CData(Stream, Stream, 0, 0, State, Acc1).

%%----------------------------------------------------------------------
%% [80] EncodingDecl ::= S 'encoding' Eq ('"' EncName '"' | "'" EncName "'" )
%%----------------------------------------------------------------------
parse_EncodingDecl(Bytes, Stream, Pos, State) ->
    {Found, Bytes1, Stream1, Pos1, State1} = maybe_consume_s(Bytes, Stream, Pos, State),
    case Found of
        true ->
            parse_EncodingDeclS(Bytes1, Stream1, Pos1, State1);
        false ->
            % no encoding
            {{<<"UTF-8">>, false}, Bytes1, Stream1, Pos1, State1}
    end.

parse_EncodingDeclS(<<"encoding"/utf8, Rest/binary>>, Stream, Pos, State) ->
    parse_EncodingDecl_encoding(Rest, Stream, Pos + 8, State);
parse_EncodingDeclS(<<"encodin"/utf8>>, _Stream, _Pos, State) ->
    parse_EncodingDecl_encodin(cf(State));
parse_EncodingDeclS(<<"encodi"/utf8>>, _Stream, _Pos, State) ->
    parse_EncodingDecl_encodi(cf(State));
parse_EncodingDeclS(<<"encod"/utf8>>, _Stream, _Pos, State) ->
    parse_EncodingDecl_encod(cf(State));
parse_EncodingDeclS(<<"enco"/utf8>>, _Stream, _Pos, State) ->
    parse_EncodingDecl_enco(cf(State));
parse_EncodingDeclS(<<"enc"/utf8>>, _Stream, _Pos, State) ->
    parse_EncodingDecl_enc(cf(State));
parse_EncodingDeclS(<<"en"/utf8>>, _Stream, _Pos, State) ->
    parse_EncodingDecl_en(cf(State));
parse_EncodingDeclS(<<"e"/utf8>>, _Stream, _Pos, State) ->
    parse_EncodingDecl_e(cf(State));
parse_EncodingDeclS(<<>>, _Stream, _Pos, State) ->
    {Stream1, State1} = cf(State),
    parse_EncodingDeclS(Stream1, Stream1, 0, State1);
parse_EncodingDeclS(Bytes, Stream, Pos, State) ->
    % no encoding
    {{<<"UTF-8">>, false}, Bytes, Stream, Pos, State}.

parse_EncodingDecl_e({<<"ncoding"/utf8, Rest/binary>> = Stream, State}) ->
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

parse_EncodingDecl_en({<<"coding"/utf8, Rest/binary>> = Stream, State}) ->
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

parse_EncodingDecl_enc({<<"oding"/utf8, Rest/binary>> = Stream, State}) ->
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

parse_EncodingDecl_enco({<<"ding"/utf8, Rest/binary>> = Stream, State}) ->
    parse_EncodingDecl_encoding(Rest, Stream, 4, State);
parse_EncodingDecl_enco({<<"din"/utf8>>, State}) ->
    parse_EncodingDecl_encodin(cf(State));
parse_EncodingDecl_enco({<<"di"/utf8>>, State}) ->
    parse_EncodingDecl_encodi(cf(State));
parse_EncodingDecl_enco({<<"d"/utf8>>, State}) ->
    parse_EncodingDecl_encod(cf(State));
parse_EncodingDecl_enco({_, State}) ->
    fatal_error(bad_encoding, State).

parse_EncodingDecl_encod({<<"ing"/utf8, Rest/binary>> = Stream, State}) ->
    parse_EncodingDecl_encoding(Rest, Stream, 3, State);
parse_EncodingDecl_encod({<<"in"/utf8>>, State}) ->
    parse_EncodingDecl_encodin(cf(State));
parse_EncodingDecl_encod({<<"i"/utf8>>, State}) ->
    parse_EncodingDecl_encodi(cf(State));
parse_EncodingDecl_encod({_, State}) ->
    fatal_error(bad_encoding, State).

parse_EncodingDecl_encodi({<<"ng"/utf8, Rest/binary>> = Stream, State}) ->
    parse_EncodingDecl_encoding(Rest, Stream, 2, State);
parse_EncodingDecl_encodi({<<"n"/utf8>>, State}) ->
    parse_EncodingDecl_encodin(cf(State));
parse_EncodingDecl_encodi({_, State}) ->
    fatal_error(bad_encoding, State).

parse_EncodingDecl_encodin({<<"g"/utf8, Rest/binary>> = Stream, State}) ->
    parse_EncodingDecl_encoding(Rest, Stream, 1, State);
parse_EncodingDecl_encodin({_, State}) ->
    fatal_error(bad_encoding, State).

parse_EncodingDecl_encoding(Bytes, Stream, Pos, State) ->
    {Bytes1, Stream1, Pos1, State1} = parse_Eq(Bytes, Stream, Pos, State),
    parse_EncodingDecl_EncName(Bytes1, Stream1, Pos1, State1).

parse_EncodingDecl_EncName(<<$'/utf8, Rest/binary>>, Stream, Pos, State) ->
    parse_EncodingDecl_EncName_sq(Rest, Stream, Pos + 1, State);
parse_EncodingDecl_EncName(<<$"/utf8, Rest/binary>>, Stream, Pos, State) ->
    parse_EncodingDecl_EncName_dq(Rest, Stream, Pos + 1, State);
parse_EncodingDecl_EncName(_, _, _, State) ->
    fatal_error(bad_encoding, State).

parse_EncodingDecl_EncName_sq(Bytes, Stream, Pos, State) ->
    case parse_EncodingDecl_EncName_name(Bytes, Stream, Pos, State) of
        {Name, <<$'/utf8, Rest/binary>>, Stream1, Pos1, State1} ->
            {{Name, true}, Rest, Stream1, Pos1 + 1, State1};
        {_, _, _, _, State1} ->
            fatal_error(bad_encoding, State1)
    end.

parse_EncodingDecl_EncName_dq(Bytes, Stream, Pos, State) ->
    case parse_EncodingDecl_EncName_name(Bytes, Stream, Pos, State) of
        {Name, <<$"/utf8, Rest/binary>>, Stream1, Pos1, State1} ->
            {{Name, true}, Rest, Stream1, Pos1 + 1, State1};
        {_, _, _, _, State1} ->
            fatal_error(bad_encoding, State1)
    end.

%%----------------------------------------------------------------------
%% [81] EncName ::= [A-Za-z] ([A-Za-z0-9._] | '-')*
%% /* Encoding name contains only Latin characters */
%%----------------------------------------------------------------------
parse_EncodingDecl_EncName_name(<<>>, _Stream, _Pos, State) ->
    {Stream1, State1} = cf(State),
    parse_EncodingDecl_EncName_name(Stream1, Stream1, 0, State1);
parse_EncodingDecl_EncName_name(<<Char/utf8, Rest/binary>>, Stream, Pos, State) when
    Char >= $A andalso Char =< $Z; Char >= $a andalso Char =< $z
->
    parse_EncodingDecl_EncName_name_1(Rest, Stream, Pos, 1, State, []);
parse_EncodingDecl_EncName_name(_, _Stream, _Pos, State) ->
    fatal_error(bad_char, State).

parse_EncodingDecl_EncName_name_1(<<>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    {Stream1, State1} = cf(State),
    parse_EncodingDecl_EncName_name_1(Stream1, Stream1, 0, 0, State1, Acc1);
parse_EncodingDecl_EncName_name_1(<<Char/utf8, Rest/binary>>, Stream, Pos, Len, State, Acc) when
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
parse_Eq(Bytes, Stream, Pos, State) ->
    {_Found, Bytes1, Stream1, Pos1, State1} = maybe_consume_s(Bytes, Stream, Pos, State),
    parse_Eq_1(Bytes1, Stream1, Pos1, State1).

parse_Eq_1(<<$=/utf8, Rest/binary>>, Stream, Pos, State) ->
    {_Found, Bytes1, Stream1, Pos1, State1} = maybe_consume_s(Rest, Stream, Pos + 1, State),
    {Bytes1, Stream1, Pos1, State1};
parse_Eq_1(_, _Stream, _Pos, State) ->
    fatal_error(bad_eq, State).

%%----------------------------------------------------------------------
%% [26] VersionNum ::= '1.' [0-9]+
%%----------------------------------------------------------------------
parse_VersionNum_sq(<<$1/utf8, $./utf8, Rest/binary>>, Stream, Pos, State) ->
    parse_VersionNum_digit_sq(Rest, Stream, Pos + 2, 0, State, []);
parse_VersionNum_sq(<<$1/utf8>>, _Stream, _Pos, State) ->
    {Stream1, State1} = cf(State),
    parse_VersionNum_1_sq(Stream1, State1);
parse_VersionNum_sq(_, _, _, State) ->
    fatal_error(bad_version_num, State).

parse_VersionNum_1_sq(<<$./utf8, Rest/binary>> = Stream, State) ->
    parse_VersionNum_digit_sq(Rest, Stream, 1, 0, State, []);
parse_VersionNum_1_sq(_, State) ->
    fatal_error(bad_version_num, State).

parse_VersionNum_digit_sq(<<>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    {Stream1, State1} = cf(State),
    parse_VersionNum_digit_sq(Stream1, Stream1, 0, 0, State1, Acc1);
parse_VersionNum_digit_sq(<<Char/utf8, Rest/binary>>, Stream, Pos, Len, State, Acc) when
    Char >= $0 andalso Char =< $9
->
    parse_VersionNum_digit_sq(Rest, Stream, Pos, Len + 1, State, Acc);
parse_VersionNum_digit_sq(<<$'/utf8, Rest/binary>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    {version_to_number(Acc1, State), Rest, Stream, Pos + Len + 1, State};
parse_VersionNum_digit_sq(_, _, _, _, State, _) ->
    fatal_error(bad_version_num, State).

parse_VersionNum_dq(<<$1/utf8, $./utf8, Rest/binary>>, Stream, Pos, State) ->
    parse_VersionNum_digit_dq(Rest, Stream, Pos + 2, 0, State, []);
parse_VersionNum_dq(<<$1/utf8>>, _Stream, _Pos, State) ->
    {Stream1, State1} = cf(State),
    parse_VersionNum_1_dq(Stream1, State1);
parse_VersionNum_dq(_, _, _, State) ->
    fatal_error(bad_version_num, State).

parse_VersionNum_1_dq(<<$./utf8, Rest/binary>> = Stream, State) ->
    parse_VersionNum_digit_dq(Rest, Stream, 1, 0, State, []);
parse_VersionNum_1_dq(_, State) ->
    fatal_error(bad_version_num, State).

parse_VersionNum_digit_dq(<<>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    {Stream1, State1} = cf(State),
    parse_VersionNum_digit_dq(Stream1, Stream1, 0, 0, State1, Acc1);
parse_VersionNum_digit_dq(<<Char/utf8, Rest/binary>>, Stream, Pos, Len, State, Acc) when
    Char >= $0 andalso Char =< $9
->
    parse_VersionNum_digit_dq(Rest, Stream, Pos, Len + 1, State, Acc);
parse_VersionNum_digit_dq(<<$\"/utf8, Rest/binary>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    {version_to_number(Acc1, State), Rest, Stream, Pos + Len + 1, State};
parse_VersionNum_digit_dq(_, _, _, _, State, _) ->
    fatal_error(bad_version_num, State).

version_to_number(Acc, State) ->
    case iolist_to_binary([<<"1."/utf8>> | Acc]) of
        <<"1."/utf8>> -> fatal_error(bad_version_num, State);
        Bin -> Bin
    end.

%%----------------------------------------------------------------------
%% [24] VersionInfo ::= S 'version' Eq ("'" VersionNum "'" | '"' VersionNum '"')
%%----------------------------------------------------------------------
parse_VersionInfo(Bytes, Stream, Pos, State) ->
    {Found, Bytes1, Stream1, Pos1, State1} = maybe_consume_s(Bytes, Stream, Pos, State),
    case Found of
        true ->
            parse_VersionInfoS(Bytes1, Stream1, Pos1, State1);
        false ->
            % Must be present.
            fatal_error(bad_xml_decl, State)
    end.

parse_VersionInfo_version(Bytes, Stream, Pos, State) ->
    case parse_Eq(Bytes, Stream, Pos, State) of
        {<<$'/utf8, Bytes1/binary>>, Stream1, Pos1, State1} ->
            parse_VersionNum_sq(Bytes1, Stream1, Pos1 + 1, State1);
        {<<$\"/utf8, Bytes1/binary>>, Stream1, Pos1, State1} ->
            parse_VersionNum_dq(Bytes1, Stream1, Pos1 + 1, State1);
        {_, _, _, State1} ->
            fatal_error(bad_version, State1)
    end.

parse_VersionInfoS(<<"version"/utf8, Rest/binary>>, Stream, Pos, State) ->
    parse_VersionInfo_version(Rest, Stream, Pos + 7, State);
parse_VersionInfoS(<<"versio"/utf8>>, _Stream, _Pos, State) ->
    parse_VersionInfo_versio(cf(State));
parse_VersionInfoS(<<"versi"/utf8>>, _Stream, _Pos, State) ->
    parse_VersionInfo_versi(cf(State));
parse_VersionInfoS(<<"vers"/utf8>>, _Stream, _Pos, State) ->
    parse_VersionInfo_vers(cf(State));
parse_VersionInfoS(<<"ver"/utf8>>, _Stream, _Pos, State) ->
    parse_VersionInfo_ver(cf(State));
parse_VersionInfoS(<<"ve"/utf8>>, _Stream, _Pos, State) ->
    parse_VersionInfo_ve(cf(State));
parse_VersionInfoS(<<"v"/utf8>>, _Stream, _Pos, State) ->
    parse_VersionInfo_v(cf(State));
parse_VersionInfoS(<<>>, _Stream, _Pos, State) ->
    {Stream1, State1} = cf(State),
    parse_VersionInfoS(Stream1, Stream1, 0, State1);
parse_VersionInfoS(_, _, _, State) ->
    fatal_error(bad_xml_decl, State).

parse_VersionInfo_v({<<"ersion"/utf8, Rest/binary>> = Stream, State}) ->
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

parse_VersionInfo_ve({<<"rsion"/utf8, Rest/binary>> = Stream, State}) ->
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

parse_VersionInfo_ver({<<"sion"/utf8, Rest/binary>> = Stream, State}) ->
    parse_VersionInfo_version(Rest, Stream, 4, State);
parse_VersionInfo_ver({<<"sio"/utf8>>, State}) ->
    parse_VersionInfo_versio(cf(State));
parse_VersionInfo_ver({<<"si"/utf8>>, State}) ->
    parse_VersionInfo_versi(cf(State));
parse_VersionInfo_ver({<<"s"/utf8>>, State}) ->
    parse_VersionInfo_vers(cf(State));
parse_VersionInfo_ver({_, State}) ->
    fatal_error(bad_version_num, State).

parse_VersionInfo_vers({<<"ion"/utf8, Rest/binary>> = Stream, State}) ->
    parse_VersionInfo_version(Rest, Stream, 3, State);
parse_VersionInfo_vers({<<"io"/utf8>>, State}) ->
    parse_VersionInfo_versio(cf(State));
parse_VersionInfo_vers({<<"i"/utf8>>, State}) ->
    parse_VersionInfo_versi(cf(State));
parse_VersionInfo_vers({_, State}) ->
    fatal_error(bad_version_num, State).

parse_VersionInfo_versi({<<"on"/utf8, Rest/binary>> = Stream, State}) ->
    parse_VersionInfo_version(Rest, Stream, 2, State);
parse_VersionInfo_versi({<<"o"/utf8>>, State}) ->
    parse_VersionInfo_versio(cf(State));
parse_VersionInfo_versi({_, State}) ->
    fatal_error(bad_version_num, State).

parse_VersionInfo_versio({<<"n"/utf8, Rest/binary>> = Stream, State}) ->
    parse_VersionInfo_version(Rest, Stream, 1, State);
parse_VersionInfo_versio({_, State}) ->
    fatal_error(bad_version_num, State).

%%----------------------------------------------------------------------
%% [32] SDDecl ::= S 'standalone' Eq (("'" ('yes' | 'no') "'") | ('"' ('yes' | 'no') '"'))
%% leading space is trimmed already
%%----------------------------------------------------------------------
parse_SDDecl_standalone(Bytes, Stream, Pos, State) ->
    {Bytes1, Stream1, Pos1, State1} = parse_Eq(Bytes, Stream, Pos, State),
    parse_SDDecl_standalone_yesno(Bytes1, Stream1, Pos1, State1).

parse_SDDecl_standalone_yesno(<<$'/utf8, Rest/binary>>, Stream, Pos, State) ->
    parse_SDDecl_standalone_yesno_sq(Rest, Stream, Pos + 1, State);
parse_SDDecl_standalone_yesno(<<$"/utf8, Rest/binary>>, Stream, Pos, State) ->
    parse_SDDecl_standalone_yesno_dq(Rest, Stream, Pos + 1, State);
parse_SDDecl_standalone_yesno(<<>>, _Stream, _Pos, State) ->
    {Stream1, State1} = cf(State),
    parse_SDDecl_standalone_yesno(Stream1, Stream1, 0, State1);
parse_SDDecl_standalone_yesno(_, _, _, State) ->
    fatal_error(bad_standalone, State).

parse_SDDecl_standalone_yesno_sq(<<"no'"/utf8, Rest/binary>>, Stream, Pos, State) ->
    {{false, true}, Rest, Stream, Pos + 3, State};
parse_SDDecl_standalone_yesno_sq(<<"no"/utf8>>, _Stream, _Pos, State) ->
    parse_SDDecl_standalone_yesno_sq_no(cf(State));
parse_SDDecl_standalone_yesno_sq(<<"n"/utf8>>, _Stream, _Pos, State) ->
    parse_SDDecl_standalone_yesno_sq_n(cf(State));
parse_SDDecl_standalone_yesno_sq(<<"yes'"/utf8, Rest/binary>>, Stream, Pos, State) ->
    {{true, true}, Rest, Stream, Pos + 4, State};
parse_SDDecl_standalone_yesno_sq(<<"yes"/utf8>>, _Stream, _Pos, State) ->
    parse_SDDecl_standalone_yesno_sq_yes(cf(State));
parse_SDDecl_standalone_yesno_sq(<<"ye"/utf8>>, _Stream, _Pos, State) ->
    parse_SDDecl_standalone_yesno_sq_ye(cf(State));
parse_SDDecl_standalone_yesno_sq(<<"y"/utf8>>, _Stream, _Pos, State) ->
    parse_SDDecl_standalone_yesno_sq_y(cf(State)).

parse_SDDecl_standalone_yesno_sq_y({<<"es'"/utf8, Rest/binary>> = Stream, State}) ->
    {{true, true}, Rest, Stream, 3, State};
parse_SDDecl_standalone_yesno_sq_y({<<"es"/utf8>>, State}) ->
    parse_SDDecl_standalone_yesno_sq_yes(cf(State));
parse_SDDecl_standalone_yesno_sq_y({<<"e"/utf8>>, State}) ->
    parse_SDDecl_standalone_yesno_sq_ye(cf(State));
parse_SDDecl_standalone_yesno_sq_y({_, State}) ->
    fatal_error(bad_standalone, State).

parse_SDDecl_standalone_yesno_sq_ye({<<"s'"/utf8, Rest/binary>> = Stream, State}) ->
    {{true, true}, Rest, Stream, 2, State};
parse_SDDecl_standalone_yesno_sq_ye({<<"s"/utf8>>, State}) ->
    parse_SDDecl_standalone_yesno_sq_yes(cf(State));
parse_SDDecl_standalone_yesno_sq_ye({_, State}) ->
    fatal_error(bad_standalone, State).

parse_SDDecl_standalone_yesno_sq_yes({<<"'"/utf8, Rest/binary>> = Stream, State}) ->
    {{true, true}, Rest, Stream, 1, State};
parse_SDDecl_standalone_yesno_sq_yes({_, State}) ->
    fatal_error(bad_standalone, State).

parse_SDDecl_standalone_yesno_dq(<<"no\""/utf8, Rest/binary>>, Stream, Pos, State) ->
    {{false, true}, Rest, Stream, Pos + 3, State};
parse_SDDecl_standalone_yesno_dq(<<"no"/utf8>>, _Stream, _Pos, State) ->
    parse_SDDecl_standalone_yesno_dq_no(cf(State));
parse_SDDecl_standalone_yesno_dq(<<"n"/utf8>>, _Stream, _Pos, State) ->
    parse_SDDecl_standalone_yesno_dq_n(cf(State));
parse_SDDecl_standalone_yesno_dq(<<"yes\""/utf8, Rest/binary>>, Stream, Pos, State) ->
    {{true, true}, Rest, Stream, Pos + 4, State};
parse_SDDecl_standalone_yesno_dq(<<"yes"/utf8>>, _Stream, _Pos, State) ->
    parse_SDDecl_standalone_yesno_dq_yes(cf(State));
parse_SDDecl_standalone_yesno_dq(<<"ye"/utf8>>, _Stream, _Pos, State) ->
    parse_SDDecl_standalone_yesno_dq_ye(cf(State));
parse_SDDecl_standalone_yesno_dq(<<"y"/utf8>>, _Stream, _Pos, State) ->
    parse_SDDecl_standalone_yesno_dq_y(cf(State)).

parse_SDDecl_standalone_yesno_sq_n({<<"o'"/utf8, Rest/binary>> = Stream, State}) ->
    {{false, true}, Rest, Stream, 2, State};
parse_SDDecl_standalone_yesno_sq_n({<<"o"/utf8>>, State}) ->
    parse_SDDecl_standalone_yesno_sq_no(cf(State));
parse_SDDecl_standalone_yesno_sq_n({_, State}) ->
    fatal_error(bad_standalone, State).

parse_SDDecl_standalone_yesno_sq_no({<<"'"/utf8, Rest/binary>> = Stream, State}) ->
    {{false, true}, Rest, Stream, 1, State};
parse_SDDecl_standalone_yesno_sq_no({_, State}) ->
    fatal_error(bad_standalone, State).

parse_SDDecl_standalone_yesno_dq_n({<<"o\""/utf8, Rest/binary>> = Stream, State}) ->
    {{false, true}, Rest, Stream, 2, State};
parse_SDDecl_standalone_yesno_dq_n({<<"o"/utf8>>, State}) ->
    parse_SDDecl_standalone_yesno_dq_no(cf(State));
parse_SDDecl_standalone_yesno_dq_n({_, State}) ->
    fatal_error(bad_standalone, State).

parse_SDDecl_standalone_yesno_dq_no({<<"\""/utf8, Rest/binary>> = Stream, State}) ->
    {{false, true}, Rest, Stream, 1, State};
parse_SDDecl_standalone_yesno_dq_no({_, State}) ->
    fatal_error(bad_standalone, State).

parse_SDDecl_standalone_yesno_dq_y({<<"es\""/utf8, Rest/binary>> = Stream, State}) ->
    {{true, true}, Rest, Stream, 3, State};
parse_SDDecl_standalone_yesno_dq_y({<<"es"/utf8>>, State}) ->
    parse_SDDecl_standalone_yesno_dq_yes(cf(State));
parse_SDDecl_standalone_yesno_dq_y({<<"e"/utf8>>, State}) ->
    parse_SDDecl_standalone_yesno_dq_ye(cf(State));
parse_SDDecl_standalone_yesno_dq_y({_, State}) ->
    fatal_error(bad_standalone, State).

parse_SDDecl_standalone_yesno_dq_ye({<<"s\""/utf8, Rest/binary>> = Stream, State}) ->
    {{true, true}, Rest, Stream, 2, State};
parse_SDDecl_standalone_yesno_dq_ye({<<"s"/utf8>>, State}) ->
    parse_SDDecl_standalone_yesno_dq_yes(cf(State));
parse_SDDecl_standalone_yesno_dq_ye({_, State}) ->
    fatal_error(bad_standalone, State).

parse_SDDecl_standalone_yesno_dq_yes({<<"\""/utf8, Rest/binary>> = Stream, State}) ->
    {{true, true}, Rest, Stream, 1, State};
parse_SDDecl_standalone_yesno_dq_yes({_, State}) ->
    fatal_error(bad_standalone, State).

parse_SDDecl(<<"standalone"/utf8, Rest/binary>>, Stream, Pos, State) ->
    parse_SDDecl_standalone(Rest, Stream, Pos + 10, State);
parse_SDDecl(<<"standalon"/utf8>>, _Stream, _Pos, State) ->
    parse_SDDecl_standalon(cf(State));
parse_SDDecl(<<"standalo"/utf8>>, _Stream, _Pos, State) ->
    parse_SDDecl_standalo(cf(State));
parse_SDDecl(<<"standal"/utf8>>, _Stream, _Pos, State) ->
    parse_SDDecl_standal(cf(State));
parse_SDDecl(<<"standa"/utf8>>, _Stream, _Pos, State) ->
    parse_SDDecl_standa(cf(State));
parse_SDDecl(<<"stand"/utf8>>, _Stream, _Pos, State) ->
    parse_SDDecl_stand(cf(State));
parse_SDDecl(<<"stan"/utf8>>, _Stream, _Pos, State) ->
    parse_SDDecl_stan(cf(State));
parse_SDDecl(<<"sta"/utf8>>, _Stream, _Pos, State) ->
    parse_SDDecl_sta(cf(State));
parse_SDDecl(<<"st"/utf8>>, _Stream, _Pos, State) ->
    parse_SDDecl_st(cf(State));
parse_SDDecl(<<"s"/utf8>>, _Stream, _Pos, State) ->
    parse_SDDecl_s(cf(State));
parse_SDDecl(<<>>, _Stream, _Pos, State) ->
    {Stream1, State1} = cf(State),
    parse_SDDecl(Stream1, Stream1, 0, State1);
parse_SDDecl(Bytes, Stream, Pos, State) ->
    {{false, false}, Bytes, Stream, Pos, State}.

parse_SDDecl_s({<<"tandalone"/utf8, Rest/binary>> = Stream, State}) ->
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

parse_SDDecl_st({<<"andalone"/utf8, Rest/binary>> = Stream, State}) ->
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

parse_SDDecl_sta({<<"ndalone"/utf8, Rest/binary>> = Stream, State}) ->
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

parse_SDDecl_stan({<<"dalone"/utf8, Rest/binary>> = Stream, State}) ->
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

parse_SDDecl_stand({<<"alone"/utf8, Rest/binary>> = Stream, State}) ->
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

parse_SDDecl_standa({<<"lone"/utf8, Rest/binary>> = Stream, State}) ->
    parse_SDDecl_standalone(Rest, Stream, 4, State);
parse_SDDecl_standa({<<"lon"/utf8>>, State}) ->
    parse_SDDecl_standalon(cf(State));
parse_SDDecl_standa({<<"lo"/utf8>>, State}) ->
    parse_SDDecl_standalo(cf(State));
parse_SDDecl_standa({<<"l"/utf8>>, State}) ->
    parse_SDDecl_standal(cf(State));
parse_SDDecl_standa({_, State}) ->
    fatal_error(bad_standalone, State).

parse_SDDecl_standal({<<"one"/utf8, Rest/binary>> = Stream, State}) ->
    parse_SDDecl_standalone(Rest, Stream, 3, State);
parse_SDDecl_standal({<<"on"/utf8>>, State}) ->
    parse_SDDecl_standalon(cf(State));
parse_SDDecl_standal({<<"o"/utf8>>, State}) ->
    parse_SDDecl_standalo(cf(State));
parse_SDDecl_standal({_, State}) ->
    fatal_error(bad_standalone, State).

parse_SDDecl_standalo({<<"ne"/utf8, Rest/binary>> = Stream, State}) ->
    parse_SDDecl_standalone(Rest, Stream, 2, State);
parse_SDDecl_standalo({<<"n"/utf8>>, State}) ->
    parse_SDDecl_standalon(cf(State));
parse_SDDecl_standalo({_, State}) ->
    fatal_error(bad_standalone, State).

parse_SDDecl_standalon({<<"e"/utf8, Rest/binary>> = Stream, State}) ->
    parse_SDDecl_standalone(Rest, Stream, 1, State);
parse_SDDecl_standalon({_, State}) ->
    fatal_error(bad_standalone, State).

%%----------------------------------------------------------------------
%% [23] XMLDecl ::= '<?xml' VersionInfo EncodingDecl? SDDecl? S? '?>'
%%----------------------------------------------------------------------
parse_XMLDecl_ltqxml(Bytes, Stream, Pos, State) ->
    {Version, Bytes1, Stream1, Pos1, State1} = parse_VersionInfo(Bytes, Stream, Pos, State),
    {{Encoding, EncSet}, Bytes2, Stream2, Pos2, State2} = parse_EncodingDecl(
        Bytes1,
        Stream1,
        Pos1,
        State1
    ),
    {{Standalone, SaSet}, Bytes3, Stream3, Pos3, State3} = parse_SDDecl(
        Bytes2,
        Stream2,
        Pos2,
        State2
    ),
    {_, Bytes4, Stream4, Pos4, State4} = maybe_consume_s(Bytes3, Stream3, Pos3, State3),
    {_Bytes5, Stream5, Pos5, State5} = parse_XMLDecl_end(Bytes4, Stream4, Pos4, State4),
    State6 = set_next_parser_position(misc_pre_dtd, State5),
    yaccety_sax:event_startDocument(
        Version,
        Encoding,
        EncSet,
        Standalone,
        SaSet,
        set_state_pos(State6, Stream5, Pos5)
    ).

parse_XMLDecl_end(<<"?>"/utf8, Rest/binary>>, Stream, Pos, State) ->
    {Rest, Stream, Pos + 2, State};
parse_XMLDecl_end(<<"?"/utf8>>, _Stream, _Pos, State) ->
    parse_XMLDecl_end_q(cf(State));
parse_XMLDecl_end(_, _Stream, _Pos, State) ->
    fatal_error(bad_xmldecl, State).

parse_XMLDecl_end_q({<<$>/utf8, Rest/binary>> = Stream, State}) ->
    {Rest, Stream, 1, State};
parse_XMLDecl_end_q({_, State}) ->
    fatal_error(bad_xmldecl, State).

parse_XMLDecl(<<"<?xml"/utf8, Rest/binary>>, Stream, Pos, State) ->
    parse_XMLDecl_ltqxml(Rest, Stream, Pos + 5, State);
parse_XMLDecl(<<"<?xm"/utf8>>, _Stream, _Pos, State) ->
    parse_XMLDecl_ltqxm(cf(State));
parse_XMLDecl(<<"<?x"/utf8>>, _Stream, _Pos, State) ->
    parse_XMLDecl_ltqx(cf(State));
parse_XMLDecl(<<"<?"/utf8>>, _Stream, _Pos, State) ->
    parse_XMLDecl_ltq(cf(State));
parse_XMLDecl(<<"<"/utf8>>, _Stream, _Pos, State) ->
    parse_XMLDecl_lt(cf(State));
parse_XMLDecl(<<>>, _Stream, _Pos, State) ->
    {Stream1, State1} = cf(State),
    parse_XMLDecl(Stream1, Stream1, 0, State1);
parse_XMLDecl(_Bytes, Stream, Pos, State) ->
    % default declaration
    State1 = set_next_parser_position(misc_pre_dtd, State),
    yaccety_sax:event_startDocument(
        <<"1.0">>,
        <<"UTF-8">>,
        false,
        false,
        false,
        set_state_pos(State1, Stream, Pos)
    ).

parse_XMLDecl_lt({<<"?xml"/utf8, Rest/binary>> = Stream, State}) ->
    parse_XMLDecl_ltqxml(Rest, Stream, 4, State);
parse_XMLDecl_lt({<<"?xm"/utf8>>, State}) ->
    parse_XMLDecl_ltqxm(cf(State));
parse_XMLDecl_lt({<<"?x"/utf8>>, State}) ->
    parse_XMLDecl_ltqx(cf(State));
parse_XMLDecl_lt({<<"?"/utf8>>, State}) ->
    parse_XMLDecl_ltq(cf(State));
parse_XMLDecl_lt({_, State}) ->
    fatal_error(bad_xmldecl, State).

parse_XMLDecl_ltq({<<"xml"/utf8, Rest/binary>> = Stream, State}) ->
    parse_XMLDecl_ltqxml(Rest, Stream, 3, State);
parse_XMLDecl_ltq({<<"xm"/utf8>>, State}) ->
    parse_XMLDecl_ltqxm(cf(State));
parse_XMLDecl_ltq({<<"x"/utf8>>, State}) ->
    parse_XMLDecl_ltqx(cf(State));
parse_XMLDecl_ltq({_, State}) ->
    fatal_error(bad_xmldecl, State).

parse_XMLDecl_ltqx({<<"ml"/utf8, Rest/binary>> = Stream, State}) ->
    parse_XMLDecl_ltqxml(Rest, Stream, 2, State);
parse_XMLDecl_ltqx({<<"m"/utf8>>, State}) ->
    parse_XMLDecl_ltqxm(cf(State));
parse_XMLDecl_ltqx({_, State}) ->
    fatal_error(bad_xmldecl, State).

parse_XMLDecl_ltqxm({<<"l"/utf8, Rest/binary>> = Stream, State}) ->
    parse_XMLDecl_ltqxml(Rest, Stream, 1, State);
parse_XMLDecl_ltqxm({_, State}) ->
    fatal_error(bad_xmldecl, State).

%%----------------------------------------------------------------------
%% [27] Misc ::= Comment | PI | S
%%----------------------------------------------------------------------
parse_Misc(<<"<!--"/utf8, Rest/binary>>, Stream, Pos, State) ->
    parse_Comment(Rest, Stream, Pos + 4, State);
parse_Misc(<<"<?"/utf8, Rest/binary>>, Stream, Pos, State) ->
    parse_PI(Rest, Stream, Pos + 2, State);
parse_Misc(<<"<!-"/utf8>>, _Stream, _Pos, State) ->
    parse_Misc_ltbangdash(cf(State));
parse_Misc(<<"<!"/utf8>>, _Stream, _Pos, State) ->
    parse_Misc_ltbang(cf(State));
parse_Misc(<<"<"/utf8>>, _Stream, _Pos, State) ->
    parse_Misc_lt(cf(State));
parse_Misc(<<>>, _Stream, _Pos, State) ->
    case cf(State) of
        {no_bytes, State1} -> State1;
        {Stream1, State1} -> parse_Misc(Stream1, Stream1, 0, State1)
    end;
parse_Misc(Bytes, Stream, Pos, State) ->
    case maybe_consume_s(Bytes, Stream, Pos, State) of
        {true, Bytes1, Stream1, Pos1, State1} ->
            parse_Misc(Bytes1, Stream1, Pos1, State1);
        {false, _, Stream1, Pos1, State1} ->
            set_state_pos(State1, Stream1, Pos1)
    end.

parse_Misc_lt({<<"!--"/utf8, Rest/binary>> = Stream, State}) ->
    parse_Comment(Rest, Stream, 3, State);
parse_Misc_lt({<<"!-"/utf8>>, State}) ->
    parse_Misc_ltbangdash(cf(State));
parse_Misc_lt({<<"!"/utf8>>, State}) ->
    parse_Misc_ltbang(cf(State));
parse_Misc_lt({<<"?"/utf8, Rest/binary>> = Stream, State}) ->
    parse_PI(Rest, Stream, 1, State);
parse_Misc_lt({_, State}) ->
    fatal_error(bad_misc, State).

parse_Misc_ltbang({<<"--"/utf8, Rest/binary>> = Stream, State}) ->
    parse_Comment(Rest, Stream, 2, State);
parse_Misc_ltbang({<<"-"/utf8>>, State}) ->
    parse_Misc_ltbangdash(cf(State));
parse_Misc_ltbang({_, State}) ->
    fatal_error(bad_misc, State).

parse_Misc_ltbangdash({<<"-"/utf8, Rest/binary>> = Stream, State}) ->
    parse_Comment(Rest, Stream, 2, State);
parse_Misc_ltbangdash({_, State}) ->
    fatal_error(bad_misc, State).

%%----------------------------------------------------------------------
%% [43] content ::= CharData? ((element | Reference | CDSect | PI | Comment) CharData?)*
%%----------------------------------------------------------------------
-spec parse_content(_, _, _, _) ->
    {#{
            'line' := pos_integer(),
            'type' := 'comment' | 'endElement' | 'processingInstruction' | 'startElement',
            'attributes' => [any()],
            'data' => binary(),
            'namespaces' => [any()],
            'qname' => {_, _, _},
            'target' => binary(),
            'text' => binary()
        },
        #ys_state{}} |
    {binary() | [binary() | [any(), ...], ...], 'error' | 'no_bytes' | binary(), binary(),
        non_neg_integer(), _} |
    #ys_state{whitespace :: 'false'}.
parse_content(<<"</"/utf8, Rest/binary>>, Stream, Pos, State) ->
    parse_ETag(Rest, Stream, Pos + 2, State);
parse_content(<<"<!--"/utf8, Rest/binary>>, Stream, Pos, State) ->
    parse_Comment(Rest, Stream, Pos + 4, State);
parse_content(<<"<!-"/utf8>>, _Stream, _Pos, State) ->
    parse_content_ltbangdash(cf(State));
parse_content(<<"<!["/utf8, Rest/binary>>, Stream, Pos, State) ->
    parse_CDSect(Rest, Stream, Pos + 3, State);
parse_content(<<"<!"/utf8>>, _Stream, _Pos, State) ->
    parse_content_ltbang(cf(State));
parse_content(<<"<?"/utf8, Rest/binary>>, Stream, Pos, State) ->
    parse_PI(Rest, Stream, Pos + 2, State);
parse_content(<<"<"/utf8>>, _Stream, _Pos, State) ->
    parse_content_lt(cf(State));
parse_content(<<"<"/utf8, Rest/binary>>, Stream, Pos, State) ->
    parse_element_lt(Rest, Stream, Pos + 1, State);
parse_content(<<"&"/utf8, Rest/binary>>, Stream, Pos, State) ->
    {Ref, _Rest1, Stream1, Pos1, State1} = parse_Reference(Rest, Stream, Pos + 1, State),
    yaccety_sax:event_characters(Ref, false, false, false, set_state_pos(State1, Stream1, Pos1));
parse_content(<<>>, _Stream, _Pos, State) ->
    {Stream1, State1} = cf(State),
    parse_content(Stream1, Stream1, 0, State1);
parse_content(Bytes, Stream, Pos, State) ->
    parse_CharData(Bytes, Stream, Pos, State).

parse_content_lt({<<"!--"/utf8, Rest/binary>> = Stream, State}) ->
    parse_Comment(Rest, Stream, 3, State);
parse_content_lt({<<"!-"/utf8>>, State}) ->
    parse_content_ltbangdash(cf(State));
parse_content_lt({<<"!["/utf8, Rest/binary>> = Stream, State}) ->
    parse_CDSect(Rest, Stream, 2, State);
parse_content_lt({<<"/"/utf8, Rest/binary>> = Stream, State}) ->
    parse_ETag(Rest, Stream, 1, State);
parse_content_lt({<<"!"/utf8>>, State}) ->
    parse_content_ltbang(cf(State));
parse_content_lt({<<"?"/utf8, Rest/binary>> = Stream, State}) ->
    parse_PI(Rest, Stream, 1, State);
parse_content_lt({Stream, State}) ->
    parse_element_lt(Stream, Stream, 0, State).

parse_content_ltbang({<<"--"/utf8, Rest/binary>> = Stream, State}) ->
    parse_Comment(Rest, Stream, 2, State);
parse_content_ltbang({<<"-"/utf8>>, State}) ->
    parse_content_ltbangdash(cf(State));
parse_content_ltbang({<<"["/utf8, Rest/binary>> = Stream, State}) ->
    parse_CDSect(Rest, Stream, 1, State);
parse_content_ltbang({_, State}) ->
    fatal_error(bad_content, State).

parse_content_ltbangdash({<<"-"/utf8, Rest/binary>> = Stream, State}) ->
    parse_Comment(Rest, Stream, 2, State);
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
parse_element(<<$</utf8, Rest/binary>>, Stream, Pos, State) ->
    parse_element_lt(Rest, Stream, Pos + 1, State);
parse_element(<<>>, _Stream, _Pos, State) ->
    {Stream1, State1} = cf(State),
    parse_element(Stream1, Stream1, 0, State1);
parse_element(_, _Stream, _Pos, State) ->
    fatal_error(non_element, State).

parse_element_lt(Bytes, Stream, Pos, State) ->
    #ys_state{
        tags = Tags,
        inscope_ns = [LastNss | _] = Nss,
        position = P
    } = State,
    {{NameP, NameL}, Bytes1, Stream1, Pos1, State1} = parse_QName(Bytes, Stream, Pos, State),
    {{Ns, As}, Bytes2, Stream2, Pos2, State2} = parse_attributes(
        Bytes1,
        Stream1,
        Pos1,
        State1,
        [],
        []
    ),
    NewNsMap = namespace_map_from_list(Ns),
    NamespaceMap = merge_namespaces(LastNss, NewNsMap),
    QName = expand_qname(NameP, NameL, NamespaceMap),
    As1 = qualify_attribute_names(As, NamespaceMap),
    %% XXX
    %% after this is when to add default attributes and namespaces
    %% normalize values
    %% mark attributes as defaulted or actually in the stream, etc.
    %% also flag for if this is namespaced
    case Bytes2 of
        <<$>/utf8, _/binary>> ->
            State3 = State2#ys_state{
                position = [content | P],
                tags = [QName | Tags],
                inscope_ns = [NamespaceMap | Nss],
                stream_offset = {Stream2, Pos2 + 1}
            },
            yaccety_sax:event_startElement(QName, As1, Ns, State3);
        <<$//utf8, Rest/binary>> ->
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

parse_element_empty(<<$>, _/binary>>, Stream, Pos, State, QName, Ats, Nss, P, Tags) ->
    State1 = State#ys_state{
        position = [empty | P],
        tags = [QName | Tags],
        stream_offset = {Stream, Pos + 1}
    },
    yaccety_sax:event_startElement(QName, Ats, Nss, State1);
parse_element_empty(<<>>, _Stream, _Pos, State, Name, Ats, Nss, P, Tags) ->
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
parse_Attribute(Bytes, Stream, Pos, State) ->
    {{P, L}, Bytes1, Stream1, Pos1, State1} = parse_QName(Bytes, Stream, Pos, State),
    {Bytes2, Stream2, Pos2, State2} = parse_Eq(Bytes1, Stream1, Pos1, State1),
    {Value, Bytes3, Stream3, Pos3, State3} = parse_AttValue(Bytes2, Stream2, Pos2, State2),
    {{iolist_to_binary(P), iolist_to_binary(L), Value}, Bytes3, Stream3, Pos3, State3}.

%% caller must figure out the namespaces and bindings
parse_attributes(Bytes = <<$>/utf8, _/binary>>, Stream, Pos, State, Nss, Atts) ->
    {{Nss, Atts}, Bytes, Stream, Pos, State};
parse_attributes(Bytes, Stream, Pos, State, Nss, Atts) ->
    case maybe_consume_s(Bytes, Stream, Pos, State) of
        {false, Bytes1, Stream1, Pos1, State1} ->
            {{Nss, Atts}, Bytes1, Stream1, Pos1, State1};
        {true, <<C/utf8, _/binary>> = Bytes1, Stream1, Pos1, State1} when C == $>; C == $/ ->
            {{Nss, Atts}, Bytes1, Stream1, Pos1, State1};
        {true, Bytes1, Stream1, Pos1, State1} ->
            {Att, Bytes2, Stream2, Pos2, State2} = parse_Attribute(Bytes1, Stream1, Pos1, State1),
            case Att of
                {<<>>, <<"xmlns">>, Val} ->
                    parse_attributes(
                        Bytes2,
                        Stream2,
                        Pos2,
                        State2,
                        [{iolist_to_binary(Val), <<>>} | Nss],
                        Atts
                    );
                {<<"xmlns">>, Px, Val} ->
                    parse_attributes(
                        Bytes2,
                        Stream2,
                        Pos2,
                        State2,
                        [{iolist_to_binary(Val), iolist_to_binary(Px)} | Nss],
                        Atts
                    );
                Other ->
                    parse_attributes(Bytes2, Stream2, Pos2, State2, Nss, [Other | Atts])
            end
    end.

%%----------------------------------------------------------------------
%% [10] AttValue ::= '"' ([^<&"] | Reference)* '"' |  "'" ([^<&'] | Reference)* "'"
%%----------------------------------------------------------------------
parse_AttValue(<<$'/utf8, Rest/binary>>, Stream, Pos, State) ->
    parse_AttValue_sq(Rest, Stream, Pos + 1, 0, State, []);
parse_AttValue(<<$"/utf8, Rest/binary>>, Stream, Pos, State) ->
    parse_AttValue_dq(Rest, Stream, Pos + 1, 0, State, []);
parse_AttValue(<<>>, _Stream, _Pos, State) ->
    {Stream1, State1} = cf(State),
    parse_AttValue(Stream1, Stream1, 0, State1);
parse_AttValue(_, _, _, State) ->
    fatal_error(bad_attval, State).

parse_AttValue_sq(<<$'/utf8, Rest/binary>>, Stream, Pos, Len, State, Acc) ->
    Value = ?ACC(Stream, Pos, Len, Acc),
    {Value, Rest, Stream, Pos + Len + 1, State};
parse_AttValue_sq(<<$</utf8, _/binary>>, _Stream, _Pos, _Len, State, _Acc) ->
    fatal_error(bad_attval, State);
parse_AttValue_sq(<<$&/utf8, Rest/binary>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    {Ref, Bytes1, Stream1, Pos1, State1} = parse_Reference(Rest, Stream, Pos + 1, State),
    Acc2 = ?APPEND(Ref, Acc1),
    ?FUNCTION_NAME(Bytes1, Stream1, Pos1, 0, State1, Acc2);
parse_AttValue_sq(<<>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    {Stream1, State1} = cf(State),
    ?FUNCTION_NAME(Stream1, Stream1, 0, 0, State1, Acc1);
?ONECHAR.

parse_AttValue_dq(<<$"/utf8, Rest/binary>>, Stream, Pos, Len, State, Acc) ->
    Value = ?ACC(Stream, Pos, Len, Acc),
    {Value, Rest, Stream, Pos + Len + 1, State};
parse_AttValue_dq(<<$</utf8, _/binary>>, _Stream, _Pos, _Len, State, _Acc) ->
    fatal_error(bad_attval, State);
parse_AttValue_dq(<<$&/utf8, Rest/binary>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    {Ref, Bytes1, Stream1, Pos1, State1} = parse_Reference(Rest, Stream, Pos + 1, State),
    Acc2 = ?APPEND(Ref, Acc1),
    ?FUNCTION_NAME(Bytes1, Stream1, Pos1, 0, State1, Acc2);
parse_AttValue_dq(<<>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    {Stream1, State1} = cf(State),
    ?FUNCTION_NAME(Stream1, Stream1, 0, 0, State1, Acc1);
?ONECHAR.

%%----------------------------------------------------------------------
%% -[42] ETag ::= '</' Name S? '>'
%% +[13ns] ETag ::= '</' QName S? '>'  [NSC: Prefix Declared]
%% '</' is already trimmed
%%----------------------------------------------------------------------
parse_ETag(
    Bytes,
    Stream,
    Pos,
    #ys_state{inscope_ns = [Ns | Nss], position = [_, element | _Ps1], tags = [_ | Ts]} = State
) ->
    {{NameP, NameL}, Bytes1, Stream1, Pos1, State1} = parse_QName(Bytes, Stream, Pos, State),
    {_, Bytes2, Stream2, Pos2, State2} = maybe_consume_s(Bytes1, Stream1, Pos1, State1),
    case Bytes2 of
        <<$>/utf8, _/binary>> ->
            QName = expand_qname(NameP, NameL, Ns),
            State3 = State2#ys_state{inscope_ns = Nss, position = [misc_post_element], tags = Ts},
            yaccety_sax:event_endElement(QName, set_state_pos(State3, Stream2, Pos2 + 1));
        _ ->
            fatal_error(bad_endtag, State2)
    end;
parse_ETag(
    Bytes,
    Stream,
    Pos,
    #ys_state{inscope_ns = [Ns | Nss], position = [_ | Ps], tags = [_ | Ts]} = State
) ->
    {{NameP, NameL}, Bytes1, Stream1, Pos1, State1} = parse_QName(Bytes, Stream, Pos, State),
    case Bytes1 of
        <<$>/utf8, _/binary>> ->
            QName = expand_qname(NameP, NameL, Ns),
            % TODO: QName match
            State2 = State1#ys_state{inscope_ns = Nss, position = Ps, tags = Ts},
            yaccety_sax:event_endElement(QName, set_state_pos(State2, Stream1, Pos1 + 1));
        _ ->
            {_, Bytes2, Stream2, Pos2, State2} = maybe_consume_s(Bytes1, Stream1, Pos1, State1),
            case Bytes2 of
                <<$>/utf8, _/binary>> ->
                    QName = expand_qname(NameP, NameL, Ns),
                    % TODO: QName match
                    State3 = State2#ys_state{inscope_ns = Nss, position = Ps, tags = Ts},
                    yaccety_sax:event_endElement(QName, set_state_pos(State3, Stream2, Pos2 + 1));
                _ ->
                    fatal_error(bad_endtag, State2)
            end
    end.

%%----------------------------------------------------------------------
%% [4ns]  NCName ::= Name - (Char* ':' Char*)  /* An XML Name, minus the ":" */
%%----------------------------------------------------------------------
parse_NCName(<<Char/utf8, Rest/binary>>, Stream, Pos, State) when
    Char >= $A, Char =< $Z; Char == $_; Char >= $a, Char =< $z
->
    parse_NCName(Rest, Stream, Pos, 1, State, []);
parse_NCName(<<Char/utf8, Rest/binary>>, Stream, Pos, State) when
    Char >= 16#C0, Char =< 16#D6;
    Char >= 16#D8, Char =< 16#F6;
    Char >= 16#F8, Char =< 16#2FF;
    Char >= 16#370, Char =< 16#37D;
    Char >= 16#37F, Char =< 16#7FF
->
    parse_NCName(Rest, Stream, Pos, 2, State, []);
parse_NCName(<<Char/utf8, Rest/binary>>, Stream, Pos, State) when
    Char >= 16#800, Char =< 16#1FFF;
    Char >= 16#200C, Char =< 16#200D;
    Char >= 16#2070, Char =< 16#218F;
    Char >= 16#2C00, Char =< 16#2FEF;
    Char >= 16#3001, Char =< 16#D7FF;
    Char >= 16#F900, Char =< 16#FDCF;
    Char >= 16#FDF0, Char =< 16#FFFD
->
    parse_NCName(Rest, Stream, Pos, 3, State, []);
parse_NCName(<<Char/utf8, Rest/binary>>, Stream, Pos, State) when
    Char >= 16#10000, Char =< 16#EFFFF
->
    parse_NCName(Rest, Stream, Pos, 4, State, []);
parse_NCName(<<Char/utf8, _Rest/binary>>, _Stream, _Pos, _State) ->
    fatal_error(bad_name, Char);
parse_NCName(<<>>, _Stream, _Pos, State) ->
    {Stream1, State1} = cf(State),
    parse_NCName(Stream1, Stream1, 0, State1);
parse_NCName(Bytes = <<30:5, _:3>>, _Stream, _Pos, State) ->
    ?FSTNAMECHARPARTFUN(3, parse_Name);
parse_NCName(Bytes = <<30:5, _:3, 2:2, _:6>>, _Stream, _Pos, State) ->
    ?FSTNAMECHARPARTFUN(2, parse_Name);
parse_NCName(Bytes = <<14:4, _:4>>, _Stream, _Pos, State) ->
    ?FSTNAMECHARPARTFUN(2, parse_Name);
parse_NCName(Bytes = <<30:5, _:3, 2:2, _:6, 2:2, _:6>>, _Stream, _Pos, State) ->
    ?FSTNAMECHARPARTFUN(1, parse_Name);
parse_NCName(Bytes = <<14:4, _:4, 2:2, _:6>>, _Stream, _Pos, State) ->
    ?FSTNAMECHARPARTFUN(1, parse_Name);
parse_NCName(Bytes = <<6:3, _:5>>, _Stream, _Pos, State) ->
    ?FSTNAMECHARPARTFUN(1, parse_Name);
parse_NCName(_, _, _, State) ->
    fatal_error(bad_name, State).

% parse_NCName(Bytes = <<Char/utf8, _/binary>>, Stream, Pos, Len, State, Acc) when
%     Char == $>; Char == 16#20; Char == $:
% ->
%     Acc1 = ?ACC(Stream, Pos, Len, Acc),
%     {Acc1, Bytes, Stream, Pos + Len, State};
parse_NCName(<<Char/utf8, Rest/binary>> = Bytes, Stream, Pos, Len, State, Acc) ->
    if
        Char >= $a, Char =< $z;
        Char >= $A, Char =< $Z;
        Char >= $0, Char =< $9;
        Char == $_;
        Char == $-;
        Char == $.;
        Char == 16#B7 ->
            parse_NCName(Rest, Stream, Pos, Len + 1, State, Acc);
        Char < 16#80 ->
            Acc1 = ?ACC(Stream, Pos, Len, Acc),
            {Acc1, Bytes, Stream, Pos + Len, State};
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
            {Acc1, Bytes, Stream, Pos + Len, State}
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
    {Acc1, Bytes, Stream, Pos + Len, State}.

%%----------------------------------------------------------------------
%% [7ns]  QName               ::=       PrefixedName | UnprefixedName
%% [8ns]  PrefixedName        ::=       Prefix ':' LocalPart
%% [9ns]  UnprefixedName      ::=       LocalPart
%% [10ns] Prefix              ::=       NCName
%% [11ns] LocalPart           ::=       NCName
%%----------------------------------------------------------------------
parse_QName(Bytes, Stream, Pos, State) ->
    {Name1, Bytes1, Stream1, Pos1, State1} = parse_NCName(Bytes, Stream, Pos, State),
    case Bytes1 of
        <<$:/utf8, Rest/binary>> ->
            {Name2, Bytes2, Stream2, Pos2, State2} = parse_NCName(Rest, Stream1, Pos1 + 1, State1),
            {{Name1, Name2}, Bytes2, Stream2, Pos2, State2};
        _ ->
            {{<<>>, Name1}, Bytes1, Stream1, Pos1, State1}
    end.

qualify_attribute_names([], _) ->
    [];
qualify_attribute_names(Atts, Nss) ->
    [
        Att#{qname := expand_attribute_qname(iolist_to_binary(P), iolist_to_binary(L), Nss)}
        || #{qname := {P, L}} = Att <- Atts
    ].

expand_qname(<<>>, L, none) when is_binary(L) ->
    {<<>>, <<>>, L};
expand_qname(<<"xml">>, L, _) when is_binary(L) ->
    {<<"http://www.w3.org/XML/1998/namespace">>, <<"xml">>, L};
expand_qname(P, L, Nss) when is_binary(P), is_binary(L) ->
    case Nss of
        #{P := N} ->
            {N, P, L};
        _ ->
            fatal_error(unknown_prefix, P)
    end;
expand_qname(P, L, Nss) when is_binary(P) ->
    expand_qname(P, iolist_to_binary(L), Nss);
expand_qname(P, L, Nss) ->
    expand_qname(iolist_to_binary(P), iolist_to_binary(L), Nss).

expand_attribute_qname(<<>>, L, _) ->
    {<<>>, <<>>, L};
expand_attribute_qname(P, L, Nss) ->
    expand_qname(P, L, Nss).

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

fatal_error(Reason, State) ->
    error(Reason, [State]).

validation_error(non_whitespace, Pos, State) -> error(non_whitespace, [Pos, State]).

set_state_pos(State, Stream, Pos) -> State#ys_state{stream_offset = {Stream, Pos}}.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%
%%                                                                                                                   %%
%%                                                  DTD Stuff                                                        %%
%%                                                                                                                   %%
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %%
%%----------------------------------------------------------------------
%% [69] PEReference ::= '%' Name ';'
%% [WFC: No Recursion]
%% [WFC: In DTD]
%% Stream contains %
%% returns %Name
%%----------------------------------------------------------------------
parse_PEReference(Bytes, Stream, Pos, State) ->
    case parse_Name(Bytes, Stream, Pos, State) of
        {Name, <<$;/utf8, Bytes1/binary>>, Stream1, Pos1, State1} ->
            {<<$%/utf8, Name/binary>>, Bytes1, Stream1, Pos1 + 1, State1};
        {_, _, _, _, State1} ->
            fatal_error(bad_peref, State1)
    end.

%%----------------------------------------------------------------------
%% Consume Nmtoken
%% params:  State
%% returns: {Nmtoken, NewState}
%% [7] Nmtoken ::= (NameChar)+
%%----------------------------------------------------------------------
parse_Nmtoken(Bytes, Stream, Pos, State) ->
    case parse_Name(Bytes, Stream, Pos, 0, State, []) of
        {[], _, _, _, State1} -> fatal_error(bad_token, State1);
        Ret -> Ret
    end.

%%----------------------------------------------------------------------
%% [11] SystemLiteral ::= ('"' [^"]* '"') | ("'" [^']* "'")
%%----------------------------------------------------------------------
parse_SystemLiteral(<<>>, _Stream, _Pos, State) ->
    {Stream1, State1} = cf(State),
    parse_SystemLiteral(Stream1, Stream1, 0, State1);
parse_SystemLiteral(<<$'/utf8, Rest/binary>>, Stream, Pos, State) ->
    parse_SystemLiteral_sq(Rest, Stream, Pos + 1, 0, State, []);
parse_SystemLiteral(<<$"/utf8, Rest/binary>>, Stream, Pos, State) ->
    parse_SystemLiteral_dq(Rest, Stream, Pos + 1, 0, State, []);
parse_SystemLiteral(_, _Stream, _Pos, State) ->
    fatal_error(bad_pubid, State).

parse_SystemLiteral_sq(<<$'/utf8, Rest/binary>>, Stream, Pos, Len, State, Acc) ->
    Text = ?ACC(Stream, Pos, Len, Acc),
    {Text, Rest, Stream, Pos + Len + 1, State};
?ONECHAR.

parse_SystemLiteral_dq(<<$"/utf8, Rest/binary>>, Stream, Pos, Len, State, Acc) ->
    Text = ?ACC(Stream, Pos, Len, Acc),
    {Text, Rest, Stream, Pos + Len + 1, State};
?ONECHAR.

%%----------------------------------------------------------------------
%% [12] PubidLiteral ::= '"' PubidChar* '"' | "'" (PubidChar - "'")* "'"
%%----------------------------------------------------------------------
parse_PubidLiteral(<<>>, _Stream, _Pos, State) ->
    {Stream1, State1} = cf(State),
    parse_PubidLiteral(Stream1, Stream1, 0, State1);
parse_PubidLiteral(<<$'/utf8, Rest/binary>>, Stream, Pos, State) ->
    parse_PubidLiteral_sq(Rest, Stream, Pos + 1, 0, State, []);
parse_PubidLiteral(<<$"/utf8, Rest/binary>>, Stream, Pos, State) ->
    parse_PubidLiteral_dq(Rest, Stream, Pos + 1, 0, State, []);
parse_PubidLiteral(_, _, _, State) ->
    fatal_error(bad_pubid, State).

parse_PubidLiteral_sq(<<$'/utf8, Rest/binary>>, Stream, Pos, Len, State, Acc) ->
    Text = ?ACC(Stream, Pos, Len, Acc),
    {Text, Rest, Stream, Pos + Len + 1, State};
?ONEPUBIDCHAR.

parse_PubidLiteral_dq(<<$"/utf8, Rest/binary>>, Stream, Pos, Len, State, Acc) ->
    Text = ?ACC(Stream, Pos, Len, Acc),
    {Text, Rest, Stream, Pos + Len + 1, State};
?ONEPUBIDCHAR.

% %%----------------------------------------------------------------------
% %% [28] doctypedecl ::= '<!DOCTYPE' S Name (S ExternalID)? S? ('[' intSubset ']' S?)? '>'
% %% [16ns] doctypedecl ::= '<!DOCTYPE' S QName (S ExternalID)? S? ('[' (markupdecl | PEReference | S)* ']' S?)? '>'
% %% [VC: Root Element Type]
% %% [WFC: External Subset]
% %%----------------------------------------------------------------------
% parse_doctypedecl(<<"<!DOCTYPE"/utf8>>, _Stream, _Pos, State) ->
%     parse_doctypedecl_ltexDOCTYPE(cf(State));
% parse_doctypedecl(<<"<!DOCTYP"/utf8>>, _Stream, _Pos, State) ->
%     parse_doctypedecl_ltexDOCTYP(cf(State));
% parse_doctypedecl(<<"<!DOCTY"/utf8>>, _Stream, _Pos, State) ->
%     parse_doctypedecl_ltexDOCTY(cf(State));
% parse_doctypedecl(<<"<!DOCT"/utf8>>, _Stream, _Pos, State) ->
%     parse_doctypedecl_ltexDOCT(cf(State));
% parse_doctypedecl(<<"<!DOC"/utf8>>, _Stream, _Pos, State) ->
%     parse_doctypedecl_ltexDOC(cf(State));
% parse_doctypedecl(<<"<!DO"/utf8>>, _Stream, _Pos, State) ->
%     parse_doctypedecl_ltexDO(cf(State));
% parse_doctypedecl(<<"<!D"/utf8>>, _Stream, _Pos, State) ->
%     parse_doctypedecl_ltexD(cf(State));
% parse_doctypedecl(<<"<!"/utf8>>, _Stream, _Pos, State) ->
%     parse_doctypedecl_ltex(cf(State));
% parse_doctypedecl(<<"<"/utf8>>, _Stream, _Pos, State) ->
%     parse_doctypedecl_lt(cf(State)).

% parse_doctypedecl_lt({<<"!DOCTYPE"/utf8>>, State}) ->
%     parse_doctypedecl_ltexDOCTYPE(cf(State));
% parse_doctypedecl_lt({<<"!DOCTYP"/utf8>>, State}) ->
%     parse_doctypedecl_ltexDOCTYP(cf(State));
% parse_doctypedecl_lt({<<"!DOCTY"/utf8>>, State}) ->
%     parse_doctypedecl_ltexDOCTY(cf(State));
% parse_doctypedecl_lt({<<"!DOCT"/utf8>>, State}) ->
%     parse_doctypedecl_ltexDOCT(cf(State));
% parse_doctypedecl_lt({<<"!DOC"/utf8>>, State}) ->
%     parse_doctypedecl_ltexDOC(cf(State));
% parse_doctypedecl_lt({<<"!DO"/utf8>>, State}) ->
%     parse_doctypedecl_ltexDO(cf(State));
% parse_doctypedecl_lt({<<"!D"/utf8>>, State}) ->
%     parse_doctypedecl_ltexD(cf(State));
% parse_doctypedecl_lt({<<"!"/utf8>>, State}) ->
%     parse_doctypedecl_ltex(cf(State)).

% parse_doctypedecl_ltex({<<"DOCTYPE"/utf8>>, State}) ->
%     parse_doctypedecl_ltexDOCTYPE(cf(State));
% parse_doctypedecl_ltex({<<"DOCTYP"/utf8>>, State}) ->
%     parse_doctypedecl_ltexDOCTYP(cf(State));
% parse_doctypedecl_ltex({<<"DOCTY"/utf8>>, State}) ->
%     parse_doctypedecl_ltexDOCTY(cf(State));
% parse_doctypedecl_ltex({<<"DOCT"/utf8>>, State}) ->
%     parse_doctypedecl_ltexDOCT(cf(State));
% parse_doctypedecl_ltex({<<"DOC"/utf8>>, State}) ->
%     parse_doctypedecl_ltexDOC(cf(State));
% parse_doctypedecl_ltex({<<"DO"/utf8>>, State}) ->
%     parse_doctypedecl_ltexDO(cf(State));
% parse_doctypedecl_ltex({<<"D"/utf8>>, State}) ->
%     parse_doctypedecl_ltexD(cf(State)).

% parse_doctypedecl_ltexD({<<"OCTYPE"/utf8>>, State}) ->
%     parse_doctypedecl_ltexDOCTYPE(cf(State));
% parse_doctypedecl_ltexD({<<"OCTYP"/utf8>>, State}) ->
%     parse_doctypedecl_ltexDOCTYP(cf(State));
% parse_doctypedecl_ltexD({<<"OCTY"/utf8>>, State}) ->
%     parse_doctypedecl_ltexDOCTY(cf(State));
% parse_doctypedecl_ltexD({<<"OCT"/utf8>>, State}) ->
%     parse_doctypedecl_ltexDOCT(cf(State));
% parse_doctypedecl_ltexD({<<"OC"/utf8>>, State}) ->
%     parse_doctypedecl_ltexDOC(cf(State));
% parse_doctypedecl_ltexD({<<"O"/utf8>>, State}) ->
%     parse_doctypedecl_ltexDO(cf(State)).

% parse_doctypedecl_ltexDO({<<"CTYPE"/utf8>>, State}) ->
%     parse_doctypedecl_ltexDOCTYPE(cf(State));
% parse_doctypedecl_ltexDO({<<"CTYP"/utf8>>, State}) ->
%     parse_doctypedecl_ltexDOCTYP(cf(State));
% parse_doctypedecl_ltexDO({<<"CTY"/utf8>>, State}) ->
%     parse_doctypedecl_ltexDOCTY(cf(State));
% parse_doctypedecl_ltexDO({<<"CT"/utf8>>, State}) ->
%     parse_doctypedecl_ltexDOCT(cf(State));
% parse_doctypedecl_ltexDO({<<"C"/utf8>>, State}) ->
%     parse_doctypedecl_ltexDOC(cf(State)).

% parse_doctypedecl_ltexDOC({<<"TYPE"/utf8>>, State}) ->
%     parse_doctypedecl_ltexDOCTYPE(cf(State));
% parse_doctypedecl_ltexDOC({<<"TYP"/utf8>>, State}) ->
%     parse_doctypedecl_ltexDOCTYP(cf(State));
% parse_doctypedecl_ltexDOC({<<"TY"/utf8>>, State}) ->
%     parse_doctypedecl_ltexDOCTY(cf(State));
% parse_doctypedecl_ltexDOC({<<"T"/utf8>>, State}) ->
%     parse_doctypedecl_ltexDOCT(cf(State)).

% parse_doctypedecl_ltexDOCT({<<"YPE"/utf8>>, State}) ->
%     parse_doctypedecl_ltexDOCTYPE(cf(State));
% parse_doctypedecl_ltexDOCT({<<"YP"/utf8>>, State}) ->
%     parse_doctypedecl_ltexDOCTYP(cf(State));
% parse_doctypedecl_ltexDOCT({<<"Y"/utf8>>, State}) ->
%     parse_doctypedecl_ltexDOCTY(cf(State)).

% parse_doctypedecl_ltexDOCTY({<<"PE"/utf8>>, State}) ->
%     parse_doctypedecl_ltexDOCTYPE(cf(State));
% parse_doctypedecl_ltexDOCTY({<<"P"/utf8>>, State}) ->
%     parse_doctypedecl_ltexDOCTYP(cf(State)).

% parse_doctypedecl_ltexDOCTYP({<<"E"/utf8>>, State}) ->
%     parse_doctypedecl_ltexDOCTYPE(cf(State)).

% parse_doctypedecl(Stream, State) when
%     Stream == ?CHARS("<");
%     Stream == ?CHARS("<!");
%     Stream == ?CHARS("<!D");
%     Stream == ?CHARS("<!DO");
%     Stream == ?CHARS("<!DOC");
%     Stream == ?CHARS("<!DOCT");
%     Stream == ?CHARS("<!DOCTY");
%     Stream == ?CHARS("<!DOCTYP")
% ->
%     case cf(Stream, State) of
%         {error, Reason, PState1} ->
%             fatal_error(Reason, PState1);
%         {Stream, _} = PState1 ->
%             fatal_error(bad_doctypedecl, PState1);
%         {Bytes, State1} ->
%             parse_doctypedecl(Bytes, State1)
%     end;
% parse_doctypedecl(?CHARS_REST("<!DOCTYPE", Rest), #{line := Line} = State) ->
%     {_, {Rest1, State1}} = consume_s(Rest, State),
%     {Name, {Rest2, State2}} = parse_Name(Rest1, State1),
%     Text = ?APPEND_STREAM(<<"<!DOCTYPE ">>, Name),
%     DTD = #{type => dtd, name => Name, line => Line, proc => undefined},
%     parse_doctypedecl_1(Rest2, State2, Text, DTD);
% parse_doctypedecl(?CHARS_REST("<", _) = Bytes, State) ->
%     {Bytes, State};
% parse_doctypedecl(?CHARS_REST(C, _), State) ->
%     fatal_error(bad_doctypedecl, {C, State});
% ?EMPTY1;
% ?CHECK1.

% % maybe external id, maybe subset
% parse_doctypedecl_1(Stream, State, Text, DTD) ->
%     case peek(Stream, State) of
%         % internal subset
%         {$[, {Stream1, State1}} ->
%             DTD1 = DTD#{external => undefined},
%             parse_doctypedecl_2(Stream1, State1, Text, DTD1);
%         {W, {Stream1, State1}} when ?is_whitespace(W) ->
%             {_, {Stream2, State2}} = consume_s(Stream1, State1),
%             case peek(Stream2, State2) of
%                 % internal subset
%                 {$[, {Stream3, State3}} ->
%                     DTD1 = DTD#{external => undefined},
%                     parse_doctypedecl_2(Stream3, State3, Text, DTD1);
%                 % system id
%                 {$S, {Stream3, State3}} ->
%                     parse_doctypedecl_3(Stream3, State3, ?APPEND(Text, " "), DTD);
%                 % public id
%                 {$P, {Stream3, State3}} ->
%                     parse_doctypedecl_3(Stream3, State3, ?APPEND(Text, " "), DTD);
%                 % empty
%                 {$>, {Stream3, State3}} ->
%                     parse_doctypedecl_4(Stream3, State3, Text, DTD);
%                 {_, {_, State3}} ->
%                     fatal_error(bad_dtd, State3)
%             end;
%         {$>, {Stream1, State1}} ->
%             parse_doctypedecl_4(Stream1, State1, Text, DTD);
%         {_, {_, State1}} ->
%             fatal_error(bad_dtd, State1)
%     end.

% % internal subset always has '[' in stream
% parse_doctypedecl_2(?CHARS_REST("[", Rest), State, Text, DTD) ->
%     {{Text1, DTD1}, {Rest1, State1}} = parse_intSubset(Rest, State, Text, DTD),
%     parse_doctypedecl_4(Rest1, State1, Text1, DTD1).

% % system id, add key 'external' to DTD, do not follow link for now
% parse_doctypedecl_3(Stream, State, Text, DTD) ->
%     {PubSys, {Rest1, State1}} = parse_ExternalID(Stream, State),
%     DTD1 = DTD#{external => PubSys},
%     Text1 =
%         case PubSys of
%             {?EMPTY, Sys} ->
%                 ?APPEND_STREAM(Text, ?APPEND(?APPEND_STREAM(<<"SYSTEM \"">>, Sys), "\""));
%             {Pub, Sys} ->
%                 Pub1 = ?APPEND(?APPEND_STREAM(<<"PUBLIC \"">>, Pub), "\""),
%                 Sys1 = ?APPEND(?APPEND_STREAM(<<" SYSTEM \"">>, Sys), "\""),
%                 ?APPEND_STREAM(Text, ?APPEND_STREAM(Pub1, Sys1))
%         end,
%     case peek(Rest1, State1) of
%         % internal subset
%         {$[, {Stream2, State2}} ->
%             parse_doctypedecl_2(Stream2, State2, Text1, DTD1);
%         {W, {Stream2, State2}} when ?is_whitespace(W) ->
%             {_, {Stream3, State3}} = consume_s(Stream2, State2),
%             case peek(Stream3, State3) of
%                 % internal subset
%                 {$[, {Stream4, State4}} ->
%                     parse_doctypedecl_2(Stream4, State4, Text1, DTD1);
%                 % empty
%                 {$>, {Stream4, State4}} ->
%                     parse_doctypedecl_4(Stream4, State4, Text1, DTD1);
%                 {_, {_, State4}} ->
%                     fatal_error(bad_dtd, State4)
%             end;
%         {$>, {Stream2, State2}} ->
%             parse_doctypedecl_4(Stream2, State2, Text1, DTD1);
%         {_, {_, State2}} ->
%             fatal_error(bad_dtd, State2)
%     end.

% % S? and closing '>', sets text value of event
% parse_doctypedecl_4(?CHARS_REST(">", Rest), State, Text, DTD) ->
%     {DTD#{text => ?APPEND(Text, ">")}, {Rest, State}};
% parse_doctypedecl_4(?CHARS_REST(C, _) = Stream, State, Text, DTD) when ?is_whitespace(C) ->
%     {_, {Rest1, State1}} = consume_s(Stream, State, ?EMPTY),
%     parse_doctypedecl_4(Rest1, State1, Text, DTD);
% parse_doctypedecl_4(?CHARS_REST(_, _), State, _, _) ->
%     fatal_error(bad_dtd, State);
% ?EMPTY3;
% ?CHECK3.

% %%----------------------------------------------------------------------
% %% [75] ExternalID ::= 'SYSTEM' S SystemLiteral | 'PUBLIC' S PubidLiteral S SystemLiteral
% %% return {Pub, Sys}
% %%----------------------------------------------------------------------
% parse_ExternalID(?CHARS_REST("SYSTEM", Rest), State) ->
%     {_, {Rest1, State1}} = consume_s(Rest, State),
%     {Sys, {Rest2, State2}} = parse_SystemLiteral(Rest1, State1),
%     {{?EMPTY, Sys}, {Rest2, State2}};
% parse_ExternalID(?CHARS_REST("PUBLIC", Rest), State) ->
%     {_, {Rest1, State1}} = consume_s(Rest, State),
%     {Pub, {Rest2, State2}} = parse_PubidLiteral(Rest1, State1),
%     {_, {Rest3, State3}} = consume_s(Rest2, State2),
%     {Sys, {Rest4, State4}} = parse_SystemLiteral(Rest3, State3),
%     {{Pub, Sys}, {Rest4, State4}};
% parse_ExternalID(Stream, State) when
%     Stream == ?CHARS("S");
%     Stream == ?CHARS("SY");
%     Stream == ?CHARS("SYS");
%     Stream == ?CHARS("SYST");
%     Stream == ?CHARS("SYSTE");
%     Stream == ?CHARS("P");
%     Stream == ?CHARS("PU");
%     Stream == ?CHARS("PUB");
%     Stream == ?CHARS("PUBL");
%     Stream == ?CHARS("PUBLI")
% ->
%     case cf(Stream, State) of
%         {error, Reason, PState1} ->
%             fatal_error(Reason, PState1);
%         {Stream, _} = PState1 ->
%             fatal_error(bad_external_id, PState1);
%         {Bytes, State1} ->
%             parse_ExternalID(Bytes, State1)
%     end;
% parse_ExternalID(?CHARS_REST(_, _), State) ->
%     fatal_error(bad_external_id, State);
% ?EMPTY1;
% ?CHECK1.

% %%----------------------------------------------------------------------
% %% [83] PublicID ::= 'PUBLIC' S PubidLiteral
% %%----------------------------------------------------------------------
% parse_PublicID(?CHARS_REST("PUBLIC", Rest), State) ->
%     {_, {Rest1, State1}} = consume_s(Rest, State),
%     parse_PubidLiteral(Rest1, State1);
% parse_PublicID(Stream, State) when
%     Stream == ?CHARS("P");
%     Stream == ?CHARS("PU");
%     Stream == ?CHARS("PUB");
%     Stream == ?CHARS("PUBL");
%     Stream == ?CHARS("PUBLI")
% ->
%     case cf(Stream, State) of
%         {error, Reason, PState1} ->
%             fatal_error(Reason, PState1);
%         {Stream, _} = PState1 ->
%             fatal_error(bad_public_id, PState1);
%         {Bytes, State1} ->
%             parse_PublicID(Bytes, State1)
%     end;
% parse_PublicID(?CHARS_REST(_, _), State) ->
%     fatal_error(bad_public_id, State);
% ?EMPTY1;
% ?CHECK1.

% %%----------------------------------------------------------------------
% %% Parses a notation declaration.
% %% Param: State
% %% Returns: {Name, Pub, Sys}
% %% "<!N" is in stream
% %% [82] NotationDecl ::= '<!NOTATION' S Name S (ExternalID | PublicID) S? '>'
% %%----------------------------------------------------------------------
% parse_NotationDecl(?CHARS_REST("<!NOTATION", Rest), State) ->
%     {_, {Rest1, State1}} = consume_s(Rest, State),
%     {Name, {Rest2, State2}} = parse_Name(Rest1, State1),
%     {_, {Rest3, State3}} = consume_s(Rest2, State2),
%     case peek(Rest3, State3) of
%         {$S, {Rest4, State4}} ->
%             {{Pub, Sys}, {Rest5, State5}} = parse_ExternalID(Rest4, State4),
%             PState = trim_sgt(Rest5, State5),
%             {{Name, Pub, Sys}, PState};
%         {$P, {Rest4, State4}} ->
%             {Pub, {Rest5, State5}} = parse_PublicID(Rest4, State4),
%             case peek(Rest5, State5) of
%                 {$>, {Rest6, State6}} ->
%                     PState = trim_sgt(Rest6, State6),
%                     {{Name, Pub, ?EMPTY}, PState};
%                 {W, {Rest6, State6}} when ?is_whitespace(W) ->
%                     {_, {Rest7, State7}} = consume_s(Rest6, State6),
%                     case peek(Rest7, State7) of
%                         {$>, {Rest8, State8}} ->
%                             PState = trim_sgt(Rest8, State8),
%                             {{Name, Pub, ?EMPTY}, PState};
%                         {$S, {Rest8, State8}} ->
%                             {{?EMPTY, Sys}, {Rest9, State9}} = parse_ExternalID(Rest8, State8),
%                             PState = trim_sgt(Rest9, State9),
%                             {{Name, Pub, Sys}, PState};
%                         {_, {_, State8}} ->
%                             fatal_error(bad_notation, State8)
%                     end;
%                 {_, {_, State6}} ->
%                     fatal_error(bad_notation, State6)
%             end;
%         {_, {_, State4}} ->
%             fatal_error(bad_notation, State4)
%     end;
% parse_NotationDecl(Stream, State) when
%     Stream == ?CHARS("<!N");
%     Stream == ?CHARS("<!NO");
%     Stream == ?CHARS("<!NOT");
%     Stream == ?CHARS("<!NOTA");
%     Stream == ?CHARS("<!NOTAT");
%     Stream == ?CHARS("<!NOTATI");
%     Stream == ?CHARS("<!NOTATIO")
% ->
%     case cf(Stream, State) of
%         {error, Reason, PState1} ->
%             fatal_error(Reason, PState1);
%         {Stream, _} = PState1 ->
%             fatal_error(bad_public_id, PState1);
%         {Bytes, State1} ->
%             parse_NotationDecl(Bytes, State1)
%     end;
% parse_NotationDecl(?CHARS_REST(_, _), State) ->
%     fatal_error(bad_notation, State);
% ?CHECK1.

% %%----------------------------------------------------------------------
% %% [60] DefaultDecl ::= '#REQUIRED' | '#IMPLIED' | (('#FIXED' S)? AttValue)
% %% [WFC: No < in Attribute Values]
% %% [WFC: No External Entity References]
% %% (required | implied | {fixed, AttValue})
% %%----------------------------------------------------------------------
% parse_DefaultDecl(?CHARS_REST("#REQUIRED", Rest), State) ->
%     {required, {Rest, State}};
% parse_DefaultDecl(?CHARS_REST("#IMPLIED", Rest), State) ->
%     {implied, {Rest, State}};
% parse_DefaultDecl(?CHARS_REST("#FIXED", Rest), State) ->
%     {_, {Rest1, State1}} = consume_s(Rest, State),
%     {Value, PState} = parse_AttValue(Rest1, State1),
%     {{fixed, Value}, PState};
% parse_DefaultDecl(Stream, State) when
%     Stream == ?CHARS("#");
%     Stream == ?CHARS("#R");
%     Stream == ?CHARS("#RE");
%     Stream == ?CHARS("#REQ");
%     Stream == ?CHARS("#REQU");
%     Stream == ?CHARS("#REQUI");
%     Stream == ?CHARS("#REQUIR");
%     Stream == ?CHARS("#REQUIRE");
%     Stream == ?CHARS("#I");
%     Stream == ?CHARS("#IM");
%     Stream == ?CHARS("#IMP");
%     Stream == ?CHARS("#IMPL");
%     Stream == ?CHARS("#IMPLI");
%     Stream == ?CHARS("#IMPLIE");
%     Stream == ?CHARS("#F");
%     Stream == ?CHARS("#FI");
%     Stream == ?CHARS("#FIX");
%     Stream == ?CHARS("#FIXE")
% ->
%     case cf(Stream, State) of
%         {error, Reason, PState1} ->
%             fatal_error(Reason, PState1);
%         {Stream, _} = PState1 ->
%             fatal_error(bad_default, PState1);
%         {Bytes, State1} ->
%             parse_DefaultDecl(Bytes, State1)
%     end;
% parse_DefaultDecl(?CHARS_REST(_, _), State) ->
%     fatal_error(bad_default, State);
% ?CHECK1.

% %%----------------------------------------------------------------------
% %% [59] Enumeration ::= '(' S? Nmtoken (S? '|' S? Nmtoken)* S? ')'
% %% '(' in stream
% %%----------------------------------------------------------------------
% parse_Enumeration(?CHARS_REST("(", Rest), State) ->
%     {_, {Rest1, State1}} = maybe_consume_s(Rest, State, ?EMPTY),
%     {Name, {Rest2, State2}} = parse_Nmtoken(Rest1, State1),
%     {_, {Rest3, State3}} = maybe_consume_s(Rest2, State2, ?EMPTY),
%     parse_Enumeration_1(Rest3, State3, [Name]);
% parse_Enumeration(?CHARS_REST(_, _), State) ->
%     fatal_error(bad_enum, State);
% ?CHECK1.

% parse_Enumeration_1(?CHARS_REST(")", Rest), State, Acc) ->
%     {{enumeration, lists:reverse(Acc)}, {Rest, State}};
% parse_Enumeration_1(?CHARS_REST("|", Rest), State, Acc) ->
%     {_, {Rest1, State1}} = maybe_consume_s(Rest, State, ?EMPTY),
%     {Name, {Rest2, State2}} = parse_Nmtoken(Rest1, State1),
%     {_, {Rest3, State3}} = maybe_consume_s(Rest2, State2, ?EMPTY),
%     parse_Enumeration_1(Rest3, State3, [Name | Acc]);
% parse_Enumeration_1(?CHARS_REST(_, _), State, _) ->
%     fatal_error(bad_enum, State);
% ?EMPTY2;
% ?CHECK2.

% %%----------------------------------------------------------------------
% %% [58] NotationType ::= 'NOTATION' S '(' S? Name (S? '|' S? Name)* S? ')'
% %% 'N' in stream
% %%----------------------------------------------------------------------
% parse_NotationType(?CHARS_REST("NOTATION", Rest), State) ->
%     {_, {Rest1, State1}} = consume_s(Rest, State),
%     parse_NotationType_1(Rest1, State1);
% parse_NotationType(Stream, State) when
%     Stream == ?CHARS("N");
%     Stream == ?CHARS("NO");
%     Stream == ?CHARS("NOT");
%     Stream == ?CHARS("NOTA");
%     Stream == ?CHARS("NOTAT");
%     Stream == ?CHARS("NOTATI");
%     Stream == ?CHARS("NOTATIO")
% ->
%     case cf(Stream, State) of
%         {error, Reason, PState1} ->
%             fatal_error(Reason, PState1);
%         {Stream, _} = PState1 ->
%             fatal_error(bad_notation_type, PState1);
%         {Bytes, State1} ->
%             parse_NotationType(Bytes, State1)
%     end;
% parse_NotationType(?CHARS_REST(_, _), State) ->
%     fatal_error(bad_notation_type, State);
% ?CHECK1.

% parse_NotationType_1(?CHARS_REST("(", Rest), State) ->
%     {_, {Rest1, State1}} = maybe_consume_s(Rest, State, ?EMPTY),
%     {Name, {Rest2, State2}} = parse_Name(Rest1, State1),
%     {_, {Rest3, State3}} = maybe_consume_s(Rest2, State2, ?EMPTY),
%     parse_NotationType_2(Rest3, State3, [Name]);
% parse_NotationType_1(?CHARS_REST(_, _), State) ->
%     fatal_error(bad_notation_type, State);
% ?EMPTY1;
% ?CHECK1.

% parse_NotationType_2(?CHARS_REST(")", Rest), State, Acc) ->
%     {{notation, lists:reverse(Acc)}, {Rest, State}};
% parse_NotationType_2(?CHARS_REST("|", Rest), State, Acc) ->
%     {_, {Rest1, State1}} = maybe_consume_s(Rest, State, ?EMPTY),
%     {Name, {Rest2, State2}} = parse_Name(Rest1, State1),
%     {_, {Rest3, State3}} = maybe_consume_s(Rest2, State2, ?EMPTY),
%     parse_NotationType_2(Rest3, State3, [Name | Acc]);
% parse_NotationType_2(?CHARS_REST(_, _), State, _) ->
%     fatal_error(bad_notation_type, State);
% ?EMPTY2;
% ?CHECK2.

% %%----------------------------------------------------------------------
% %% [54] AttType        ::= StringType | TokenizedType | EnumeratedType
% %% [55] StringType     ::= 'CDATA'
% %% [56] TokenizedType  ::= 'ID' | 'IDREF' | 'IDREFS' | 'ENTITY' | 'ENTITIES' |
% %%                         'NMTOKEN' | 'NMTOKENS'
% %% [57] EnumeratedType ::= NotationType | Enumeration
% %%----------------------------------------------------------------------
% parse_AttType(Stream, State) when
%     Stream == ?CHARS("C");
%     Stream == ?CHARS("CD");
%     Stream == ?CHARS("CDA");
%     Stream == ?CHARS("CDAT");
%     Stream == ?CHARS("I");
%     Stream == ?CHARS("ID");
%     Stream == ?CHARS("IDR");
%     Stream == ?CHARS("IDRE");
%     Stream == ?CHARS("IDREF");
%     Stream == ?CHARS("E");
%     Stream == ?CHARS("EN");
%     Stream == ?CHARS("ENT");
%     Stream == ?CHARS("ENTI");
%     Stream == ?CHARS("ENTIT");
%     Stream == ?CHARS("ENTITI");
%     Stream == ?CHARS("ENTITIE");
%     Stream == ?CHARS("N");
%     Stream == ?CHARS("NM");
%     Stream == ?CHARS("NMT");
%     Stream == ?CHARS("NMTO");
%     Stream == ?CHARS("NMTOK");
%     Stream == ?CHARS("NMTOKE");
%     Stream == ?CHARS("NMTOKEN")
% ->
%     case cf(Stream, State) of
%         {error, Reason, PState1} ->
%             fatal_error(Reason, PState1);
%         {Stream, _} = PState1 ->
%             fatal_error(bad_att_type, PState1);
%         {Bytes, State1} ->
%             parse_AttType(Bytes, State1)
%     end;
% parse_AttType(?CHARS_REST("CDATA", Rest), State) ->
%     {cdata, {Rest, State}};
% parse_AttType(?CHARS_REST("IDREFS", Rest), State) ->
%     {idrefs, {Rest, State}};
% parse_AttType(?CHARS_REST("IDREF", Rest), State) ->
%     {idref, {Rest, State}};
% parse_AttType(?CHARS_REST("ID", Rest), State) ->
%     {id, {Rest, State}};
% parse_AttType(?CHARS_REST("ENTITY", Rest), State) ->
%     {entity, {Rest, State}};
% parse_AttType(?CHARS_REST("ENTITIES", Rest), State) ->
%     {entities, {Rest, State}};
% parse_AttType(?CHARS_REST("NMTOKENS", Rest), State) ->
%     {nmtokens, {Rest, State}};
% parse_AttType(?CHARS_REST("NMTOKEN", Rest), State) ->
%     {nmtoken, {Rest, State}};
% parse_AttType(?CHARS_REST("N", _) = Stream, State) ->
%     parse_NotationType(Stream, State);
% parse_AttType(?CHARS_REST("(", _) = Stream, State) ->
%     parse_Enumeration(Stream, State);
% parse_AttType(?CHARS_REST(_, _), State) ->
%     fatal_error(bad_att_type, State);
% ?EMPTY1;
% ?CHECK1.

% %%----------------------------------------------------------------------
% %% [53] AttDef   ::= S Name S AttType S DefaultDecl
% %% [21ns] AttDef ::= S (QName | NSAttName) S AttType S DefaultDecl
% %% trims trailing'>'
% %%----------------------------------------------------------------------
% parse_AttDef(?CHARS_REST(">", Rest), State, Acc) ->
%     {lists:reverse(Acc), {Rest, State}};
% parse_AttDef(?CHARS_REST(C, _) = Stream, State, Acc) when ?is_whitespace(C) ->
%     {_, {Rest1, State1}} = consume_s(Stream, State),
%     case peek(Rest1, State1) of
%         {$>, {Rest2, State2}} ->
%             parse_AttDef(Rest2, State2, Acc);
%         {_, {Rest2, State2}} ->
%             {Name, {Rest3, State3}} = parse_Name(Rest2, State2),
%             {_, {Rest4, State4}} = consume_s(Rest3, State3),
%             {Type, {Rest5, State5}} = parse_AttType(Rest4, State4),
%             {_, {Rest6, State6}} = consume_s(Rest5, State5),
%             {Def, {Rest7, State7}} = parse_DefaultDecl(Rest6, State6),
%             parse_AttDef(Rest7, State7, [{Name, Type, Def} | Acc])
%     end;
% parse_AttDef(?CHARS_REST(_, _), State, _) ->
%     fatal_error(bad_att_def, State);
% ?EMPTY2;
% ?CHECK2.

% %%----------------------------------------------------------------------
% %% [52] AttlistDecl   ::= '<!ATTLIST' S Name AttDef* S? '>'
% %% [20ns] AttlistDecl ::= '<!ATTLIST' S QName AttDef* S? '>'
% %% '<!A' in stream, returns {Name, AttDefs}
% %%----------------------------------------------------------------------
% parse_AttlistDecl(?CHARS_REST("<!ATTLIST", Rest), State) ->
%     {_, {Rest1, State1}} = consume_s(Rest, State),
%     {Name, {Rest2, State2}} = parse_Name(Rest1, State1),
%     {AttDefs, PState3} = parse_AttDef(Rest2, State2, []),
%     {{Name, AttDefs}, PState3};
% parse_AttlistDecl(Stream, State) when
%     Stream == ?CHARS("<!A");
%     Stream == ?CHARS("<!AT");
%     Stream == ?CHARS("<!ATT");
%     Stream == ?CHARS("<!ATTL");
%     Stream == ?CHARS("<!ATTLI");
%     Stream == ?CHARS("<!ATTLIS");
%     Stream == ?CHARS("<!ATTLIST")
% ->
%     case cf(Stream, State) of
%         {error, Reason, PState1} ->
%             fatal_error(Reason, PState1);
%         {Stream, _} = PState1 ->
%             fatal_error(bad_att_list, PState1);
%         {Bytes, State1} ->
%             parse_AttlistDecl(Bytes, State1)
%     end;
% parse_AttlistDecl(?CHARS_REST(_, _), State) ->
%     fatal_error(bad_att_list, State);
% ?CHECK1.

% %%----------------------------------------------------------------------
% %% [28a] DeclSep    ::= PEReference | S
% %% [WFC: PE Between Declarations]
% %% [28b] intSubset  ::= (markupdecl | DeclSep)*
% %% [29]  markupdecl ::= elementdecl | AttlistDecl | EntityDecl | NotationDecl | PI | Comment
% %% [WFC: PEs in Internal Subset]
% %%----------------------------------------------------------------------
% parse_intSubset(Stream, State, Text, DTD) when
%     Stream == ?CHARS("<"); Stream == ?CHARS("<!"); Stream == ?CHARS("<!-"); Stream == ?CHARS("<!E")
% ->
%     case cf(Stream, State) of
%         {error, Reason, PState1} ->
%             fatal_error(Reason, PState1);
%         {Stream, _} = PState1 ->
%             fatal_error(bad_dtd, PState1);
%         {Bytes, State1} ->
%             parse_intSubset(Bytes, State1, Text, DTD)
%     end;
% parse_intSubset(?CHARS_REST("]", Rest), State, Text, DTD) ->
%     {{Text, DTD}, {Rest, State}};
% parse_intSubset(?CHARS_REST("<!EL", _) = Stream, State, Text, DTD) ->
%     {Elem, {Rest1, State1}} = parse_elementdecl(Stream, State),
%     DTD1 = add_element_to_dtd(Elem, DTD),
%     parse_intSubset(Rest1, State1, Text, DTD1);
% parse_intSubset(?CHARS_REST("<!A", _) = Stream, State, Text, DTD) ->
%     {AttList, {Rest1, State1}} = parse_AttlistDecl(Stream, State),
%     DTD1 = add_attlist_to_dtd(AttList, DTD),
%     parse_intSubset(Rest1, State1, Text, DTD1);
% parse_intSubset(?CHARS_REST("<!E", _) = Stream, State, Text, DTD) ->
%     {Entity, {Rest1, State1}} = parse_EntityDecl(Stream, State),
%     DTD1 = add_entity_to_dtd(Entity, DTD),
%     {Rest2, State2} = trim_sgt(Rest1, State1),
%     parse_intSubset(Rest2, State2, Text, DTD1);
% parse_intSubset(?CHARS_REST("<!N", _) = Stream, State, Text, DTD) ->
%     {Notation, {Rest1, State1}} = parse_NotationDecl(Stream, State),
%     DTD1 = add_notation_to_dtd(Notation, DTD),
%     parse_intSubset(Rest1, State1, Text, DTD1);
% parse_intSubset(?CHARS_REST("<?", Rest), State, Text, DTD) ->
%     %XXX just add to the text
%     {_PI, {Rest1, State1}} = parse_PI(Rest, State),
%     parse_intSubset(Rest1, State1, Text, DTD);
% parse_intSubset(?CHARS_REST("<!--", Rest), State, Text, DTD) ->
%     %XXX just add to the text
%     {_Comment, {Rest1, State1}} = parse_Comment(Rest, State),
%     parse_intSubset(Rest1, State1, Text, DTD);
% parse_intSubset(?CHARS_REST(C, _) = Stream, State, Text, DTD) when ?is_whitespace(C) ->
%     {_, {Rest1, State1}} = consume_s(Stream, State),
%     parse_intSubset(Rest1, State1, Text, DTD);
% parse_intSubset(?CHARS_REST("%", _) = Stream, State, Text, DTD) ->
%     {PE, {Rest1, State1}} = parse_PEReference(Stream, State),
%     case resolve_parameter_entity(PE, DTD, State1) of
%         {internal, Value, State2} ->
%             Stream1 = ?APPEND_STREAM(Value, Rest1),
%             parse_intSubset(Stream1, State2, Text, DTD);
%         %% XXX do something with externals...
%         {external, _, State2} ->
%             parse_intSubset(Rest1, State2, Text, DTD);
%         unknown ->
%             parse_intSubset(Rest1, State1, Text, DTD)
%     end;
% parse_intSubset(?CHARS_REST(C, _), State, _, DTD) ->
%     fatal_error(bad_dtd, {DTD, C, State});
% ?EMPTY3;
% ?CHECK3.

% %%----------------------------------------------------------------------
% %% [9] EntityValue ::= '"' ([^%&"] | PEReference | Reference)* '"' |
% %%                     "'" ([^%&'] | PEReference | Reference)* "'"
% %%----------------------------------------------------------------------
% parse_EntityValue(?CHARS_REST(C, Rest), State) when C == $'; C == $\" ->
%     parse_EntityValue_1(Rest, State, ?EMPTY, C);
% parse_EntityValue(?CHARS_REST(_, _), State) ->
%     fatal_error(bad_entval, State);
% ?EMPTY1;
% ?CHECK1.

% parse_EntityValue_1(?CHARS_REST(C, Rest), State, Acc, C) ->
%     {Acc, {Rest, State}};
% parse_EntityValue_1(?CHARS_REST("&", Rest), State, Acc, Stop) ->
%     case parse_Reference(Rest, State) of
%         {C, {Rest1, State1}} when is_integer(C) ->
%             parse_EntityValue_1(Rest1, State1, ?APPEND(Acc, C), Stop);
%         {_N, {Rest1, State1}} ->
%             %% TODO resolve entity references ! (here as replacement text)
%             parse_EntityValue_1(Rest1, State1, Acc, Stop)
%     end;
% parse_EntityValue_1(?CHARS_REST("%", Rest), State, Acc, Stop) ->
%     {_N, {Rest1, State1}} = parse_PEReference(Rest, State),
%     %% TODO resolve parameter entity references ! (here as replacement text)
%     parse_EntityValue_1(Rest1, State1, Acc, Stop);
% parse_EntityValue_1(?CHARS_REST(C, Rest), State, Acc, Stop) when ?is_char(C) ->
%     parse_EntityValue_1(Rest, State, ?APPEND(Acc, C), Stop);
% parse_EntityValue_1(?CHARS_REST(_, _), State, _, _) ->
%     fatal_error(bad_entval, State);
% ?EMPTY3;
% ?CHECK3.

% %%----------------------------------------------------------------------
% %% [70] EntityDecl ::= GEDecl | PEDecl
% %% '<!E' is in the stream,
% %% {Name, {internal, Value} | {external, PubSys} | {external, PubSys, NData}}
% %%----------------------------------------------------------------------
% parse_EntityDecl(?CHARS_REST("<!ENTITY", Rest), State) ->
%     {_, {Rest1, State1}} = consume_s(Rest, State),
%     case peek(Rest1, State1) of
%         {$%, {Rest2, State2}} ->
%             parse_PEDecl(Rest2, State2);
%         {_, {Rest2, State2}} ->
%             parse_GEDecl(Rest2, State2)
%     end;
% parse_EntityDecl(Stream, State) when
%     Stream == ?CHARS("<!E");
%     Stream == ?CHARS("<!EN");
%     Stream == ?CHARS("<!ENT");
%     Stream == ?CHARS("<!ENTI");
%     Stream == ?CHARS("<!ENTIT");
%     Stream == ?CHARS("<!ENTITY")
% ->
%     case cf(Stream, State) of
%         {error, Reason, PState1} ->
%             fatal_error(Reason, PState1);
%         {Stream, _} = PState1 ->
%             fatal_error(bad_entity, PState1);
%         {Bytes, State1} ->
%             parse_EntityDecl(Bytes, State1)
%     end;
% parse_EntityDecl(?CHARS_REST(_, _), State) ->
%     fatal_error(bad_entity, State);
% ?CHECK1.

% %%----------------------------------------------------------------------
% %% [71] GEDecl ::= '<!ENTITY' S Name S EntityDef S? '>'
% %% has 1 char on the stream at least
% %% {general|parameter, internal|external|unparsed, Value}
% %%----------------------------------------------------------------------
% parse_GEDecl(Stream, State) ->
%     {Name, {Rest1, State1}} = parse_Name(Stream, State),
%     {_, {Rest2, State2}} = consume_s(Rest1, State1),
%     {PE, {Rest3, State3}} = parse_PEDef(Rest2, State2),
%     {_, {Rest4, State4}} = maybe_consume_s(Rest3, State3, ?EMPTY),
%     case PE of
%         {_, internal, Value} ->
%             {{Name, {general, internal, Value}}, {Rest4, State4}};
%         {_, external, PubSys} ->
%             case peek(Rest4, State4) of
%                 {$>, PState} ->
%                     {{Name, {general, external, PubSys}}, PState};
%                 {$N, {Rest5, State5}} ->
%                     {NName, PState} = parse_NDataDecl(Rest5, State5),
%                     {{Name, {general, unparsed, {PubSys, NName}}}, PState};
%                 _ ->
%                     fatal_error(bad_entity, State4)
%             end
%     end.

% %%----------------------------------------------------------------------
% %% [72] PEDecl ::= '<!ENTITY' S '%' S Name S PEDef S? '>'
% %% has '%' on the stream
% %%----------------------------------------------------------------------
% parse_PEDecl(?CHARS_REST("%", Rest), State) ->
%     {_, {Rest1, State1}} = consume_s(Rest, State),
%     {Name, {Rest2, State2}} = parse_Name(Rest1, State1),
%     {_, {Rest3, State3}} = consume_s(Rest2, State2),
%     {PE, PState} = parse_PEDef(Rest3, State3),
%     {{?APPEND_STREAM(<<"%">>, Name), PE}, PState}.

% %%----------------------------------------------------------------------
% %% [73] EntityDef ::= EntityValue | (ExternalID NDataDecl?)
% %% [74] PEDef ::= EntityValue | ExternalID
% %% only reads EntityValue | ExternalID any NDataDecl after
% %%----------------------------------------------------------------------
% parse_PEDef(Stream, State) ->
%     case peek(Stream, State) of
%         {$', {Rest1, State1}} ->
%             {Value, PState} = parse_EntityValue(Rest1, State1),
%             {{parameter, internal, Value}, PState};
%         {$\", {Rest1, State1}} ->
%             {Value, PState} = parse_EntityValue(Rest1, State1),
%             {{parameter, internal, Value}, PState};
%         {_, {Rest1, State1}} ->
%             {PubSys, PState} = parse_ExternalID(Rest1, State1),
%             {{parameter, external, PubSys}, PState}
%     end.

% %%----------------------------------------------------------------------
% %% [76] NDataDecl ::= S 'NDATA' S Name
% %% [VC: Notation Declared]
% %% leading S has been stripped
% %%----------------------------------------------------------------------
% parse_NDataDecl(?CHARS_REST("NDATA", Rest), State) ->
%     {_, {Rest1, State1}} = consume_s(Rest, State),
%     parse_Name(Rest1, State1);
% parse_NDataDecl(Stream, State) when
%     Stream == ?CHARS("N");
%     Stream == ?CHARS("ND");
%     Stream == ?CHARS("NDA");
%     Stream == ?CHARS("NDAT");
%     Stream == ?CHARS("NDATA")
% ->
%     case cf(Stream, State) of
%         {error, Reason, PState1} ->
%             fatal_error(Reason, PState1);
%         {Stream, _} = PState1 ->
%             fatal_error(bad_ndata, PState1);
%         {Bytes, State1} ->
%             parse_NDataDecl(Bytes, State1)
%     end;
% parse_NDataDecl(?CHARS_REST(_, _), State) ->
%     fatal_error(bad_ndata, State);
% ?EMPTY1;
% ?CHECK1.

% %%----------------------------------------------------------------------
% %% [45] elementdecl ::= '<!ELEMENT' S Name S contentspec S? '>'
% %% [17ns] elementdecl ::= '<!ELEMENT' S QName S contentspec S? '>' [VC: Unique Element Type Declaration]
% %% '<!EL' is in the stream,
% %%----------------------------------------------------------------------
% parse_elementdecl(?CHARS_REST("<!ELEMENT", Rest), State) ->
%     {_, {Rest1, State1}} = consume_s(Rest, State),
%     {Name, {Rest2, State2}} = parse_Name(Rest1, State1),
%     {_, {Rest3, State3}} = consume_s(Rest2, State2),
%     {Spec, PState} = parse_contentspec(Rest3, State3),
%     {{Name, Spec}, PState};
% parse_elementdecl(Stream, State) when
%     Stream == ?CHARS("<!EL");
%     Stream == ?CHARS("<!ELE");
%     Stream == ?CHARS("<!ELEM");
%     Stream == ?CHARS("<!ELEME");
%     Stream == ?CHARS("<!ELEMEN");
%     Stream == ?CHARS("<!ELEMENT")
% ->
%     case cf(Stream, State) of
%         {error, Reason, PState1} ->
%             fatal_error(Reason, PState1);
%         {Stream, _} = PState1 ->
%             fatal_error(bad_entity, PState1);
%         {Bytes, State1} ->
%             parse_elementdecl(Bytes, State1)
%     end;
% parse_elementdecl(?CHARS_REST(_, _), State) ->
%     fatal_error(bad_element, State);
% ?CHECK1.

% %%----------------------------------------------------------------------
% %% [46] contentspec ::= 'EMPTY' | 'ANY' | Mixed | children
% %%----------------------------------------------------------------------
% parse_contentspec(?CHARS_REST("EMPTY", Rest), State) ->
%     PState = trim_sgt(Rest, State),
%     {empty, PState};
% parse_contentspec(?CHARS_REST("ANY", Rest), State) ->
%     PState = trim_sgt(Rest, State),
%     {any, PState};
% parse_contentspec(?CHARS_REST("(", Rest), State) ->
%     {_, {Rest1, State1}} = maybe_consume_s(Rest, State, ?EMPTY),
%     case peek(Rest1, State1) of
%         {$#, {Rest2, State2}} ->
%             parse_Mixed(Rest2, State2);
%         {_, {Rest2, State2}} ->
%             parse_children(Rest2, State2)
%     end;
% parse_contentspec(Stream, State) when
%     Stream == ?CHARS("E");
%     Stream == ?CHARS("EM");
%     Stream == ?CHARS("EMP");
%     Stream == ?CHARS("EMPT");
%     Stream == ?CHARS("A");
%     Stream == ?CHARS("AN")
% ->
%     case cf(Stream, State) of
%         {error, Reason, PState1} ->
%             fatal_error(Reason, PState1);
%         {Stream, _} = PState1 ->
%             fatal_error(bad_element, PState1);
%         {Bytes, State1} ->
%             parse_contentspec(Bytes, State1)
%     end;
% parse_contentspec(?CHARS_REST(_, _), State) ->
%     fatal_error(bad_element, State);
% ?EMPTY1;
% ?CHECK1.

% %%----------------------------------------------------------------------
% %% [47] children ::= (choice | seq) ('?' | '*' | '+')?
% %% [48] cp       ::= (Name | choice | seq) ('?' | '*' | '+')?
% %% [18ns] cp     ::= (QName | choice | seq) ('?' | '*' | '+')?
% %% [49] choice   ::= '(' S? cp ( S? '|' S? cp )+ S? ')'
% %% [50] seq      ::= '(' S? cp ( S? ',' S? cp )* S? ')'
% %% '(' has been trimmed, one char on stream
% %%----------------------------------------------------------------------
% parse_children(Stream, State) ->
%     {Acc, {Rest1, State1}} = parse_seq(Stream, State),
%     parse_quantifier(Rest1, State1, Acc).

% %%----------------------------------------------------------------------
% %% [49] choice   ::= '(' S? cp ( S? '|' S? cp )+ S? ')'
% %% [50] seq      ::= '(' S? cp ( S? ',' S? cp )* S? ')'
% %% if a choice and not seq, passes to parse_choice
% %%----------------------------------------------------------------------
% parse_seq(Stream, State) ->
%     {Cp, {Rest1, State1}} = parse_cp(Stream, State),
%     {_, {Rest2, State2}} = maybe_consume_s(Rest1, State1, ?EMPTY),
%     case peek(Rest2, State2) of
%         {$|, {Rest3, State3}} ->
%             parse_choice(Rest3, State3, [Cp]);
%         {$,, {Rest3, State3}} ->
%             parse_seq_1(Rest3, State3, [Cp]);
%         {$), {Rest3, State3}} ->
%             parse_seq_1(Rest3, State3, [Cp])
%     end.

% parse_seq_1(?CHARS_REST(")", Rest), State, Acc) ->
%     {{seq, lists:reverse(Acc)}, {Rest, State}};
% parse_seq_1(?CHARS_REST(",", Rest), State, Acc) ->
%     {_, {Rest1, State1}} = maybe_consume_s(Rest, State, ?EMPTY),
%     {Cp, {Rest2, State2}} = parse_cp(Rest1, State1),
%     {_, {Rest3, State3}} = maybe_consume_s(Rest2, State2, ?EMPTY),
%     parse_seq_1(Rest3, State3, [Cp | Acc]);
% parse_seq_1(?CHARS_REST(_, _), State, _) ->
%     fatal_error(bad_element, State);
% ?EMPTY2;
% ?CHECK2.

% %%----------------------------------------------------------------------
% %% [49] choice   ::= '(' S? cp ( S? '|' S? cp )+ S? ')'
% %%----------------------------------------------------------------------
% parse_choice(?CHARS_REST(")", _), State, [_]) ->
%     fatal_error(bad_element, State);
% parse_choice(?CHARS_REST(")", Rest), State, Acc) ->
%     {{choice, lists:reverse(Acc)}, {Rest, State}};
% parse_choice(?CHARS_REST(",", Rest), State, Acc) ->
%     {_, {Rest1, State1}} = maybe_consume_s(Rest, State, ?EMPTY),
%     {Cp, {Rest2, State2}} = parse_cp(Rest1, State1),
%     {_, {Rest3, State3}} = maybe_consume_s(Rest2, State2, ?EMPTY),
%     parse_choice(Rest3, State3, [Cp | Acc]);
% parse_choice(?CHARS_REST(_, _), State, _) ->
%     fatal_error(bad_element, State);
% ?EMPTY2;
% ?CHECK2.

% %%----------------------------------------------------------------------
% %% [48] cp       ::= (Name | choice | seq) ('?' | '*' | '+')?
% %%----------------------------------------------------------------------
% parse_cp(?CHARS_REST(C, _) = Stream, State) when ?is_name_start_char(C) ->
%     {Name, {Rest1, State1}} = parse_Name(Stream, State),
%     parse_quantifier(Rest1, State1, Name);
% parse_cp(?CHARS_REST("(", Rest), State) ->
%     {_, {Rest1, State1}} = maybe_consume_s(Rest, State, ?EMPTY),
%     {Seq, {Rest2, State2}} = parse_seq(Rest1, State1),
%     parse_quantifier(Rest2, State2, Seq);
% parse_cp(?CHARS_REST(_, _), State) ->
%     fatal_error(bad_element, State);
% ?EMPTY1;
% ?CHECK1.

% parse_quantifier(?CHARS_REST("?", Rest), State, Acc) ->
%     {{Acc, '?'}, {Rest, State}};
% parse_quantifier(?CHARS_REST("*", Rest), State, Acc) ->
%     {{Acc, '*'}, {Rest, State}};
% parse_quantifier(?CHARS_REST("+", Rest), State, Acc) ->
%     {{Acc, '+'}, {Rest, State}};
% parse_quantifier(?CHARS_REST(_, _) = Stream, State, Acc) ->
%     {{Acc, one}, {Stream, State}};
% ?EMPTY2;
% ?CHECK2.

% %%----------------------------------------------------------------------
% %% [51] Mixed   ::= '(' S? '#PCDATA' (S? '|' S? Name)* S? ')*' | '(' S? '#PCDATA' S? ')'
% %% [19ns] Mixed ::= '(' S? '#PCDATA' (S? '|' S? QName)* S? ')*' | '(' S? '#PCDATA' S? ')'
% %% '(' has been trimmed '#' is in stream
% %%----------------------------------------------------------------------
% parse_Mixed(?CHARS_REST("#PCDATA", Rest), State) ->
%     {_, {Rest1, State1}} = maybe_consume_s(Rest, State, ?EMPTY),
%     case peek(Rest1, State1) of
%         {$|, {Rest2, State2}} ->
%             parse_Mixed_1(Rest2, State2, []);
%         {$), {Rest2, State2}} ->
%             parse_Mixed_2(Rest2, State2);
%         {_, PState} ->
%             fatal_error(bad_element, PState)
%     end;
% parse_Mixed(Stream, State) when
%     Stream == ?CHARS("#");
%     Stream == ?CHARS("#P");
%     Stream == ?CHARS("#PC");
%     Stream == ?CHARS("#PCD");
%     Stream == ?CHARS("#PCDA");
%     Stream == ?CHARS("#PCDAT");
%     Stream == ?CHARS("#PCDATA")
% ->
%     case cf(Stream, State) of
%         {error, Reason, PState1} ->
%             fatal_error(Reason, PState1);
%         {Stream, _} = PState1 ->
%             fatal_error(bad_element, PState1);
%         {Bytes, State1} ->
%             parse_Mixed(Bytes, State1)
%     end;
% parse_Mixed(?CHARS_REST(_, _), State) ->
%     fatal_error(bad_element, State);
% ?CHECK1.

% parse_Mixed_1(?CHARS_REST("|", Rest), State, Acc) ->
%     {_, {Rest1, State1}} = maybe_consume_s(Rest, State, ?EMPTY),
%     {Name, {Rest2, State2}} = parse_Name(Rest1, State1),
%     {_, {Rest3, State3}} = maybe_consume_s(Rest2, State2, ?EMPTY),
%     parse_Mixed_1(Rest3, State3, [Name | Acc]);
% parse_Mixed_1(?CHARS_REST(")*", Rest), State, Acc) ->
%     {lists:reverse(Acc), {Rest, State}};
% parse_Mixed_1(Stream, State, Acc) when Stream == ?CHARS(")") ->
%     case cf(Stream, State) of
%         {error, Reason, PState1} ->
%             fatal_error(Reason, PState1);
%         {Stream, _} = PState1 ->
%             fatal_error(bad_element, PState1);
%         {Bytes, State1} ->
%             parse_Mixed_1(Bytes, State1, Acc)
%     end;
% parse_Mixed_1(?CHARS_REST(_, _), State, _) ->
%     fatal_error(bad_element, State);
% ?EMPTY2;
% ?CHECK2.

% resolve_parameter_entity(PERef, #{proc := #{params := Params}} = _DTD, State) ->
%     case Params of
%         #{PERef := {external, PubSys}} ->
%             %% XXX maybe resolve then add the value to the DTD to cache it
%             {external, PubSys, State};
%         #{PERef := {internal, Value}} ->
%             {internal, Value, State};
%         _ ->
%             unknown
%     end;
% resolve_parameter_entity(_, _, _) ->
%     unknown.

% parse_Mixed_2(?CHARS_REST(")", Rest), State) ->
%     {mixed, {Rest, State}}.

% add_attlist_to_dtd({ElemName, AttDefs}, #{proc := #{atts := Atts} = Proc} = DTD) ->
%     DTD#{proc := Proc#{atts := Atts#{ElemName => AttDefs}}};
% add_attlist_to_dtd(AttList, #{proc := undefined} = DTD) ->
%     add_attlist_to_dtd(AttList, DTD#{proc => empty_proc_dtd()}).

% add_notation_to_dtd({Name, Pub, Sys}, #{proc := #{nots := Nots} = Proc} = DTD) ->
%     DTD#{proc := Proc#{nots := Nots#{Name => {Pub, Sys}}}};
% add_notation_to_dtd(Notation, #{proc := undefined} = DTD) ->
%     add_notation_to_dtd(Notation, DTD#{proc => empty_proc_dtd()}).

% %{Name, {general|parameter, internal|external|unparsed, Value}}
% add_entity_to_dtd({Name, {general, Type, Value}}, #{proc := #{refs := Ents} = Proc} = DTD) ->
%     DTD#{proc := Proc#{refs := Ents#{Name => {Type, Value}}}};
% add_entity_to_dtd({Name, {parameter, Type, Value}}, #{proc := #{params := Ents} = Proc} = DTD) ->
%     DTD#{proc := Proc#{params := Ents#{Name => {Type, Value}}}};
% add_entity_to_dtd(Entity, #{proc := undefined} = DTD) ->
%     add_entity_to_dtd(Entity, DTD#{proc => empty_proc_dtd()}).

% add_element_to_dtd({Name, Elem}, #{proc := #{elems := Elems} = Proc} = DTD) ->
%     DTD#{proc := Proc#{elems := Elems#{Name => Elem}}};
% add_element_to_dtd(Elem, #{proc := undefined} = DTD) ->
%     add_element_to_dtd(Elem, DTD#{proc => empty_proc_dtd()}).

% empty_proc_dtd() ->
%     #{
%         elems => #{},
%         atts => #{},
%         nots => #{},
%         refs => #{},
%         params => #{}
%     }.
