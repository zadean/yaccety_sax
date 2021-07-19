-module(ys_parse_simple).

-export([
    parse_content/2,
    parse_Misc/2,
    parse_element/2,
    parse_XMLDecl/2
]).
-export([
    event_endDocument/1,
    event_endElement/2
]).


-include("yaccety_sax_simple.hrl").

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

-define(XML_NS, <<"http://www.w3.org/XML/1998/namespace">>).
-define(XMLNS_NS, <<"http://www.w3.org/2000/xmlns/">>).

-define(WS(Char), (Char == 16#20 orelse Char == 16#9 orelse Char == 16#A orelse Char == 16#D)).

-compile({inline, [append/2, set_state_pos/2, to_binary/1, fatal_error/2]}).

append(<<>>, Acc) -> Acc;
append(Thing, []) -> Thing;
append(Thing, Acc) -> [Acc, Thing].

%%----------------------------------------------------------------------
%% XML character range
%% [2] Char ::= #x9 | #xA | #xD | [#x20-#xD7FF] | [#xE000-#xFFFD] |
%%              [#x10000-#x10FFFF]
%% any Unicode character, excluding the surrogate blocks, FFFE, and FFFF.
%%----------------------------------------------------------------------
-define(ONECHAR,
?FUNCTION_NAME(<<16#9, Rest/bitstring>>, Stream, Pos, Len, State, Acc) ->
    ?FUNCTION_NAME(Rest, Stream, Pos, Len + 1, State, Acc);
?FUNCTION_NAME(<<16#A, Rest/bitstring>>, Stream, Pos, Len, State, Acc) ->
    ?FUNCTION_NAME(Rest, Stream, Pos, Len + 1, State, Acc);
?FUNCTION_NAME(<<Char, _/bitstring>>, _, _, _, State, _) when Char < 16#20  ->
    fatal_error({invalid_character, {?LINE, [Char]}}, State);
?FUNCTION_NAME(<<Char, Rest/bitstring>>, Stream, Pos, Len, State, Acc) when Char < 16#80 ->
    ?FUNCTION_NAME(Rest, Stream, Pos, Len + 1, State, Acc);
?FUNCTION_NAME(<<Char/utf8, Rest/bitstring>>, Stream, Pos, Len, State, Acc)  ->
    if
        Char < 16#800 ->
            ?FUNCTION_NAME(Rest, Stream, Pos, Len + 2, State, Acc);
        Char == 16#FFFE; Char == 16#FFFF ->
            fatal_error({invalid_character, {?LINE, [Char]}}, State);
        Char < 16#10000 ->
            ?FUNCTION_NAME(Rest, Stream, Pos, Len + 3, State, Acc);
        true ->
            ?FUNCTION_NAME(Rest, Stream, Pos, Len + 4, State, Acc)
    end;
?FUNCTION_NAME(Bytes, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    {to_binary(Acc1), Bytes, Stream, Pos + Len, State}
).

%%----------------------------------------------------------------------
%% Consume whitespace characters
%% params:  State
%% returns: {NewPos, NewState} | {error, non_whitespace}
%% [3] S ::= (#x20 | #x9 | #xD | #xA)+
%%----------------------------------------------------------------------
maybe_consume_s(<<Char, Rest/bitstring>>, Stream, Pos, State) when
    Char == 16#20; Char == 16#9; Char == 16#D; Char == 16#A
->
    maybe_consume_s(Rest, Stream, Pos + 1, State, true);
maybe_consume_s(Part, Stream, Pos, State) ->
    {false, Part, Stream, Pos, State}.

maybe_consume_s(<<Char, Rest/bitstring>>, Stream, Pos, State, _) when
    Char == 16#20; Char == 16#9; Char == 16#D; Char == 16#A
->
    maybe_consume_s(Rest, Stream, Pos + 1, State, true);
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
parse_Name(Bytes, _, _, State) ->
    fatal_error(bad_name, {Bytes, State}).

parse_Name(<<Char/utf8, Rest/bitstring>>, Stream, Pos, Len, State, Acc) when
    Char == $-;
    Char == $.;
    Char >= $0, Char =< $9;
    Char == $:;
    Char >= $A, Char =< $Z;
    Char == $_;
    Char >= $a, Char =< $z
->
    parse_Name(Rest, Stream, Pos, Len + 1, State, Acc);
parse_Name(<<Char/utf8, Rest/bitstring>>, Stream, Pos, Len, State, Acc) when
    Char == 16#B7;
    Char >= 16#C0, Char =< 16#D6;
    Char >= 16#D8, Char =< 16#F6;
    Char >= 16#F8, Char =< 16#37D;
    Char >= 16#37F, Char =< 16#7FF
->
    parse_Name(Rest, Stream, Pos, Len + 2, State, Acc);
parse_Name(<<Char/utf8, Rest/bitstring>>, Stream, Pos, Len, State, Acc) when
    Char >= 16#800, Char =< 16#1FFF;
    Char >= 16#200C, Char =< 16#200D;
    Char >= 16#203F, Char =< 16#2040;
    Char >= 16#2070, Char =< 16#218F;
    Char >= 16#2C00, Char =< 16#2FEF;
    Char >= 16#3001, Char =< 16#D7FF;
    Char >= 16#F900, Char =< 16#FDCF;
    Char >= 16#FDF0, Char =< 16#FFFD
->
    parse_Name(Rest, Stream, Pos, Len + 3, State, Acc);
parse_Name(<<Char/utf8, Rest/bitstring>>, Stream, Pos, Len, State, Acc) when
    Char >= 16#10000, Char =< 16#EFFFF
->
    parse_Name(Rest, Stream, Pos, Len + 4, State, Acc);
parse_Name(Bytes, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    {to_binary(Acc1), Bytes, Stream, Pos + Len, State}.

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
parse_Reference(<<$#, $x, Rest/bitstring>>, Stream, Pos, State, _Type) ->
    parse_Reference_hex(Rest, Stream, Pos + 2, 0, State, []);
parse_Reference(<<$#, Rest/bitstring>>, Stream, Pos, State, _Type) ->
    parse_Reference_dec(Rest, Stream, Pos + 1, 0, State, []);
parse_Reference(?MATCH, Type) ->
    parse_Reference_name(?MATCH, Type).

% hex parse until ';' return char
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
    {_, _, _, _, State1} = parse_Comment(Bytes, Stream, Pos, 0, State, []),
    State1.

parse_Comment(<<$\r, $\n, Rest/bitstring>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    Acc2 = ?APPEND(<<$\n>>, Acc1),
    ?FUNCTION_NAME(Rest, Stream, Pos + Len + 2, 0, State, Acc2);
parse_Comment(<<$\r, Rest/bitstring>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    Acc2 = ?APPEND(<<$\n>>, Acc1),
    ?FUNCTION_NAME(Rest, Stream, Pos + Len + 1, 0, State, Acc2);
parse_Comment(<<$-/utf8, $-/utf8, $>/utf8, Rest/bitstring>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    {Acc1, Rest, Stream, Pos + Len + 3, State};
parse_Comment(<<$-/utf8, $-/utf8, _/bitstring>>, _, _, _, State, _Acc) ->
    fatal_error(bad_comment, State);
?ONECHAR.

%%----------------------------------------------------------------------
%% Parse character data. In content, everything that is not
%%   (element | Reference | CDSect | PI | Comment)
%% params:  State
%% returns: {CharData, IsWs, NewState}
%% [14] CharData ::= [^<&]* - ([^<&]* ']]>' [^<&]*)
%%----------------------------------------------------------------------
parse_CharData(?MATCH) ->
    {{IsWs, Text}, Bytes1, _, _, State1} = parse_CharData_ws(Bytes, Stream, Pos, 0, State, []),
    State2 = set_state_pos(State1, Bytes1),
    case IsWs of
        true ->
            State2;
        _ ->
            event_characters(to_binary(Text), State2)
    end.

parse_CharData(?MATCH, Acc) ->
    {{_, Text}, Bytes1, _, _, State1} = parse_CharData(Bytes, Stream, Pos, 0, State, Acc),
    event_characters(to_binary(Text), set_state_pos(State1, Bytes1)).

parse_CharData(<<$\r, $\n, Rest/bitstring>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    Acc2 = ?APPEND(<<$\n>>, Acc1),
    parse_CharData(Rest, Stream, Pos + Len + 2, 0, State, Acc2);
parse_CharData(<<$\r, Rest/bitstring>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    Acc2 = ?APPEND(<<$\n>>, Acc1),
    parse_CharData(Rest, Stream, Pos + Len + 1, 0, State, Acc2);
parse_CharData(Bytes = <<$</utf8, _/bitstring>>, Stream, Pos, Len, State, Acc) ->
    Text = ?ACC(Stream, Pos, Len, Acc),
    {{false, Text}, Bytes, Stream, Pos + Len, State};
parse_CharData(<<$&/utf8, Rest/bitstring>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    {{_, Ref}, ?MATCH1} = parse_Reference(Rest, Stream, Pos + Len + 1, State, text),
    Acc2 = ?APPEND(Ref, Acc1),
    ?FUNCTION_NAME(Bytes1, Stream1, Pos1, 0, State1, Acc2);
parse_CharData(<<"]]>"/utf8, _/bitstring>>, _, _, _, State, _Acc) ->
    fatal_error(bad_char_data, State);
?ONECHAR.

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
parse_CharData_ws(<<$\r, $\n, Rest/bitstring>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    Acc2 = ?APPEND(<<$\n>>, Acc1),
    parse_CharData_ws(Rest, Stream, Pos + Len + 2, 0, State, Acc2);
parse_CharData_ws(<<$\r, Rest/bitstring>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    Acc2 = ?APPEND(<<$\n>>, Acc1),
    parse_CharData_ws(Rest, Stream, Pos + 1, 0, State, Acc2);
parse_CharData_ws(<<Char, Rest/bitstring>>, Stream, Pos, Len, State, Acc) when ?WS(Char) ->
    parse_CharData_ws(Rest, Stream, Pos, Len + 1, State, Acc);
parse_CharData_ws(Bytes = <<$<, _/bitstring>>, Stream, Pos, Len, State, Acc) ->
    Text = ?ACC(Stream, Pos, Len, Acc),
    {{true, Text}, Bytes, Stream, Pos + Len, State};
parse_CharData_ws(<<$&, Rest/bitstring>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    {{_, Ref}, ?MATCH1} = parse_Reference(Rest, Stream, Pos + Len + 1, State, text),
    Acc2 = ?APPEND(Ref, Acc1),
    parse_CharData(Bytes1, Stream1, Pos1, 0, State1, Acc2);
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
parse_CDSect(_, _, _, State) ->
    fatal_error(bad_cdata, State).

parse_CData(Rest, Stream, Pos, State) ->
    {Text, Bytes1, _, _, State1} = parse_CData(Rest, Stream, Pos, 0, State, []),
    event_characters(to_binary(Text), set_state_pos(State1, Bytes1)).

parse_CData(<<$\r, $\n, Rest/bitstring>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    Acc2 = ?APPEND(<<$\n>>, Acc1),
    ?FUNCTION_NAME(Rest, Stream, Pos + Len + 2, 0, State, Acc2);
parse_CData(<<$\r, Rest/bitstring>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    Acc2 = ?APPEND(<<$\n>>, Acc1),
    ?FUNCTION_NAME(Rest, Stream, Pos + Len + 1, 0, State, Acc2);
parse_CData(<<"]]>"/utf8, Rest/bitstring>>, Stream, Pos, Len, State, Acc) ->
    Text = ?ACC(Stream, Pos, Len, Acc),
    {Text, Rest, Stream, Pos + Len + 3, State};
?ONECHAR.

%%----------------------------------------------------------------------
%% [80] EncodingDecl ::= S 'encoding' Eq ('"' EncName '"' | "'" EncName "'" )
%%----------------------------------------------------------------------
parse_EncodingDecl(?MATCH) ->
    parse_EncodingDeclS(?MATCH).

parse_EncodingDeclS(<<"encoding"/utf8, Rest/bitstring>>, Stream, Pos, State) ->
    parse_EncodingDecl_encoding(Rest, Stream, Pos + 8, State);
parse_EncodingDeclS(?MATCH) ->
    % no encoding
    {{<<"UTF-8">>, false}, ?MATCH}.

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
parse_EncodingDecl_EncName_name(<<Char/utf8, Rest/bitstring>>, Stream, Pos, State) when
    Char >= $A andalso Char =< $Z; Char >= $a andalso Char =< $z
->
    parse_EncodingDecl_EncName_name_1(Rest, Stream, Pos, 1, State, []);
parse_EncodingDecl_EncName_name(_, _, _, State) ->
    fatal_error(bad_char, State).

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
parse_Eq(<<$=, Rest/bitstring>>, Stream, Pos, State) ->
    {_Found, ?MATCH1} = maybe_consume_s(Rest, Stream, Pos + 1, State),
    {?MATCH1};
parse_Eq(?MATCH) ->
    {_Found, ?MATCH1} = maybe_consume_s(?MATCH),
    parse_Eq_1(?MATCH1).

parse_Eq_1(<<$=, Rest/bitstring>>, Stream, Pos, State) ->
    {_Found, ?MATCH1} = maybe_consume_s(Rest, Stream, Pos + 1, State),
    {?MATCH1};
parse_Eq_1(_, _, _, State) ->
    fatal_error(bad_eq, State).

%%----------------------------------------------------------------------
%% [26] VersionNum ::= '1.' [0-9]+
%%----------------------------------------------------------------------
parse_VersionNum_sq(<<$1/utf8, $./utf8, Rest/bitstring>>, Stream, Pos, State) ->
    parse_VersionNum_digit_sq(Rest, Stream, Pos + 2, 0, State, []);
parse_VersionNum_sq(_, _, _, State) ->
    fatal_error(bad_version_num, State).

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
parse_VersionNum_dq(_, _, _, State) ->
    fatal_error(bad_version_num, State).

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
parse_VersionInfoS(?MATCH) ->
    {{<<>>, false}, ?MATCH}.

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
parse_SDDecl_standalone_yesno(_, _, _, State) ->
    fatal_error(bad_standalone, State).

parse_SDDecl_standalone_yesno_sq(<<"no'"/utf8, Rest/bitstring>>, Stream, Pos, State) ->
    {{false, true}, Rest, Stream, Pos + 3, State};
parse_SDDecl_standalone_yesno_sq(<<"yes'"/utf8, Rest/bitstring>>, Stream, Pos, State) ->
    {{true, true}, Rest, Stream, Pos + 4, State}.

parse_SDDecl_standalone_yesno_dq(<<"no\""/utf8, Rest/bitstring>>, Stream, Pos, State) ->
    {{false, true}, Rest, Stream, Pos + 3, State};
parse_SDDecl_standalone_yesno_dq(<<"yes\""/utf8, Rest/bitstring>>, Stream, Pos, State) ->
    {{true, true}, Rest, Stream, Pos + 4, State}.

parse_SDDecl(<<"standalone"/utf8, Rest/bitstring>>, Stream, Pos, State) ->
    parse_SDDecl_standalone(Rest, Stream, Pos + 10, State);
parse_SDDecl(?MATCH) ->
    {{false, false}, ?MATCH}.

%%----------------------------------------------------------------------
%% [23] XMLDecl ::= '<?xml' VersionInfo EncodingDecl? SDDecl? S? '?>'
%%----------------------------------------------------------------------
parse_XMLDecl_ltqxml(?MATCH) ->
    {{Version, true}, ?MATCH1} = parse_VersionInfo(?MATCH),
    {IsWs2, ?MATCH2} = maybe_consume_s(?MATCH1),
    {{Encoding, EncSet}, ?MATCH3} = parse_EncodingDecl(?MATCH2),
    {{_, _}, ?MATCH6} =
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
    State9 = set_next_parser_position(misc_post_dtd, State8),
    event_startDocument(
        Version,
        Encoding,
        set_state_pos(State9, Bytes8)
    ).

parse_XMLDecl_end(<<"?>"/utf8, Rest/bitstring>>, Stream, Pos, State) ->
    {Rest, Stream, Pos + 2, State};
parse_XMLDecl_end(Bytes, _, _, State) ->
    fatal_error(bad_xmldecl, {Bytes, State}).

parse_XMLDecl(Stream, State) -> parse_XMLDecl(Stream, Stream, 0, State).

parse_XMLDecl(<<"<?xml"/utf8, Rest/bitstring>>, Stream, Pos, State) ->
    parse_XMLDecl_ltqxml(Rest, Stream, Pos + 5, State);
parse_XMLDecl(Bytes, _, _, State) ->
    % default declaration
    State1 = set_next_parser_position(misc_post_dtd, State),
    event_startDocument(
        <<"1.0">>,
        <<"UTF-8">>,
        set_state_pos(State1, Bytes)
    ).

%%----------------------------------------------------------------------
%% [27] Misc ::= Comment | PI | S
%%----------------------------------------------------------------------
parse_Misc(Stream, State) -> parse_Misc(Stream, Stream, 0, State).

parse_Misc(<<"<!--"/utf8, Rest/bitstring>>, Stream, Pos, State) ->
    parse_Comment(Rest, Stream, Pos + 4, State);
parse_Misc(<<>>, _, _, State) ->
    {no_bytes, State};
parse_Misc(?MATCH) ->
    case maybe_consume_s(?MATCH) of
        {true, ?MATCH1} ->
            parse_Misc(?MATCH1);
        {false, Bytes1, _, _, State1} ->
            set_state_pos(State1, Bytes1)
    end.

%%----------------------------------------------------------------------
%% [43] content ::= CharData? ((element | Reference | CDSect | PI | Comment) CharData?)*
%%----------------------------------------------------------------------
parse_content(Stream, State) -> parse_content(Stream, Stream, 0, State).

parse_content(<<"</", Rest/bitstring>>, Stream, Pos, State) ->
    parse_ETag(Rest, Stream, Pos + 2, State);
parse_content(<<"<!--", Rest/bitstring>>, Stream, Pos, State) ->
    parse_Comment(Rest, Stream, Pos + 4, State);
parse_content(<<"<![", Rest/bitstring>>, Stream, Pos, State) ->
    parse_CDSect(Rest, Stream, Pos + 3, State);
parse_content(<<$<, Rest/bitstring>>, Stream, Pos, State) ->
    parse_element_lt(Rest, Stream, Pos + 1, State);
parse_content(<<$&, Rest/bitstring>>, Stream, Pos, State) ->
    {{_, Ref}, ?MATCH1} = parse_Reference(Rest, Stream, Pos + 1, State, content),
    parse_CharData(?MATCH1, Ref);
parse_content(?MATCH) ->
    parse_CharData(?MATCH).

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

parse_element(<<$<, Rest/bitstring>>, Stream, Pos, State) ->
    parse_element_lt(Rest, Stream, Pos + 1, State);
parse_element(B, _, _, State) ->
    fatal_error(non_element, {B, State}).

parse_element_lt(
    Bytes,
    Stream,
    Pos,
    #ys_state_simple{
        tags = Tags,
        inscope_ns = [LastNss | _] = Nss,
        position = P
    } = State
) ->
    {{NameP, NameL}, ?MATCH1} = parse_QName(?MATCH),
    case Bytes1 of
        <<$>, Bytes2/bitstring>> ->
            QName = expand_qname(NameP, NameL, LastNss),
            State2 = #ys_state_simple{
                position = [content | P],
                tags = [QName | Tags],
                inscope_ns = [LastNss | Nss],
                rest_stream = Bytes2
            },
            event_startElement(QName, [], [], State2);
        <<$/, Rest/bitstring>> ->
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
                {NameP, NameL}
            ),
            NewNsMap = namespace_map_from_list(Ns),
            NamespaceMap = merge_namespaces(LastNss, NewNsMap),
            QName = expand_qname(NameP, NameL, NamespaceMap),
            As1 = qualify_attribute_names(As, NamespaceMap),
            ok = check_attributes(As1, State2),
            case Bytes2 of
                <<$>, Bytes3/bitstring>> ->
                    State3 = #ys_state_simple{
                        position = [content | P],
                        tags = [QName | Tags],
                        inscope_ns = [NamespaceMap | Nss],
                        rest_stream = Bytes3
                    },
                    event_startElement(QName, As1, Ns, State3);
                <<$/, Rest/bitstring>> ->
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

% misc_post_element
parse_element_empty(<<$>, Bytes/bitstring>>, _, _, State, QName, Ats, Nss, P, Tags) ->
    Pss =
        case P of
            % Empty root element
            [element | Ps] -> [empty, misc_post_element | Ps];
            _ -> [empty | P]
        end,
    State1 = State#ys_state_simple{
        position = Pss,
        tags = [QName | Tags],
        rest_stream = Bytes
    },
    event_startElement(QName, Ats, Nss, State1);
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

parse_attributes(Bytes = <<$>, _/bitstring>>, Stream, Pos, State, Nss, Atts, _) ->
    {{Nss, Atts}, ?MATCH};
parse_attributes(?MATCH, Nss, Atts, EName) ->
    case maybe_consume_s(?MATCH) of
        {false, ?MATCH1} ->
            {{Nss, Atts}, ?MATCH1};
        {true, <<C, _/bitstring>> = ?MATCH1} when C == $>; C == $/ ->
            {{Nss, Atts}, ?MATCH1};
        {true, ?MATCH1} ->
            {Att, ?MATCH2} = parse_Attribute(?MATCH1),
            parse_attributes_(Att, ?MATCH2, Nss, Atts, EName)
    end.

parse_attributes_({<<>>, <<"xmlns">>, ?XML_NS}, _, _, _, State, _, _, _) ->
    fatal_error(rebinding_prefix, {<<"xml">>, State});
parse_attributes_({<<>>, <<"xmlns">>, ?XMLNS_NS}, _, _, _, State, _, _, _) ->
    fatal_error(rebinding_prefix, {<<"xmlns">>, State});
parse_attributes_({<<>>, <<"xmlns">>, Val}, ?MATCH, Nss, Atts, EName) ->
    parse_attributes(?MATCH, [{normalize_attribute_value(Val), <<>>} | Nss], Atts, EName);
parse_attributes_({<<"xmlns">>, Px, <<>>}, _, _, _, State, _, _, _) ->
    fatal_error(unbinding_prefix, {Px, State});
parse_attributes_({<<"xmlns">>, <<"xml">>, Val}, _, _, _, State, _, _, _) when Val =/= ?XML_NS ->
    fatal_error(rebinding_prefix, {<<"xml">>, State});
parse_attributes_({<<"xmlns">>, Px, ?XML_NS}, _, _, _, State, _, _, _) when Px =/= <<"xml">> ->
    fatal_error(rebinding_prefix, {<<"xml">>, State});
parse_attributes_({<<"xmlns">>, _, ?XMLNS_NS}, _, _, _, State, _, _, _) ->
    fatal_error(rebinding_prefix, {<<"xmlns">>, State});
parse_attributes_({<<"xmlns">>, <<"xmlns">>, _}, _, _, _, State, _, _, _) ->
    fatal_error(rebinding_prefix, {<<"xmlns">>, State});
parse_attributes_({<<"xmlns">>, Px, Val}, ?MATCH, Nss, Atts, EName) ->
    parse_attributes(?MATCH, [{normalize_attribute_value(Val), Px} | Nss], Atts, EName);
parse_attributes_({Px, Ln, Val}, ?MATCH, Nss, Atts, EName) ->
    Norm = normalize_attribute_value(Val),
    parse_attributes(?MATCH, Nss, [{Px, Ln, Norm} | Atts], EName).

%%----------------------------------------------------------------------
%% [10] AttValue ::= '"' ([^<&"] | Reference)* '"' |  "'" ([^<&'] | Reference)* "'"
%%----------------------------------------------------------------------
parse_AttValue(<<$', Rest/bitstring>>, Stream, Pos, State) ->
    parse_AttValue_sq(Rest, Stream, Pos + 1, 0, State, []);
parse_AttValue(<<$", Rest/bitstring>>, Stream, Pos, State) ->
    parse_AttValue_dq(Rest, Stream, Pos + 1, 0, State, []);
parse_AttValue(_, _, _, State) ->
    fatal_error(bad_attval, State).

parse_AttValue_sq(<<$\r, $\n, Rest/bitstring>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    Acc2 = ?APPEND(<<$\s>>, Acc1),
    ?FUNCTION_NAME(Rest, Stream, Pos + Len + 2, 0, State, Acc2);
parse_AttValue_sq(<<$', Rest/bitstring>>, Stream, Pos, Len, State, Acc) ->
    Value = ?ACC(Stream, Pos, Len, Acc),
    {to_binary(Value), Rest, Stream, Pos + Len + 1, State};
parse_AttValue_sq(<<$<, _/bitstring>>, _, _, _, State, _Acc) ->
    fatal_error(bad_attval, State);
parse_AttValue_sq(<<$&, Rest/bitstring>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    {{_, Ref}, ?MATCH1} = parse_Reference(Rest, Stream, Pos + Len + 1, State, attribute),
    Acc2 = ?APPEND(Ref, Acc1),
    ?FUNCTION_NAME(Bytes1, Stream1, Pos1, 0, State1, Acc2);
parse_AttValue_sq(<<Char, Rest/bitstring>>, Stream, Pos, Len, State, Acc) when ?WS(Char) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    Acc2 = ?APPEND(<<$\s>>, Acc1),
    ?FUNCTION_NAME(Rest, Stream, Pos + Len + 1, 0, State, Acc2);
?ONECHAR.

parse_AttValue_dq(
    <<$\r, $\n, Rest/bitstring>>, Stream, Pos, Len, State, Acc
) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    Acc2 = ?APPEND(<<$\s>>, Acc1),
    ?FUNCTION_NAME(Rest, Stream, Pos + Len + 2, 0, State, Acc2);
parse_AttValue_dq(<<$", Rest/bitstring>>, Stream, Pos, Len, State, Acc) ->
    Value = ?ACC(Stream, Pos, Len, Acc),
    {to_binary(Value), Rest, Stream, Pos + Len + 1, State};
parse_AttValue_dq(<<$<, _/bitstring>>, _, _, _, State, _Acc) ->
    fatal_error(bad_attval, State);
parse_AttValue_dq(<<$&, Rest/bitstring>>, Stream, Pos, Len, State, Acc) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    {{_, Ref}, ?MATCH1} = parse_Reference(Rest, Stream, Pos + Len + 1, State, attribute),
    Acc2 = ?APPEND(Ref, Acc1),
    ?FUNCTION_NAME(Bytes1, Stream1, Pos1, 0, State1, Acc2);
parse_AttValue_dq(<<Char, Rest/bitstring>>, Stream, Pos, Len, State, Acc) when ?WS(Char) ->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    Acc2 = ?APPEND(<<$\s>>, Acc1),
    ?FUNCTION_NAME(Rest, Stream, Pos + Len + 1, 0, State, Acc2);
?ONECHAR.

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
    #ys_state_simple{inscope_ns = [Ns | Nss], position = [_, element | _Ps1], tags = [Tag | Ts]} =
        State
) ->
    {{NameP, NameL}, ?MATCH1} = parse_QName(?MATCH),
    {_, Bytes2, _, _, State2} = maybe_consume_s(?MATCH1),
    case Bytes2 of
        <<$>, Bytes3/bitstring>> ->
            QName = expand_qname(NameP, NameL, Ns),
            case QName of
                Tag ->
                    State3 = #ys_state_simple{
                        rest_stream = Bytes3,
                        inscope_ns = Nss,
                        position = [misc_post_element],
                        tags = Ts
                    },
                    event_endElement(QName, State3);
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
    #ys_state_simple{inscope_ns = [Ns | Nss], position = [_ | Ps], tags = [Tag | Ts]} = State
) ->
    {{NameP, NameL}, ?MATCH1} = parse_QName(?MATCH),
    case Bytes1 of
        <<$>, Bytes2/bitstring>> ->
            QName = expand_qname(NameP, NameL, Ns),
            case QName of
                Tag ->
                    State2 = #ys_state_simple{
                        rest_stream = Bytes2, inscope_ns = Nss, position = Ps, tags = Ts
                    },
                    event_endElement(QName, State2);
                _ ->
                    fatal_error(unmatched_tag, {Tag, QName})
            end;
        _ ->
            {_, Bytes2, _, _, State2} = maybe_consume_s(?MATCH1),
            case Bytes2 of
                <<$>, Bytes3/bitstring>> ->
                    QName = expand_qname(NameP, NameL, Ns),
                    case QName of
                        Tag ->
                            State3 = #ys_state_simple{
                                rest_stream = Bytes3, inscope_ns = Nss, position = Ps, tags = Ts
                            },
                            event_endElement(QName, State3);
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
parse_NCName(<<Char, Rest/bitstring>>, Stream, Pos, State) when
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

parse_NCName(<<Char, _/bitstring>> = Bytes, Stream, Pos, Len, State, Acc) when
    ?WS(Char);
    Char == $:;
    Char == $=
->
    Acc1 = ?ACC(Stream, Pos, Len, Acc),
    {to_binary(Acc1), Bytes, Stream, Pos + Len, State};
parse_NCName(<<Char, Char1, Char2, Char3, Rest/bitstring>>, Stream, Pos, Len, State, Acc) when
    ?NCNameChar(Char),
    ?NCNameChar(Char1),
    ?NCNameChar(Char2),
    ?NCNameChar(Char3)
->
    parse_NCName(Rest, Stream, Pos, Len + 4, State, Acc);
parse_NCName(<<Char, Char1, Char2, Rest/bitstring>>, Stream, Pos, Len, State, Acc) when
    ?NCNameChar(Char), ?NCNameChar(Char1), ?NCNameChar(Char2)
->
    parse_NCName(Rest, Stream, Pos, Len + 3, State, Acc);
parse_NCName(<<Char, Char1, Rest/bitstring>>, Stream, Pos, Len, State, Acc) when
    ?NCNameChar(Char), ?NCNameChar(Char1)
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
    parse_QName_(?MATCH1, Name1).

parse_QName_(<<$:, Rest/bitstring>>, Stream, Pos, State, Name1) ->
    {Name2, ?MATCH2} = parse_NCName(Rest, Stream, Pos + 1, State),
    {{to_binary(Name1), to_binary(Name2)}, ?MATCH2};
parse_QName_(?MATCH, Name1) ->
    {{<<>>, to_binary(Name1)}, ?MATCH}.

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

set_next_parser_position(Pos, #ys_state_simple{position = S} = State) ->
    State#ys_state_simple{position = [Pos | S]}.

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

set_state_pos(#ys_state_simple{position = Ps, tags = Ts, inscope_ns = Is}, Bytes) ->
    #ys_state_simple{rest_stream = Bytes, position = Ps, tags = Ts, inscope_ns = Is}.

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
resolve_general_entity(Name, _, _) ->
    {gen, <<$&, Name/binary, $;>>}.

to_binary(Bin) when is_binary(Bin) -> Bin;
to_binary(IoList) -> iolist_to_binary(IoList).

%% ====================================================================
%% Events
%% ====================================================================

event_startDocument(Version, Encoding, State) ->
    Event = {startDocument, Version, Encoding},
    {Event, State}.

event_endDocument(State) ->
    Event = endDocument,
    {Event, State}.

event_startElement(QName, Attributes, Namespaces, State) ->
    Event = {startElement, QName, Attributes, Namespaces},
    {Event, State}.

event_endElement(QName, State) ->
    Event = {endElement, QName},
    {Event, State}.

event_characters(<<>>, State) ->
    State;
event_characters(Data, State) ->
    Event = {characters, Data},
    {Event, State}.
