-module(stax).

-include("stax.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

-export([event_characters/5,
         event_comment/2,
         event_dtd/3,
         event_endDocument/1,
         event_endElement/2,
         event_processingInstruction/3,
         event_startDocument/6,
         event_startElement/4]).

-export([file/1]).

-export([get_element_text/1,
         next_event/1,
         next_tag/1
        ]).

-type parser_state() ::
        #{whitespace := boolean(),
          comments := boolean(),
          proc_inst := boolean(),
          continuation := undefined | fun(),
          tags := [qname()],
          line := pos_integer(),
          base := undefined | binary(),
          standalone := boolean(),
          inscope_ns := list(map()),
          position := list(atom()),
          dtd := undefined | map(),
          split := term()
         }.
-type ext_parser_state() :: {binary(), parser_state()}.
-export_type([ext_parser_state/0]).

-define(LINE_NUMBERS, true).

-ifdef(LINE_NUMBERS).
bump_line(#{line := L} = State) -> State#{line := L + 1}.
-define(bl(State), bump_line(State)).
-else.
-define(bl(State), State).
-endif.

file(Filename) when is_list(Filename) ->
    file(unicode:characters_to_binary(Filename));
file(Filename) ->
    State = default_state(),
    Cont = default_file_continuation(Filename),
    Base = filename:dirname(filename:absname(Filename)),
    State1 = State#{continuation := {Cont, <<>>},
                    base         := Base},
    {<<>>, State1}.

default_state() ->
    #{whitespace   => false,
      comments     => true,
      proc_inst    => true,
      continuation => undefined,
      tags         => [],
      line         => 1,
      base         => undefined,
      standalone   => false,
      inscope_ns   => [#{<<>> => <<>>,
                         <<"xml">> => <<"http://www.w3.org/XML/1998/namespace">>}],
      position     => [document],
      split        => binary:compile_pattern(<<":">>),
      dtd          => undefined
     }.

default_file_continuation(Filename) ->
    case file:open(Filename, [raw, read_ahead, read, binary]) of
        {ok, FD} ->
            fun(close) ->
                   file:close(FD);
               (_) ->
                   %case file:read(FD, 1) of
                   case file:read(FD, 64 * 1024) of
                       eof ->
                           eof;
                       {ok, Bin} ->
                           {Bin, nil};
                       {error, _} = Err ->
                           Err
                   end
            end;
        {error, _} = OpenErr ->
            OpenErr
    end.



%% -spec get_element_text(State) -> {Text, State}
%%         when State :: ext_parser_state(),
%%              Text  :: xml_characters() | {error, no_character}.
%% Returns the content of a text-only element or error if not a 
%% text-only element.
%% TODO implement
get_element_text(State) ->
    {text, State}.

-spec next_event(State) -> {Event, State}
        when State :: ext_parser_state(),
             Event :: xml_event() | {error, no_event}.
%% Returns the next event or error if no event in the stream.
next_event({Stream, #{position := []} = State}) ->
    {endDocument, {Stream, State}};
next_event({Stream, #{position := [empty|Ps],
                      tags := [T|Ts]} = State}) ->
    event_endElement(T, {Stream, State#{position := Ps,
                                        tags := Ts}});
next_event({Stream, #{position := [P|Ps]} = State}) ->
    case P of
        content -> 
            case parse_content(Stream, State) of
                {Event, {Bytes, State1}} ->
                    {Event, {Bytes, State1}};
                State1 ->
                    next_event(State1)
            end;
        document ->
            %% [1]  document ::= prolog element Misc*
            %% [22] prolog   ::= XMLDecl? Misc* (doctypedecl Misc*)?
            parse_XMLDecl(Stream, State); % sets position to misc_pre_dtd 
        fragment ->
            %% [22] prolog ::= XMLDecl? Misc* (doctypedecl Misc*)?
            parse_XMLDecl(Stream, State); % sets position to misc_pre_dtd 
        misc_pre_dtd ->
            case parse_Misc(Stream, State) of
                {Event, {Bytes, State1}} ->
                    {Event, {Bytes, State1}};
                {Stream1, State1} ->
                    next_event({Stream1, State1#{position := [dtd|Ps]}})
            end;
        dtd ->
            case parse_doctypedecl(Stream, State) of
                {#{proc := Proc} = Event, {Bytes, State1}} ->
                    {Event, {Bytes, State1#{dtd := Proc,
                                            position := [misc_post_dtd|Ps]}}};
                {Stream1, State1} ->
                    next_event({Stream1, State1#{position := [element|Ps]}})
            end;
        misc_post_dtd ->
            case parse_Misc(Stream, State) of
                {Event, {Bytes, State1}} ->
                    {Event, {Bytes, State1}};
                {Stream1, State1} ->
                    next_event({Stream1, State1#{position := [element|Ps]}})
            end;
        element -> 
            parse_element(Stream, State);
        misc_post_element ->
            case parse_Misc(Stream, State) of
                {Event, {Bytes, State1}} ->
                    {Event, {Bytes, State1}};
                {_, _} = State1 ->
                    event_endDocument(State1)
            end;
        _ ->
            {text, State}
    end.

%% -spec next_tag(State) -> {Event, State}
%%         when State :: parser_state(),
%%              Event :: xml_startElement() | xml_endElement() | 
%%                       {error, no_whitespace}.
%% Skips any insignificant space events until a startElement or 
%%  endElement is reached. If anything other than space characters are 
%%  encountered, an error is returned. This method should be used when 
%%  processing element-only content because the parser is not able to recognize 
%%  ignorable whitespace if the DTD is missing or not interpreted.
%% TODO implement
next_tag(State) ->
    {text, State}.

%% ====================================================================
%% Events
%% ====================================================================

-spec event_startDocument(Version, Encoding, EncSet, 
                          StandAlone, StandSet, State) ->
          {Event, State} when
          Version :: binary(), 
          Encoding :: binary(), 
          EncSet :: boolean(), 
          StandAlone :: boolean(), 
          StandSet :: boolean(), 
          State :: ext_parser_state(),
          Event :: xml_startDocument().
event_startDocument(Version, Encoding, EncSet, StandAlone, StandSet, 
                    {_, #{line := Line}} = State) -> 
    Event = #{type => startDocument,
              line => Line,
              version => Version,
              encoding => Encoding,
              enc_set => EncSet,
              standalone => StandAlone,
              sa_set => StandSet},
    {Event, State}.

-spec event_endDocument(State) ->
          {Event, State} when
          State :: ext_parser_state(),
          Event :: xml_endDocument().
event_endDocument({Stream, #{line := Line} = State}) -> 
    Event = #{type => endDocument,
              line => Line},
    {Event, {Stream, State}}.

%xml_entityDeclaration() -> ok. % in DTD
xml_entityReference() -> ok. % only reported if not being handled
%xml_notationDeclaration() -> ok. % in DTD

-spec event_dtd(Text :: binary(),
                Processed :: map(),
                State :: ext_parser_state()) ->
          {xml_dtd(), ext_parser_state()}.
event_dtd(Text, Processed, {_, #{line := Line}} = State) -> 
    Event = #{type => dtd,
              line => Line,
              text => Text,
              proc => Processed},
    {Event, State}.

-spec event_startElement(QName, Attributes, Namespaces, State) ->
          {Event, State} when
          QName :: qname(),
          Attributes :: list(xml_attribute()),
          Namespaces :: list(xml_namespace()),
          State :: ext_parser_state(),
          Event :: xml_startElement().
event_startElement(QName, Attributes, Namespaces, {_, #{line := Line}} = State) ->
    Event = #{type => startElement,
              line => Line,
              qname => QName,
              attributes => Attributes,
              namespaces => Namespaces},
    {Event, State}.

-spec event_endElement(QName, State) ->
          {Event, State} when
          QName :: qname(),
          State :: ext_parser_state(),
          Event :: xml_endElement().
event_endElement(QName, {Stream, #{line := Line,
                                   inscope_ns := [_|Nss]} = State}) -> 
    Event = #{type => endElement,
              line => Line,
              qname => QName},
    {Event, {Stream, State#{inscope_ns := Nss}}}.

-spec event_processingInstruction(Target, Data, State) ->
          {Event, State} when
          Target :: binary(),
          Data :: binary(),
          State :: ext_parser_state(),
          Event :: xml_processingInstruction().
event_processingInstruction(Target, Data, {_, #{line := Line}} = State) ->
    Event = #{type => processingInstruction,
              line => Line,
              target => Target,
              data => Data},
    {Event, State}.

-spec event_characters(Data, CData, Ignorable, IsWs, State) ->
          {Event, State} | State when
          Data :: binary(),
          CData :: boolean(),
          Ignorable :: boolean(),
          IsWs :: boolean(),
          State :: ext_parser_state(),
          Event :: xml_characters().
event_characters(_Data, _CData, _Ignorable, true, {_, #{whitespace := false}} = State) ->
    State;
event_characters(Data, CData, Ignorable, IsWs, {_, #{line := Line}} = State) -> 
    Event = #{type => characters,
              line => Line,
              data => iolist_to_binary(Data),
              cdata => CData,
              ignore => Ignorable,
              ws => IsWs},
    {Event, State}.

-spec event_comment(Text, State) ->
          {Event, State} when
          Text :: binary(),
          State :: ext_parser_state(),
          Event :: xml_comment().
event_comment(Text, {_, #{line := Line}} = State) -> 
    Event = #{type => comment,
              line => Line,
              text => Text},
    {Event, State}.

%xml_attribute() -> ok. % in startElement
%xml_namespace() -> ok. % in startElement/endElement

%% ====================================================================
%% Internal functions
%% ====================================================================

-define(is_whitespace(C), C =:= ?space orelse C =:= ?cr 
                          orelse C =:= ?lf orelse C =:= ?tab).
-define(space, 32).
-define(cr,    13).
-define(lf,    10).
-define(tab,   9).

-define(ENCODING, utf8).
-define(EMPTY, <<>>).
-define(CHARS(Cs), <<Cs/?ENCODING>>).
-define(CHARS_REST(Cs, Rest), <<Cs/?ENCODING, Rest/binary>>).
-define(APPEND(Acc, Cs), <<Acc/binary, Cs/utf8>>).
-define(APPEND_STREAM(Acc, Cs), <<Acc/binary, Cs/binary>>).

-define(APPEND_IO(Acc, Cs), if Cs < 256 -> [Acc,Cs]; true -> [Acc,<<Cs/utf8>>] end).
-define(APPEND_IO_SMALL(Acc, Cs), [Acc,Cs]).
%-define(APPEND_IO(Acc, Cs), if Cs < 256 -> [Cs|Acc]; true -> [<<Cs/utf8>> | Acc] end).

-define(CHECK1,
        ?FUNCTION_NAME(Bytes, State) when is_binary(Bytes) ->
            case unicode:characters_to_list(Bytes, ?ENCODING) of
                {incomplete, _, _} ->
                    {C1, C2} = cf(Bytes, State),
                    ?FUNCTION_NAME(C1, C2);
                {error, _Encoded, _Rest} ->
                    {error, lists:flatten(io_lib:format("Bad character, not in ~p\n", [?ENCODING])), {Bytes, State}}
            end).

-define(CHECK2,
        ?FUNCTION_NAME(Bytes, State, A) when is_binary(Bytes) ->
            case unicode:characters_to_list(Bytes, ?ENCODING) of
                {incomplete, _, _} ->
                    {C1, C2} = cf(Bytes, State),
                    ?FUNCTION_NAME(C1, C2, A);
                {error, _Encoded, _Rest} ->
                    {error, lists:flatten(io_lib:format("Bad character, not in ~p\n", [?ENCODING])), {Bytes, State}}
            end).

-define(CHECK3,
        ?FUNCTION_NAME(Bytes, State, A, B) when is_binary(Bytes) ->
            case unicode:characters_to_list(Bytes, ?ENCODING) of
                {incomplete, _, _} ->
                    {C1, C2} = cf(Bytes, State),
                    ?FUNCTION_NAME(C1, C2, A, B);
                {error, _Encoded, _Rest} ->
                    {error, lists:flatten(io_lib:format("Bad character, not in ~p\n", [?ENCODING])), {Bytes, State}}
            end).

-define(EMPTY1,
        ?FUNCTION_NAME(?EMPTY, State) ->
            case cf(?EMPTY, State) of
                {error, Reason, PState1} ->
                    fatal_error(Reason, PState1);
                {?EMPTY, _} = PState1 ->
                    fatal_error({no_bytes, ?FUNCTION_NAME}, PState1);
                {Bytes, State1} ->
                    ?FUNCTION_NAME(Bytes, State1)
            end).

-define(EMPTY2,
        ?FUNCTION_NAME(?EMPTY, State, A) ->
            case cf(?EMPTY, State) of
                {error, Reason, PState1} ->
                    fatal_error(Reason, PState1);
                {?EMPTY, _} = PState1 ->
                    fatal_error({no_bytes, ?FUNCTION_NAME}, PState1);
                {Bytes, State1} ->
                    ?FUNCTION_NAME(Bytes, State1, A)
            end).

-define(EMPTY3,
        ?FUNCTION_NAME(?EMPTY, State, A, B) ->
            case cf(?EMPTY, State) of
                {error, Reason, PState1} ->
                    fatal_error(Reason, PState1);
                {?EMPTY, _} = PState1 ->
                    fatal_error({no_bytes, ?FUNCTION_NAME}, PState1);
                {Bytes, State1} ->
                    ?FUNCTION_NAME(Bytes, State1, A, B)
            end).

%%----------------------------------------------------------------------
%% XML character range
%% [2] Char ::= #x9 | #xA | #xD | [#x20-#xD7FF] | [#xE000-#xFFFD] | 
%%              [#x10000-#x10FFFF]
%% any Unicode character, excluding the surrogate blocks, FFFE, and FFFF.
%%----------------------------------------------------------------------
-define(is_char(C), ?space =< C, C =< 55295; C=:=?cr ; C=:=?lf ; C=:=?tab;
       57344 =< C, C =< 65533; 65536 =< C, C =< 1114111).

%% !! only use after all whitespace is captured !!
-define(is_not_char(C),
        C < 16#20 ;
        C =:= 16#FFFE ;
        C =:= 16#FFFF).

%%----------------------------------------------------------------------
%% XML name start character
%% [4] NameStartChar ::= ":" | [A-Z] | "_" | [a-z] | [#xC0-#xD6] | [#xD8-#xF6] | 
%%                       [#xF8-#x2FF] | [#x370-#x37D] | [#x37F-#x1FFF] | 
%%                       [#x200C-#x200D] | [#x2070-#x218F] | [#x2C00-#x2FEF] | 
%%                       [#x3001-#xD7FF] | [#xF900-#xFDCF] | [#xFDF0-#xFFFD] | 
%%                       [#x10000-#xEFFFF]
%%----------------------------------------------------------------------
-define(is_name_start_char(C),
        C >= $a , C =< $z ; 
        C >= $A , C =< $Z ; 
        C =:= $_ ; 
        C =:= $: ; 
        C >= 16#C0 , C =< 16#D6 ; 
        C >= 16#D8 , C =< 16#F6 ; 
        C >= 16#F8 , C =< 16#2FF ; 
        C >= 16#370 , C =< 16#37D ; 
        C >= 16#37F , C =< 16#1FFF ; 
        C >= 16#200C , C =< 16#200D ; 
        C >= 16#2070 , C =< 16#218F ; 
        C >= 16#2C00 , C =< 16#2FEF ; 
        C >= 16#3001 , C =< 16#D7FF ; 
        C >= 16#F900 , C =< 16#FDCF ; 
        C >= 16#FDF0 , C =< 16#FFFD ; 
        C >= 16#10000 , C =< 16#EFFFF
       ).

%%----------------------------------------------------------------------
%% XML name character
%% [4a] NameChar ::= NameStartChar | "-" | "." | [0-9] | #xB7 | 
%%                   [#x0300-#x036F] | [#x203F-#x2040] 
%%----------------------------------------------------------------------
-define(is_name_char(C),
        C >= $a , C =< $z ; 
        C =:= $_ ; 
        C >= $A , C =< $Z ; 
        C =:= $- ; 
        C =:= $. ; 
        C >= $0 , C =< $9 ; 
        C =:= $: ; 
        C =:= 16#B7 ; 
        C >= 16#C0 , C =< 16#D6 ; 
        C >= 16#D8 , C =< 16#F6 ; 
        C >= 16#F8 , C =< 16#2FF ; 
        C >= 16#300 , C =< 16#36F ; 
        C >= 16#370 , C =< 16#37D ; 
        C >= 16#37F , C =< 16#1FFF ; 
        C >= 16#200C , C =< 16#200D ; 
        C >= 16#203F , C =< 16#2040 ;
        C >= 16#2070 , C =< 16#218F ; 
        C >= 16#2C00 , C =< 16#2FEF ; 
        C >= 16#3001 , C =< 16#D7FF ; 
        C >= 16#F900 , C =< 16#FDCF ; 
        C >= 16#FDF0 , C =< 16#FFFD ; 
        C >= 16#10000 , C =< 16#EFFFF
       ).

%%----------------------------------------------------------------------
%% [13] PubidChar ::= #x20 | #xD | #xA | [a-zA-Z0-9] | [-'()+,./:=?;!*#@$_%]
%%----------------------------------------------------------------------
-define(is_pubid_char(C),
        C =:= 16#0A ; C =:= 16#0D ; C =:= 16#5F ;
        C >= 16#61 , C =< 16#7A ;
        C >= 16#20 , C =< 16#5A , C =/= 16#22 ,
         C =/= 16#26 , C =/= 16#3C , C =/= 16#3E).

%%----------------------------------------------------------------------
%% Consume whitespace characters
%% params:  State
%% returns: {Whitespace, NewState}
%% [3] S ::= (#x20 | #x9 | #xD | #xA)+          
%%----------------------------------------------------------------------
parse_S(Bytes, State) ->
    parse_S(Bytes, State, ?EMPTY).

parse_S(?EMPTY, State, Acc) ->
    case cf(?EMPTY, State) of
        {error, Reason, PState1} ->
            fatal_error(Reason, PState1);
        {?EMPTY, _} = PState1 ->
            {Acc, PState1};
        {Bytes, State1} ->
            parse_S(Bytes, State1, Acc)
    end;
parse_S(?CHARS("\r") = Stream, State, Acc) ->
    case cf(Stream, State) of
        {error, Reason, PState1} ->
            fatal_error(Reason, PState1);
        {Stream, State1} ->
            {?APPEND(Acc, ?lf), State1};
        {Bytes, State1} ->
            parse_S_1(Bytes, State1, Acc)
    end;
parse_S(?CHARS_REST("\n", Rest), State, Acc) ->
    parse_S_1(Rest, ?bl(State), ?APPEND(Acc, ?lf));
parse_S(?CHARS_REST("\r\n", Rest), State, Acc) ->
    parse_S_1(Rest, ?bl(State), ?APPEND(Acc, ?lf));
parse_S(?CHARS_REST("\r", Rest), State, Acc) ->
    parse_S_1(Rest, ?bl(State), ?APPEND(Acc, ?lf));
parse_S(?CHARS_REST(C, Rest), State, Acc) when C =:= ?space;
                                               C =:= ?tab ->
    parse_S_1(Rest, State, ?APPEND(Acc, C));
parse_S(?CHARS_REST(_, _), State, _) ->
    fatal_error(no_whitespace, State);
?CHECK2.

parse_S_1(?EMPTY, State, Acc) ->
    case cf(?EMPTY, State) of
        {error, Reason, PState1} ->
            fatal_error(Reason, PState1);
        {?EMPTY, _} = PState1 ->
            {Acc, PState1};
        {Bytes, State1} ->
            parse_S_1(Bytes, State1, Acc)
    end;
parse_S_1(?CHARS("\r") = Stream, State, Acc) ->
    case cf(Stream, State) of
        {error, Reason, PState1} ->
            fatal_error(Reason, PState1);
        {Stream, _} = PState1 ->
            {?APPEND(Acc, ?lf), PState1};
        {Bytes, State1} ->
            parse_S_1(Bytes, State1, Acc)
    end;
parse_S_1(?CHARS_REST("\n", Rest), State, Acc) ->
    parse_S_1(Rest, ?bl(State), ?APPEND(Acc, ?lf));
parse_S_1(?CHARS_REST("\r\n", Rest), State, Acc) ->
    parse_S_1(Rest, ?bl(State), ?APPEND(Acc, ?lf));
parse_S_1(?CHARS_REST("\r", Rest), State, Acc) ->
    parse_S_1(Rest, ?bl(State), ?APPEND(Acc, ?lf));
parse_S_1(?CHARS_REST(C, Rest), State, Acc) when C =:= ?space;
                                                 C =:= ?tab ->
    parse_S_1(Rest, State, ?APPEND(Acc, C));
parse_S_1(?CHARS_REST(_, _) = Bytes, State, Acc) ->
    {Acc, {Bytes, State}};
?CHECK2.

%%----------------------------------------------------------------------
%% Consume Name (does not split prefix and local parts)
%% params:  State
%% returns: {Name, NewState}
%% [5] Name ::= NameStartChar (NameChar)*          
%%----------------------------------------------------------------------
-spec parse_Name(binary(), parser_state()) -> {binary(), ext_parser_state()}.
parse_Name(?CHARS_REST(C, Rest), State) when ?is_name_start_char(C) ->
    parse_Name_1(Rest, State, ?APPEND_IO([], C));
parse_Name(?CHARS_REST(_, _), State) ->
    fatal_error(bad_name, State);
?EMPTY1;
?CHECK1.

parse_Name_1(?CHARS_REST(C, Rest) = Stream, State, Acc) ->
    if
        ?is_name_char(C) ->
            parse_Name_1(Rest, State, ?APPEND_IO(Acc, C));
        true ->
            {erlang:iolist_to_binary(Acc), {Stream, State}}
    end;
?EMPTY2;
?CHECK2.

%%----------------------------------------------------------------------
%% Consume Nmtoken
%% params:  State
%% returns: {Nmtoken, NewState}
%% [7] Nmtoken ::= (NameChar)+          
%%----------------------------------------------------------------------
parse_Nmtoken(Stream, State) ->
    parse_Nmtoken(Stream, State, ?EMPTY).

parse_Nmtoken(?CHARS_REST(C, Rest), State, Acc) when ?is_name_char(C) ->
    parse_Nmtoken_1(Rest, State, ?APPEND(Acc, C));
parse_Nmtoken(?CHARS_REST(_, _), State, _Acc) ->
    fatal_error(bad_nmtoken, State);
?EMPTY2;
?CHECK2.

parse_Nmtoken_1(?CHARS_REST(C, Rest), State, Acc) when ?is_name_char(C) ->
    parse_Nmtoken_1(Rest, State, ?APPEND(Acc, C));
parse_Nmtoken_1(?CHARS_REST(_, _) = Bytes, State, Acc) ->
    {Acc, {Bytes, State}};
?EMPTY2;
?CHECK2.

%%----------------------------------------------------------------------
%% [11] SystemLiteral ::= ('"' [^"]* '"') | ("'" [^']* "'")
%%----------------------------------------------------------------------
parse_SystemLiteral(?CHARS_REST(C, Rest), State) when C == $';
                                                      C == $\" ->
    parse_SystemLiteral_1(Rest, State, ?EMPTY, C);
parse_SystemLiteral(?CHARS_REST(_, _), State) ->
    fatal_error(bad_pubid, State);
?EMPTY1;
?CHECK1.

parse_SystemLiteral_1(?CHARS_REST(C, Rest), State, Acc, C) ->
    {Acc, {Rest, State}};
parse_SystemLiteral_1(?CHARS_REST(C, Rest), State, Acc, Stop) when ?is_char(C) ->
    parse_SystemLiteral_1(Rest, State, ?APPEND(Acc, C), Stop);
parse_SystemLiteral_1(?CHARS_REST(_, _), State, _, _) ->
    fatal_error(bad_pubid, State);
?EMPTY3;
?CHECK3.

%%----------------------------------------------------------------------
%% [12] PubidLiteral ::= '"' PubidChar* '"' | "'" (PubidChar - "'")* "'"
%%----------------------------------------------------------------------
parse_PubidLiteral(?CHARS_REST(C, Rest), State) when C == $';
                                                     C == $\" ->
    parse_PubidLiteral_1(Rest, State, ?EMPTY, C);
parse_PubidLiteral(?CHARS_REST(_, _), State) ->
    fatal_error(bad_pubid, State);
?EMPTY1;
?CHECK1.

parse_PubidLiteral_1(?CHARS_REST(C, Rest), State, Acc, C) ->
    {Acc, {Rest, State}};
parse_PubidLiteral_1(?CHARS_REST(C, Rest), State, Acc, Stop) when ?is_pubid_char(C) ->
    parse_PubidLiteral_1(Rest, State, ?APPEND(Acc, C), Stop);
parse_PubidLiteral_1(?CHARS_REST(_, _), State, _, _) ->
    fatal_error(bad_pubid, State);
?EMPTY3;
?CHECK3.

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
parse_Reference(Stream, State) when Stream == ?CHARS("#");
                                    Stream == ?CHARS("#x") ->
    case cf(Stream, State) of
        {error, Reason, PState1} ->
            fatal_error(Reason, PState1);
        {Stream, _} = PState1 ->
            fatal_error(bad_charref, PState1);
        {Stream1, State1} ->
            parse_Reference(Stream1, State1)
    end;
parse_Reference(?CHARS_REST("#x", Rest), State) ->
    parse_Reference_1(Rest, State, ?EMPTY);
parse_Reference(?CHARS_REST("#", Rest), State) ->
    parse_Reference_2(Rest, State, ?EMPTY);
parse_Reference(?CHARS_REST(_, _) = Stream, State) ->
    parse_Reference_3(Stream, State, ?EMPTY);
?EMPTY1;
?CHECK1.

% hex parse until ';' return char
parse_Reference_1(?CHARS_REST(C, Rest), State, Acc)
    when (C >= $0 andalso C =< $9);
         (C >= $a andalso C =< $f);
         (C >= $A andalso C =< $F) ->
    parse_Reference_1(Rest, State, ?APPEND(Acc, C));
parse_Reference_1(?CHARS_REST(";", Rest), State, Acc) ->
    try
        Int = binary_to_integer(Acc, 16),
        {Int, {Rest, State}}
    catch
        _:_ ->
            fatal_error(bad_charref, State)
    end;        
parse_Reference_1(?CHARS_REST(_, _), State, _) ->
    fatal_error(bad_charref, State);
?EMPTY2;
?CHECK2.

% decimal parse until ';' return char
parse_Reference_2(?CHARS_REST(C, Rest), State, Acc) when (C >= $0 andalso C =< $9) ->
    parse_Reference_2(Rest, State, ?APPEND(Acc, C));
parse_Reference_2(?CHARS_REST(";", Rest), State, Acc) ->
    try
        Int = binary_to_integer(Acc),
        {Int, {Rest, State}}
    catch
        _:_ ->
            fatal_error(bad_charref, State)
    end;        
parse_Reference_2(?CHARS_REST(_, _), State, _) ->
    fatal_error(bad_charref, State);
?EMPTY2;
?CHECK2.

% Name parse until ';' return Name
parse_Reference_3(?CHARS_REST(C, _) = Bytes, State, _) when ?is_name_start_char(C) ->
    {Name, {Rest1, State1}} = parse_Name(Bytes, State),
    parse_Reference_3(Rest1, State1, Name);
parse_Reference_3(?CHARS_REST(";", Rest), State, Acc) ->
    {Acc, {Rest, State}};
parse_Reference_3(?CHARS_REST(_, _), State, _) ->
    fatal_error(bad_charref, State);
?EMPTY2;
?CHECK2.

%%----------------------------------------------------------------------
%% [69] PEReference ::= '%' Name ';'
%% [WFC: No Recursion]
%% [WFC: In DTD]
%% Stream contains %
%% returns %Name
%%----------------------------------------------------------------------
parse_PEReference(?CHARS_REST("%", Rest), State) ->
    parse_PEReference_1(Rest, State, ?CHARS("%")).

parse_PEReference_1(?CHARS_REST(C, _) = Bytes, State, Acc) when ?is_name_start_char(C) ->
    {Name, {Rest, State1}} = parse_Name(Bytes, State),
    parse_PEReference_1(Rest, State1, ?APPEND_STREAM(Acc, Name));
parse_PEReference_1(?CHARS_REST(";", Rest), State, Acc) ->
    {Acc, {Rest, State}};
parse_PEReference_1(?CHARS_REST(_, _), State, _) ->
    fatal_error(bad_charref, State);
?EMPTY2;
?CHECK2.


%%----------------------------------------------------------------------
%% Parse a Comment, leading '<!--' already removed
%% params:  State
%% returns: {Comment, NewState} | NewState (when ignoring)
%% [15] Comment ::= '<!--' ((Char - '-') | ('-' (Char - '-')))* '-->'
%%----------------------------------------------------------------------
parse_Comment(Stream, #{comments := true} = State) ->
    {Text, PState1} = parse_Comment(Stream, State, ?EMPTY),
    event_comment(Text, PState1);
parse_Comment(Stream, State) ->
    consume_Comment(Stream, State).

parse_Comment(Stream, State, Acc) 
    when Stream == ?CHARS("\r");
         Stream == ?CHARS("-");
         Stream == ?CHARS("--") ->
    case cf(Stream, State) of
        {error, Reason, PState1} ->
            fatal_error(Reason, PState1);
        {Stream, _} = PState1 ->
            fatal_error(no_comment, PState1);
        {Bytes, State1} ->
            parse_Comment(Bytes, State1, Acc)
    end;
parse_Comment(?CHARS_REST("\n", Rest), State, Acc) ->
    parse_Comment(Rest, ?bl(State), ?APPEND(Acc, ?lf));
parse_Comment(?CHARS_REST("\r\n", Rest), State, Acc) ->
    parse_Comment(Rest, ?bl(State), ?APPEND(Acc, ?lf));
parse_Comment(?CHARS_REST("\r", Rest), State, Acc) ->
    parse_Comment(Rest, ?bl(State), ?APPEND(Acc, ?lf));
parse_Comment(?CHARS_REST("-->", Rest), State, Acc) ->
    {Acc, {Rest, State}};
parse_Comment(?CHARS_REST("--", _), State, _) ->
    fatal_error(bad_comment, State);
parse_Comment(?CHARS_REST(C, Rest), State, Acc) when ?is_char(C) ->
    parse_Comment(Rest, State, ?APPEND(Acc, C));
parse_Comment(?CHARS_REST(_, _), State, _) ->
    fatal_error(bad_comment_character, State);
?EMPTY2;
?CHECK2.

consume_Comment(Stream, State) 
    when Stream == ?CHARS("\r");
         Stream == ?CHARS("-");
         Stream == ?CHARS("--") ->
    case cf(Stream, State) of
        {error, Reason, PState1} ->
            fatal_error(Reason, PState1);
        {Stream, _} = PState1 ->
            fatal_error(no_comment, PState1);
        {Stream1, State1} ->
            consume_Comment(Stream1, State1)
    end;
consume_Comment(?CHARS_REST("\n", Rest), State) ->
    consume_Comment(Rest, ?bl(State));
consume_Comment(?CHARS_REST("\r\n", Rest), State) ->
    consume_Comment(Rest, ?bl(State));
consume_Comment(?CHARS_REST("\r", Rest), State) ->
    consume_Comment(Rest, ?bl(State));
consume_Comment(?CHARS_REST("-->", Rest), State) ->
    {Rest, State};
consume_Comment(?CHARS_REST("--", _) = Stream, State) ->
    fatal_error(bad_comment, {Stream, State});
consume_Comment(?CHARS_REST(C, Rest), State) when ?is_char(C) ->
    consume_Comment(Rest, State);
consume_Comment(?CHARS_REST(_, _) = Stream, State) ->
    fatal_error(bad_comment_character, {Stream, State});
?EMPTY1;
?CHECK1.

%%----------------------------------------------------------------------
%% Parse a processing-instruction, leading '<?' already removed
%% params:  State
%% returns: {PI, NewState} | NewState (when ignoring)
%% [16] PI       ::= '<?' PITarget (S (Char* - (Char* '?>' Char*)))? '?>'
%% [17] PITarget ::= Name - (('X' | 'x') ('M' | 'm') ('L' | 'l'))
%%----------------------------------------------------------------------
parse_PI(Stream, #{proc_inst := false} = State) ->
    consume_PI(Stream, State);
parse_PI(?CHARS_REST(C, _) = Bytes, State) when ?is_name_start_char(C) ->
    {Name, {Rest, State1}} = parse_Name(Bytes, State),
    case Name of
        <<X,M,L>> when X == $x orelse X == $X,
                       M == $m orelse M == $M,
                       L == $l orelse L == $L ->
            fatal_error(bad_pi, State1);
        _ ->
            {Data, PState2} = parse_PI_1(Rest, State1),
            event_processingInstruction(Name, Data, PState2)
    end;
parse_PI(?CHARS_REST(_, _), State) ->
    fatal_error(bad_pi, State);
?EMPTY1;
?CHECK1.

parse_PI_1(?CHARS_REST(C, _) = Bytes, State) when ?is_whitespace(C) ->
    {_, {Bytes1, State1}} = parse_S(Bytes, State),
    parse_PI_2(Bytes1, State1, <<>>);
?EMPTY1;
?CHECK1.

parse_PI_2(Stream, State, Acc) 
    when Stream == ?CHARS("?") ->
    case cf(Stream, State) of
        {error, Reason, PState1} ->
            fatal_error(Reason, PState1);
        {Stream, _} = PState1 ->
            fatal_error(bad_pi, PState1);
        {Bytes, State1} ->
            parse_PI_2(Bytes, State1, Acc)
    end;
parse_PI_2(?CHARS_REST("?>", Rest), State, Acc) ->
    {Acc, {Rest, State}};
parse_PI_2(?CHARS_REST(C, Rest), State, Acc) when ?is_char(C) ->
    parse_PI_2(Rest, State, ?APPEND(Acc, C));
parse_PI_2(?CHARS_REST(_, _), State, _) ->
    fatal_error(bad_pi, State);
?EMPTY2;
?CHECK2.

consume_PI(?CHARS_REST(C, _) = Bytes, State) when ?is_name_start_char(C) ->
    {Name, {Bytes1, State1}} = parse_Name(Bytes, State),
    case Name of
        <<X,M,L>> when X == $x orelse X == $X,
                       M == $m orelse M == $M,
                       L == $l orelse L == $L ->
            fatal_error(bad_pi, State1);
        _ ->
            {Data, PState2} = consume_PI_1(Bytes1, State1),
            {{Name, Data}, PState2}
    end;
consume_PI(?CHARS_REST(_, _), State) ->
    fatal_error(bad_pi, State);
?EMPTY1;
?CHECK1.

consume_PI_1(?CHARS_REST(C, _) = Bytes, State) when ?is_whitespace(C) ->
    {_, {Bytes1, State1}} = parse_S(Bytes, State),
    consume_PI_2(Bytes1, State1);
?EMPTY1;
?CHECK1.

consume_PI_2(Stream, State) 
    when Stream == ?CHARS("?") ->
    case cf(Stream, State) of
        {error, Reason, PState1} ->
            fatal_error(Reason, PState1);
        {Stream, _} = PState1 ->
            fatal_error(bad_pi, PState1);
        {Bytes, State1} ->
            consume_PI_2(Bytes, State1)
    end;
consume_PI_2(?CHARS_REST("?>", Rest), State) ->
    {Rest, State};
consume_PI_2(?CHARS_REST(C, Rest), State) when ?is_char(C) ->
    consume_PI_2(Rest, State);
consume_PI_2(?CHARS_REST(_, _), State) ->
    fatal_error(bad_pi, State);
?EMPTY1;
?CHECK1.

%%----------------------------------------------------------------------
%% Parse character data. In content, everything that is not 
%%   (element | Reference | CDSect | PI | Comment)
%% params:  State
%% returns: {CharData, IsWs, NewState}
%% [14] CharData ::= [^<&]* - ([^<&]* ']]>' [^<&]*)
%%----------------------------------------------------------------------
parse_CharData(?EMPTY, State, IsWs, Acc) ->
    case cf(?EMPTY, State) of
        {error, _, PState1} ->
            event_characters(Acc, false, false, IsWs, PState1);
        {?EMPTY, _} = PState1 ->
            event_characters(Acc, false, false, IsWs, PState1);
        {Bytes, State1} ->
            parse_CharData(Bytes, State1, IsWs, Acc)
    end;
parse_CharData(Stream, State, IsWs, Acc)
    when Stream == ?CHARS("\r");
         Stream == ?CHARS("]");
         Stream == ?CHARS("]]") ->
    case cf(Stream, State) of
        {error, Reason, PState1} ->
            fatal_error(Reason, PState1);
        {Stream, _} = PState1 ->
            fatal_error(bad_char_data, PState1);
        {Bytes, State1} ->
            parse_CharData(Bytes, State1, IsWs, Acc)
    end;
parse_CharData(?CHARS_REST("<", _) = Stream, State, IsWs, Acc) ->
    event_characters(Acc, false, false, IsWs, {Stream, State});
parse_CharData(?CHARS_REST("&", _) = Stream, State, IsWs, Acc) ->
    %% XXX get the reference and insert it into the stream, continue parsing
    %% the replacement text needs to be checked for balance before inserting
    event_characters(Acc, false, false, IsWs, {Stream, State});
parse_CharData(?CHARS_REST("]]>", _), State, _, _) ->
    fatal_error(bad_char_data, State);
parse_CharData(?CHARS_REST("\n", Rest), State, IsWs, Acc) ->
    parse_CharData(Rest, ?bl(State), IsWs, ?APPEND_IO_SMALL(Acc, ?lf));
parse_CharData(?CHARS_REST("\r\n", Rest), State, IsWs, Acc) ->
    parse_CharData(Rest, ?bl(State), IsWs, ?APPEND_IO_SMALL(Acc, ?lf));
parse_CharData(?CHARS_REST("\r", Rest), State, IsWs, Acc) ->
    parse_CharData(Rest, ?bl(State), IsWs, ?APPEND_IO_SMALL(Acc, ?lf));
parse_CharData(?CHARS_REST(" ", Rest), State, IsWs, Acc) ->
    parse_CharData(Rest, State, IsWs, ?APPEND_IO_SMALL(Acc, ?space));
parse_CharData(?CHARS_REST("\t", Rest), State, IsWs, Acc) ->
    parse_CharData(Rest, State, IsWs, ?APPEND_IO_SMALL(Acc, ?tab));
parse_CharData(?CHARS_REST(C, Rest), State, _IsWs, Acc) ->
    if
        ?is_not_char(C) ->
            fatal_error(bad_char_data, State);
        true ->
            parse_CharData(Rest, State, false, ?APPEND_IO(Acc, C))
    end;
?CHECK3.


%%----------------------------------------------------------------------
%% [18] CDSect  ::= CDStart CData CDEnd
%% [19] CDStart ::= '<![CDATA['
%% [20] CData   ::= (Char* - (Char* ']]>' Char*))
%% [21] CDEnd   ::= ']]>'
%% Parse CDATA Section. '<![' is already removed. 
%% params:  State
%% returns: {CharData, IsWs, NewState}
%%----------------------------------------------------------------------
parse_CDSect(?CHARS_REST("<![CDATA[", Rest), State) ->
    parse_CDSect(Rest, State, ?EMPTY, true);
parse_CDSect(Stream, State)
    when Stream == ?CHARS("<![");
         Stream == ?CHARS("<![C");
         Stream == ?CHARS("<![CD");
         Stream == ?CHARS("<![CDA");
         Stream == ?CHARS("<![CDAT");
         Stream == ?CHARS("<![CDATA") ->
    case cf(Stream, State) of
        {error, Reason, PState1} ->
            fatal_error(Reason, PState1);
        {Stream, _} = PState1 ->
            fatal_error(bad_cdata, PState1);
        {Bytes, State1} ->
            parse_CDSect(Bytes, State1)
    end;
parse_CDSect(?CHARS_REST(_, _), State) ->
    fatal_error(bad_cdata, State);
?CHECK1.

parse_CDSect(Stream, State, Acc, IsWs)
    when Stream == ?CHARS("\r");
         Stream == ?CHARS("]");
         Stream == ?CHARS("]]") ->
    case cf(Stream, State) of
        {error, Reason, PState1} ->
            fatal_error(Reason, PState1);
        {Stream, _} = PState1 ->
            fatal_error(bad_char_data, PState1);
        {Bytes, State1} ->
            parse_CDSect(Bytes, State1, Acc, IsWs)
    end;
parse_CDSect(?CHARS_REST("]]>", Rest), State, Acc, IsWs) ->
     event_characters(Acc, true, false, IsWs, {Rest, State});
parse_CDSect(?CHARS_REST("\n", Rest), State, Acc, IsWs) ->
    parse_CDSect(Rest, ?bl(State), ?APPEND_IO_SMALL(Acc, ?lf), IsWs);
parse_CDSect(?CHARS_REST("\r\n", Rest), State, Acc, IsWs) ->
    parse_CDSect(Rest, ?bl(State), ?APPEND_IO_SMALL(Acc, ?lf), IsWs);
parse_CDSect(?CHARS_REST("\r", Rest), State, Acc, IsWs) ->
    parse_CDSect(Rest, ?bl(State), ?APPEND_IO_SMALL(Acc, ?lf), IsWs);
parse_CDSect(?CHARS_REST(C, Rest), State, Acc, IsWs) when ?is_whitespace(C) ->
    parse_CDSect(Rest, State, ?APPEND_IO_SMALL(Acc, C), IsWs);
parse_CDSect(?CHARS_REST(C, Rest), State, Acc, true) when ?is_char(C) ->
    parse_CDSect(Rest, State, ?APPEND_IO(Acc, C), false);
parse_CDSect(?CHARS_REST(C, Rest), State, Acc, IsWs) when ?is_char(C) ->
    parse_CDSect(Rest, State, ?APPEND_IO(Acc, C), IsWs);
parse_CDSect(?CHARS_REST(_, _), State, _, _) ->
    fatal_error(bad_char_data, State);
?EMPTY3;
?CHECK3.


%%----------------------------------------------------------------------
%% [80] EncodingDecl ::= S 'encoding' Eq ('"' EncName '"' | "'" EncName "'" )
%%----------------------------------------------------------------------
parse_EncodingDecl(?CHARS_REST("encoding", Rest), State) ->
    {Rest1, State1} = parse_Eq(Rest, State),
    parse_EncodingDecl_1(Rest1, State1);
parse_EncodingDecl(Stream, State)
    when Stream == ?CHARS("e");
         Stream == ?CHARS("en");
         Stream == ?CHARS("enc");
         Stream == ?CHARS("enco");
         Stream == ?CHARS("encod");
         Stream == ?CHARS("encodi");
         Stream == ?CHARS("encodin") ->
    case cf(Stream, State) of
        {error, Reason, PState1} ->
            fatal_error(Reason, PState1);
        {Stream, _} = PState1 ->
            fatal_error(bad_encoding, PState1);
        {Bytes, State1} ->
            parse_EncodingDecl(Bytes, State1)
    end;
parse_EncodingDecl(?CHARS_REST(_, _) = Stream, State) ->
    {?EMPTY, {Stream, State}}; % was not an encoding
?EMPTY1;
?CHECK1.

parse_EncodingDecl_1(?CHARS_REST(C, Rest), State) when C == $';
                                                       C == $\" ->
    parse_EncodingDecl_2(Rest, State, C);
parse_EncodingDecl_1(?CHARS_REST(_, _), State) ->
    fatal_error(bad_encoding, State);
?EMPTY1;
?CHECK1.


%%----------------------------------------------------------------------
%% [81] EncName ::= [A-Za-z] ([A-Za-z0-9._] | '-')* 
%% /* Encoding name contains only Latin characters */
%%----------------------------------------------------------------------
parse_EncodingDecl_2(?CHARS_REST(C, Rest), State, Stop) 
    when C >= $A andalso C =< $Z;
         C >= $a andalso C =< $z ->
    parse_EncodingDecl_3(Rest, State, ?APPEND(<<>>, C), Stop);
parse_EncodingDecl_2(?CHARS_REST(_, _), State, _) ->
    fatal_error(bad_char, State);
?EMPTY2;
?CHECK2.

parse_EncodingDecl_3(?CHARS_REST(C, Rest), State, Acc, C) ->
    {Acc, {Rest, State}};
parse_EncodingDecl_3(?CHARS_REST(C, Rest), State, Acc, Stop)
    when C >= $A andalso C =< $Z;
         C >= $a andalso C =< $z;
         C >= $0 andalso C =< $9;
         C == $.; C == $_; C == $- ->
    parse_EncodingDecl_3(Rest, State, ?APPEND(Acc, C), Stop);
parse_EncodingDecl_3(?CHARS_REST(_, _), State, _, _) ->
    fatal_error(bad_char, State);
?EMPTY3;
?CHECK3.

%%----------------------------------------------------------------------
%% [25] Eq ::= S? '=' S?
%%----------------------------------------------------------------------
parse_Eq(?EMPTY, State) ->
    case cf(?EMPTY, State) of
        {error, Reason, PState1} ->
            fatal_error(Reason, PState1);
        {?EMPTY, _} = PState1 ->
            fatal_error(bad_eq_empty, PState1);
        {Bytes, State1} ->
            parse_Eq(Bytes, State1)
    end;
parse_Eq(Bytes, State) ->
    {_, {Bytes1, State1}} = parse_S_1(Bytes, State, ?EMPTY), % maybe ws
    parse_Eq_1(Bytes1, State1).

parse_Eq_1(?CHARS_REST("=", Rest), State) ->
    {_, PState1} = parse_S_1(Rest, State, ?EMPTY), % maybe ws
    PState1;
parse_Eq_1(?CHARS_REST(C, R), State) ->
    fatal_error(bad_eq, {C, R, State});
?EMPTY1;
?CHECK1.

%%----------------------------------------------------------------------
%% [26] VersionNum ::= '1.' [0-9]+
%%----------------------------------------------------------------------
parse_VersionNum(?CHARS("1") = Stream, State) ->
    case cf(Stream, State) of
        {error, Reason, PState1} ->
            fatal_error(Reason, PState1);
        {Stream, _} = PState1 ->
            fatal_error(bad_version_num, PState1);
        {Bytes, State1} ->
            parse_VersionNum(Bytes, State1)
    end;
parse_VersionNum(?CHARS_REST("1.", Rest), State) ->
    case parse_VersionNum_1(Rest, State, ?EMPTY) of
        {?EMPTY, PState1} ->
            fatal_error(bad_version_num, PState1);
        {Vers, PState1} ->
            {?APPEND_STREAM(<<"1.">>, Vers), PState1}
    end;
parse_VersionNum(?CHARS_REST(_, _), State) ->
    fatal_error(bad_version_num, State);
?EMPTY1;
?CHECK1.

parse_VersionNum_1(?CHARS_REST(C, Rest), State, Acc)
    when C >= $0 andalso C =< $9 ->
    parse_VersionNum_1(Rest, State, ?APPEND(Acc, C));
parse_VersionNum_1(?CHARS_REST(_, _) = Rest, State, Acc) ->
    {Acc, {Rest, State}};
?EMPTY2;
?CHECK2.

%%----------------------------------------------------------------------
%% [24] VersionInfo ::= S 'version' Eq ("'" VersionNum "'" | '"' VersionNum '"')
%% leading S is trimmed already
%%----------------------------------------------------------------------
parse_VersionInfo(?CHARS_REST("version", Rest), State) ->
    {Rest1, State1} = parse_Eq(Rest, State),
    parse_VersionInfo_1(Rest1, State1);
parse_VersionInfo(Stream, State)
    when Stream == ?CHARS("v");
         Stream == ?CHARS("ve");
         Stream == ?CHARS("ver");
         Stream == ?CHARS("vers");
         Stream == ?CHARS("versi");
         Stream == ?CHARS("versio") ->
    case cf(Stream, State) of
        {error, Reason, PState1} ->
            fatal_error(Reason, PState1);
        {Stream, _} = PState1 ->
            fatal_error(bad_version_info, PState1);
        {Bytes, State1} ->
            parse_VersionInfo(Bytes, State1)
    end;
parse_VersionInfo(?CHARS_REST(_, _) = Bytes, State) ->
    {?EMPTY, {Bytes, State}}; % was not a version info
?EMPTY1;
?CHECK1.

parse_VersionInfo_1(?CHARS_REST(C, Rest), State)
    when C == $';
         C == $\" ->
    {VersNum, {Rest1, State1}} = parse_VersionNum(Rest, State),
    PState2 = parse_VersionInfo_2(Rest1, State1, C),
    {VersNum, PState2};
parse_VersionInfo_1(?CHARS_REST(_, _), State) ->
    fatal_error(bad_version_info, State);
?EMPTY1;
?CHECK1.

parse_VersionInfo_2(?CHARS_REST(C, Rest), State, C) ->
    {Rest, State};
parse_VersionInfo_2(?CHARS_REST(_, _), State, _) ->
    fatal_error(bad_version_info, State);
?EMPTY2;
?CHECK2.

%%----------------------------------------------------------------------
%% [32] SDDecl ::= S 'standalone' Eq (("'" ('yes' | 'no') "'") | ('"' ('yes' | 'no') '"'))
%% leading space is trimmed already
%%----------------------------------------------------------------------
parse_SDDecl(?CHARS_REST("standalone", Rest), State) ->
    {Rest1, State1} = parse_Eq(Rest, State),
    parse_SDDecl_1(Rest1, State1);
parse_SDDecl(Stream, State)
    when Stream == ?CHARS("s");
         Stream == ?CHARS("st");
         Stream == ?CHARS("sta");
         Stream == ?CHARS("stan");
         Stream == ?CHARS("stand");
         Stream == ?CHARS("standa");
         Stream == ?CHARS("standal");
         Stream == ?CHARS("standalo");
         Stream == ?CHARS("standalon") ->
    case cf(Stream, State) of
        {error, Reason, PState1} ->
            fatal_error(Reason, PState1);
        {Stream, _} = PState1 ->
            fatal_error(bad_standalone, PState1);
        {Bytes, State1} ->
            parse_SDDecl(Bytes, State1)
    end;
parse_SDDecl(?CHARS_REST(_, _) = Bytes, State) ->
    {?EMPTY, {Bytes, State}}; % was not a standalone decl
?EMPTY1;
?CHECK1.

parse_SDDecl_1(?CHARS_REST(C, Rest), State)
    when C == $';
         C == $\" ->
    {Standalone, {Rest1, State1}} = parse_SDDecl_2(Rest, State),
    PState2 = parse_SDDecl_3(Rest1, State1, C),
    {Standalone, PState2};
parse_SDDecl_1(?CHARS_REST(_, _), State) ->
    fatal_error(bad_version_info, State);
?EMPTY1;
?CHECK1.

parse_SDDecl_2(?CHARS_REST("no", Rest), State) ->
    {false, {Rest, State}};
parse_SDDecl_2(?CHARS_REST("yes", Rest), State) ->
    {true, {Rest, State}};
parse_SDDecl_2(Stream, State)
    when Stream == ?CHARS("y");
         Stream == ?CHARS("ye");
         Stream == ?CHARS("n") ->
    case cf(Stream, State) of
        {error, Reason, PState1} ->
            fatal_error(Reason, PState1);
        {Stream, _} = PState1 ->
            fatal_error(bad_standalone, PState1);
        {Bytes, State1} ->
            parse_SDDecl_2(Bytes, State1)
    end;
parse_SDDecl_2(?CHARS_REST(_, _), State) ->
    fatal_error(bad_standalone, State);
?EMPTY1;
?CHECK1.

parse_SDDecl_3(?CHARS_REST(C, Rest), State, C) ->
    {Rest, State};
parse_SDDecl_3(?CHARS_REST(_, _), State, _) ->
    fatal_error(bad_standalone, State);
?EMPTY2;
?CHECK2.

%%----------------------------------------------------------------------
%% [23] XMLDecl ::= '<?xml' VersionInfo EncodingDecl? SDDecl? S? '?>' 
%%----------------------------------------------------------------------
parse_XMLDecl(?CHARS_REST("<?xml", Rest), State) ->
    {_, {Rest1, State1}} = parse_S(Rest, State), % must be whitespace
    case parse_VersionInfo(Rest1, State1) of
        {?EMPTY, PState2} -> % version cannot be empty
            fatal_error(bad_xml_decl, PState2);
        {Vers, {Rest2, State2}} ->
            {_, {Rest3, State3}} = parse_S_1(Rest2, State2, ?EMPTY), % maybe trim ws
            {Enc, EncSet, {Rest4, State4}} = 
                case parse_EncodingDecl(Rest3, State3) of
                    {?EMPTY, PState4} ->
                        {<<"UTF-8">>, false, PState4};
                    {Enc1, PState4} ->
                        {Enc1, true, PState4}
                end,
            {Std, StdSet, {Rest5, State5}} = 
                case parse_SDDecl(Rest4, State4) of
                    {?EMPTY, PState6} ->
                        {false, false, PState6};
                    {Std1, {Stream6, State6}} ->
                        {Std1, true, {Stream6, State6#{standalone := Std1}}}
                end,
            {_, {Rest7, State7}} = parse_S_1(Rest5, State5, ?EMPTY),
            PState1 = trim_qgt(Rest7, State7),
            PState2 = set_next_parser_position(misc_pre_dtd, PState1),
            event_startDocument(Vers, Enc, EncSet, Std, StdSet, PState2)
    end;
parse_XMLDecl(Stream, State)
    when Stream == ?CHARS("<");
         Stream == ?CHARS("<?");
         Stream == ?CHARS("<?x");
         Stream == ?CHARS("<?xm") ->
    case cf(Stream, State) of
        {error, Reason, PState1} ->
            fatal_error(Reason, PState1);
        {Stream, _} = PState1 ->
            fatal_error(bad_standalone, PState1);
        {Bytes, State1} ->
            parse_XMLDecl(Bytes, State1)
    end;
parse_XMLDecl(?CHARS_REST(_, _) = Bytes, State) ->
    % default declaration
    PState1 = set_next_parser_position(misc_pre_dtd, {Bytes, State}),
    event_startDocument(<<"1.0">>, <<"UTF-8">>, false, false, false, PState1);
?EMPTY1;
?CHECK1.

%%----------------------------------------------------------------------
%% [27] Misc ::= Comment | PI | S 
%%----------------------------------------------------------------------
parse_Misc(?CHARS_REST("<!--", Rest), State) ->
    parse_Comment(Rest, State);
parse_Misc(?CHARS_REST("<?", Rest), State) ->
    parse_PI(Rest, State);
parse_Misc(?CHARS_REST(C, _) = Bytes, State) when ?is_whitespace(C) ->
    {_, {Rest1, State1}} = parse_S(Bytes, State),
    parse_Misc(Rest1, State1);
parse_Misc(Stream, State)
    when Stream == ?EMPTY;
         Stream == ?CHARS("<");
         Stream == ?CHARS("<!");
         Stream == ?CHARS("<!-") ->
    case cf(Stream, State) of
        {error, no_bytes, PState1} ->
            PState1;
        {?EMPTY, State1} ->
            {?EMPTY, State1};
        {Stream, _} = PState1 ->
            fatal_error(bad_standalone, PState1);
        {Stream1, State1} ->
            parse_Misc(Stream1, State1)
    end;
parse_Misc(?CHARS_REST(_, _) = Bytes, State) ->
    %% XXX return that it was not Misc?
    {Bytes, State};
?CHECK1.

%%----------------------------------------------------------------------
%% [43] content ::= CharData? ((element | Reference | CDSect | PI | Comment) CharData?)* 
%%----------------------------------------------------------------------
parse_content(Stream, State)
    when Stream == ?EMPTY;
         Stream == ?CHARS("<");
         Stream == ?CHARS("<!");
         Stream == ?CHARS("<!-") ->
    case cf(Stream, State) of
        {error, Reason, PState1} ->
            fatal_error(Reason, PState1);
        {?EMPTY, State1} ->
            State1;
        {Stream, _} = PState1 ->
            fatal_error(bad_content, PState1);
        {Bytes, State1} ->
            parse_content(Bytes, State1)
    end;
parse_content(?CHARS_REST("</", Rest), State) ->
    parse_ETag(Rest, State);
parse_content(?CHARS_REST("<!--", Rest), State) ->
    parse_Comment(Rest, State);
parse_content(?CHARS_REST("<?", Rest), State) ->
    parse_PI(Rest, State);
parse_content(?CHARS_REST("<![", Rest), State) ->
    parse_CDSect(Rest, State);
parse_content(?CHARS_REST("<", _) = Bytes, State) ->
    parse_element(Bytes, State);
parse_content(?CHARS_REST("&", Rest), State) ->
    {_Name, {Rest1, State1}} = parse_Reference(Rest, State),
    %%XXX do something with the ref
    parse_content(Rest1, State1);
parse_content(?CHARS_REST(_, _) = Bytes, State) ->
    parse_CharData(Bytes, State, true, ?EMPTY);
?CHECK1.

%%----------------------------------------------------------------------
%% [28] doctypedecl ::= '<!DOCTYPE' S Name (S ExternalID)? S? ('[' intSubset ']' S?)? '>'
%% [WFC: External Subset] 
%%----------------------------------------------------------------------
parse_doctypedecl(Stream, State)
    when Stream == ?CHARS("<");
         Stream == ?CHARS("<!");
         Stream == ?CHARS("<!D");
         Stream == ?CHARS("<!DO");
         Stream == ?CHARS("<!DOC");
         Stream == ?CHARS("<!DOCT");
         Stream == ?CHARS("<!DOCTY");
         Stream == ?CHARS("<!DOCTYP") ->
    case cf(Stream, State) of
        {error, Reason, PState1} ->
            fatal_error(Reason, PState1);
        {Stream, _} = PState1 ->
            fatal_error(bad_doctypedecl, PState1);
        {Bytes, State1} ->
            parse_doctypedecl(Bytes, State1)
    end;
parse_doctypedecl(?CHARS_REST("<!DOCTYPE", Rest), #{line := Line} = State) ->
    {_, {Rest1, State1}} = parse_S(Rest, State),
    {Name, {Rest2, State2}} = parse_Name(Rest1, State1),
    Text = ?APPEND_STREAM(<<"<!DOCTYPE ">>, Name),
    DTD = #{type => dtd, name => Name, line => Line, proc => undefined},
    parse_doctypedecl_1(Rest2, State2, Text, DTD);
parse_doctypedecl(?CHARS_REST("<", _) = Bytes, State) ->
    {Bytes, State};
parse_doctypedecl(?CHARS_REST(C, _), State) ->
    fatal_error(bad_doctypedecl, {C, State});
?EMPTY1;
?CHECK1.

% maybe external id, maybe subset
parse_doctypedecl_1(Stream, State, Text, DTD) ->
    case peek(Stream, State) of
        {$[, {Stream1, State1}} -> % internal subset
            DTD1 = DTD#{external => undefined},
            parse_doctypedecl_2(Stream1, State1, Text, DTD1);
        {W, {Stream1, State1}} when ?is_whitespace(W) ->
            {_, {Stream2, State2}} = parse_S(Stream1, State1),
            case peek(Stream2, State2) of
                {$[, {Stream3, State3}} -> % internal subset
                    DTD1 = DTD#{external => undefined},
                    parse_doctypedecl_2(Stream3, State3, Text, DTD1);
                {$S, {Stream3, State3}} -> % system id
                    parse_doctypedecl_3(Stream3, State3, ?APPEND(Text, " "), DTD);
                {$P, {Stream3, State3}} -> % public id
                    parse_doctypedecl_3(Stream3, State3, ?APPEND(Text, " "), DTD);
                {$>, {Stream3, State3}} -> % empty
                    parse_doctypedecl_4(Stream3, State3, Text, DTD);
                {_, {_, State3}} ->
                    fatal_error(bad_dtd, State3)
            end;
        {$>, {Stream1, State1}} ->
            parse_doctypedecl_4(Stream1, State1, Text, DTD);
        {_, {_, State1}} ->
            fatal_error(bad_dtd, State1)
    end.

% internal subset always has '[' in stream
parse_doctypedecl_2(?CHARS_REST("[", Rest), State, Text, DTD) ->
    {{Text1, DTD1}, {Rest1, State1}} = parse_intSubset(Rest, State, Text, DTD),
    parse_doctypedecl_4(Rest1, State1, Text1, DTD1).

% system id, add key 'external' to DTD, do not follow link for now
parse_doctypedecl_3(Stream, State, Text, DTD) ->
    {PubSys, {Rest1, State1}} = parse_ExternalID(Stream, State),
    DTD1 = DTD#{external => PubSys},
    Text1 = case PubSys of
                {?EMPTY, Sys} ->
                    ?APPEND_STREAM(Text, ?APPEND(?APPEND_STREAM(<<"SYSTEM \"">>, Sys), "\""));
                {Pub, Sys} ->
                    Pub1 = ?APPEND(?APPEND_STREAM(<<"PUBLIC \"">>, Pub), "\""),
                    Sys1 = ?APPEND(?APPEND_STREAM(<<" SYSTEM \"">>, Sys), "\""),
                    ?APPEND_STREAM(Text, ?APPEND_STREAM(Pub1, Sys1))
            end,
    case peek(Rest1, State1) of
        {$[, {Stream2, State2}} -> % internal subset
            parse_doctypedecl_2(Stream2, State2, Text1, DTD1);
        {W, {Stream2, State2}} when ?is_whitespace(W) ->
            {_, {Stream3, State3}} = parse_S(Stream2, State2),
            case peek(Stream3, State3) of
                {$[, {Stream4, State4}} -> % internal subset
                    parse_doctypedecl_2(Stream4, State4, Text1, DTD1);
                {$>, {Stream4, State4}} -> % empty
                    parse_doctypedecl_4(Stream4, State4, Text1, DTD1);
                {_, {_, State4}} ->
                    fatal_error(bad_dtd, State4)
            end;
        {$>, {Stream2, State2}} ->
            parse_doctypedecl_4(Stream2, State2, Text1, DTD1);
        {_, {_, State2}} ->
            fatal_error(bad_dtd, State2)
    end.

% S? and closing '>', sets text value of event
parse_doctypedecl_4(?CHARS_REST(">", Rest), State, Text, DTD) ->
    {DTD#{text => ?APPEND(Text, ">")}, {Rest, State}};
parse_doctypedecl_4(?CHARS_REST(C, _) = Stream, State, Text, DTD) when ?is_whitespace(C) ->
    {_, {Rest1, State1}} = parse_S(Stream, State, ?EMPTY),
    parse_doctypedecl_4(Rest1, State1, Text, DTD);
parse_doctypedecl_4(?CHARS_REST(_, _), State, _, _) ->
    fatal_error(bad_dtd, State);
?EMPTY3;
?CHECK3.

%%----------------------------------------------------------------------
%% [75] ExternalID ::= 'SYSTEM' S SystemLiteral | 'PUBLIC' S PubidLiteral S SystemLiteral
%% return {Pub, Sys}
%%----------------------------------------------------------------------
parse_ExternalID(?CHARS_REST("SYSTEM", Rest), State) ->
    {_, {Rest1, State1}} = parse_S(Rest, State),
    {Sys, {Rest2, State2}} = parse_SystemLiteral(Rest1, State1),
    {{?EMPTY, Sys}, {Rest2, State2}};
parse_ExternalID(?CHARS_REST("PUBLIC", Rest), State) ->
    {_, {Rest1, State1}} = parse_S(Rest, State),
    {Pub, {Rest2, State2}} = parse_PubidLiteral(Rest1, State1),
    {_, {Rest3, State3}} = parse_S(Rest2, State2),
    {Sys, {Rest4, State4}} = parse_SystemLiteral(Rest3, State3),
    {{Pub, Sys}, {Rest4, State4}};
parse_ExternalID(Stream, State)
    when Stream == ?CHARS("S");
         Stream == ?CHARS("SY");
         Stream == ?CHARS("SYS");
         Stream == ?CHARS("SYST");
         Stream == ?CHARS("SYSTE");
         Stream == ?CHARS("P");
         Stream == ?CHARS("PU");
         Stream == ?CHARS("PUB");
         Stream == ?CHARS("PUBL");
         Stream == ?CHARS("PUBLI") ->
    case cf(Stream, State) of
        {error, Reason, PState1} ->
            fatal_error(Reason, PState1);
        {Stream, _} = PState1 ->
            fatal_error(bad_external_id, PState1);
        {Bytes, State1} ->
            parse_ExternalID(Bytes, State1)
    end;
parse_ExternalID(?CHARS_REST(_, _), State) ->
    fatal_error(bad_external_id, State);
?EMPTY1;
?CHECK1.

%%----------------------------------------------------------------------
%% [83] PublicID ::= 'PUBLIC' S PubidLiteral
%%----------------------------------------------------------------------
parse_PublicID(?CHARS_REST("PUBLIC", Rest), State) ->
    {_, {Rest1, State1}} = parse_S(Rest, State),
    parse_PubidLiteral(Rest1, State1);
parse_PublicID(Stream, State)
    when Stream == ?CHARS("P");
         Stream == ?CHARS("PU");
         Stream == ?CHARS("PUB");
         Stream == ?CHARS("PUBL");
         Stream == ?CHARS("PUBLI") ->
    case cf(Stream, State) of
        {error, Reason, PState1} ->
            fatal_error(Reason, PState1);
        {Stream, _} = PState1 ->
            fatal_error(bad_public_id, PState1);
        {Bytes, State1} ->
            parse_PublicID(Bytes, State1)
    end;
parse_PublicID(?CHARS_REST(_, _), State) ->
    fatal_error(bad_public_id, State);
?EMPTY1;
?CHECK1.

%%----------------------------------------------------------------------
%% Parses a notation declaration.
%% Param: State
%% Returns: {Name, Pub, Sys}
%% "<!N" is in stream
%% [82] NotationDecl ::= '<!NOTATION' S Name S (ExternalID | PublicID) S? '>'
%%----------------------------------------------------------------------
parse_NotationDecl(?CHARS_REST("<!NOTATION", Rest), State) ->
    {_, {Rest1, State1}} = parse_S(Rest, State),
    {Name, {Rest2, State2}} = parse_Name(Rest1, State1),
    {_, {Rest3, State3}} = parse_S(Rest2, State2),
    case peek(Rest3, State3) of
        {$S, {Rest4, State4}} ->
            {{Pub, Sys}, {Rest5, State5}} = parse_ExternalID(Rest4, State4),
            PState = trim_sgt(Rest5, State5),
            {{Name, Pub, Sys}, PState};
        {$P, {Rest4, State4}} ->
            {Pub, {Rest5, State5}} = parse_PublicID(Rest4, State4),
            case peek(Rest5, State5) of
                {$>, {Rest6, State6}} ->
                    PState = trim_sgt(Rest6, State6),
                    {{Name, Pub, ?EMPTY}, PState};
                {W, {Rest6, State6}} when ?is_whitespace(W) ->
                    {_, {Rest7, State7}} = parse_S(Rest6, State6),
                    case peek(Rest7, State7) of
                        {$>, {Rest8, State8}} ->
                            PState = trim_sgt(Rest8, State8),
                            {{Name, Pub, ?EMPTY}, PState};
                        {$S, {Rest8, State8}} ->
                            {{?EMPTY, Sys}, {Rest9, State9}} = parse_ExternalID(Rest8, State8),
                            PState = trim_sgt(Rest9, State9),
                            {{Name, Pub, Sys}, PState};
                        {_, {_, State8}} ->
                            fatal_error(bad_notation, State8)
                    end;
                {_, {_, State6}} ->
                    fatal_error(bad_notation, State6)
            end;
        {_, {_, State4}} ->
            fatal_error(bad_notation, State4)
    end;
parse_NotationDecl(Stream, State)
    when Stream == ?CHARS("<!N");
         Stream == ?CHARS("<!NO");
         Stream == ?CHARS("<!NOT");
         Stream == ?CHARS("<!NOTA");
         Stream == ?CHARS("<!NOTAT");
         Stream == ?CHARS("<!NOTATI");
         Stream == ?CHARS("<!NOTATIO") ->
    case cf(Stream, State) of
        {error, Reason, PState1} ->
            fatal_error(Reason, PState1);
        {Stream, _} = PState1 ->
            fatal_error(bad_public_id, PState1);
        {Bytes, State1} ->
            parse_NotationDecl(Bytes, State1)
    end;
parse_NotationDecl(?CHARS_REST(_, _), State) ->
    fatal_error(bad_notation, State);
?CHECK1.

%%----------------------------------------------------------------------
%% [60] DefaultDecl ::= '#REQUIRED' | '#IMPLIED' | (('#FIXED' S)? AttValue)
%% [WFC: No < in Attribute Values]
%% [WFC: No External Entity References]
%% (required | implied | {fixed, AttValue}) 
%%----------------------------------------------------------------------
parse_DefaultDecl(?CHARS_REST("#REQUIRED", Rest), State) ->
    {required, {Rest, State}};
parse_DefaultDecl(?CHARS_REST("#IMPLIED", Rest), State) ->
    {implied, {Rest, State}};
parse_DefaultDecl(?CHARS_REST("#FIXED", Rest), State) -> 
    {_, {Rest1, State1}} = parse_S(Rest, State),
    {Value, PState} = parse_AttValue(Rest1, State1),
    {{fixed, Value}, PState};
parse_DefaultDecl(Stream, State)
    when Stream == ?CHARS("#");
         Stream == ?CHARS("#R");
         Stream == ?CHARS("#RE");
         Stream == ?CHARS("#REQ");
         Stream == ?CHARS("#REQU");
         Stream == ?CHARS("#REQUI");
         Stream == ?CHARS("#REQUIR");
         Stream == ?CHARS("#REQUIRE");
         Stream == ?CHARS("#I");
         Stream == ?CHARS("#IM");
         Stream == ?CHARS("#IMP");
         Stream == ?CHARS("#IMPL");
         Stream == ?CHARS("#IMPLI");
         Stream == ?CHARS("#IMPLIE");
         Stream == ?CHARS("#F");
         Stream == ?CHARS("#FI");
         Stream == ?CHARS("#FIX");
         Stream == ?CHARS("#FIXE") ->
    case cf(Stream, State) of
        {error, Reason, PState1} ->
            fatal_error(Reason, PState1);
        {Stream, _} = PState1 ->
            fatal_error(bad_default, PState1);
        {Bytes, State1} ->
            parse_DefaultDecl(Bytes, State1)
    end;
parse_DefaultDecl(?CHARS_REST(_, _), State) -> 
    fatal_error(bad_default, State);
?CHECK1.

%%----------------------------------------------------------------------
%% [59] Enumeration ::= '(' S? Nmtoken (S? '|' S? Nmtoken)* S? ')'
%% '(' in stream 
%%----------------------------------------------------------------------
parse_Enumeration(?CHARS_REST("(", Rest), State) ->
    {_, {Rest1, State1}} = parse_S_1(Rest, State, ?EMPTY),
    {Name, {Rest2, State2}} = parse_Nmtoken(Rest1, State1),
    {_, {Rest3, State3}} = parse_S_1(Rest2, State2, ?EMPTY),
    parse_Enumeration_1(Rest3, State3, [Name]);
parse_Enumeration(?CHARS_REST(_, _), State) ->
    fatal_error(bad_enum, State);
?CHECK1.

parse_Enumeration_1(?CHARS_REST(")", Rest), State, Acc) ->
    {{enumeration, lists:reverse(Acc)}, {Rest, State}};
parse_Enumeration_1(?CHARS_REST("|", Rest), State, Acc) ->
    {_, {Rest1, State1}} = parse_S_1(Rest, State, ?EMPTY),
    {Name, {Rest2, State2}} = parse_Nmtoken(Rest1, State1),
    {_, {Rest3, State3}} = parse_S_1(Rest2, State2, ?EMPTY),
    parse_Enumeration_1(Rest3, State3, [Name|Acc]);
parse_Enumeration_1(?CHARS_REST(_, _), State, _) ->
    fatal_error(bad_enum, State);
?EMPTY2;
?CHECK2.


%%----------------------------------------------------------------------
%% [58] NotationType ::= 'NOTATION' S '(' S? Name (S? '|' S? Name)* S? ')'
%% 'N' in stream 
%%----------------------------------------------------------------------
parse_NotationType(?CHARS_REST("NOTATION", Rest), State) ->
    {_, {Rest1, State1}} = parse_S(Rest, State),
    parse_NotationType_1(Rest1, State1);
parse_NotationType(Stream, State)
    when Stream == ?CHARS("N");
         Stream == ?CHARS("NO");
         Stream == ?CHARS("NOT");
         Stream == ?CHARS("NOTA");
         Stream == ?CHARS("NOTAT");
         Stream == ?CHARS("NOTATI");
         Stream == ?CHARS("NOTATIO") ->
    case cf(Stream, State) of
        {error, Reason, PState1} ->
            fatal_error(Reason, PState1);
        {Stream, _} = PState1 ->
            fatal_error(bad_notation_type, PState1);
        {Bytes, State1} ->
            parse_NotationType(Bytes, State1)
    end;
parse_NotationType(?CHARS_REST(_, _), State) ->
    fatal_error(bad_notation_type, State);
?CHECK1.

parse_NotationType_1(?CHARS_REST("(", Rest), State) ->
    {_, {Rest1, State1}} = parse_S_1(Rest, State, ?EMPTY),
    {Name, {Rest2, State2}} = parse_Name(Rest1, State1),
    {_, {Rest3, State3}} = parse_S_1(Rest2, State2, ?EMPTY),
    parse_NotationType_2(Rest3, State3, [Name]);
parse_NotationType_1(?CHARS_REST(_, _), State) ->
    fatal_error(bad_notation_type, State);
?EMPTY1;
?CHECK1.

parse_NotationType_2(?CHARS_REST(")", Rest), State, Acc) ->
    {{notation, lists:reverse(Acc)}, {Rest, State}};
parse_NotationType_2(?CHARS_REST("|", Rest), State, Acc) ->
    {_, {Rest1, State1}} = parse_S_1(Rest, State, ?EMPTY),
    {Name, {Rest2, State2}} = parse_Name(Rest1, State1),
    {_, {Rest3, State3}} = parse_S_1(Rest2, State2, ?EMPTY),
    parse_NotationType_2(Rest3, State3, [Name|Acc]);
parse_NotationType_2(?CHARS_REST(_, _), State, _) ->
    fatal_error(bad_notation_type, State);
?EMPTY2;
?CHECK2.

%%----------------------------------------------------------------------
%% [54] AttType        ::= StringType | TokenizedType | EnumeratedType
%% [55] StringType     ::= 'CDATA'
%% [56] TokenizedType  ::= 'ID' | 'IDREF' | 'IDREFS' | 'ENTITY' | 'ENTITIES' |
%%                         'NMTOKEN' | 'NMTOKENS'
%% [57] EnumeratedType ::= NotationType | Enumeration
%%----------------------------------------------------------------------
parse_AttType(Stream, State)
    when Stream == ?CHARS("C");
         Stream == ?CHARS("CD");
         Stream == ?CHARS("CDA");
         Stream == ?CHARS("CDAT");
         Stream == ?CHARS("I");
         Stream == ?CHARS("ID");
         Stream == ?CHARS("IDR");
         Stream == ?CHARS("IDRE");
         Stream == ?CHARS("IDREF");
         Stream == ?CHARS("E");
         Stream == ?CHARS("EN");
         Stream == ?CHARS("ENT");
         Stream == ?CHARS("ENTI");
         Stream == ?CHARS("ENTIT");
         Stream == ?CHARS("ENTITI");
         Stream == ?CHARS("ENTITIE");
         Stream == ?CHARS("N");
         Stream == ?CHARS("NM");
         Stream == ?CHARS("NMT");
         Stream == ?CHARS("NMTO");
         Stream == ?CHARS("NMTOK");
         Stream == ?CHARS("NMTOKE");
         Stream == ?CHARS("NMTOKEN") ->
    case cf(Stream, State) of
        {error, Reason, PState1} ->
            fatal_error(Reason, PState1);
        {Stream, _} = PState1 ->
            fatal_error(bad_att_type, PState1);
        {Bytes, State1} ->
            parse_AttType(Bytes, State1)
    end;
parse_AttType(?CHARS_REST("CDATA", Rest), State) ->
    {cdata, {Rest, State}};
parse_AttType(?CHARS_REST("IDREFS", Rest), State) ->
    {idrefs, {Rest, State}};
parse_AttType(?CHARS_REST("IDREF", Rest), State) ->
    {idref, {Rest, State}};
parse_AttType(?CHARS_REST("ID", Rest), State) ->
    {id, {Rest, State}};
parse_AttType(?CHARS_REST("ENTITY", Rest), State) ->
    {entity, {Rest, State}};
parse_AttType(?CHARS_REST("ENTITIES", Rest), State) ->
    {entities, {Rest, State}};
parse_AttType(?CHARS_REST("NMTOKENS", Rest), State) ->
    {nmtokens, {Rest, State}};
parse_AttType(?CHARS_REST("NMTOKEN", Rest), State) ->
    {nmtoken, {Rest, State}};
parse_AttType(?CHARS_REST("N", _) = Stream, State) ->
    parse_NotationType(Stream, State);
parse_AttType(?CHARS_REST("(", _) = Stream, State) ->
    parse_Enumeration(Stream, State);
parse_AttType(?CHARS_REST(_, _), State) ->
    fatal_error(bad_att_type, State);
?EMPTY1;
?CHECK1.

%%----------------------------------------------------------------------
%% [53] AttDef ::= S Name S AttType S DefaultDecl
%% trims trailing'>'
%%----------------------------------------------------------------------
parse_AttDef(?CHARS_REST(">", Rest), State, Acc) ->
    {lists:reverse(Acc), {Rest, State}};
parse_AttDef(?CHARS_REST(C, _) = Stream, State, Acc) when ?is_whitespace(C) ->
    {_, {Rest1, State1}} = parse_S(Stream, State),
    case peek(Rest1, State1) of
        {$>, {Rest2, State2}} ->
            parse_AttDef(Rest2, State2, Acc);
        {_, {Rest2, State2}} ->
            {Name, {Rest3, State3}} = parse_Name(Rest2, State2),
            {_, {Rest4, State4}} = parse_S(Rest3, State3),
            {Type, {Rest5, State5}} = parse_AttType(Rest4, State4),
            {_, {Rest6, State6}} = parse_S(Rest5, State5),
            {Def, {Rest7, State7}} = parse_DefaultDecl(Rest6, State6),
            parse_AttDef(Rest7, State7, [{Name, Type, Def}|Acc])
    end;
parse_AttDef(?CHARS_REST(_, _), State, _) ->
    fatal_error(bad_att_def, State);
?EMPTY2;
?CHECK2.

%%----------------------------------------------------------------------
%% [52] AttlistDecl ::= '<!ATTLIST' S Name AttDef* S? '>'
%% '<!A' in stream, returns {Name, AttDefs}
%%----------------------------------------------------------------------
parse_AttlistDecl(?CHARS_REST("<!ATTLIST", Rest), State) ->
    {_, {Rest1, State1}} = parse_S(Rest, State),
    {Name, {Rest2, State2}} = parse_Name(Rest1, State1),
    {AttDefs, PState3} = parse_AttDef(Rest2, State2, []),
    {{Name, AttDefs}, PState3};
parse_AttlistDecl(Stream, State)
    when Stream == ?CHARS("<!A");
         Stream == ?CHARS("<!AT");
         Stream == ?CHARS("<!ATT");
         Stream == ?CHARS("<!ATTL");
         Stream == ?CHARS("<!ATTLI");
         Stream == ?CHARS("<!ATTLIS");
         Stream == ?CHARS("<!ATTLIST") -> 
    case cf(Stream, State) of
        {error, Reason, PState1} ->
            fatal_error(Reason, PState1);
        {Stream, _} = PState1 ->
            fatal_error(bad_att_list, PState1);
        {Bytes, State1} ->
            parse_AttlistDecl(Bytes, State1)
    end;
parse_AttlistDecl(?CHARS_REST(_, _), State) ->
    fatal_error(bad_att_list, State);
?CHECK1.

%%----------------------------------------------------------------------
%% [28a] DeclSep    ::= PEReference | S
%% [WFC: PE Between Declarations]
%% [28b] intSubset  ::= (markupdecl | DeclSep)*
%% [29]  markupdecl ::= elementdecl | AttlistDecl | EntityDecl | NotationDecl | PI | Comment
%% [WFC: PEs in Internal Subset]
%%----------------------------------------------------------------------
parse_intSubset(Stream, State, Text, DTD)
    when Stream == ?CHARS("<");
         Stream == ?CHARS("<!");
         Stream == ?CHARS("<!-");
         Stream == ?CHARS("<!E") -> 
    case cf(Stream, State) of
        {error, Reason, PState1} ->
            fatal_error(Reason, PState1);
        {Stream, _} = PState1 ->
            fatal_error(bad_dtd, PState1);
        {Bytes, State1} ->
            parse_intSubset(Bytes, State1, Text, DTD)
    end;
parse_intSubset(?CHARS_REST("]", Rest), State, Text, DTD) ->
    {{Text, DTD}, {Rest, State}};
parse_intSubset(?CHARS_REST("<!EL", _) = Stream, State, Text, DTD) ->
    {Elem, {Rest1, State1}} = parse_elementdecl(Stream, State),
    DTD1 = add_element_to_dtd(Elem, DTD),
    parse_intSubset(Rest1, State1, Text, DTD1);
parse_intSubset(?CHARS_REST("<!A", _) = Stream, State, Text, DTD) ->
    {AttList, {Rest1, State1}} = parse_AttlistDecl(Stream, State),
    DTD1 = add_attlist_to_dtd(AttList, DTD),
    parse_intSubset(Rest1, State1, Text, DTD1);
parse_intSubset(?CHARS_REST("<!E", _) = Stream, State, Text, DTD) ->
    {Entity, {Rest1, State1}} = parse_EntityDecl(Stream, State),
    DTD1 = add_entity_to_dtd(Entity, DTD),
    {Rest2, State2} = trim_sgt(Rest1, State1),
    parse_intSubset(Rest2, State2, Text, DTD1);
parse_intSubset(?CHARS_REST("<!N", _) = Stream, State, Text, DTD) ->
    {Notation, {Rest1, State1}} = parse_NotationDecl(Stream, State),
    DTD1 = add_notation_to_dtd(Notation, DTD),
    parse_intSubset(Rest1, State1, Text, DTD1);
parse_intSubset(?CHARS_REST("<?", Rest), State, Text, DTD) ->
    %XXX just add to the text
    {_PI, {Rest1, State1}} = parse_PI(Rest, State),
    parse_intSubset(Rest1, State1, Text, DTD);
parse_intSubset(?CHARS_REST("<!--", Rest), State, Text, DTD) ->
    %XXX just add to the text
    {_Comment, {Rest1, State1}} = parse_Comment(Rest, State),
    parse_intSubset(Rest1, State1, Text, DTD);
parse_intSubset(?CHARS_REST(C, _) = Stream, State, Text, DTD) when ?is_whitespace(C) ->
    {_, {Rest1, State1}} = parse_S(Stream, State),
    parse_intSubset(Rest1, State1, Text, DTD);
parse_intSubset(?CHARS_REST("%", _) = Stream, State, Text, DTD) ->
    {PE, {Rest1, State1}} = parse_PEReference(Stream, State),
    case resolve_parameter_entity(PE, DTD, State1) of
        {internal, Value, State2} ->
            Stream1 = ?APPEND_STREAM(Value, Rest1),
            parse_intSubset(Stream1, State2, Text, DTD);
        {external, _, State2} -> %% XXX do something with externals...
            parse_intSubset(Rest1, State2, Text, DTD);
        unknown ->
            parse_intSubset(Rest1, State1, Text, DTD)
    end;
parse_intSubset(?CHARS_REST(C, _), State, _, DTD) -> 
    fatal_error(bad_dtd, {DTD, C, State});
?EMPTY3;
?CHECK3.


%%----------------------------------------------------------------------
%% [39] element ::= EmptyElemTag | STag content ETag
%% [WFC: Element Type Match]
%% [40] STag    ::= '<' Name (S Attribute)* S? '>'
%% [WFC: Unique Att Spec]
%%----------------------------------------------------------------------
parse_element(?CHARS_REST("<", Rest),#{tags := Tags,
                                       inscope_ns := [LastNss|_] = Nss,
                                       position := P} = State) ->
    {Name, {Rest1, State1}} = parse_Name(Rest, State),
    {{Ns, As}, {Rest2, State2}} = parse_attributes(Rest1, State1),
    NamespaceMap = namespace_map_from_list(Ns, LastNss),
    State3 = State2#{inscope_ns := [NamespaceMap|Nss]},
    QName = qualify_element_name(Name, State3),
    As1 = qualify_attribute_names(As, State3),
    %% XXX
    %% after this is when to add default attributes and namespaces
    %% normalize values
    %% mark attributes as defaulted or actually in the stream, etc.
    %% also flag for if this is namespaced
    case parse_element_1(Rest2, State3) of
        {true, {Rest3, State4}} ->
            State5 = State4#{position := [empty|P],
                             tags := [QName|Tags]},
            event_startElement(QName, As1, Ns, {Rest3, State5});
        {false, {Rest3, State4}} ->
            State5 = State4#{position := [content|P],
                             tags := [QName|Tags]},
            event_startElement(QName, As1, Ns, {Rest3, State5})
    end;
parse_element(?CHARS_REST(_, _), State) ->
    fatal_error(bad_element, State);
?EMPTY1;
?CHECK1.

%%----------------------------------------------------------------------
%% [44] EmptyElemTag ::= '<' Name (S Attribute)* S? '/>'
%% [WFC: Unique Att Spec]
%% return {IsEmpty, State}, trims ws and end bracket off
%%----------------------------------------------------------------------
parse_element_1(?CHARS("/") = Stream, State) ->
    case cf(Stream, State) of
        {error, Reason, PState1} ->
            fatal_error(Reason, PState1);
        {Stream, _} = PState1 ->
            fatal_error(no_element, PState1);
        {Bytes, State1} ->
            parse_element_1(Bytes, State1)
    end;
parse_element_1(?CHARS_REST("/>", Rest), State) ->
    {true, {Rest, State}};
parse_element_1(?CHARS_REST(">", Rest), State) ->
    {false, {Rest, State}}.

%%----------------------------------------------------------------------
%% [41] Attribute ::= Name Eq AttValue 
%% [WFC: No External Entity References]
%% [WFC: No < in Attribute Values]
%%----------------------------------------------------------------------
parse_attributes(?CHARS_REST(C, _) = Bytes, State) when ?is_whitespace(C) ->
    {_, {Bytes1, State1}} = parse_S(Bytes, State),
    parse_attributes_1(Bytes1, State1, {[], []});
parse_attributes(?CHARS_REST(_, _) = Bytes, State) ->
    {{[], []}, {Bytes, State}};
?EMPTY1;
?CHECK1.

parse_attributes_1(?CHARS_REST("/", _) = Bytes, State, Acc) ->
    {Acc, {Bytes, State}};
parse_attributes_1(?CHARS_REST(">", _) = Bytes, State, Acc) ->
    {Acc, {Bytes, State}};
parse_attributes_1(?CHARS_REST(C, _) = Bytes, State, Acc) when ?is_name_start_char(C) ->
    {Acc1, {Bytes1, State1}} = parse_attributes_2(Bytes, State, Acc),
    case peek(Bytes1, State1) of
        {W, _} when ?is_whitespace(W) ->
            {_, {Bytes2, State2}} = parse_S(Bytes1, State1), % trim whitespace
            parse_attributes_1(Bytes2, State2, Acc1);
        {_, {Bytes2, State2}} ->
            parse_attributes_1(Bytes2, State2, Acc1)
    end;
parse_attributes_1(?CHARS_REST(_, _), State, _) ->
    fatal_error(bad_attribute, State);
?EMPTY2;
?CHECK2.

parse_attributes_2(Bytes, State, {Ns, As}) ->
    {Name, {Bytes1, State1}} = parse_Name(Bytes, State),
    {Bytes2, State2} = parse_Eq(Bytes1, State1),
    {Value, {Bytes3, State3}} = parse_AttValue(Bytes2, State2),
    case split_name(Name, State3) of
        {<<>>, <<"xmlns">>} ->
            {{[{Value, <<>>}|Ns], As}, {Bytes3, State3}};
        {<<"xmlns">>, Px} ->
            {{[{Value, Px}|Ns], As}, {Bytes3, State3}};
        Split ->
            case lists:member(Split, As) of
                true ->
                    fatal_error(duplicate_attribute, State3);
                false ->
                    {{Ns, [{Split, Value}|As]}, {Bytes3, State3}}
            end
    end.

%%----------------------------------------------------------------------
%% [10] AttValue ::= '"' ([^<&"] | Reference)* '"' |  "'" ([^<&'] | Reference)* "'"
%%----------------------------------------------------------------------
parse_AttValue(?CHARS_REST(C, Rest), State) when C == $';
                                                 C == $\" ->
    parse_AttValue_1(Rest, State, ?EMPTY, C);
parse_AttValue(?CHARS_REST(_, _), State) ->
    fatal_error(bad_attval, State);
?EMPTY1;
?CHECK1.

parse_AttValue_1(?CHARS_REST(C, Rest), State, Acc, C) ->
    {Acc, {Rest, State}};
parse_AttValue_1(?CHARS_REST("<", _), State, _, _) ->
    fatal_error(bad_attval, State);
parse_AttValue_1(?CHARS_REST("&", Rest), State, Acc, Stop) ->
    case parse_Reference(Rest, State) of
        {C, {Rest1, State1}} when is_integer(C) ->
            parse_AttValue_1(Rest1, State1, ?APPEND(Acc, C), Stop);
        {_N, {Rest1, State1}} ->
            %% TODO resolve entity references ! (here as replacement text)
            parse_AttValue_1(Rest1, State1, Acc, Stop)
    end;
parse_AttValue_1(?CHARS_REST(C, Rest), State, Acc, Stop) when ?is_char(C) ->
    parse_AttValue_1(Rest, State, ?APPEND(Acc, C), Stop);
parse_AttValue_1(?CHARS_REST(_, _), State, _, _) ->
    fatal_error(bad_attval, State);
?EMPTY3;
?CHECK3.

%%----------------------------------------------------------------------
%% [42] ETag ::= '</' Name S? '>'
%% '</' is already trimmed
%%----------------------------------------------------------------------
parse_ETag(?CHARS_REST(C, _) = Bytes, State) when ?is_name_start_char(C) ->
    {Name, {Bytes1, State1}} = parse_Name(Bytes, State),
    {Bytes2, #{position := [_|Ps], tags := [_|Ts]} = State2} = trim_sgt(Bytes1, State1),
    %% now check for tag match and pop tag, position, and maybe NS off the stack
    QName = qualify_element_name(Name, State),
    case Ps of
        [element|_Ps1] -> % last closing tag
            event_endElement(QName, {Bytes2, State2#{position := [misc_post_element], tags := Ts}});
        _ ->
            event_endElement(QName, {Bytes2, State2#{position := Ps, tags := Ts}})
    end;
parse_ETag(?CHARS_REST(_, _), State) ->
    fatal_error(bad_endtag, State);
?EMPTY1;
?CHECK1.

%%----------------------------------------------------------------------
%% [9] EntityValue ::= '"' ([^%&"] | PEReference | Reference)* '"' |
%%                     "'" ([^%&'] | PEReference | Reference)* "'"
%%----------------------------------------------------------------------
parse_EntityValue(?CHARS_REST(C, Rest), State) when C == $';
                                                    C == $\" ->
    parse_EntityValue_1(Rest, State, ?EMPTY, C);
parse_EntityValue(?CHARS_REST(_, _), State) ->
    fatal_error(bad_entval, State);
?EMPTY1;
?CHECK1.

parse_EntityValue_1(?CHARS_REST(C, Rest), State, Acc, C) ->
    {Acc, {Rest, State}};
parse_EntityValue_1(?CHARS_REST("&", Rest), State, Acc, Stop) ->
    case parse_Reference(Rest, State) of
        {C, {Rest1, State1}} when is_integer(C) ->
            parse_EntityValue_1(Rest1, State1, ?APPEND(Acc, C), Stop);
        {_N, {Rest1, State1}} ->
            %% TODO resolve entity references ! (here as replacement text)
            parse_EntityValue_1(Rest1, State1, Acc, Stop)
    end;
parse_EntityValue_1(?CHARS_REST("%", Rest), State, Acc, Stop) ->
    {_N, {Rest1, State1}} = parse_PEReference(Rest, State),
    %% TODO resolve parameter entity references ! (here as replacement text)
    parse_EntityValue_1(Rest1, State1, Acc, Stop);
parse_EntityValue_1(?CHARS_REST(C, Rest), State, Acc, Stop) when ?is_char(C) ->
    parse_EntityValue_1(Rest, State, ?APPEND(Acc, C), Stop);
parse_EntityValue_1(?CHARS_REST(_, _), State, _, _) ->
    fatal_error(bad_entval, State);
?EMPTY3;
?CHECK3.

%%----------------------------------------------------------------------
%% [70] EntityDecl ::= GEDecl | PEDecl
%% '<!E' is in the stream,
%% {Name, {internal, Value} | {external, PubSys} | {external, PubSys, NData}}
%%----------------------------------------------------------------------
parse_EntityDecl(?CHARS_REST("<!ENTITY", Rest), State) ->
    {_, {Rest1, State1}} = parse_S(Rest, State),
    case peek(Rest1, State1) of
        {$%, {Rest2, State2}} ->
            parse_PEDecl(Rest2, State2);
        {_, {Rest2, State2}} ->
            parse_GEDecl(Rest2, State2)
    end;
parse_EntityDecl(Stream, State)
    when Stream == ?CHARS("<!E");
         Stream == ?CHARS("<!EN");
         Stream == ?CHARS("<!ENT");
         Stream == ?CHARS("<!ENTI");
         Stream == ?CHARS("<!ENTIT");
         Stream == ?CHARS("<!ENTITY") ->
    case cf(Stream, State) of
        {error, Reason, PState1} ->
            fatal_error(Reason, PState1);
        {Stream, _} = PState1 ->
            fatal_error(bad_entity, PState1);
        {Bytes, State1} ->
            parse_EntityDecl(Bytes, State1)
    end;
parse_EntityDecl(?CHARS_REST(_, _), State) ->
    fatal_error(bad_entity, State);
?CHECK1.

%%----------------------------------------------------------------------
%% [71] GEDecl ::= '<!ENTITY' S Name S EntityDef S? '>'
%% has 1 char on the stream at least 
%% {general|parameter, internal|external|unparsed, Value}
%%----------------------------------------------------------------------
parse_GEDecl(Stream, State) -> 
    {Name, {Rest1, State1}} = parse_Name(Stream, State),
    {_, {Rest2, State2}} = parse_S(Rest1, State1),
    {PE, {Rest3, State3}} = parse_PEDef(Rest2, State2),
    {_, {Rest4, State4}} = parse_S_1(Rest3, State3, ?EMPTY),
    case PE of
        {_, internal, Value} ->
            {{Name, {general, internal, Value}}, {Rest4, State4}};
        {_, external, PubSys} ->
            case peek(Rest4, State4) of
                {$>, PState} ->
                    {{Name, {general, external, PubSys}}, PState};
                {$N, {Rest5, State5}} ->
                    {NName, PState} = parse_NDataDecl(Rest5, State5),
                    {{Name, {general, unparsed, {PubSys, NName}}}, PState};
                _ ->
                    fatal_error(bad_entity, State4)
            end
    end.

%%----------------------------------------------------------------------
%% [72] PEDecl ::= '<!ENTITY' S '%' S Name S PEDef S? '>'
%% has '%' on the stream 
%%----------------------------------------------------------------------
parse_PEDecl(?CHARS_REST("%", Rest), State) ->
    {_, {Rest1, State1}} = parse_S(Rest, State),
    {Name, {Rest2, State2}} = parse_Name(Rest1, State1),
    {_, {Rest3, State3}} = parse_S(Rest2, State2),
    {PE, PState} = parse_PEDef(Rest3, State3),
    {{?APPEND_STREAM(<<"%">>, Name), PE}, PState}.

%%----------------------------------------------------------------------
%% [73] EntityDef ::= EntityValue | (ExternalID NDataDecl?)
%% [74] PEDef ::= EntityValue | ExternalID
%% only reads EntityValue | ExternalID any NDataDecl after
%%----------------------------------------------------------------------
parse_PEDef(Stream, State) ->
    case peek(Stream, State) of
        {$', {Rest1, State1}} ->
            {Value, PState} = parse_EntityValue(Rest1, State1),
            {{parameter, internal, Value}, PState};
        {$\", {Rest1, State1}} ->
            {Value, PState} = parse_EntityValue(Rest1, State1),
            {{parameter, internal, Value}, PState};
        {_, {Rest1, State1}} ->
            {PubSys, PState} = parse_ExternalID(Rest1, State1),
            {{parameter, external, PubSys}, PState}
    end.

%%----------------------------------------------------------------------
%% [76] NDataDecl ::= S 'NDATA' S Name
%% [VC: Notation Declared]
%% leading S has been stripped 
%%----------------------------------------------------------------------
parse_NDataDecl(?CHARS_REST("NDATA", Rest), State) ->
    {_, {Rest1, State1}} = parse_S(Rest, State),
    parse_Name(Rest1, State1);
parse_NDataDecl(Stream, State) 
    when Stream == ?CHARS("N");
         Stream == ?CHARS("ND");
         Stream == ?CHARS("NDA");
         Stream == ?CHARS("NDAT");
         Stream == ?CHARS("NDATA") ->
    case cf(Stream, State) of
        {error, Reason, PState1} ->
            fatal_error(Reason, PState1);
        {Stream, _} = PState1 ->
            fatal_error(bad_ndata, PState1);
        {Bytes, State1} ->
            parse_NDataDecl(Bytes, State1)
    end;
parse_NDataDecl(?CHARS_REST(_, _), State) ->
    fatal_error(bad_ndata, State);
?EMPTY1;
?CHECK1.

%%----------------------------------------------------------------------
%% [45] elementdecl ::= '<!ELEMENT' S Name S contentspec S? '>'
%% '<!EL' is in the stream,
%%----------------------------------------------------------------------
parse_elementdecl(?CHARS_REST("<!ELEMENT", Rest), State) ->
    {_, {Rest1, State1}} = parse_S(Rest, State),
    {Name, {Rest2, State2}} = parse_Name(Rest1, State1),
    {_, {Rest3, State3}} = parse_S(Rest2, State2),
    {Spec, PState} = parse_contentspec(Rest3, State3),
    {{Name, Spec}, PState};
parse_elementdecl(Stream, State)
    when Stream == ?CHARS("<!EL");
         Stream == ?CHARS("<!ELE");
         Stream == ?CHARS("<!ELEM");
         Stream == ?CHARS("<!ELEME");
         Stream == ?CHARS("<!ELEMEN");
         Stream == ?CHARS("<!ELEMENT") ->
    case cf(Stream, State) of
        {error, Reason, PState1} ->
            fatal_error(Reason, PState1);
        {Stream, _} = PState1 ->
            fatal_error(bad_entity, PState1);
        {Bytes, State1} ->
            parse_elementdecl(Bytes, State1)
    end;
parse_elementdecl(?CHARS_REST(_, _), State) ->
    fatal_error(bad_element, State);
?CHECK1.

%%----------------------------------------------------------------------
%% [46] contentspec ::= 'EMPTY' | 'ANY' | Mixed | children
%%----------------------------------------------------------------------
parse_contentspec(?CHARS_REST("EMPTY", Rest), State) ->
    PState = trim_sgt(Rest, State),
    {empty, PState};
parse_contentspec(?CHARS_REST("ANY", Rest), State) ->
    PState = trim_sgt(Rest, State),
    {any, PState};
parse_contentspec(?CHARS_REST("(", Rest), State) ->
    {_, {Rest1, State1}} = parse_S_1(Rest, State, ?EMPTY),
    case peek(Rest1, State1) of
        {$#, {Rest2, State2}} ->
            parse_Mixed(Rest2, State2);
        {_, {Rest2, State2}} ->
            parse_children(Rest2, State2)
    end;
parse_contentspec(Stream, State)
    when Stream == ?CHARS("E");
         Stream == ?CHARS("EM");
         Stream == ?CHARS("EMP");
         Stream == ?CHARS("EMPT");
         Stream == ?CHARS("A");
         Stream == ?CHARS("AN") ->
    case cf(Stream, State) of
        {error, Reason, PState1} ->
            fatal_error(Reason, PState1);
        {Stream, _} = PState1 ->
            fatal_error(bad_element, PState1);
        {Bytes, State1} ->
            parse_contentspec(Bytes, State1)
    end;
parse_contentspec(?CHARS_REST(_, _), State) ->
    fatal_error(bad_element, State);
?EMPTY1;
?CHECK1.

%%----------------------------------------------------------------------
%% [47] children ::= (choice | seq) ('?' | '*' | '+')?
%% [48] cp       ::= (Name | choice | seq) ('?' | '*' | '+')?
%% [49] choice   ::= '(' S? cp ( S? '|' S? cp )+ S? ')'
%% [50] seq      ::= '(' S? cp ( S? ',' S? cp )* S? ')'
%% '(' has been trimmed, one char on stream
%%----------------------------------------------------------------------
parse_children(Stream, State) -> 
    {Acc, {Rest1, State1}} = parse_seq(Stream, State),
    parse_quantifier(Rest1, State1, Acc).


%%----------------------------------------------------------------------
%% [49] choice   ::= '(' S? cp ( S? '|' S? cp )+ S? ')'
%% [50] seq      ::= '(' S? cp ( S? ',' S? cp )* S? ')'
%% if a choice and not seq, passes to parse_choice
%%----------------------------------------------------------------------
parse_seq(Stream, State) ->
    {Cp, {Rest1, State1}} = parse_cp(Stream, State),
    {_, {Rest2, State2}} = parse_S_1(Rest1, State1, ?EMPTY),
    case peek(Rest2, State2) of
        {$|, {Rest3, State3}} ->
            parse_choice(Rest3, State3, [Cp]);
        {$,, {Rest3, State3}} ->
            parse_seq_1(Rest3, State3, [Cp]);
        {$), {Rest3, State3}} ->
            parse_seq_1(Rest3, State3, [Cp])
    end.

parse_seq_1(?CHARS_REST(")", Rest), State, Acc) ->
    {{seq, lists:reverse(Acc)}, {Rest, State}};
parse_seq_1(?CHARS_REST(",", Rest), State, Acc) ->
    {_, {Rest1, State1}} = parse_S_1(Rest, State, ?EMPTY),
    {Cp, {Rest2, State2}} = parse_cp(Rest1, State1),
    {_, {Rest3, State3}} = parse_S_1(Rest2, State2, ?EMPTY),
    parse_seq_1(Rest3, State3, [Cp|Acc]);
parse_seq_1(?CHARS_REST(_, _), State, _) ->
    fatal_error(bad_element, State);
?EMPTY2;
?CHECK2.

%%----------------------------------------------------------------------
%% [49] choice   ::= '(' S? cp ( S? '|' S? cp )+ S? ')'
%%----------------------------------------------------------------------
parse_choice(?CHARS_REST(")", _), State, [_]) ->
    fatal_error(bad_element, State);
parse_choice(?CHARS_REST(")", Rest), State, Acc) ->
    {{choice, lists:reverse(Acc)}, {Rest, State}};
parse_choice(?CHARS_REST(",", Rest), State, Acc) ->
    {_, {Rest1, State1}} = parse_S_1(Rest, State, ?EMPTY),
    {Cp, {Rest2, State2}} = parse_cp(Rest1, State1),
    {_, {Rest3, State3}} = parse_S_1(Rest2, State2, ?EMPTY),
    parse_choice(Rest3, State3, [Cp|Acc]);
parse_choice(?CHARS_REST(_, _), State, _) ->
    fatal_error(bad_element, State);
?EMPTY2;
?CHECK2.

%%----------------------------------------------------------------------
%% [48] cp       ::= (Name | choice | seq) ('?' | '*' | '+')?
%%----------------------------------------------------------------------
parse_cp(?CHARS_REST(C, _) = Stream, State) when ?is_name_start_char(C) ->
    {Name, {Rest1, State1}} = parse_Name(Stream, State),
    parse_quantifier(Rest1, State1, Name);
parse_cp(?CHARS_REST("(", Rest), State) ->
    {_, {Rest1, State1}} = parse_S_1(Rest, State, ?EMPTY),
    {Seq, {Rest2, State2}} = parse_seq(Rest1, State1),
    parse_quantifier(Rest2, State2, Seq);
parse_cp(?CHARS_REST(_, _), State) ->
    fatal_error(bad_element, State);
?EMPTY1;
?CHECK1.

parse_quantifier(?CHARS_REST("?", Rest), State, Acc) -> 
    {{Acc, '?'}, {Rest, State}};
parse_quantifier(?CHARS_REST("*", Rest), State, Acc) -> 
    {{Acc, '*'}, {Rest, State}};
parse_quantifier(?CHARS_REST("+", Rest), State, Acc) -> 
    {{Acc, '+'}, {Rest, State}};
parse_quantifier(?CHARS_REST(_, _) = Stream, State, Acc) -> 
    {{Acc, one}, {Stream, State}};
?EMPTY2;
?CHECK2.

%%----------------------------------------------------------------------
%% [51] Mixed ::= '(' S? '#PCDATA' (S? '|' S? Name)* S? ')*' | '(' S? '#PCDATA' S? ')'
%% '(' has been trimmed '#' is in stream
%%----------------------------------------------------------------------
parse_Mixed(?CHARS_REST("#PCDATA", Rest), State) ->
    {_, {Rest1, State1}} = parse_S_1(Rest, State, ?EMPTY),
    case peek(Rest1, State1) of
        {$|, {Rest2, State2}} ->
            parse_Mixed_1(Rest2, State2, []);
        {$), {Rest2, State2}} ->
            parse_Mixed_2(Rest2, State2);
        {_, PState} ->
            fatal_error(bad_element, PState)
    end;
parse_Mixed(Stream, State)
    when Stream == ?CHARS("#");
         Stream == ?CHARS("#P");
         Stream == ?CHARS("#PC");
         Stream == ?CHARS("#PCD");
         Stream == ?CHARS("#PCDA");
         Stream == ?CHARS("#PCDAT");
         Stream == ?CHARS("#PCDATA") -> 
    case cf(Stream, State) of
        {error, Reason, PState1} ->
            fatal_error(Reason, PState1);
        {Stream, _} = PState1 ->
            fatal_error(bad_element, PState1);
        {Bytes, State1} ->
            parse_Mixed(Bytes, State1)
    end;
parse_Mixed(?CHARS_REST(_, _), State) ->
    fatal_error(bad_element, State);
?CHECK1.

parse_Mixed_1(?CHARS_REST("|", Rest), State, Acc) ->
    {_, {Rest1, State1}} = parse_S_1(Rest, State, ?EMPTY),
    {Name, {Rest2, State2}} = parse_Name(Rest1, State1),
    {_, {Rest3, State3}} = parse_S_1(Rest2, State2, ?EMPTY),
    parse_Mixed_1(Rest3, State3, [Name|Acc]);
parse_Mixed_1(?CHARS_REST(")*", Rest), State, Acc) ->
    {lists:reverse(Acc), {Rest, State}};
parse_Mixed_1(Stream, State, Acc) when Stream == ?CHARS(")") ->
    case cf(Stream, State) of
        {error, Reason, PState1} ->
            fatal_error(Reason, PState1);
        {Stream, _} = PState1 ->
            fatal_error(bad_element, PState1);
        {Bytes, State1} ->
            parse_Mixed_1(Bytes, State1, Acc)
    end;
parse_Mixed_1(?CHARS_REST(_, _), State, _) ->
    fatal_error(bad_element, State);
?EMPTY2;
?CHECK2.

parse_Mixed_2(?CHARS_REST(")", Rest), State) ->
    {mixed, {Rest, State}}.


qualify_element_name(Name, #{inscope_ns := [Nss|_]} = State) ->
    Split = split_name(Name, State),
    expand_qname(Split, Nss).

qualify_attribute_names(Atts, #{inscope_ns := [Nss|_]} = State) ->
    [Att#{qname := qualify_attribute_name(Name, State, Nss)} ||
     #{qname := Name} = Att <- Atts].

qualify_attribute_name(Name, State, Nss) ->
    Split = split_name(Name, State),
    expand_attribute_qname(Split, Nss).

expand_qname({P, L}, Nss) ->
    case Nss of
        #{P := N} ->
            {N, P, L};
        _ ->
            fatal_error(unknown_prefix, P)
    end.

expand_attribute_qname({<<>>, L}, _) ->
    {<<>>, <<>>, L};
expand_attribute_qname({P, L}, Nss) ->
    case Nss of
        #{P := N} ->
            {N, P, L};
        _ ->
            fatal_error(unknown_prefix, P)
    end.


split_name(Name, #{split := Pattern} = State) ->
    case binary:split(Name, Pattern, [global]) of
        [L] ->
            {?EMPTY, L};
        [P, L] ->
            {P, L};
        _ ->
            fatal_error(invalid_name, State)
    end.

% S? and closing '>'
trim_sgt(?CHARS_REST(">", Rest), State) ->
    {Rest, State};
trim_sgt(?CHARS_REST(C, _) = Stream, State) when ?is_whitespace(C) ->
    {_, {Rest1, State1}} = parse_S(Stream, State, ?EMPTY),
    trim_sgt(Rest1, State1);
trim_sgt(?CHARS_REST(_, _), State) ->
    fatal_error(bad_end, State);
?EMPTY1;
?CHECK1.

trim_qgt(?CHARS_REST("?>", Rest), State) ->
    {Rest, State};
trim_qgt(Stream, State) when Stream == ?CHARS("?") ->
    case cf(Stream, State) of
        {error, Reason, PState1} ->
            fatal_error(Reason, PState1);
        {Stream, _} = PState1 ->
            fatal_error(bad_end, PState1);
        {Stream1, State1} ->
            trim_qgt(Stream1, State1)
    end;
trim_qgt(?CHARS_REST(_, _), State) ->
    fatal_error(bad_end, State);
?EMPTY1;
?CHECK1.

peek(?CHARS_REST(C, _) = Stream, State) -> {C, {Stream, State}};
peek(?EMPTY, State) ->
    case cf(?EMPTY, State) of
        {error, Reason, PState1} ->
            fatal_error(Reason, PState1);
        {?EMPTY, State1} ->
            {?EMPTY, State1};
        {Stream, State1} ->
            peek(Stream, State1)
    end;
?CHECK1.


set_next_parser_position(Pos, {Stream, #{position := S} = State}) ->
    {Stream, State#{position := [Pos|S]}}.

resolve_parameter_entity(PERef, #{proc := #{params := Params}} = _DTD, State) ->
    case Params of
        #{PERef := {external, PubSys}} ->
            %% XXX maybe resolve then add the value to the DTD to cache it
            {external, PubSys, State};
        #{PERef := {internal, Value}} ->
            {internal, Value, State};
        _ ->
            unknown
    end;
resolve_parameter_entity(_, _, _) -> unknown.

add_attlist_to_dtd({ElemName, AttDefs}, #{proc := #{atts := Atts} = Proc} = DTD) ->
    DTD#{proc := Proc#{atts := Atts#{ElemName => AttDefs}}};
add_attlist_to_dtd(AttList, #{proc := undefined} = DTD) ->
    add_attlist_to_dtd(AttList, DTD#{proc => empty_proc_dtd()}).

add_notation_to_dtd({Name, Pub, Sys}, #{proc := #{nots := Nots} = Proc} = DTD) ->
    DTD#{proc := Proc#{nots := Nots#{Name => {Pub, Sys}}}};
add_notation_to_dtd(Notation, #{proc := undefined} = DTD) ->
    add_notation_to_dtd(Notation, DTD#{proc => empty_proc_dtd()}).

%{Name, {general|parameter, internal|external|unparsed, Value}}
add_entity_to_dtd({Name, {general, Type, Value}}, #{proc := #{refs := Ents} = Proc} = DTD) ->
    DTD#{proc := Proc#{refs := Ents#{Name => {Type, Value}}}};
add_entity_to_dtd({Name, {parameter, Type, Value}}, #{proc := #{params := Ents} = Proc} = DTD) ->
    DTD#{proc := Proc#{params := Ents#{Name => {Type, Value}}}};
add_entity_to_dtd(Entity, #{proc := undefined} = DTD) ->
    add_entity_to_dtd(Entity, DTD#{proc => empty_proc_dtd()}).

add_element_to_dtd({Name, Elem}, #{proc := #{elems := Elems} = Proc} = DTD) ->
    DTD#{proc := Proc#{elems := Elems#{Name => Elem}}};
add_element_to_dtd(Elem, #{proc := undefined} = DTD) ->
    add_element_to_dtd(Elem, DTD#{proc => empty_proc_dtd()}).

empty_proc_dtd() ->
    #{elems => #{},
      atts => #{},
      nots => #{},
      refs => #{},
      params => #{}
     }.

namespace_map_from_list([#{prefix := P, uri := U}|T], Map) ->
    namespace_map_from_list(T, Map#{P => U});
namespace_map_from_list([], Map) ->
    Map.

-spec cf(binary(), parser_state()) ->
          {error, term(), ext_parser_state()} | ext_parser_state().
cf(Stream, #{continuation := undefined} = State) ->
    {error, no_bytes, {Stream, State}};
cf(Stream, #{continuation := {CF, CS}} = State) ->
    case CF(CS) of
        {Bin, CS1} when is_binary(Bin) ->
            {?APPEND_STREAM(Stream, Bin), State#{continuation := {CF, CS1}}};
        eof ->
            _ = CF(close),
            {Stream, State#{continuation := undefined}};
        {error, Err} ->
            fatal_error(Err, {Stream, State})
    end.

fatal_error(Reason, State) ->
    error(Reason, [State]).


