-module(yaccety_sax).

-include("yaccety_sax.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

-export([
    event_characters/5,
    event_comment/2,
    event_dtd/3,
    event_endDocument/1,
    event_endElement/2,
    event_processingInstruction/3,
    event_startDocument/6,
    event_startElement/4
]).

-export([file/1, file/2]).
-export([stream/1, stream/2]).
-export([default_file_continuation/1, default_file_continuation/2]).

-export([write_event/2]).

-export([
    get_element_text/1,
    next_event/1,
    next_tag/1
]).

-type parser_state() :: #ys_state{}.
-type ext_parser_state() :: parser_state().

-export_type([ext_parser_state/0]).

-type options() :: [
    %% report whitespace character events?
    {whitespace, boolean()}
    %% report comment events?
    | {comments, boolean()}
    %% report processing-instruction events?
    | {proc_inst, boolean()}
    %% base-uri of this stream
    | {base, binary()}
    %% pre-processed dtd to use for this stream
    | {dtd, processed_dtd()}
    %% Continuation fun and State
    | {continuation, {Fun :: fun(), State :: any()}}
].

-export_type([options/0]).

-define(BYTES, <<_:Offset/binary, Bytes/binary>> = Stream).

stream(Stream) -> stream(Stream, []).

stream(Stream, Opts) ->
    State = opts(Opts, #ys_state{}),
    State#ys_state{stream_offset = {Stream, 0}}.

file(Filename) -> file(Filename, []).

file(Filename, Opts) when is_list(Filename) ->
    file(unicode:characters_to_binary(Filename), Opts);
file(Filename, Opts) ->
    State = opts(Opts, #ys_state{}),
    Cont = default_file_continuation(Filename),
    Base = filename:dirname(filename:absname(Filename)),
    State#ys_state{
        continuation = {Cont, <<>>},
        base = Base
    }.

opts([{continuation, {F, S}} | T], Acc) when is_function(F, 1) ->
    opts(T, Acc#ys_state{continuation = {F, S}});
opts([{base, Base} | T], Acc) when is_binary(Base) ->
    opts(T, Acc#ys_state{base = Base});
opts([{dtd, DTD} | T], Acc) when is_map(DTD) ->
    opts(T, Acc#ys_state{dtd = DTD});
opts([{proc_inst, Bool} | T], Acc) when is_boolean(Bool) ->
    opts(T, Acc#ys_state{proc_inst = Bool});
opts([{comments, Bool} | T], Acc) when is_boolean(Bool) ->
    opts(T, Acc#ys_state{comments = Bool});
opts([{whitespace, Bool} | T], Acc) when is_boolean(Bool) ->
    opts(T, Acc#ys_state{whitespace = Bool});
opts([], Acc) ->
    Acc;
opts([H | _], _) ->
    fatal_error(unknown_option, H).

default_file_continuation(Filename) -> default_file_continuation(Filename, 16384).

default_file_continuation(Filename, Size) ->
    case file:open(Filename, [raw, {read_ahead, Size}, read, binary]) of
        {ok, FD} ->
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
            end;
        {error, _} = OpenErr ->
            OpenErr
    end.

write_event(#{type := startDocument}, {Bytes, State}) ->
    % XXX this should add version, standalone, encoding
    {Bytes, State};
write_event(#{type := endDocument}, {Bytes, State}) ->
    {Bytes, State};
write_event(
    #{
        type := characters,
        data := Data
    },
    {Bytes, State}
) ->
    % XXX should normalize text, maybe CDATA wrap
    {<<Bytes/binary, Data/binary>>, State};
write_event(
    #{
        type := endElement,
        qname := {_, Px, Ln}
    },
    {Bytes, State}
) ->
    case Px of
        <<>> ->
            {<<Bytes/binary, "</", Ln/binary, ">">>, State};
        _ ->
            {<<Bytes/binary, "</", Px/binary, ":", Ln/binary, ">">>, State}
    end;
write_event(
    #{
        type := startElement,
        namespaces := Nss,
        attributes := Atts,
        qname := QName
    },
    {Bytes, State}
) ->
    NmFun = fun
        ({_, <<>>, Ln}) ->
            Ln;
        ({_, Px, Ln}) ->
            <<Px/binary, ":", Ln/binary>>
    end,
    Name = NmFun(QName),
    NsFun = fun
        (#{prefix := <<>>, uri := NsUri}) ->
            <<" xmlns=\"", NsUri, "\"">>;
        (#{prefix := NsPx, uri := NsUri}) ->
            <<" xmlns:", NsPx/binary, "=\"", NsUri, "\"">>
    end,
    AtFun = fun(
        #{
            qname := AQName,
            value := AValue
        }
    ) ->
        AQName1 = NmFun(AQName),
        <<" ", AQName1/binary, "=\"", AValue/binary, "\"">>
    end,
    IoList = ["<", Name, [NsFun(N) || N <- Nss], [AtFun(A) || A <- Atts], ">"],
    Out = erlang:iolist_to_binary(IoList),
    {<<Bytes/binary, Out/binary>>, State};
% TODO processing-instructions, comments
write_event(_Event, {Bytes, State}) ->
    {Bytes, State}.

%% -spec get_element_text(State) -> {Text, State}
%%         when State :: ext_parser_state(),
%%              Text  :: xml_characters() | {error, no_character}.
%% Returns the content of a text-only element or error if not a
%% text-only element.
%% TODO implement
get_element_text(State) ->
    {text, State}.

-spec next_event(State) -> {Event, State} when
    State :: ext_parser_state(),
    Event :: xml_event() | {error, no_event}.
%% Returns the next event or error if no event in the stream.
next_event(#ys_state{position = []} = State) ->
    {endDocument, State};
next_event(#ys_state{position = [empty | Ps], tags = [T | Ts]} = State) ->
    event_endElement(T, State#ys_state{position = Ps, tags = Ts});
next_event(#ys_state{position = [content | _], stream_offset = {Stream, Offset}} = State) ->
    ?BYTES,
    case ys_parse:parse_content(Bytes, Stream, Offset, State) of
        {Event, State1} ->
            {Event, State1};
        State1 ->
            next_event(State1)
    end;
next_event(#ys_state{position = [document | _], stream_offset = {Stream, Offset}} = State) ->
    ?BYTES,
    % sets position to misc_pre_dtd
    ys_parse:parse_XMLDecl(Bytes, Stream, Offset, State);
next_event(#ys_state{position = [fragment | _], stream_offset = {Stream, Offset}} = State) ->
    ?BYTES,
    % sets position to misc_pre_dtd
    ys_parse:parse_XMLDecl(Bytes, Stream, Offset, State);
next_event(#ys_state{position = [misc_pre_dtd | Ps], stream_offset = {Stream, Offset}} = State) ->
    ?BYTES,
    case ys_parse:parse_Misc(Bytes, Stream, Offset, State) of
        {Event, State1} ->
            {Event, State1};
        State1 ->
            next_event(State1#ys_state{position = [dtd | Ps]})
    end;
next_event(#ys_state{position = [dtd | Ps], stream_offset = {Stream, Offset}} = State) ->
    ?BYTES,
    next_event(State#ys_state{position = [element | Ps]});
% case ys_parse:parse_doctypedecl(Bytes, Stream, Offset, State) of
%     {#{proc := Proc} = Event, State1} ->
%         {Event, State1#{
%             dtd := Proc,
%             position := [misc_post_dtd | Ps]
%         }};
%     State1 ->
%         next_event(State1#{position := [element | Ps]})
% end;
next_event(#ys_state{position = [misc_post_dtd | Ps], stream_offset = {Stream, Offset}} = State) ->
    ?BYTES,
    case ys_parse:parse_Misc(Bytes, Stream, Offset, State) of
        {Event, State1} ->
            {Event, State1};
        State1 ->
            next_event(State1#ys_state{position = [element | Ps]})
    end;
next_event(#ys_state{position = [element | _], stream_offset = {Stream, Offset}} = State) ->
    ?BYTES,
    ys_parse:parse_element(Bytes, Stream, Offset, State);
next_event(#ys_state{position = [misc_post_element | _], stream_offset = {Stream, Offset}} = State) ->
    ?BYTES,
    case ys_parse:parse_Misc(Bytes, Stream, Offset, State) of
        {Event, State1} ->
            {Event, State1};
        State1 ->
            event_endDocument(State1#ys_state{stream_offset = {<<>>, 0}})
    end;
next_event(State) ->
    {text, State}.

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

-spec event_startDocument(
    Version,
    Encoding,
    EncSet,
    StandAlone,
    StandSet,
    State
) -> {Event, State} when
    Version :: binary(),
    Encoding :: binary(),
    EncSet :: boolean(),
    StandAlone :: boolean(),
    StandSet :: boolean(),
    State :: ext_parser_state(),
    Event :: xml_startDocument().
event_startDocument(
    Version,
    Encoding,
    EncSet,
    StandAlone,
    StandSet,
    #ys_state{line = Line} = State
) ->
    Event = #{
        type => startDocument,
        line => Line,
        version => Version,
        encoding => Encoding,
        enc_set => EncSet,
        standalone => StandAlone,
        sa_set => StandSet
    },
    {Event, State}.

-spec event_endDocument(State) -> {Event, State} when
    State :: ext_parser_state(),
    Event :: xml_endDocument().
event_endDocument(#ys_state{line = Line} = State) ->
    Event = #{
        type => endDocument,
        line => Line
    },
    {Event, State}.

% only reported if not being handled
% xml_entityReference() -> ok.

-spec event_dtd(
    Text :: binary(),
    Processed :: map(),
    State :: ext_parser_state()
) -> {xml_dtd(), ext_parser_state()}.
event_dtd(Text, Processed, {_, #ys_state{line = Line}} = State) ->
    Event = #{
        type => dtd,
        line => Line,
        text => Text,
        proc => Processed
    },
    {Event, State}.

-spec event_startElement(QName, Attributes, Namespaces, State) -> {Event, State} when
    QName :: qname(),
    Attributes :: list(xml_attribute()),
    Namespaces :: list(xml_namespace()),
    State :: ext_parser_state(),
    Event :: xml_startElement().
event_startElement(QName, Attributes, Namespaces, #ys_state{line = Line} = State) ->
    Event = #{
        type => startElement,
        line => Line,
        qname => QName,
        attributes => Attributes,
        namespaces => Namespaces
    },
    {Event, State}.

-spec event_endElement(QName, State) -> {Event, State} when
    QName :: qname(),
    State :: ext_parser_state(),
    Event :: xml_endElement().
event_endElement(
    QName,
    #ys_state{
        line = Line
    } = State
) ->
    Event = #{
        type => endElement,
        line => Line,
        qname => QName
    },
    {Event, State}.

-spec event_processingInstruction(Target, Data, State) -> {Event, State} when
    Target :: binary(),
    Data :: binary(),
    State :: ext_parser_state(),
    Event :: xml_processingInstruction().
event_processingInstruction(Target, Data, #ys_state{line = Line} = State) ->
    Event = #{
        type => processingInstruction,
        line => Line,
        target => Target,
        data => Data
    },
    {Event, State}.

-spec event_characters(Data, CData, Ignorable, IsWs, State) -> {Event, State} | State when
    Data :: binary(),
    CData :: boolean(),
    Ignorable :: boolean(),
    IsWs :: boolean(),
    State :: ext_parser_state(),
    Event :: xml_characters().
event_characters(_Data, _CData, _Ignorable, true, #ys_state{whitespace = false} = State) ->
    State;
event_characters(Data, CData, Ignorable, IsWs, #ys_state{line = Line} = State) ->
    Event = #{
        type => characters,
        line => Line,
        data => iolist_to_binary(Data),
        cdata => CData,
        ignore => Ignorable,
        ws => IsWs
    },
    {Event, State}.

-spec event_comment(Text, State) -> {Event, State} when
    Text :: binary(),
    State :: ext_parser_state(),
    Event :: xml_comment().
event_comment(Text, #ys_state{line = Line} = State) ->
    Event = #{
        type => comment,
        line => Line,
        text => Text
    },
    {Event, State}.

%xml_attribute() -> ok. % in startElement
%xml_namespace() -> ok. % in startElement/endElement

fatal_error(Reason, State) ->
    error(Reason, [State]).
