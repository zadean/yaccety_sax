-module(yaccety_sax).

-include("yaccety_sax.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

-export([
    event_characters/5,
    event_comment/2,
    event_dtd/2,
    event_endDocument/1,
    event_endElement/2,
    event_processingInstruction/3,
    event_startDocument/6,
    event_startElement/4
]).

-export([file/1, file/2]).
-export([stream/1, stream/2]).

-export([write_event/2]).

-export([next_event/1]).

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
    %% External entity reader fun, if any
    | {external, Fun :: fun()}
].

-export_type([options/0]).

stream(Stream) -> stream(Stream, []).

stream(Stream, Opts) ->
    State = opts(Opts, #ys_state{}),
    State#ys_state{rest_stream = Stream}.

file(Filename) -> file(Filename, []).

file(Filename, Opts) when is_list(Filename) ->
    file(unicode:characters_to_binary(Filename), Opts);
file(Filename, Opts) ->
    State = opts(Opts, #ys_state{}),
    Base = filename:dirname(filename:absname(Filename)),
    case State#ys_state.continuation of
        undefined ->
            State#ys_state{
                continuation = ys_utils:default_file_continuation(Filename),
                base = Base
            };
        _ ->
            State#ys_state{base = Base}
    end.

opts([{continuation, {F, S}} | T], Acc) when is_function(F, 1) ->
    opts(T, Acc#ys_state{continuation = {F, S}});
opts([{external, F} | T], Acc) when is_function(F, 2) ->
    opts(T, Acc#ys_state{external = F});
opts([{base, Base} | T], Acc) when is_list(Base) ->
    opts(T, Acc#ys_state{base = unicode:characters_to_binary(Base)});
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
opts([{namespace_aware, Bool} | T], Acc) when is_boolean(Bool) ->
    opts(T, Acc#ys_state{namespace_aware = Bool});
opts([], Acc) ->
    Acc;
opts([H | _], _) ->
    fatal_error(unknown_option, H).

-spec next_event(State) -> {Event, State} when
    State :: ext_parser_state(), Event :: xml_event() | {error, no_event}.
%% Returns the next event or error if no event in the stream.
next_event(#ys_state{position = []}) ->
    {error, no_event};
next_event(#ys_state{position = [Position | Ps], rest_stream = Stream} = State) ->
    case Position of
        ?content ->
            case ys_parse:parse_content(Stream, State) of
                {no_bytes, State1} ->
                    fatal_error(illegal_data, {Stream, State1});
                {Event, State1} ->
                    {Event, State1};
                State1 ->
                    next_event(State1)
            end;
        ?empty ->
            [T | Ts] = State#ys_state.tags,
            event_endElement(T, State#ys_state{position = Ps, tags = Ts});
        ?document ->
            % sets position to misc_pre_dtd
            ys_parse:parse_XMLDecl(Stream, State);
        ?misc_pre_dtd ->
            case ys_parse:parse_Misc(Stream, State) of
                {no_bytes, State1} ->
                    fatal_error(illegal_data, {Stream, State1});
                {Event, State1} ->
                    {Event, State1};
                State1 ->
                    next_event(State1#ys_state{position = [?dtd | Ps]})
            end;
        ?dtd ->
            case ys_parse:parse_doctypedecl(Stream, State) of
                {Event, State1} ->
                    {Event, State1#ys_state{position = [?misc_post_dtd | Ps]}};
                State1 ->
                    next_event(State1#ys_state{position = [?element | Ps]})
            end;
        ?misc_post_dtd ->
            case ys_parse:parse_Misc(Stream, State) of
                {no_bytes, State1} ->
                    fatal_error(missing_element, {Stream, State1});
                {Event, State1} ->
                    {Event, State1};
                State1 ->
                    next_event(State1#ys_state{position = [?element | Ps]})
            end;
        ?element ->
            ys_parse:parse_element(Stream, State);
        ?misc_post_element ->
            case ys_parse:parse_Misc(Stream, State) of
                {no_bytes, State1} ->
                    event_endDocument(State1#ys_state{rest_stream = <<>>});
                {Event, State1} ->
                    {Event, State1};
                State1 ->
                    fatal_error(illegal_data, {Stream, State1})
            end
    end.

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
    {Event, State#ys_state{standalone = StandAlone}}.

-spec event_endDocument(State) -> {Event, State} when
    State :: ext_parser_state(), Event :: xml_endDocument().
event_endDocument(#ys_state{line = Line} = State) ->
    Event = #{
        type => endDocument,
        line => Line
    },
    {Event, State}.

-spec event_dtd(
    Processed :: processed_dtd(),
    State :: ext_parser_state()
) -> {xml_dtd(), ext_parser_state()}.
event_dtd(Processed, #ys_state{line = Line} = State) ->
    Event = #{
        type => dtd,
        line => Line,
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
    QName :: qname(), State :: ext_parser_state(), Event :: xml_endElement().
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
event_characters(<<>>, _CData, _Ignorable, _, State) ->
    State;
event_characters(Data, CData, Ignorable, IsWs, #ys_state{line = Line} = State) ->
    Event = #{
        type => characters,
        line => Line,
        data => Data,
        cdata => CData,
        ignore => Ignorable,
        ws => IsWs
    },
    {Event, State}.

-spec event_comment(Text, State) -> {Event, State} when
    Text :: binary(), State :: ext_parser_state(), Event :: xml_comment().
event_comment(Text, #ys_state{line = Line} = State) ->
    Event = #{
        type => comment,
        line => Line,
        text => Text
    },
    {Event, State}.

write_event(#{type := startDocument}, {Bytes, State}) ->
    % XXX this should add version, standalone, encoding
    {Bytes, State};
write_event(#{type := endDocument}, {Bytes, State}) ->
    {Bytes, State};
write_event(#{type := characters, data := Data}, {Bytes, State}) ->
    % XXX should normalize text, maybe CDATA wrap
    {<<Bytes/binary, Data/binary>>, State};
write_event(#{type := endElement, qname := {_, Px, Ln}}, {Bytes, State}) ->
    case Px of
        <<>> -> {<<Bytes/binary, "</", Ln/binary, ">">>, State};
        _ -> {<<Bytes/binary, "</", Px/binary, ":", Ln/binary, ">">>, State}
    end;
write_event(
    #{type := startElement, namespaces := Nss, attributes := Atts, qname := QName}, {Bytes, State}
) ->
    NmFun = fun
        ({_, <<>>, Ln}) ->
            Ln;
        ({_, Px, Ln}) ->
            [Px, <<":">>, Ln]
    end,
    Name = NmFun(QName),
    NsFun = fun
        ({NsUri, <<>>}) ->
            [<<" xmlns=\"">>, NsUri, <<"\"">>];
        ({NsUri, NsPx}) ->
            [<<" xmlns:">>, NsPx, <<"=\"">>, NsUri, <<"\"">>]
    end,
    AtFun = fun({AQName, AValue}) ->
        AQName1 = NmFun(AQName),
        [<<" ">>, AQName1, <<"=\"">>, AValue, <<"\"">>]
    end,
    IoList = ["<", Name, [NsFun(N) || N <- Nss], [AtFun(A) || A <- Atts], ">"],
    Out = iolist_to_binary(IoList),
    {<<Bytes/binary, Out/binary>>, State};
% TODO processing-instructions, comments
write_event(_Event, {Bytes, State}) ->
    {Bytes, State}.

fatal_error(Reason, State) ->
    error(Reason, [State]).
