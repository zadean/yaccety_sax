-module(yaccety_sax_simple).

-include("yaccety_sax_simple.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

-export([string/1]).

-export([next_event/1]).

-type ext_parser_state() :: #ys_state_simple{}.

-export_type([ext_parser_state/0]).

string(Stream) -> #ys_state_simple{rest_stream = Stream}.

-spec next_event(State) -> {Event, State} when
    State :: ext_parser_state(), Event :: xml_event().
%% Returns the next event or error if no event in the stream.
next_event(#ys_state_simple{position = []} = State) ->
    {endDocument, State};
next_event(#ys_state_simple{position = [Position | _], rest_stream = Stream} = State) ->
    case Position of
        ?content ->
            next_event_content(Stream, State);
        ?empty ->
            [T | _] = State#ys_state_simple.tags,
            ys_parse_simple:event_endElement(T, trim_pos_tag(State));
        ?document ->
            % sets position to misc_post_dtd
            ys_parse_simple:parse_XMLDecl(Stream, State);
        ?misc_post_dtd ->
            case ys_parse_simple:parse_Misc(Stream, State) of
                {no_bytes, State1} ->
                    fatal_error(missing_element, {Stream, State1});
                {Event, State1} ->
                    {Event, State1};
                State1 ->
                    next_event(add_element_pos(State1))
            end;
        ?element ->
            ys_parse_simple:parse_element(Stream, State);
        ?misc_post_element ->
            case ys_parse_simple:parse_Misc(Stream, State) of
                {no_bytes, State1} ->
                    ys_parse_simple:event_endDocument(State1#ys_state_simple{rest_stream = <<>>});
                {Event, State1} ->
                    {Event, State1};
                State1 ->
                    fatal_error(illegal_data, {Stream, State1})
            end;
        _ ->
            {{unknown_state, Position}, State}
    end.

next_event_content(Stream, State) ->
    case ys_parse_simple:parse_content(Stream, State) of
        {no_bytes, State1} ->
            fatal_error(illegal_data, {Stream, State1});
        {Event, State1} ->
            {Event, State1};
        State1 ->
            next_event(State1)
    end.

add_element_pos(#ys_state_simple{rest_stream = Stream, position = Ps, tags = Ts}) ->
    #ys_state_simple{rest_stream = Stream, position = [element | Ps], tags = Ts}.

trim_pos_tag(#ys_state_simple{rest_stream = Stream, position = [_ | Ps], tags = [_ | Ts]}) ->
    #ys_state_simple{rest_stream = Stream, position = Ps, tags = Ts}.

fatal_error(Reason, State) ->
    error(Reason, [State]).
