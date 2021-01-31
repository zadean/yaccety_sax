# yaccety_sax

Fast, StAX-like XML Parser for BEAM Languages

## The big idea

Instead of as with SAX or DOM parsing of XML, forcing the user to handle everything at once, this parser allows the user to consume events from a stream as it suits them. Simply call `next_event` on the stream.
This means that the user can parse multiple streams from the same process at the same time.
It works like an iterator on any set or list-like type, but returns XML events instead.

### Conformance and Features

`yaccety_sax` is a Namespace aware, non-validating XML 1.0 parser.

* Accepts only UTF-8 encoding directly.
* It __does__ check DTDs for wellformedness.
* It __can__ read and parse external DTDs (When given a callback function to retrieve the DTD).
* Returns the parsed DTD as an event to the user for custom validation if needed.
* Comment, Processing Instructions, and whitespace Text nodes are all optionally ignorable.
* All parsed events, report UTF-8 binary (No character lists).

### Roadmap of Coming Features

* "Simple" mode for parsing things like SOAP from a single binary. (No DTD, no partial binaries, no PIs, comments ignored, etc.)
* Handling of common security risks known from parsing DTDs
* Content skipping - skipping entire content of an element
* Event writing - Serializing event streams back into XML
* YACC-like syntax for setting up complex, selective parsers.
* Validation from DTD
* XML 1.1 ??
* Others? add an issue to the repo :-)

### Examples

#### Checking XMLs with different encodings and human-readability for equality

```erlang
kinda_equal(Filename1, Filename2) ->
    % UTF-16 file with external DTD and full of whitespace nodes
    LhState = yaccety_sax:stream(<<>>, [
        {whitespace, false},
        {comments, false},
        {proc_inst, false},
        {continuation, ys_utils:trancoding_file_continuation(Filename1)},
        {base, filename:dirname(Filename1)},
        {external, fun ys_utils:external_file_reader/2}
    ]),
    % Start Document event
    {_, LhState1} = yaccety_sax:next_event(LhState),
    % DTD event
    {_, LhState2} = yaccety_sax:next_event(LhState1),

    % UTF-8 file with no DTD or whitespace nodes
    % Could have streamed this file as well...
    {ok, Bin2} = file:read_file(Filename2),
    RhState = yaccety_sax:stream(Bin2),
    % Start Document event
    {_, RhState1} = yaccety_sax:next_event(RhState),

    % Now both streams are in a comparable state, so diff them
    diff_loop(LhState2, RhState1).

diff_loop(LhState, RhState) ->
    {LhEvent, LhState1} = yaccety_sax:next_event(LhState),
    {RhEvent, RhState1} = yaccety_sax:next_event(RhState),
    #{type := EventType} = LhEvent,
    % Some function that checks equality, maybe ignoring 
    % namespaces or prefixes or something.
    case equal_enough(LhEvent, RhEvent) of
        true when EventType =:= endDocument -> true;
        true -> diff_loop(LhState1, RhState1);
        false -> false
    end.
```

### some (early) numbers

Just-for-fun parsing a 5.2 GB Wiki abstract dump with a callback that throws away all events:

* There are 113,593,892 elements in the file.
* `yaccety_sax` takes around 5 minutes on my machine.
* `xmerl_sax_parser` with default settings is still running...
* `xmerl_sax_parser` with a larger buffer in the continuation function takes around 12 minutes.

Another big difference is that the xmerl process held onto about 42 MB by the end of parsing. yaccety never went above 109 KB.

I didn't attempt using the xmerl_scan on the 5.2 GB file. Not sure it's a good idea to try.

I'm sure there are other parsers out there that stream-parse large data.
It would be cool to see how all of them react.

## The repo name

Anyone who has seen The Benny Hill Show knows the song that inspired the name for the repo. ***Yakety Sax***
