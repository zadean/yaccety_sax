# yaccety_sax
Fast, Selective XML StAX-like Pull-Parser for BEAM Languages

## The big idea
Instead of as with SAX or DOM parsing of XML, forcing the user to handle everything at once, this parser allows the user to consume events from a stream as it suits them. Simply call `next_event` on the stream.
This means that the user can parse multiple streams from the same process at the same time.
It works like an iterator on any set or list-like type, but returns XML events instead.
Whitespace-`text`, `comment` and `processing-instruction` events can be ignored optionally.

### Brainstorming
* Everything should work with Elixir and Erlang (and other BEAM languages).
* Should be bi-directional. This means one should be able to write to a stream as well as read from one (or more).
* Wellformed-ness of the input should always be checked.
* All parsed events, regardless of original encoding, should report UTF-8 to the user. Names, text, whatever.
* Must handle at minimum UTF-8 and UTF-16 input. Others? UCS-4? ISO*
* XML 1.1 ??
* More thoughts? add your ideas here with a PR!!

### some (early) numbers
Just-for-fun parsing a 5.2 GB Wiki abstract dump with a callback that throws away all events:

- There are 113,593,892 elements in the file.
- `yaccety_sax` takes around 5 minutes on my machine.
- `xmerl_sax_parser` with default settings is still running...
- `xmerl_sax_parser` with a larger buffer in the continuation function takes around 12 minutes.

Another big difference is that the xmerl process used about 42 MB by the end of parsing. yaccety never went above 109 KB.

I didn't attempt using the xmerl_scan on the 5.2 GB file. Not sure it's a good idea to try.

I'm sure there are other parsers out there that stream-parse large data.
It would be cool to see how all of them react.


## The repo name
Anyone who has seen The Benny Hill Show knows the song that inspired the name for the repo. ***Yakety Sax***
