# yaccety_sax
Fast, Selective XML SAX Parser for BEAM Languages

## The big idea
Given a function that is used as a callback for the `xmerl_sax_parser`, build a complete module around that function that only reports the events that are actually used. Ignored events should cause the parser to go into a "not reporting" mode that does not collect bytes to report. After the ignored event is over the parser should go to a "reporting" mode.

### Brainstorming
* The callback will then no longer be called as a passed-in callback function, but rather be a local call hardcoded in the module. This will speed everything up quite a bit on its own.
* Everything should work with Elixir and Erlang (and other BEAM languages).
* Skipping events that are of no interest will save memory and allow the parser to focus on what is important.
* Validity of the input should still be checked when not capturing.
* All parsed events, regardless of original encoding, should report UTF-8 to the user. Names, text, whatever.
* Must handle at minimum UTF-8 and UTF-16 input. Others? UCS-4? ISO*
* XML 1.1 ??
* More? PR your ideas here!!




## The repo name
Anyone who has seen The Benny Hill Show knows the song that inspired the name for the repo. ***Yakety Sax***
