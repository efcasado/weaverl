-module(speaker_test).

-export([run/0]).

run() ->
    io:format("Testing speaker:talk/0~n"),
    speaker:talk(),
    io:format("Testing speaker:explicit_talk/0~n"),
    speaker:explicit_talk(),
    io:format("Testing speaker:whisper/0~n"),
    speaker:whisper(),
    io:format("Testing speaker:meta_talk/0~n"),
    speaker:meta_talk(),
    io:format("Testing speaker:coerced_talk/0~n"),
    speaker:coerced_talk().
