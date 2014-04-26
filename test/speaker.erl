-module(speaker).

%% Tell Erlang's compiler we want to use Weaverl's parse transform.
-compile([{parse_transform, weaverl}]).


-export([talk/0, explicit_talk/0, whisper/0, meta_talk/0]).
%% required by explicit talk
-export([choose_saying/1]).



-define(SAYINGS,
        [
         "A bad penny always turns up",
         "A bird in the hand is worth two in the bush",
         "A good name is better than riches",
         "Death is a remedy for all ills",
         "Dead men tell no tales",
         "Death pays all debts",
         "Nothing is certain but death and taxes",
         "Two wrongs do not make a right"
        ]).

-define(DEFAULT_NSAYINGS, 3).



talk() ->
    talk_(?DEFAULT_NSAYINGS).

talk_(0) ->
    ok;
talk_(N) ->
    io:format("Speaker says: ~p~n", [choose_saying(?SAYINGS)]),
    timer:sleep(1000),
    talk_(N - 1).


explicit_talk() ->
    explicit_talk_(?DEFAULT_NSAYINGS).

explicit_talk_(0) ->
    ok;
explicit_talk_(N) ->
    io:format("Speaker says: ~p~n", [speaker:choose_saying(?SAYINGS)]),
    timer:sleep(1000),
    explicit_talk_(N - 1).


whisper() ->
    whisper_(?DEFAULT_NSAYINGS).

whisper_(0) ->
    ok;
whisper_(N) ->
    io:format("Speaker says: ~p~n",
              [erlang:apply(speaker, choose_saying, [?SAYINGS])]),
    timer:sleep(1000),
    whisper_(N - 1).


meta_talk() ->
    meta_talk_(?DEFAULT_NSAYINGS, fun choose_saying/1).

meta_talk_(0, _F) ->
    ok;
meta_talk_(N, F) ->
    io:format("Speaker says: ~p~n", [F(?SAYINGS)]),
    timer:sleep(1000),
    meta_talk_(N - 1, F).


%% Randomly choose a saying
choose_saying(Sayings) ->
    lists:nth(random:uniform(length(Sayings)), Sayings).
