-module(censor).

-export([censor/1]).

-define(BLACKLIST, ["death",  "dead"]).


censor(Text) ->
    Words = string:tokens(Text, " "),
    NewWords = [ case lists:member(string:to_lower(W), ?BLACKLIST) of
                     true ->
                         lists:flatten(lists:duplicate(length(W), "*"));
                     false ->
                         W
                 end || W <- Words ],
    string:join(NewWords, " ").

