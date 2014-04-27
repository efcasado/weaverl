-export([coerced_talk/0]).

coerced_talk() ->
    coerced_talk_(?DEFAULT_NSAYINGS).

coerced_talk_(0) ->
    ok;
coerced_talk_(N) ->
    io:format("Speaker says: ~p~n", [choose_saying(?SAYINGS)]),
    timer:sleep(1000),
    talk_(N - 1).
