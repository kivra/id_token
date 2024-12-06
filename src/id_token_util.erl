-module(id_token_util).

-export([now_gregorian_seconds/0]).

now_gregorian_seconds() ->
    calendar:datetime_to_gregorian_seconds(calendar:local_time()).
