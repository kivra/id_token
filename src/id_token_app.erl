%%%-------------------------------------------------------------------
%% @doc id_token public API
%% @end
%%%-------------------------------------------------------------------

-module(id_token_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    id_token_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
