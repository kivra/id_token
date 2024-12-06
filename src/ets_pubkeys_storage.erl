-module(ets_pubkeys_storage).

-behaviour(id_token_pubkeys_storage).
-behaviour(gen_server).

-define(ETS_OPTIONS, [set, public, named_table, {read_concurrency, true}]).

%% API
-export([start/0, stop/0, delete/1, get/1, get_all/0, put/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2]).

start() -> gen_server:start({local, ?MODULE}, ?MODULE, [], []).

stop() -> gen_server:stop(?MODULE).

delete(Key) ->
    ets:delete(?MODULE, Key),
    ok.

get(Kid) ->
    case ets:lookup(?MODULE, Kid) of
        [{Kid, Key}] -> {ok, Key};
        [] -> {error, not_found}
    end.

get_all() ->
    Objects = ets:tab2list(?MODULE),
    {ok, [K || {_, K} <- Objects]}.

put(#{<<"kid">> := Kid} = Key) ->
    ets:insert(?MODULE, {Kid, Key}),
    ok.

%% gen_server callbacks
init(A) ->
    ?MODULE = ets:new(?MODULE, ?ETS_OPTIONS),
    {ok, A}.
handle_call(_, _, S) -> {noreply, S}.
handle_cast(_, S) -> {noreply, S}.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
