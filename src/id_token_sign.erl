-module(id_token_sign).

-behaviour(gen_server).

%% API
-define(API, [start_link/0, stop/0,
              get_active_public_keys/0,
              get_sign_algs/0, get_sign_key_fun/1,
              add_key_for/2]).
-ignore_xref(?API).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2
        | ?API]).

-define(SERVER, ?MODULE).
-define(KEY_USE_TIME, 86400). %% 1 day
-define(KEY_EXPIRES_IN, 172800). %% 2 days
-define(ETS_OPTIONS, [set, public, named_table, {read_concurrency, true}]).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
  id_token_pubkeys_storage:stop(),
  gen_server:stop(?SERVER).

get_active_public_keys() ->
  id_token_pubkeys_storage:get_all().

get_sign_key_fun(Alg) ->
  case ets:lookup(?MODULE, Alg) of
    [{Alg, KeyFun}] -> {ok, KeyFun};
    [] -> {error, not_found}
  end.

get_sign_algs() ->
  Objects = ets:tab2list(?MODULE),
  [Alg || {Alg, _} <- Objects].

add_key_for(Alg, Options) ->
  gen_server:call(?SERVER, {add_key, Alg, Options}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  id_token_pubkeys_storage:start(),
  ets:new(?MODULE, ?ETS_OPTIONS),
  {ok, #{}}.

handle_call({add_key, Alg, Options}, _From, State) ->
  {Jwk, PublicKeyMap} = id_token_jws:generate_key_for(Alg, Options),
  SignKeyFun = fun() -> Jwk end,
  ets:insert(?MODULE, {Alg, SignKeyFun}),
  %% crashing means loosing the private key that are in ets
  {reply, id_token_pubkeys_storage:put(PublicKeyMap), State}.

handle_cast(_Request, State) ->
  {noreply, State}.

-spec handle_info(Info :: timeout() | term(), State :: term()) ->
                     {noreply, NewState :: term()} |
                     {noreply, NewState :: term(), Timeout :: timeout()} |
                     {noreply, NewState :: term(), hibernate} |
                     {stop, Reason :: normal | term(), NewState :: term()}.
handle_info(_Info, State) ->
  {noreply, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
