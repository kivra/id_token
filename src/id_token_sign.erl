-module(id_token_sign).

-behaviour(gen_server).

%% API
-define(API, [start_link/0, stop/0,
              get_active_public_keys/0,
              get_sign_algs/0, get_sign_key_fun/1,
              add_key_for/2]).
-ignore_xref(?API).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2 | ?API]).

-define(SERVER, ?MODULE).
-define(TTU, 86400). %% 1 day
-define(TTL, ?TTU * 2).
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
    [{Alg, #{sign_fun := KeyFun}}] -> {ok, KeyFun};
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
  SignKeys = application:get_env(id_token, sign_keys, []),
  Timers = lists:sort([put_key_for(Alg, Opts) || {Alg, Opts} <- SignKeys]),
  {ok, Timers, timeout(Timers)}.

handle_call({add_key, Alg, Options}, _From, Timers0) ->
  Timer = put_key_for(Alg, Options),
  Timers = lists:keystore(Alg, 2, Timers0, Timer),
  State = refresh_exp_keys(Timers),
  {reply, ok, State, timeout(State)}.

handle_cast(_Request, State) -> {noreply, State}.

handle_info(timeout, Timers) ->
  State = refresh_exp_keys(Timers),
  {noreply, State, timeout(State)};
handle_info(_Request, Timers0) ->
  case timeout(Timers0) of
    T when T > 0 ->
      {noreply, Timers0, T};
    _T ->
      Timers = refresh_exp_keys(Timers0),
      {noreply, Timers, timeout(Timers)}
  end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
put_key_for(Alg, Options) ->
  {JWK, PublicKeyMap} = id_token_jws:generate_key_for(Alg, Options),
  SignKeyFun = fun() -> JWK end,
  #{<<"kid">> := Kid, <<"iat">> := Iat} = PublicKeyMap,
  TTU = maps:get(ttu, Options, ?TTU),
  Exp = Iat + TTU,
  KeyData = #{kid => Kid, exp => Exp, sign_fun => SignKeyFun, ttu => TTU},
  ets:insert(?MODULE, {Alg, KeyData}),
  id_token_pubkeys_storage:put(PublicKeyMap),
  {Exp, Alg, Options}.

timeout([]) -> infinity;
timeout([{Exp, _, _} | _Timers]) ->
  (Exp - erlang:system_time(seconds)) * 1000.

refresh_exp_keys(KeyTimers) ->
  Now = erlang:system_time(seconds),
  lists:sort(
    lists:map(
      fun({Exp, _, _} = KeyTimer) when Exp > Now ->
          KeyTimer;
         ({_Exp, Alg, Options}) ->
          put_key_for(Alg, Options)
      end, KeyTimers)).
