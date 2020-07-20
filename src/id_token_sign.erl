-module(id_token_sign).

-behaviour(gen_server).

%% API
-define(API, [start_link/0, stop/0,
              get_active_public_keys/0, get_active_public_keys/1,
              get_sign_algs/0, get_sign_key_fun/1,
              refresh_key/2, add_key_for/2]).
-ignore_xref(?API).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2
        | ?API]).

-define(SERVER, ?MODULE).
-define(KEY_USE_TIME, 86400). %% 1 day
-define(KEY_EXPIRES_IN, 172800). %% 2 days

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
  gen_server:stop(?SERVER).

get_active_public_keys() ->
  [].

get_active_public_keys(_Alg) ->
  [].

get_sign_key_fun(_Alg) ->
  fun() -> ok end.

get_sign_algs() ->
  [].

refresh_key(_Alg, _Options) ->
  ok.

add_key_for(_Alg, _Options) ->
  ok.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  {ok, #{}}.

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

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
