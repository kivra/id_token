-module(id_token_provider).

-behaviour(gen_server).

%% API
-define(API, [start_link/0, get_cached_keys/1, refresh_keys/1, add_provider/2]).
-ignore_xref(?API).
-export([init/1, handle_call/3, handle_cast/2 | ?API]).

-define(SERVER, id_token_provider_server).
-define(ID_TOKEN_CACHE, id_token_provider_keys).
-define(ETS_OPTIONS, [set, public, named_table, {read_concurrency, true}]).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

get_cached_keys(Provider) ->
  [{Provider, #{exp_at := ExpAt, keys := Keys}}] =
    ets:lookup(?ID_TOKEN_CACHE, Provider),
  #{exp_at => ExpAt, keys => Keys}.

refresh_keys(Provider) ->
  gen_server:call(?SERVER, {refresh, Provider}).

-spec add_provider(atom(), binary()) -> ok.
add_provider(Name, Uri) ->
  add_provider({Name, Uri}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
  ets:new(?ID_TOKEN_CACHE, ?ETS_OPTIONS),
  Providers = id_token_jwks:get_providers(),
  lists:foreach(fun add_provider/1, Providers),
  {ok, #{}}.

handle_call({refresh, Provider}, _From, State) ->
  [{Provider, CacheEntry}] = ets:lookup(?ID_TOKEN_CACHE, Provider),
  #{ exp_at := ExpAt, well_known_uri := WellKnownUri} = CacheEntry,
  case ExpAt > id_token_util:now_gregorian_seconds() of
    true -> {reply, CacheEntry, State};
    false ->
      {ok, KeysUrl} = id_token_jwks:get_jwks_uri(WellKnownUri),
      {ok, NewKeys} = id_token_jwks:get_pub_keys(KeysUrl),
      NewCacheEntry = NewKeys#{well_known_uri => WellKnownUri},
      ets:insert(?ID_TOKEN_CACHE, {Provider, NewCacheEntry}),
      {reply, NewKeys, State}
  end.

handle_cast(_Request, State) ->
  {noreply, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
add_provider({Name, Uri}) ->
  EtsEntry = {Name, #{ exp_at => 0
                     , keys => []
                     , well_known_uri => Uri
                     }},
  true = ets:insert(?ID_TOKEN_CACHE, EtsEntry),
  ok.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
