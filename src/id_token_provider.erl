-module(id_token_provider).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

%% API
-define(API, [start_link/0, get_cached_keys/1,
              refresh_keys/1, refresh_keys/2, add_provider/2]).
-ignore_xref(?API).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2 | ?API]).

-define(SERVER, id_token_provider_server).
-define(ID_TOKEN_CACHE, id_token_provider_keys).
-define(ETS_OPTIONS, [set, public, named_table, {read_concurrency, true}]).

-define(REVALIDATE_DELAY, 7).

-type refresh_keys_opts() :: #{force_refresh => boolean()}.

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec get_cached_keys(atom()) -> id_token_jwks:keys().
get_cached_keys(Provider) ->
  [{Provider, #{exp_at := ExpAt, keys := Keys}}] =
    ets:lookup(?ID_TOKEN_CACHE, Provider),
  #{exp_at => ExpAt, keys => Keys}.

-spec refresh_keys(atom()) -> id_token_jwks:keys().
refresh_keys(Provider) ->
  refresh_keys(Provider, #{}).

-spec refresh_keys(atom(), refresh_keys_opts()) -> id_token_jwks:keys().
refresh_keys(Provider, Opts) ->
  gen_server:call(?SERVER, {refresh, Provider, Opts}).

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
  case application:get_env(id_token, async_revalidate, false) of
    true ->
        lists:foreach(fun({Provider, _}) ->
                          self() ! {refresh, Provider}
                      end, Providers);
    false -> ok
  end,
  {ok, #{}}.

handle_call({refresh, Provider}, _From, State) ->
  {reply, maybe_refresh(Provider, #{}), State};
handle_call({refresh, Provider, Opts}, _From, State) ->
  {reply, maybe_refresh(Provider, Opts), State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info({refresh, Provider}, State) ->
  #{exp_at := ExpAt} = refresh(Provider),
  Now = id_token_util:now_gregorian_seconds(),
  RevalidateTime = ExpAt - ?REVALIDATE_DELAY,
  Delay =
    case Now < RevalidateTime of
      true ->
        (RevalidateTime - Now) * 1000;
      false ->
        %% Not enough time for revalidation, let the first request pay
        %% the price and re-initiate async_revalidate-loop after 60 seconds
        60_000
    end,
  timer:send_after(Delay, self(), {refresh, Provider}),
  {noreply, State}.

maybe_refresh(Provider, #{force_refresh := true}) ->
  refresh(Provider);
maybe_refresh(Provider, _Opts) ->
  [{Provider, CacheEntry}] = ets:lookup(?ID_TOKEN_CACHE, Provider),
  #{exp_at := ExpAt, well_known_uri := WellKnownUri} = CacheEntry,
  case ExpAt > id_token_util:now_gregorian_seconds() of
    true -> CacheEntry;
    false -> refresh(Provider, WellKnownUri)
  end.

refresh(Provider) ->
  [{Provider, CacheEntry}] = ets:lookup(?ID_TOKEN_CACHE, Provider),
  #{well_known_uri := WellKnownUri} = CacheEntry,
  refresh(Provider, WellKnownUri).

refresh(Provider, WellKnownUri) ->
  case id_token_jwks:get_jwks_uri(WellKnownUri) of
    {ok, KeysUrl} ->
      case id_token_jwks:get_pub_keys(KeysUrl) of
        {ok, NewKeys} ->
          NewCacheEntry = NewKeys#{well_known_uri => WellKnownUri},
          ets:insert(?ID_TOKEN_CACHE, {Provider, NewCacheEntry}),
          NewKeys;
        {error, Reason} ->
          handle_error("failed to refresh pubkey cache", Provider, Reason)
      end;
    {error, Reason} ->
      handle_error("failed to fetch jwks uri", Provider, Reason)
  end.

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

handle_error(Message, Provider, Reason) ->
  ?LOG_ERROR(#{ message => Message,
                reason => Reason,
                provider => Provider }),
  [{Provider, CacheEntry}] = ets:lookup(?ID_TOKEN_CACHE, Provider),
  CacheEntry.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
