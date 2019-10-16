-module(id_token).

-behaviour(gen_server).

%% API
-ignore_xref([start_link/0, validate/2]).
-export([start_link/0, validate/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-define(SERVER, id_token_server).
-define(ID_TOKEN_CACHE, id_token_cache).
-define(ETS_OPTIONS, [set, public, named_table, {read_concurrency, true}]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, Pid :: pid()} |
                      {error, Error :: {already_started, pid()}} |
                      {error, Error :: term()} |
                      ignore.
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


-spec validate(atom(), binary()) -> {ok, map()} |
                                    {error, provider_not_supported} |
                                    {error, invalid_signature} |
                                    {error, expired}.

validate(Provider, IdToken) ->
  [{Provider, #{exp_at := ExpAt, keys := Keys}}] =
    ets:lookup(?ID_TOKEN_CACHE, Provider),
   case ExpAt > id_token_util:now_gregorian_seconds() of
     true  ->
       id_token_jwt:validate(IdToken, Keys, []);
     false ->
       #{keys := FreshKeys} = refresh_keys(Provider),
       id_token_jwt:validate(IdToken, FreshKeys, [])
     end.

refresh_keys(Provider) ->
  gen_server:call(?SERVER, {refresh, Provider}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
init([]) ->
  ets:new(?ID_TOKEN_CACHE, ?ETS_OPTIONS),
  Providers = id_token_jwks:get_providers(),
  lists:foreach(fun({Name, Uri}) ->
                    EtsEntry = {Name, #{ exp_at => 0
                                       , keys => []
                                       , well_known_uri => Uri
                                       }},
                      ets:insert(?ID_TOKEN_CACHE, EtsEntry)
                end, Providers),
  {ok, #{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for changing the form and appearance
%% of gen_server status when it is returned from sys:get_status/1,2
%% or when it appears in termination error logs.
%% @end
%%--------------------------------------------------------------------
format_status(_Opt, Status) ->
  Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
