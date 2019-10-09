-module(id_token).

-behaviour(gen_server).

%% API
-export([start_link/0, validate/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-define(SERVER, id_token_server).
-define(ID_TOKEN_CACHE, id_token_cache).


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

validate(Provider, _IdToken) ->
  [{google, #{exp_at := ExpAt, keys := Keys}}] = ets:lookup(?ID_TOKEN_CACHE, Provider),
   case ExpAt > id_token_util:now_gregorian_seconds() of
     true  ->
       validate_todo;
     false ->
       refresh_keys(Provider)
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
  Providers = id_token_jwks:get_providers(),
  State = lists:foldl(fun({Name, Uri}, Acc) ->
                          Acc#{ Name => #{ exp_at => 0
                                         , keys => []
                                         , well_known_uri => Uri
                                         }}
                      end, #{}, Providers),
  ets:new(?ID_TOKEN_CACHE, [set, public, named_table, {read_concurrency, true}]),
  maps:fold(fun(Key, Value, _Acc) ->
                ets:insert(?ID_TOKEN_CACHE, {Key, Value})
            end, #{}, State),
  {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
handle_call({refresh, Provider}, _From, State) ->
  #{Provider := #{well_known_uri := WellKnownUri
                 , exp_at := ExpAt
                 } = Keys} = State,
  case ExpAt > id_token_util:now_gregorian_seconds() of
    true -> {reply, Keys, State};
    false ->
      {ok, KeysUrl} = id_token_jwks:get_jwks_uri(WellKnownUri),
      {ok, Keys} = id_token_jwks:get_pub_keys(KeysUrl),
      Value = Keys#{well_known_uri => WellKnownUri},
      ets:insert(?ID_TOKEN_CACHE, {Provider, Value}),
      NewState = State#{Provider => Value},
      {reply, Keys, NewState}
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
