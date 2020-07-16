-module(id_token_sign).

-behaviour(gen_server).

%% API
-define(API, [start_link/0]).
-ignore_xref(?API).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2
        | ?API]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  process_flag(trap_exit, true),
  {ok, #state{}}.

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
