%%%-------------------------------------------------------------------
%% @doc id_token top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(id_token_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
  SupFlags = #{strategy => one_for_one, intensity => 10, period => 1},

  ChildSpecs = [#{id => id_token,
                 start => {id_token, start_link, []},
                 restart => permanent,
                 shutdown => brutal_kill,
                 type => worker,
                 modules => [id_token]
                }
               ],
  {ok, {SupFlags, ChildSpecs}}.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
