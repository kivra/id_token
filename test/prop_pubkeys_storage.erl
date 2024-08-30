-module(prop_pubkeys_storage).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Model Callbacks
-export([command/1, initial_state/0, next_state/3,
         precondition/2, postcondition/3]).

%%%%%%%%%%%%%%%%%%%%
%%% Eunit runner %%%
%%%%%%%%%%%%%%%%%%%%
eunit_test_() ->
  Opts = [{numtests, 30}],
  ?_assert(proper:quickcheck(prop_test1(), Opts)).

%%%%%%%%%%%%%%%%%%
%%% PROPERTIES %%%
%%%%%%%%%%%%%%%%%%
prop_test1() ->
  ?FORALL(Cmds, commands(?MODULE),
          begin
            id_token_pubkeys_storage:start(),
            {History, State, Result} = run_commands(?MODULE, Cmds),
            id_token_pubkeys_storage:stop(),
            ?WHENFAIL(io:format("History: ~p\nState: ~p\nResult: ~p\n",
                                [History, State, Result]),
                      aggregate(command_names(Cmds), Result =:= ok))
          end).

%%%%%%%%%%%%%
%%% MODEL %%%
%%%%%%%%%%%%%
%% @doc Initial model value at system start. Should be deterministic.
initial_state() ->
  #{}.

%% @doc List of possible commands to run against the system
command(_State) ->
  frequency([{1, {call, id_token_pubkeys_storage, get_all, []}},
             {1, {call, id_token_pubkeys_storage, get, [kid()]}},
             {3, {call, id_token_pubkeys_storage, put, [key()]}},
             {1, {call, id_token_pubkeys_storage, delete, [kid()]}}
            ]).

%% @doc Determines whether a command should be valid under the
%% current state.
precondition(_State, {call, _Mod, _Fun, _Args}) ->
  true.

%% @doc Given the state `State' *prior* to the call
%% `{call, Mod, Fun, Args}', determine whether the result
%% `Res' (coming from the actual system) makes sense.
postcondition(State, {call, _Mod, get_all, _Args}, {ok, Res}) ->
  lists:sort(maps:values(State)) =:= lists:sort(Res);
postcondition(State, {call, _Mod, get, [Kid]}, Res) ->
  case {maps:find(Kid, State), Res} of
    {{ok, V}, {ok, V}} -> true;
    {error, {error, not_found}} -> true;
    _ -> false
  end;
postcondition(_State, {call, _Mod, _Fun, _Args}, _Res) ->
  true.

%% @doc Assuming the postcondition for a call was true, update the model
%% accordingly for the test to proceed.
next_state(State, _Res, {call, id_token_pubkeys_storage, put, [Key]}) ->
  #{<<"kid">> := Kid} = Key,
  State#{Kid => Key};
next_state(State, _Res, {call, id_token_pubkeys_storage, delete, [Kid]}) ->
  maps:remove(Kid, State);
next_state(State, _Res, {call, _Mod, _Fun, _Args}) ->
  NewState = State,
  NewState.

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
key() ->
  ?LET(Id, kid(),
       begin
         Options = #{kid => Id},
         {_, PubKeyMap} = id_token_jws:generate_key_for(<<"ES256">>, Options),
         PubKeyMap
       end).

kid() ->
  ?LET(Int, integer(1, 10), integer_to_binary(Int)).

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
