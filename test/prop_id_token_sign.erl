-module(prop_id_token_sign).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Model Callbacks
-export([command/1, initial_state/0, next_state/3,
         precondition/2, postcondition/3]).

-define(ALGS, [<<"PS256">>, <<"PS384">>, <<"PS512">>,
               <<"RS256">>, <<"RS384">>, <<"RS512">>,
               <<"ES256">>, <<"ES384">>, <<"ES512">>]).

%%%%%%%%%%%%%%%%%%%%
%%% Eunit runner %%%
%%%%%%%%%%%%%%%%%%%%
eunit_test_() ->
  Opts = [{numtests, 30}],
  ?_assert(proper:quickcheck(prop_test2(), Opts)).

%%%%%%%%%%%%%%%%%%
%%% PROPERTIES %%%
%%%%%%%%%%%%%%%%%%
prop_test2() ->
  ?FORALL(Cmds, commands(?MODULE),
          begin
            id_token_sign:start_link(),
            {History, State, Result} = run_commands(?MODULE, Cmds),
            id_token_sign:stop(),
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
  oneof([
         {call, id_token, sign, [alg(), jwt_claims()]},
         {call, id_token_sign, get_sign_algs, []},
         {call, id_token_sign, get_active_public_keys, []},
         {call, id_token_sign, add_key_for, [alg(), #{key_size => 1024}]}
        ]).

%% @doc Determines whether a command should be valid under the
%% current state.
precondition(_State, {call, _Mod, _Fun, _Args}) ->
  true.

%% @doc Given the state `State' *prior* to the call
%% `{call, Mod, Fun, Args}', determine whether the result
%% `Res' (coming from the actual system) makes sense.
postcondition(State, {call, id_token_sign, get_active_public_keys, _}, Res) ->
  StateKeysCount = maps:fold(fun(_K, V, Acc) -> Acc + V end, 0, State),
  {ok, ResKeys} = Res,
  StateKeysCount =:= length(ResKeys);
postcondition(State, {call, id_token_sign, get_sign_algs, _}, Res) ->
  StateAlgs = maps:fold(fun(K, _V, Acc) -> [K | Acc] end, [], State),
  lists:sort(StateAlgs) =:= lists:sort(Res);
postcondition(State, {call, id_token, sign, [Alg, Claims]}, Res) ->
  case State of
    #{Alg := _} ->
      {ok, PubKeys} = id_token_sign:get_active_public_keys(),
      #{<<"exp">> := Exp} = Claims,
      Result = id_token_jws:validate(Res, PubKeys),
      Exp =<  erlang:system_time(second)
        andalso {error, expired} =:= Result
        orelse {ok, Claims} =:= Result;
    _ -> Res =:= {error, no_key_for_alg}
  end;
postcondition(_State, {call, _Mod, _Fun, _Args}, _Res) ->
  true.

%% @doc Assuming the postcondition for a call was true, update the model
%% accordingly for the test to proceed.
next_state(State, _Res, {call, id_token_sign, add_key_for, [Alg, _]}) ->
  CurrentCount = maps:get(Alg, State, 0),
  State#{Alg => CurrentCount + 1};
next_state(State, _Res, {call, _Mod, _Fun, _Args}) ->
  State.

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%

alg() -> oneof(?ALGS).

jwt_claims() ->
  BaseTime = erlang:system_time(second),
  ?LET({BaseMap, Aud, Exp},
       {map(utf8(), utf8()), utf8(), choose(BaseTime - 3, BaseTime + 3)},
       BaseMap#{<<"aud">> => Aud, <<"exp">> => Exp}).
