-module(id_token_SUITE).
-compile([export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-define(ID_PROVIDER, google).
-define(WELL_KNOWN_URI, <<"https://well-known">>).
-define(JWKS_URI, <<"https://jwks">>).

groups() -> [].

all() -> [ validate_jwt,
           keys_are_only_refreshed_once_per_kid ].

init_per_suite(Config) ->
  application:ensure_all_started(jose),
  Config.
end_per_suite(_Config) -> ok.

init_per_testcase(_TestCase, Config) ->
  {JWK, PublicKeyMap} =
    id_token_jws:generate_key_for(<<"RS256">>, #{key_size => 1024}),
  Claims = #{ <<"exp">> => erlang:system_time(second) + 10},
  JWT = id_token_jws:sign(Claims, JWK),
  mock_id_provider(PublicKeyMap, 0),
  application:ensure_all_started(id_token),
  [{jwt, JWT}, {pubkeys, [PublicKeyMap]} | Config].
end_per_testcase(_TestCase, Config) ->
  application:stop(id_token),
  meck:unload([id_token_jwks, hackney]),
  Config.

validate_jwt(Config) ->
  JWT = ?config(jwt, Config),
  ?assertMatch({ok, _}, id_token:validate(?ID_PROVIDER, JWT)).

keys_are_cached(Config) ->
  JWT = ?config(jwt, Config),
  ?assertMatch({ok, _}, id_token:validate(?ID_PROVIDER, JWT)),
  ?assertMatch({ok, _}, id_token:validate(?ID_PROVIDER, JWT)),
  1 = meck:num_calls(id_token_jwks, get_pub_keys, 1),
  ok.

keys_are_only_refreshed_once_per_kid(Config) ->
  %% init pubkey cache with config from init_per_testcase
  CurrentKeyCache = #{ exp_at => id_token_util:now_gregorian_seconds() + 10,
                       keys => ?config(pubkeys, Config)},
  ok = meck:expect(id_token_provider, get_cached_keys, 1, CurrentKeyCache),

  %% create JWT with kid that's not yet in the pubkey cache
  {JWK, NewPubkey} = id_token_jws:generate_key_for(<<"RS256">>, #{key_size => 1024}),
  Claims = #{ <<"exp">> => erlang:system_time(second) + 10 },
  JWT = id_token_jws:sign(Claims, JWK),

  %% set up provider to return new pubkeys with a 50 ms delay
  HttpReponseDelay = 50,
  mock_id_provider(NewPubkey, HttpReponseDelay),
  ?assertEqual(0, meck:num_calls(id_token_jwks, get_pub_keys, 1)),

  %% try to validate multiple JWTs based on kid that's not yet in the key cache
  spawn(fun() -> id_token:validate(?ID_PROVIDER, JWT) end),
  spawn(fun() -> id_token:validate(?ID_PROVIDER, JWT) end),
  spawn(fun() -> id_token:validate(?ID_PROVIDER, JWT) end),
  timer:sleep(10 * HttpReponseDelay),

  %% ensure that the pubkey cache was only refreshed once
  ?assertEqual(1, meck:num_calls(id_token_jwks, get_pub_keys, 1)),
  ?assertMatch({ok, _}, id_token:validate(?ID_PROVIDER, JWT)),

  meck:unload([id_token_provider]).

mock_id_provider(PublicKeyMap, HttpReponseDelay) ->
  meck:expect(id_token_jwks, get_providers, 0, [{?ID_PROVIDER, ?WELL_KNOWN_URI}]),
  meck:expect(hackney, request,
              fun(get, ?WELL_KNOWN_URI, _, _, _) ->
                  Body = jsx:encode(#{<<"jwks_uri">> => ?JWKS_URI}),
                  {ok, 200, [], Body};
                 (get, ?JWKS_URI, _, _, _) ->
                  timer:sleep(HttpReponseDelay),
                  Body = jsx:encode(#{<<"keys">> => [PublicKeyMap]}),
                  MaxAge = <<"max-age=3600">>,
                  Headers = [{<<"Cache-Control">>, MaxAge}],
                  {ok, 200, Headers, Body}
              end).

%%%_* Editor ===================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
