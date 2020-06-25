-module(id_token_SUITE).
-compile([export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-define(ID_PROVIDER, google).
-define(WELL_KNOWN_URI, <<"https://well-known">>).
-define(JWKS_URI, <<"https://jwks">>).

groups() -> [].

all() -> [validate_jwt, keys_are_cached].

init_per_suite(Config) -> Config.
end_per_suite(_Config) -> ok.

init_per_testcase(_TestCase, Config) ->
  {Jwk, PublicKeyMap} = id_token_test_util:generate_rsa_key_pair(1024),
  Claims = #{ <<"exp">> => erlang:system_time(second) + 10},
  Jwt = id_token_jws:sign(Claims, Jwk),
  mock_id_provider(PublicKeyMap),
  application:ensure_all_started(id_token),
  [{jwt, Jwt} | Config].
end_per_testcase(_TestCase, Config) ->
  application:stop(id_token),
  meck:unload([id_token_jwks, hackney]),
  Config.

validate_jwt(Config) ->
  Jwt = ?config(jwt, Config),
  ?assertMatch({ok, _}, id_token:validate(?ID_PROVIDER, Jwt)).

keys_are_cached(Config) ->
  Jwt = ?config(jwt, Config),
  id_token:validate(?ID_PROVIDER, Jwt),
  id_token:validate(?ID_PROVIDER, Jwt),
  1 = meck:num_calls(id_token_jwks, get_pub_keys, 1),
  ok.

mock_id_provider(PublicKeyMap) ->
  meck:expect(id_token_jwks, get_providers, 0,
              [{?ID_PROVIDER, ?WELL_KNOWN_URI}]),
  meck:expect(hackney, request,
              fun(get, ?WELL_KNOWN_URI, _, _, _) ->
                  Body = jsx:encode(#{<<"jwks_uri">> => ?JWKS_URI}),
                  {ok, 200, [], Body};
                 (get, ?JWKS_URI, _, _, _) ->
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
