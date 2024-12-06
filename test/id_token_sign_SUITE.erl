-module(id_token_sign_SUITE).
-compile([export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-define(ID_PROVIDER, google).
-define(WELL_KNOWN_URI, <<"https://well-known">>).
-define(JWKS_URI, <<"https://jwks">>).

groups() -> [].

all() -> [start_without_config, start_with_key_in_config].

init_per_suite(Config) ->
    application:ensure_all_started(jose),
    Config.
end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.
end_per_testcase(_TestCase, Config) ->
    Config.

start_without_config(_Config) ->
    {ok, _Pid} = id_token_sign:start_link(),
    ?assertMatch({ok, []}, id_token_pubkeys_storage:get_all()),
    ok = id_token_sign:stop().

start_with_key_in_config(_Config) ->
    application:set_env(id_token, sign_keys, [{<<"ES256">>, #{ttu => 1}}]),
    {ok, _Pid} = id_token_sign:start_link(),
    ?assertMatch({ok, [_]}, id_token_pubkeys_storage:get_all()),
    timer:sleep(1000),
    ?assertMatch({ok, [_, _]}, id_token_pubkeys_storage:get_all()),
    ok = id_token_sign:stop().

%%%_* Editor ===================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
