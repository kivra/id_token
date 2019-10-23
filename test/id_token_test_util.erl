-module(id_token_test_util).

-include_lib("jose/include/jose_jwk.hrl").

-export([ generate_rsa_key_pair/1,
          sign/2
        ]).

-define(EXPONENT_SIZE, 65537).

generate_rsa_key_pair(KeySize) ->
  RsaKey = public_key:generate_key({rsa, KeySize, ?EXPONENT_SIZE}),
  Jwk0 = jose_jwk:from_key(RsaKey),
  Kid = base64url:encode(crypto:strong_rand_bytes(16)),
  Jwk = Jwk0#jose_jwk{fields = #{<<"kid">> => Kid}},
  {_, PublicKeyMap} = jose_jwk:to_public_map(Jwk),
  {Jwk, PublicKeyMap}.

sign(#jose_jwk{fields = #{<<"kid">> := Kid}} = Jwk, Claims) ->
  JWS = #{
          <<"alg">> => <<"PS256">>,
          <<"kid">> => Kid
         },
  Jwt = jose_jwt:sign(Jwk, JWS, Claims),
  jose_jws:compact(Jwt).

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
