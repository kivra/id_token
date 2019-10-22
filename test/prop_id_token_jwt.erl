-module(prop_id_token_jwt).

-include_lib("proper/include/proper.hrl").
-include_lib("jose/include/jose_jwk.hrl").
-export([rsa_key_pair/0]).

-define(MODULUS_SIZES, [1024]). %Higher sizes are too slow  %, 2048, 4096, 8192]).
-define(EXPONENT_SIZE, 65537).

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_valid_signature() ->
  ?FORALL({{Jwk, PublicKeyMap}, Claims},
          {rsa_key_pair(), jwt_claims()},
          begin
            #{<<"exp">> := Exp} = Claims,
            Jwt = sign(Jwk, Claims),
            Result = id_token_jwt:validate(Jwt, [PublicKeyMap]),
            Exp >=  erlang:system_time(second)
              andalso {error, expired} =:= Result
              orelse {ok, Claims} =:= Result
          end).

prop_invalid_signature() ->
  ?FORALL({{Jwk, _PublicKeyMap}, {OtherJwk, OtherPublicKeyMap}, Claims},
          {rsa_key_pair(), rsa_key_pair(), jwt_claims()},
          begin
            #jose_jwk{fields = OtherFields} = OtherJwk,
            JwkWithChangedKid = Jwk#jose_jwk{fields = OtherFields},
            Jwt = sign(JwkWithChangedKid, Claims),
            {error, invalid_signature} =:= id_token_jwt:validate(Jwt, [OtherPublicKeyMap])
          end).

prop_no_matching_key() ->
  ?FORALL({[{Jwk, _PublicKeyMap} | OtherKeys], Claims},
          {non_empty(list(rsa_key_pair())), jwt_claims()},
          begin
            Jwt = sign(Jwk, Claims),
            PublicKeys = lists:map(fun({_, Key}) -> Key end, OtherKeys),
            {error, no_public_key_matches} =:= id_token_jwt:validate(Jwt, PublicKeys)
          end).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%

sign(#jose_jwk{fields = #{<<"kid">> := Kid}} = Jwk, Claims) ->
  JWS = #{
          <<"alg">> => <<"PS256">>,
          <<"kid">> => Kid
         },
  Jwt = jose_jwt:sign(Jwk, JWS, Claims),
  jose_jws:compact(Jwt).

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%

jwt_claims() ->
  BaseTime = erlang:system_time(second),
  ?LET({BaseMap, Aud, Exp},
       {map(utf8(), utf8()), utf8(), choose(BaseTime - 3, BaseTime + 3)},
       BaseMap#{<<"aud">> => Aud, <<"exp">> => Exp}).

rsa_key_pair() ->
  ?LET(KeySize, oneof(?MODULUS_SIZES),
       begin
         RsaKey = public_key:generate_key({rsa, KeySize, ?EXPONENT_SIZE}),
         Jwk0 = jose_jwk:from_key(RsaKey),
         Kid = base64url:encode(crypto:strong_rand_bytes(16)),
         Jwk = Jwk0#jose_jwk{fields = #{<<"kid">> => Kid}},
         {_, PublicKeyMap} = jose_jwk:to_public_map(Jwk),
         {Jwk, PublicKeyMap}
       end).

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
