-module(prop_id_token_jwt).

-include_lib("proper/include/proper.hrl").
-include_lib("jose/include/jose_jwk.hrl").
-include_lib("eunit/include/eunit.hrl").
-export([rsa_key_pair/0]).

%Higher sizes are too slow
-define(MODULUS_SIZES, [1024]).

%%%%%%%%%%%%%%%%%%%%
%%% Eunit runner %%%
%%%%%%%%%%%%%%%%%%%%

eunit_test_() ->
  Opts = [{numtests, 100}],
  {inparallel,
   [ ?_assert(proper:quickcheck(prop_valid_signature(), Opts)),
     ?_assert(proper:quickcheck(prop_invalid_signature(), Opts)),
     {timeout, 300, ?_assert(proper:quickcheck(prop_no_matching_key(), Opts))}
   ]}.

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_valid_signature() ->
  ?FORALL({{Jwk, PublicKeyMap}, Claims},
          {rsa_key_pair(), jwt_claims()},
          begin
            #{<<"exp">> := Exp} = Claims,
            Jwt = id_token_jws:sign(Claims, Jwk),
            Result = id_token_jws:validate(Jwt, [PublicKeyMap]),
            Exp =<  erlang:system_time(second)
              andalso {error, expired} =:= Result
              orelse {ok, Claims} =:= Result
          end).

prop_invalid_signature() ->
  ?FORALL({{Jwk, _PublicKeyMap}, {OtherJwk, OtherPublicKeyMap}, Claims},
          {rsa_key_pair(), rsa_key_pair(), jwt_claims()},
          begin
            #jose_jwk{fields = OtherFields} = OtherJwk,
            JwkWithChangedKid = Jwk#jose_jwk{fields = OtherFields},
            Jwt = id_token_jws:sign(Claims, JwkWithChangedKid),
            {error, invalid_signature}
              =:= id_token_jws:validate(Jwt, [OtherPublicKeyMap])
          end).

prop_no_matching_key() ->
  ?FORALL({[{Jwk, _PublicKeyMap} | OtherKeys], Claims},
          {non_empty(list(rsa_key_pair())), jwt_claims()},
          begin
            Jwt = id_token_jws:sign(Claims, Jwk),
            PublicKeys = lists:map(fun({_, Key}) -> Key end, OtherKeys),
            {error, no_public_key_matches}
              =:= id_token_jws:validate(Jwt, PublicKeys)
          end).

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%

jwt_claims() ->
  BaseTime = erlang:system_time(second),
  ?LET({BaseMap, Aud, Exp},
       {map(utf8(), utf8()), utf8(), choose(BaseTime - 3, BaseTime + 3)},
       BaseMap#{<<"aud">> => Aud, <<"exp">> => Exp}).

rsa_key_pair() ->
  ?LET(KeySize,
       oneof(?MODULUS_SIZES),
       id_token_test_util:generate_rsa_key_pair(KeySize)).

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
