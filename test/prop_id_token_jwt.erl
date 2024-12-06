-module(prop_id_token_jwt).

-include_lib("proper/include/proper.hrl").
-include_lib("jose/include/jose_jwk.hrl").
-include_lib("eunit/include/eunit.hrl").

%Higher sizes are too slow
-define(MODULUS_SIZES, [1024]).
-define(ALGS, [
    <<"PS256">>,
    <<"PS384">>,
    <<"PS512">>,
    <<"RS256">>,
    <<"RS384">>,
    <<"RS512">>,
    <<"ES256">>,
    <<"ES384">>,
    <<"ES512">>
]).

-elvis([{elvis_style, used_ignored_variable, disable}]).

%%%%%%%%%%%%%%%%%%%%
%%% Eunit runner %%%
%%%%%%%%%%%%%%%%%%%%

eunit_test_() ->
    Opts = [{numtests, 25}],
    {inparallel, [
        ?_assert(proper:quickcheck(prop_valid_signature(), Opts)),
        ?_assert(proper:quickcheck(prop_invalid_signature(), Opts)),
        {timeout, 300, ?_assert(proper:quickcheck(prop_no_matching_key(), Opts))}
    ]}.

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_valid_signature() ->
    ?FORALL(
        {{JWK, PublicKeyMap}, Claims},
        {key_pair(), jwt_claims()},
        begin
            #{<<"exp">> := Exp} = Claims,
            JWT = id_token_jws:sign(Claims, JWK),
            Result = id_token_jws:validate(JWT, [PublicKeyMap]),
            Exp =< erlang:system_time(second) andalso
                {error, expired} =:= Result orelse
                {ok, Claims} =:= Result
        end
    ).

prop_invalid_signature() ->
    ?FORALL(
        {{JWK, _PublicKeyMap}, {OtherJWK, OtherPublicKeyMap}, Claims},
        {key_pair(), key_pair(), jwt_claims()},
        begin
            #jose_jwk{fields = OtherFields} = OtherJWK,
            JWKWithChangedKid = JWK#jose_jwk{fields = OtherFields},
            JWT = id_token_jws:sign(Claims, JWKWithChangedKid),
            {error, invalid_signature} =:=
                id_token_jws:validate(JWT, [OtherPublicKeyMap])
        end
    ).

prop_no_matching_key() ->
    ?FORALL(
        {[{JWK, _PublicKeyMap} | OtherKeys], Claims},
        {non_empty(list(key_pair())), jwt_claims()},
        begin
            JWT = id_token_jws:sign(Claims, JWK),
            PublicKeys = lists:map(fun({_, Key}) -> Key end, OtherKeys),
            {error, no_public_key_matches} =:=
                id_token_jws:validate(JWT, PublicKeys)
        end
    ).

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%

jwt_claims() ->
    BaseTime = erlang:system_time(second),
    ?LET(
        {BaseMap, Aud, Exp},
        {map(utf8(), utf8()), utf8(), choose(BaseTime - 3, BaseTime + 3)},
        BaseMap#{<<"aud">> => Aud, <<"exp">> => Exp}
    ).

key_pair() ->
    ?LET(
        {KeySize, Alg},
        {oneof(?MODULUS_SIZES), oneof(?ALGS)},
        id_token_jws:generate_key_for(Alg, #{key_size => KeySize})
    ).

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
