-module(id_token_jws).

-define(API_CALLS, [
    generate_key_for/1,
    generate_key_for/2,
    sign/2,
    sign/3,
    validate/1,
    validate/2,
    extract_kid/1
]).
-ignore_xref(?API_CALLS).
-export(?API_CALLS).

-include_lib("jose/include/jose_jwt.hrl").
-include_lib("jose/include/jose_jwk.hrl").

%%%=============================================================================
%%% TYPES
%%%=============================================================================
%% -type alg() :: <<"PS256">> | <<"PS384">> | <<"PS512">> |
%%                <<"RS256">> | <<"RS384">> | <<"RS512">> |
%%                <<"ES256">> | <<"ES384">> | <<"ES512">>.

%% -export_type([alg/0]).

-type jose_jwk() :: #jose_jwk{}.
-type key_options() :: #{key_size => pos_integer()}.

%%%=============================================================================
%%% API
%%%=============================================================================
-spec sign(map(), jose_jwk()) -> binary().
sign(Claims, #jose_jwk{fields = #{<<"kid">> := Kid}} = JWK) ->
    JWS0 = jose_jwk:signer(JWK),
    JWS = JWS0#{<<"kid">> => Kid},
    sign(Claims, JWK, JWS).
-spec sign(map(), jose_jwk(), map()) -> binary().
sign(Claims, JWK, JWS) ->
    JWT = jose_jwt:sign(JWK, JWS, Claims),
    {_Modules, JWTBin} = jose_jws:compact(JWT),
    JWTBin.

-spec validate(binary()) ->
    {ok, map()}
    | {error, invalid_signature | expired | no_public_key_matches}.
validate(IdToken) ->
    Kid = extract_kid(IdToken),
    case id_token_pubkeys_storage:get(Kid) of
        {error, _} ->
            {error, no_public_key_matches};
        {ok, Key} ->
            validate_exp(validate_signature(Key, IdToken))
    end.

-spec validate(binary(), [map()]) ->
    {ok, map()}
    | {error, invalid_signature | expired | no_public_key_matches}.
validate(IdToken, Keys) ->
    Kid = extract_kid(IdToken),
    SearchResult = lists:search(
        fun(#{<<"kid">> := OtherKid}) ->
            OtherKid =:= Kid
        end,
        Keys
    ),
    case SearchResult of
        {value, Key} ->
            validate_exp(validate_signature(Key, IdToken));
        false ->
            {error, no_public_key_matches}
    end.

-spec generate_key_for(binary()) -> {jose_jwk(), map()}.
generate_key_for(Alg) -> generate_key_for(Alg, #{}).

-spec generate_key_for(binary(), key_options()) -> {jose_jwk(), map()}.
generate_key_for(Alg, Options) ->
    Key =
        case Alg of
            <<"ES", _S/binary>> -> generate_ec_key(Alg, Options);
            _ -> generate_rsa_key(Alg, Options)
        end,
    JWK0 = jose_jwk:from_key(Key),
    JWK = JWK0#jose_jwk{
        fields = #{
            <<"kid">> => kid(Options),
            <<"use">> => <<"sig">>,
            <<"iat">> => iat(Options)
        }
    },
    {_, PublicKeyMap} = jose_jwk:to_public_map(JWK),
    {JWK, PublicKeyMap}.

extract_kid(IdToken) ->
    Protected = jose_jwt:peek_protected(IdToken),
    {_M, #{<<"kid">> := Kid}} = jose_jws:to_map(Protected),
    Kid.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================
-define(EXPONENT_SIZE, 65537).

validate_signature(Key, IdToken) ->
    case jose_jwt:verify(Key, IdToken) of
        {true, #jose_jwt{fields = Claims}, _JWS} ->
            {ok, Claims};
        {false, _, _} ->
            {error, invalid_signature}
    end.

validate_exp({error, _} = Error) ->
    Error;
validate_exp({ok, #{<<"exp">> := Exp} = Claims}) ->
    case Exp =< erlang:system_time(second) of
        true ->
            {error, expired};
        false ->
            {ok, Claims}
    end.

generate_rsa_key(_Alg, Options) ->
    KeySize = maps:get(key_size, Options, 2048),
    public_key:generate_key({rsa, KeySize, ?EXPONENT_SIZE}).

generate_ec_key(Alg, _Options) ->
    Curve = alg_to_curve(Alg),
    {Key, _Fields} = jose_jwk_kty_ec:generate_key(Curve),
    Key.

alg_to_curve(<<"ES256">>) -> <<"P-256">>;
alg_to_curve(<<"ES384">>) -> <<"P-384">>;
alg_to_curve(<<"ES512">>) -> <<"P-521">>.

kid(#{kid := Kid}) -> Kid;
kid(_) -> jose_base64url:encode(crypto:strong_rand_bytes(16)).

iat(#{iat := Iat}) -> Iat;
iat(_) -> erlang:system_time(seconds).

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
