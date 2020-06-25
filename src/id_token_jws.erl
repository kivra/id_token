-module(id_token_jws).

-ignore_xref([sign/2, sign/3]).
-export([sign/2, sign/3, validate/2]).

-include_lib("jose/include/jose_jwt.hrl").
-include_lib("jose/include/jose_jwk.hrl").

%%%=============================================================================
%%% TYPES
%%%=============================================================================
%% -type alg() :: <<"PS256">> | <<"PS384">> | <<"PS512">> |
%%                <<"RS256">> | <<"RS384">> | <<"RS512">> |
%%                <<"ES256">> | <<"ES384">> | <<"ES512">>.

%% -export_type([alg/0]).

%%%=============================================================================
%%% API
%%%=============================================================================
-spec sign(map(), #jose_jwk{}) -> binary().
sign(Claims, #jose_jwk{fields = #{<<"kid">> := Kid}} = JWK) ->
  JWS0 = jose_jwk:signer(JWK),
  JWS = JWS0#{<<"kid">> => Kid},
  sign(Claims, JWK, JWS).
-spec sign(map(), #jose_jwk{}, map()) -> binary().
sign(Claims, JWK, JWS) ->
  JWT = jose_jwt:sign(JWK, JWS, Claims),
  jose_jws:compact(JWT).

-spec validate(binary(), [map()]) ->
                  {ok, map()} |
                  {error, invalid_signature | expired | no_public_key_matches}.
validate(IdToken, Keys) ->
  Protected = jose_jwt:peek_protected(IdToken),
  {_M, #{<<"kid">> := Kid}} = jose_jws:to_map(Protected),
  SearchResult = lists:search(fun(#{<<"kid">> := OtherKid}) ->
                                  OtherKid =:= Kid
                              end, Keys),
  case SearchResult of
    {value, Key} ->
      validate_exp(validate_signature(Key, IdToken));
    false -> {error, no_public_key_matches}
  end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

validate_signature(Key, IdToken) ->
  case jose_jwt:verify(Key, IdToken) of
    {true, #jose_jwt{fields = Claims}, _Jws} ->
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

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
