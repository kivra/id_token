-module(id_token_jwt).

-export([validate/3]).

-spec validate(binary(), id_token_jwks:keys(), [{binary(), binary()}]) ->
                  {ok, map()} |
                  {error, provider_not_supported} |
                  {error, invalid_signature} |
                  {error, expired}.
validate(IdToken, Keys, ExpectedClaims) ->
  {ok, #{result => [IdToken, Keys, ExpectedClaims]}}.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
