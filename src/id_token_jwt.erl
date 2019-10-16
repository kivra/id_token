-module(id_token_jwt).

-export([validate/3]).


%-spec validate(binary(), keys(), [{binary(), binary()}]) -> ok.
validate(IdToken, Keys, ExpectedClaims) ->
  {IdToken, Keys, ExpectedClaims}.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
