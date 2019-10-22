-module(id_token_jwt).

-export([validate/2]).

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

validate_signature(Key, IdToken) ->
  case jose_jwt:verify(Key, IdToken) of
    {true, {jose_jwt, Claims}, _Jws} ->
      {ok, Claims};
    {false, _, _} ->
      {error, invalid_signature}
  end.

validate_exp({error, _} = Error) ->
  Error;
validate_exp({ok, #{<<"exp">> := Exp} = Claims}) ->
  case Exp >= erlang:system_time(second) of
    true ->
      {error, expired};
    false ->
      {ok, Claims}
  end.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
