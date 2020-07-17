-module(id_token).

%% API
-ignore_xref([validate/2]).
-export([validate/2]).

-spec validate(atom(), binary()) -> {ok, map()} |
                                    {error, invalid_signature |
                                            expired |
                                            no_public_key_matches
                                    }.
validate(Provider, IdToken) ->
  #{exp_at := ExpAt, keys := Keys} =
    id_token_provider:get_cached_keys(Provider),
  case ExpAt > id_token_util:now_gregorian_seconds() of
    true  ->
      id_token_jws:validate(IdToken, Keys);
    false ->
      #{keys := FreshKeys} = id_token_provider:refresh_keys(Provider),
      id_token_jws:validate(IdToken, FreshKeys)
  end.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
