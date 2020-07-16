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
  id_token_validation:validate(Provider, IdToken).

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
