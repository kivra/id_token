-module(id_token).

%% API
-ignore_xref([validate/2, sign/2]).
-export([validate/2, sign/2]).

-spec validate(atom(), binary()) ->
    {ok, map()}
    | {error,
        invalid_signature
        | expired
        | no_public_key_matches}.
validate(Provider, IdToken) ->
    #{exp_at := ExpAt, keys := Keys} =
        id_token_provider:get_cached_keys(Provider),
    case ExpAt > id_token_util:now_gregorian_seconds() of
        true ->
            case id_token_jws:validate(IdToken, Keys) of
                {error, no_public_key_matches} ->
                    Kid = id_token_jws:extract_kid(IdToken),
                    refresh_and_validate(Provider, IdToken, #{kid => Kid});
                Result ->
                    Result
            end;
        false ->
            refresh_and_validate(Provider, IdToken)
    end.

sign(Alg, Claims) ->
    case id_token_sign:get_sign_key_fun(Alg) of
        {error, not_found} -> {error, no_key_for_alg};
        {ok, SignKeyFun} -> id_token_jws:sign(Claims, SignKeyFun())
    end.

refresh_and_validate(Provider, IdToken) ->
    refresh_and_validate(Provider, IdToken, #{}).

refresh_and_validate(Provider, IdToken, Opts) ->
    #{keys := FreshKeys} =
        id_token_provider:refresh_keys(Provider, Opts),
    id_token_jws:validate(IdToken, FreshKeys).

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
