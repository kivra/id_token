-module(id_token_jwks).

-export([get_pub_keys/1,
         get_jwks_uri/1,
         get_providers/0
        ]).

-type keys() :: #{exp_at := integer(), keys := list()}.
-type provider() :: {atom(), binary()}.

-export_type([keys/0, provider/0]).

-spec get_pub_keys(binary()) -> keys() | {error, any()}.
get_pub_keys(Uri) ->
  case hackney:request(get, Uri, [], <<>>, [with_body]) of
    {ok, 200, Headers, Body} ->
      #{<<"keys">> := Keys} = jsx:decode(Body, [return_maps]),
      CacheControl = hackney_headers:parse(<<"Cache-Control">>, Headers),
      {match, [MaxAgeBin]} = re:run(CacheControl,
                                    <<"max-age=(\\d+)">>,
                                    [{capture, all_but_first, binary}]),
      MaxAge = binary_to_integer(MaxAgeBin),
      {ok, #{exp_at => id_token_util:now_gregorian_seconds() + MaxAge,
             keys => Keys}};
    {ok, _, _, _} ->
      {error, service_unavailable};
    {error, Reason} ->
      {error, Reason}
  end.

%% Returns the jwks_uri given the well-known open id connect configuration URI
-spec get_jwks_uri(binary()) -> {ok, binary()} | {error, any()}.
get_jwks_uri(Uri) ->
  case hackney:request(get, Uri, [], <<>>, [with_body]) of
    {ok, 200, _Headers, Body} ->
      #{<<"jwks_uri">> := JwksUri} = jsx:decode(Body, [return_maps]),
      {ok, JwksUri};
    {ok, _, _, _} ->
      {error, service_unavailable};
    {error, Reason} ->
      {error, Reason}
  end.

-spec get_providers() -> [provider()].
get_providers() ->
  application:get_env(id_token, providers, []).

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
