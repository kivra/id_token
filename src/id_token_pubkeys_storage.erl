-module(id_token_pubkeys_storage).

-export([delete/1,
         get/1,
         get_all/0,
         put/1,
         start/0,
         stop/0,
         delete_older_than/1
        ]).

-callback delete(Kid :: binary()) -> ok | {error, Reason :: term()}.
-callback get(Kid :: binary()) -> {ok, Key :: map()} | {error, Reason :: term()}.
-callback get_all() -> {ok, Keys :: [map()]} | {error, Reason :: term()}.
-callback put(Key :: map()) -> ok | {error, Reason :: term()}.
-callback start() -> term().
-callback stop() -> term().

-optional_callbacks([start/0, stop/0]).

-define(BACKEND,
        (application:get_env(id_token, pubkeys_storage_module, ets_pubkeys_storage))).

-define(call_callback(Args),
        begin
          Mod = ?BACKEND,
          code:ensure_loaded(Mod),
          case erlang:function_exported(Mod, ?FUNCTION_NAME, ?FUNCTION_ARITY) of
            true -> erlang:apply(Mod, ?FUNCTION_NAME, Args);
            false -> {error, not_exported}
          end
        end).

delete(Key) -> ?call_callback([Key]).
get(Kid) -> ?call_callback([Kid]).
get_all() -> ?call_callback([]).
put(Key) -> ?call_callback([Key]).
start() -> ?call_callback([]).
stop() -> ?call_callback([]).

-spec delete_older_than(Timestamp :: integer()) -> {ok, Keys :: [map()]} |
                                                   {error, Reason :: term()}.
delete_older_than(_Timestamp) ->
  try
    Mod = ?BACKEND,
    {ok, Keys} = Mod:get_all(),
    Now = erlang:system_time(seconds),
    OlderKeys = [begin
                   ok = Mod:delete(Kid),
                   Key
                 end
                 || #{<<"expiry_time">> := ET, <<"kid">> := Kid} = Key <- Keys,
                    ET > Now
                ],
    {ok, OlderKeys}
  catch
    _C:E:_S ->
      {badmatch, Error} = E,
      Error
  end.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
