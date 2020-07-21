id_token -- Validate and/or generate ID tokens
===========================================

# Validation

`id_token` can be used to validate ID Tokens. Signature and expiry date are verified. The library will cache the public keys from the provider to avoid
doing an HTTP request every time an ID Token is validated.

## Usage

Add the providers well known URI to the `sys.config` file of your application:

```
[{id_token,
   [ { providers
     , [ {google, <<"https://accounts.google.com/.well-known/openid-configuration">>}
       , {microsoft, <<"https://login.microsoftonline.com/common/.well-known/openid-configuration">>}
       ]
     }
   ]
 }].
```

Include `id_token` as a rebar dependency:

```
{deps, [{id_token, ".*", {git, "git://github.com/kivra/id_token.git", {tag, "0.1.1"}}}]}.
```

Add `id_token` to your `*.app.src` file so `id_token` and it's dependencies are started and use it:

```
1> id_token:validate(google, <<"eyJhbGci...">>).
{ok, #{ <<"aud">> => <<"...">>, ...}}
```
# Signature

## Using id_token_sign

Use the `id_token_sign:add_key_for/2` to add a key for the algorithm you want to use to sign. Then use the function `id_token:sign/2` to sign some claims and produce a JSON Web Token. By default the keys will be stored in an ETS table, which means you'll loose the keys if the node goes down and if you have multiple nodes the keys won't be synced between the nodes. You should implement the behaviour `id_token_pubkeys_storage` and set the environment variable `pubkeys_storage_module` to your implementation.

Example usage:
```
1> application:ensure_all_started(id_token).
{ok,[]}
2> id_token_sign:add_key_for(<<"ES256">>, #{}).
ok
3> id_token:sign(<<"ES256">>, #{<<"sub">> => <<"1242542">>, <<"iat">> => erlang:system_time(seconds)}).
<<"eyJhbGciOiJFUzI1NiIsImtpZCI6ImpRLXgwbE9LZ0NCX3ZuZXdITmRGWFEiLCJ0eXAiOiJKV1QifQ.eyJpYXQiOjE1OTUzMzU0NzEsInN1YiI6IjEyN"...>>
```

## License
The KIVRA `id_token` library uses an [MIT license](http://en.wikipedia.org/wiki/MIT_License). So go ahead and do what
you want!

Lots of fun!
