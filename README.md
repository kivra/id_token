id_token -- A library to validate Id Tokens
===========================================

id_token is a library to validate Id Tokens. Signature and expiry date are verified. The library will cache the public keys from the provider to avoid
doing an HTTP request every time an Id Token is validated.

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

Include id_token as a rebar dependency:

```
{deps, [{id_token, ".*", {git, "git://github.com/kivra/id_token.git", {tag, "0.1.1"}}}]}.
```

Add `id_token` to your `*.app.src` file so `id_token` and it's dependencies are started and use it:

```
1> id_token:validate(google, <<"eyJhbGci...">>).
{ok, #{ <<"aud">> => <<"...">>, ...}}
```

## License
The KIVRA id_token library uses an [MIT license](http://en.wikipedia.org/wiki/MIT_License). So go ahead and do what
you want!

Lots of fun!
