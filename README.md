# dear.college

anyone who visits is invited to log in with a (social) login

https://dear.college/course

first time someone visits, get the "results" code and associate logged in user as the "owner"

https://dear.college/course/results-code

https://dear.college/course/https://external.tool/

The 256-byte file `key.bin` is used as a symmetric key for the JWTs.

## Redirect flow

Suppose the user already has a `dear.college` cookie.  Then when that user visits

https://dear.college/slug/https://external.tool/

they are directed to https://external.tool/#jwt-scoped-to-external-tool

Since `external.tool` is running our JavaScript, the page looks for
the JWT stored in the URL hash, and decodes it.

The `aud` in the JWT will be https://api.dear.college

The `scp` in the JWT will be ["https://external.tool/"]

The `sub` claim identifies the user.

## API

### GET https://api.dear.college/users/:user

Returns information about the :user.

Requires a JWT with a `sub` claim equal to `user`.

### GET https://api.dear.college/users/:user
