# dear.college

anyone who visits is invited to log in with a (social) login

https://dear.college/courses/:slug

first time someone visits, get the "results" code and associate logged in user as the "owner"

https://dear.college/courses/:slug/results/:code

https://dear.college/:slug/https://external.tool/

The 256-byte file `key.bin` is used as a symmetric key for the JWTs.

## Redirect flow

Suppose the user already has a `dear.college` cookie.  Then when that user visits

https://dear.college/courses/:slug/https://external.tool/

they are directed to https://external.tool/#jwt-scoped-to-external-tool

and they are added to the course :slug
and the assignment is added to the course

Since `external.tool` is running our JavaScript, the page looks for
the JWT stored in the URL hash, and decodes it.

The `aud` in the JWT will be https://dear.college

The `scp` in the JWT will be ["https://external.tool/"]

The `sub` claim identifies the user.

## API

### GET https://dear.college/api/v1/users/:user

Returns information about the :user.

Requires a JWT with a `sub` claim equal to `user`.

### GET https://dear.college/api/v1/progress/:sha
### PUT https://dear.college/api/v1/progress/:sha
### GET https://dear.college/api/v1/state/:sha
### PUT https://dear.college/api/v1/state/:sha

Requires a JWT and an X-Worksheet header with the full URL which hashes to sha.

Get and set the "progress" or page state for the user represented by the JWT on the worksheet.

The JWT must have a scope which agrees with the worksheet URL.

## How to run

Clone this repository, and then...

```
nix-shell
cd frontend
npm install
npm run build
cd ..
cabal run
```

## License

The server (the code in `src/` and `frontend/`) is licensed under the AGPL.

The code that other sites would use (namely the code in `library/`) is MIT licensed.
