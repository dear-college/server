# dear.college

anyone who visits is invited to log in with a (social) login

https://dear.college/courses/:slug

first time someone visits, get the "results" code and associate logged in user as the "owner"

https://dear.college/courses/:slug/results/:code

https://dear.college/:slug/https://external.tool/

The 256-byte file `key.bin` is used as a symmetric key for the JWTs.

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
