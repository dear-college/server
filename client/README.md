# @dear.college/client

## How to use this library

### login()

Check to see if `document.location` has a URL fragment ("hash") which is a valid JWT.  (We cannot verify the claims in the JWT, but we can check that it is a compactly encoded JWT.)  If so, store the JWT in localStorage.

This can be added to the event handler for `DOMContentLoaded`.

### logout()

Clear any JWT in localStorage.

### getProgress( location = document.location )

Fetch the current user's "score" on the assignment associated with `location`.  "Score" is a number between 0 and 1.

The `aud` claim in the JWT from localStorage provides a URL.  Make a `GET` request to that URL with the path `/api/v1/progress/:sha` where sha is the hash of `location`.  Include the JWT in the `Authorization` header, and include the `location` in a `X-Worksheet` header.

### putProgress( score = 1, location = document.location )

Update the current user's "score" on the assignment associated with `location`.

### getState( location = document.location )

### putState( state = { }, location = document.location )
