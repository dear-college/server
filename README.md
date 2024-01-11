# The Xaturator

The Ximera Xloud depends on being able to get .tex files, including
.tex files which have been compiled into .dvi for xourses.  The
Xaturator is the server which, sitting in front of GitHub, facilitates
this.

## API

### GET /api/repos/:owner/:repo

that will provide the 'default branch'
which then can be queried with

### GET /api/repos/:owner/:repo/commits/main

to get the most recent commit.

### GET /api/repos/:owner/:repo/files/:path
