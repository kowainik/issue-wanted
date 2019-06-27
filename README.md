# issue-wanted

[![CircleCI](https://circleci.com/gh/kowainik/issue-wanted.svg?style=svg)](https://circleci.com/gh/kowainik/issue-wanted)
[![Hackage](https://img.shields.io/hackage/v/issue-wanted.svg?logo=haskell)](https://hackage.haskell.org/package/issue-wanted)
[![MPL-2.0 license](https://img.shields.io/badge/license-MPL--2.0-blue.svg)](https://github.com/kowainik/issue-wanted/blob/master/LICENSE)

`issue-wanted` is a web application focused on improving the open-source Haskell
community by centralizing GitHub issues across many Haskell repositories into a
single location. The goals of `issue-wanted` are to make it easier for
programmers of all skill levels to find Haskell projects to contribute to,
increase the number of contributions to open-source Haskell projects, and
encourage more programmers to become a part of the Haskell community.

# Testing

## Testing the Server

For testing the `issue-wanted` server follow these steps:

1. Open up the terminal in the base folder and run `stack build`

2. Run the command `stack exec issue-wanted`

The server will begin running at `http://localhost:8080/`.

Issue-wanted endpoints available:

* `http://localhost:8080/issues`

  Returns all issues.

* `http://localhost:8080/issues/:id`

  Returns a single issue with the corresponding id.

* `http://localhost:8080/issues?label=<label name>`

  Returns a list of issues with the corresponding label.  

Tip: If you stop running the server and then try to restart it, you may need to run the command `fuser -n tcp -k 8080` to free the port.

## Testing the Database

For testing the database follow these steps:

1. Open up the terminal in the base folder and run `cd test/Db`

2. Run the command `stack ghci` to get into the REPL

3. Run the command `:l Test` to load the database testing module

You should now be able to test the insert and query functions in `issue-wanted/src/IW/Db` from the REPL.
