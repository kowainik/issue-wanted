# Issue Wanted

[![CircleCI](https://circleci.com/gh/kowainik/issue-wanted.svg?style=svg)](https://circleci.com/gh/kowainik/issue-wanted)
[![Hackage](https://img.shields.io/hackage/v/issue-wanted.svg?logo=haskell)](https://hackage.haskell.org/package/issue-wanted)
[![MPL-2.0 license](https://img.shields.io/badge/license-MPL--2.0-blue.svg)](https://github.com/kowainik/issue-wanted/blob/master/LICENSE)

`issue-wanted` is a web application focused on improving the open-source Haskell
community by centralizing GitHub issues across many Haskell repositories into a
single location. The goals of `issue-wanted` are to make it easier for
programmers of all skill levels to find Haskell projects to contribute to,
increase the number of contributions to open-source Haskell projects, and
encourage more programmers to become a part of the Haskell community.


## Backend

### Prerequisites (what you need to have locally)

You will need to have the following installed on your system in order to build and test `issue-wanted`. Click on the links to learn how to install each one:

1. [ghc](https://www.haskell.org/ghcup/)
2. [cabal](https://www.haskell.org/cabal/) or [stack](https://docs.haskellstack.org/en/stable/README/)
3. [docker](https://docs.docker.com/v17.12/install/)
4. libpq-dev (Run the command `sudo apt install libpq-dev` to install)

With `docker` installed, open up a terminal (make sure your in the `issue-wanted` directory) and run the command `make postgres`. This will setup the database for you and you should be ready to go!

Follow the instructions under [`How to run server`](https://github.com/kowainik/issue-wanted/tree/rashadg1030/81-Document-Postgres-setup#how-to-run-server) and test the endpoints to see if everything is set up correctly.

Refer to issue [#81](https://github.com/kowainik/issue-wanted/issues/81) if you're still having trouble.

### How to build

To build the project, open up a terminal in the base folder and run

```shell
stack build
```

or

```shell
cabal v2-build
```

### How to generate Elm types

If any types are changed one should update the generated to Elm types by running:

```shell
stack run generate-elm
```

or

```shell
cabal v2-run generate-elm
```

### How to run server

For testing the `issue-wanted` server follow these steps:

1. Open up the terminal in the base folder and run `stack build`

2. Run the command `stack exec issue-wanted`

The server will begin running at `http://localhost:8080/`.

### API

Issue-wanted endpoints available:

| Endpoint                     | Description |
|------------------------------|-------------|
| `/issues`                    | Returns all issues. |
| `/issues/:id`                | Returns a single issue with the corresponding id. |
| `/issues?label=<label name>` | Returns a list of issues with the corresponding label. |

Tip: If you stop running the server and then try to restart it, you may need to run the command `fuser -n tcp -k 8080` to free the port.

### How to run automatic tests locally

1. In a separate terminal run `make postgres` (this command will run database in a Docker container)

2. In the first terminal run `stack test` or `cabal new-test`

## Frontend

Currently, frontend is implemented using [`Elm`]().

### Prerequisites (what you need to have locally)

* [`elm`](https://elm-lang.org/)
* [`npm`](https://www.npmjs.com/)
* [`create-elm-app`](https://github.com/halfzebra/create-elm-app): to install run `npm install -g create-elm-app@2.2.0**

### How to run

**NOTE:** All actions should be done in the `frontend` folder:

```shell
cd frontend/
```

#### First time

* Get node modules: `npm install`
* Proceed to Subsequent steps

#### Subsequent

* in one tab: `elm-app start`
* in another tab: `npm run-script watch-css`

Frontend will begin running at `http://localhost:3000/`.
