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

## Prerequisites (what you need to have locally)

For the project to build, you will need to have `libpq-dev` installed on you computer. You can install it by running the command `sudo apt install libpq-dev`.

You will also need to setup Postgres on your computer. Here are the instructions for doing so:

1. Run the folowing commands with `<username>` replaced by a username of your choice.

   ```
   $ sudo apt install postgresql postgresql-contrib
   $ sudo service postgres start
   $ sudo -u postgres psql
   postgres=# create database "issue-wanted";
   postgres=# create user <username>;
   postgres=# grant all privileges on database "issue-wanted" to <username>;
   ```

2. Modify the [`pg_hba.conf`](https://dba.stackexchange.com/questions/101280/how-to-handle-user-with-no-password-in-postgresql) file with the following lines.

   ```
   local all <username>          trust
   host all <username> 0.0.0.0/0 trust
   ```

   Where `<username>` is the same one you used above. 

3. Next, in the `config.toml` file in the repository update the line `user=<username>` with the `<username>` from above.

4. Add the line `listen_address = '127.0.0.1'` to the `/etc/postgresql/10/main/postgresql.conf` file.

5. Finally, restart the database and intitalize it with the following commands.

   ```
   $ sudo service postgres restart
   $ psql issue-wanted < sql/schema.sql
   $ psql issue-wanted < sql/seed.sql
   ```

Follow the instructions under [`How to run server`](https://github.com/kowainik/issue-wanted/tree/rashadg1030/81-Document-Postgres-setup#how-to-run-server) and test the endpoints to see if everything is set up correctly.

Refer to issue [#81](https://github.com/kowainik/issue-wanted/issues/81) if you're still having trouble.

## How to build

To build the project, open up a terminal in the base folder and run `stack build`.

## How to run server

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

## How to run automatic tests locally

1. In a separate terminal run `make postgres` (this command will run database in a Docker container)

2. In the first terminal run `stack test` or `cabal new-test`
