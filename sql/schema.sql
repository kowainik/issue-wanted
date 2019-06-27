-- To execute this file from SQL REPL:
-- \i sql/schema.sql

-----------------
-- BASE TABLES --
-----------------

CREATE TABLE IF NOT EXISTS repos
( id         SERIAL PRIMARY KEY        
, name       TEXT   NOT NULL UNIQUE
, owner      TEXT   NOT NULL
, descr      TEXT
, categories TEXT   ARRAY
);

CREATE TABLE IF NOT EXISTS issues
( id        SERIAL PRIMARY KEY 
, number    INT    NOT NULL
, title     TEXT   NOT NULL
, body      TEXT
, url       TEXT   NOT NULL
, repo_name TEXT   NOT NULL
, labels    TEXT   ARRAY
);

-----------------------------
-- FOREIGN KEY CONSTRAINTS --
-----------------------------

ALTER TABLE ONLY issues
  ADD CONSTRAINT fk_repos FOREIGN KEY (repo_name) REFERENCES repos (name) ON DELETE CASCADE;
