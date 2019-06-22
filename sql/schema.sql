-- To execute this file from SQL REPL:
-- \i sql/schema.sql

-----------------
-- BASE TABLES --
-----------------

CREATE TABLE IF NOT EXISTS repos
( id    SERIAL PRIMARY KEY        
, name  TEXT   NOT NULL
, owner TEXT   NOT NULL
, descr TEXT
);

CREATE TABLE IF NOT EXISTS issues
( id      SERIAL PRIMARY KEY 
, number  INT    NOT NULL
, title   TEXT   NOT NULL
, body    TEXT
, url     TEXT   NOT NULL
, repo_id INT    NOT NULL
);

CREATE TABLE IF NOT EXISTS categories
( id   SERIAL PRIMARY KEY
, name TEXT   NOT NULL
);

CREATE TABLE IF NOT EXISTS labels
( id   SERIAL PRIMARY KEY
, name TEXT   NOT NULL
);

-----------------
-- JOIN TABLES --
-----------------

CREATE TABLE IF NOT EXISTS repos_categories
( repo_id     INT NOT NULL
, category_id INT NOT NULL  
);

CREATE TABLE IF NOT EXISTS issues_labels
( issue_id INT NOT NULL
, label_id INT NOT NULL  
);

-----------------------------
-- FOREIGN KEY CONSTRAINTS --
-----------------------------

ALTER TABLE ONLY issues
  ADD CONSTRAINT fk_repos FOREIGN KEY (repo_id) REFERENCES repos (id) ON DELETE CASCADE;

ALTER TABLE ONLY repos_categories
  ADD CONSTRAINT fk_repos FOREIGN KEY (repo_id) REFERENCES repos (id) ON DELETE CASCADE;

ALTER TABLE ONLY repos_categories
  ADD CONSTRAINT fk_categories FOREIGN KEY (category_id) REFERENCES categories (id) ON DELETE CASCADE;

ALTER TABLE ONLY issues_labels
  ADD CONSTRAINT fk_issues FOREIGN KEY (issue_id) REFERENCES issues (id) ON DELETE CASCADE;

ALTER TABLE ONLY issues_labels
  ADD CONSTRAINT fk_labels FOREIGN KEY (label_id) REFERENCES labels (id) ON DELETE CASCADE;
