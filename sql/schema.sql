-- To execute this file from SQL REPL:
-- \i sql/schema.sql

-----------------
-- BASE TABLES --
-----------------

CREATE TABLE IF NOT EXISTS repos
( id         SERIAL       
, repo_name  TEXT   NOT NULL
, repo_owner TEXT   NOT NULL
, repo_desc  TEXT
-- The repo's html_url
, repo_url   TEXT   NOT NULL 
);

CREATE TABLE IF NOT EXISTS issues
( id           SERIAL 
, issue_number INT    NOT NULL
, issue_title  TEXT   NOT NULL
, issue_body   TEXT
, issue_url    TEXT   NOT NULL
, repo_id      INT    NOT NULL
);

CREATE TABLE IF NOT EXISTS categories
( id            SERIAL
, category_name TEXT   NOT NULL
);

CREATE TABLE IF NOT EXISTS labels
( id         SERIAL
, label_name TEXT   NOT NULL
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
-- PRIMARY KEY CONSTRAINTS --
-----------------------------

ALTER TABLE ONLY repos 
  ADD CONSTRAINT pk_repos PRIMARY KEY (id);

ALTER TABLE ONLY issues
  ADD CONSTRAINT pk_issues PRIMARY KEY (id);

ALTER TABLE ONLY categories 
  ADD CONSTRAINT pk_categories PRIMARY KEY (id);

ALTER TABLE ONLY labels
  ADD CONSTRAINT pk_labels PRIMARY KEY (id);

ALTER TABLE ONLY repos_categories
  ADD CONSTRAINT pk_repos_categories PRIMARY KEY (repo_id, category_id);

ALTER TABLE ONLY issues_labels
  ADD CONSTRAINT pk_issues_labels PRIMARY KEY (issue_id, label_id);

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
