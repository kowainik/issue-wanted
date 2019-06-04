-- To execute this file from SQL REPL:
-- \i sql/schema.sql

------------
-- TABLES --
------------

CREATE TABLE IF NOT EXISTS repos
( id                  INT                      NOT NULL
, repo_name           TEXT                     NOT NULL
, repo_desc           TEXT                     NOT NULL
, repo_url            TEXT                     NOT NULL -- The repo html_url
, stargazer_count     INT                      NOT NULL
, issue_count         INT                      NOT NULL
, fork_count          INT                      NOT NULL
);

CREATE TABLE IF NOT EXISTS issues
( id               INT                      NOT NULL
, issue_number     INT                      NOT NULL
, issue_title      TEXT                     NOT NULL
, issue_body       TEXT                     NOT NULL
, issue_url        TEXT                     NOT NULL
, repo_id          INT                      NOT NULL
);

CREATE TABLE IF NOT EXISTS categories
( id                  INT                      NOT NULL
, category_name       TEXT                     NOT NULL
);

CREATE TABLE IF NOT EXISTS labels
( id               INT                      NOT NULL
, label_name       TEXT                     NOT NULL
, label_color      TEXT                     NOT NULL
);

-----------------
-- JOIN TABLES --
-----------------

CREATE TABLE IF NOT EXISTS issues_labels
( issue_id     INT                        NOT NULL
, label_id     INT                        NOT NULL  
);

CREATE TABLE IF NOT EXISTS repos_categories
( repo_id         INT                        NOT NULL
, category_id     INT                        NOT NULL  
);

-----------------
-- CONSTRAINTS --
-----------------

ALTER TABLE ONLY repos 
  ADD CONSTRAINT pk_repos PRIMARY KEY (id);

ALTER TABLE ONLY issues
  ADD CONSTRAINT pk_issues PRIMARY KEY (id);

ALTER TABLE ONLY issues
  ADD CONSTRAINT fk_issues FOREIGN KEY (repo_id) REFERENCES repos (id) ON DELETE CASCADE;

ALTER TABLE ONLY categories 
  ADD CONSTRAINT pk_categories PRIMARY KEY (id);

ALTER TABLE ONLY labels
  ADD CONSTRAINT pk_labels PRIMARY KEY (id);

ALTER TABLE ONLY repos_categories
  ADD CONSTRAINT pk_repos_categories PRIMARY KEY (repo_id, category_id);

ALTER TABLE ONLY issues_labels
  ADD CONSTRAINT pk_issues_labels PRIMARY KEY (issue_id, label_id);
