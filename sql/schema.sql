-- To execute this file from SQL REPL:
-- \i sql/schema.sql

-----------------
-- BASE TABLES --
-----------------

CREATE TABLE IF NOT EXISTS repos
( id         SERIAL      PRIMARY KEY  
, owner      TEXT        NOT NULL       
, name       TEXT        NOT NULL
, descr      TEXT        NOT NULL
, categories TEXT ARRAY  NOT NULL
, created_at TIMESTAMP   WITH TIME ZONE DEFAULT NOW() NOT NULL
, updated_at TIMESTAMP   WITH TIME ZONE DEFAULT NOW() NOT NULL
);

CREATE TABLE IF NOT EXISTS issues
( id         SERIAL      PRIMARY KEY 
, repo_owner TEXT        NOT NULL
, repo_name  TEXT        NOT NULL
, number     INT         NOT NULL
, title      TEXT        NOT NULL
, body       TEXT        NOT NULL
, labels     TEXT ARRAY  NOT NULL
, created_at TIMESTAMP   WITH TIME ZONE DEFAULT NOW() NOT NULL
, updated_at TIMESTAMP   WITH TIME ZONE DEFAULT NOW() NOT NULL
);

-----------------------------
-- FOREIGN KEY CONSTRAINTS --
-----------------------------

ALTER TABLE ONLY repos
  ADD CONSTRAINT unique_repos UNIQUE (owner, name);

ALTER TABLE ONLY issues
  ADD CONSTRAINT unique_issues UNIQUE (repo_owner, repo_name, number);

ALTER TABLE ONLY issues
  ADD CONSTRAINT fk_repos FOREIGN KEY (repo_owner, repo_name) 
  REFERENCES repos (owner, name) ON DELETE CASCADE;

--------------
-- TRIGGERS --
--------------

CREATE OR REPLACE FUNCTION update_updated_at() 
RETURNS TRIGGER AS $$
BEGIN
    NEW.updated_at = NOW();
    RETURN NEW; 
END;
$$ LANGUAGE 'plpgsql';

CREATE TRIGGER update_updated_at_repos BEFORE UPDATE ON repos FOR EACH ROW EXECUTE PROCEDURE update_updated_at();
CREATE TRIGGER update_updated_at_issues BEFORE UPDATE ON issues FOR EACH ROW EXECUTE PROCEDURE update_updated_at();
