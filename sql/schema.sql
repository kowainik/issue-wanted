-- To execute this file from SQL REPL:
-- \i sql/schema.sql

-----------------
-- BASE TABLES --
-----------------

CREATE TABLE IF NOT EXISTS repos
( id         SERIAL    PRIMARY KEY  
, owner      TEXT      NOT NULL       
, name       TEXT      NOT NULL
, descr      TEXT      NOT NULL
, categories TEXT      ARRAY
, created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW()
, updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW()
);

CREATE TABLE IF NOT EXISTS issues
( id         SERIAL PRIMARY KEY 
, number     INT    NOT NULL
, title      TEXT   NOT NULL
, body       TEXT   NOT NULL
, repo_owner TEXT   NOT NULL
, repo_name  TEXT   NOT NULL
, labels     TEXT   ARRAY
);

-----------------------------
-- FOREIGN KEY CONSTRAINTS --
-----------------------------

ALTER TABLE ONLY repos
  ADD UNIQUE (owner, name);

ALTER TABLE ONLY issues
  ADD CONSTRAINT fk_repos FOREIGN KEY (repo_owner, repo_name) 
  REFERENCES repos (owner, name) ON DELETE CASCADE;

--------------
-- TRIGGERS --
--------------

CREATE OR REPLACE FUNCTION update_timestamp() 
RETURNS TRIGGER AS $$
BEGIN
    NEW.updated_at = NOW();
    RETURN NEW; 
END;
$$ LANGUAGE 'plpgsql';

CREATE TRIGGER update_issues_timestamp BEFORE UPDATE ON issues FOR EACH ROW EXECUTE PROCEDURE update_timestamp();
CREATE TRIGGER update_repos_timestamp BEFORE UPDATE ON repos FOR EACH ROW EXECUTE PROCEDURE update_timestamp();
