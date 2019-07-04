-- To execute this file from SQL REPL:
-- \i sql/seed.sql

-------------
-- UPSERTS --
-------------

INSERT INTO repos 
    (owner, name, descr, categories)
VALUES 
    ('owner123', 'repo123', 'A test repo.', ARRAY['Testing'])
ON CONFLICT ON CONSTRAINT unique_repos DO 
UPDATE SET 
    descr = EXCLUDED.descr
  , categories = EXCLUDED.categories;

INSERT INTO issues 
    (repo_owner, repo_name, number, title, body, labels)
SELECT 
    repo_owner, repo_name, number, title, body, labels
FROM ( VALUES 
           ('owner123', 'repo123', 123, 'This is a test issue', 'Use this issue for testing.', ARRAY['good first issue'])
     )
AS new (repo_owner, repo_name, number, title, body, labels)
WHERE EXISTS (
    SELECT (owner, name) 
    FROM repos 
    WHERE (repos.owner, repos.name) = (new.repo_owner, new.repo_name)
)
ON CONFLICT ON CONSTRAINT unique_issues DO 
UPDATE SET 
    title = EXCLUDED.title
  , body = EXCLUDED.body
  , labels = EXCLUDED.labels;
