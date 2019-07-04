-- To execute this file from SQL REPL:
-- \i sql/seed.sql

-------------
-- UPSERTS --
-------------

INSERT INTO repos (owner, name, descr, categories)
VALUES ('jgm', 'pandoc', 'Universal markup converter', ARRAY['Text', 'Parser'])
     , ('john117', 'test1', 'A test repo', ARRAY['Text', 'FFI'])
ON CONFLICT ON CONSTRAINT unique_repos DO 
UPDATE SET descr = EXCLUDED.descr, categories = EXCLUDED.categories;

INSERT INTO repos (owner, name, descr, categories)
VALUES ('jgm', 'pandoc', 'Universal markup converter for various text formats.', ARRAY['Text'])
ON CONFLICT ON CONSTRAINT unique_repos DO 
UPDATE SET descr = EXCLUDED.descr, categories = EXCLUDED.categories;

INSERT INTO issues (repo_owner, repo_name, number, title, body, labels)
SELECT repo_owner, repo_name, number, title, body, labels
FROM ( VALUES ('jgm','pandoc',342,'Fix docs','Docs need to be fixed.',ARRAY['good first issue','low hanging fruit','docs'])
            , ('marry5454', 'test0', 6, 'Update config file', 'Update the server configuration file.', ARRAY['good first issue'])
            , ('john117', 'test1', 3, 'Update config file', 'Update the server configuration file.', ARRAY['good first issue', 'help wanted'])
     )
AS new (repo_owner, repo_name, number, title, body, labels)
WHERE EXISTS (
    SELECT (owner, name) FROM repos WHERE (repos.owner, repos.name) = (new.repo_owner, new.repo_name)
)
ON CONFLICT ON CONSTRAINT unique_issues DO 
UPDATE SET title = EXCLUDED.title, body = EXCLUDED.body, labels = EXCLUDED.labels;

INSERT INTO issues (repo_owner, repo_name, number, title, body, labels)
SELECT repo_owner, repo_name, number, title, body, labels
FROM ( VALUES ('john117', 'test1', 3, 'An experimental repo', 'Update the server configuration file.' , ARRAY['Text', 'FFI', 'NLP'])
            , ('jgm', 'pandoc', 342, 'Fix docs', 'Docs need to be fixed. Especially the section on building the project.', ARRAY['good first issue'])
     )
AS new (repo_owner, repo_name, number, title, body, labels)
WHERE EXISTS (
    SELECT (owner, name) FROM repos WHERE (repos.owner, repos.name) = (new.repo_owner, new.repo_name)
)
ON CONFLICT ON CONSTRAINT unique_issues DO 
UPDATE SET title = EXCLUDED.title, body = EXCLUDED.body, labels = EXCLUDED.labels;
