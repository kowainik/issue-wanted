-- To execute this file from SQL REPL:
-- \i sql/seed.sql

-----------------
-- BASE TABLES --
-----------------

INSERT INTO repos (owner, name, descr, categories)
VALUES ( 'jgm'
       , 'pandoc'
       , 'Universal markup converter'
       , ARRAY['Text']
       )
       ,
       ( 'john117'
       , 'test1'
       , 'A test repo'
       , ARRAY['Text', 'FFI']
       );

insert into issues (repo_owner, repo_name, number, title, body, labels)
select 'jgm', 'pandoc', 342, 'Fix docs', 'Docs need to be fixed.', ARRAY['good first issue']
where exists (
    select * from repos where repos.owner = NEW.repo_owner and repos.name = NEW.repo_name
)
ON CONFLICT (repo_owner, repo_name, number) DO 
UPDATE SET title = EXCLUDED.title, body = EXCLUDED.body, labels = EXCLUDED.labels;

insert into issues (repo_owner, repo_name, number, title, body, labels)
select 'marry5454', 'test0', 6, 'Update config file', 'Update the server configuration file.', ARRAY['good first issue']
where exists (
    select * from repos where repos.owner = 'marry5454' and repos.name = 'test0'
) 
ON CONFLICT (repo_owner, repo_name, number) DO 
UPDATE SET title = EXCLUDED.title, body = EXCLUDED.body, labels = EXCLUDED.labels;

INSERT INTO issues (repo_owner, repo_name, number, title, body, labels)
VALUES ( 'john117'
       , 'test1'
       , 3
       , 'Update config file'
       , 'Update the server configuration file.'
       , ARRAY['good first issue', 'help wanted']
       ) ON CONFLICT DO NOTHING;

INSERT INTO issues (repo_owner, repo_name, number, title, body, labels)
VALUES ( 'john117'
       , 'test1'
       , 20
       , 'Change increment function'
       , 'Increment function should increase by two instead of one.'
       , ARRAY[] :: TEXT[]
       );

-------------
-- UPSERTS --
-------------

INSERT INTO repos (owner, name, descr, categories)
VALUES ( 'john117'
       , 'test1'
       , 'An experimental repo'
       , ARRAY['Text', 'FFI', 'NLP']
       )
ON CONFLICT (owner, name) DO 
UPDATE SET descr = EXCLUDED.descr, categories = EXCLUDED.categories;

INSERT INTO issues (repo_owner, repo_name, number, title, body, labels)
VALUES ( 'jgm'
       , 'pandoc'
       , 342
       , 'Fix docs'
       , 'Docs need to be fixed. Especially the section on building the project.'
       , ARRAY['good first issue', 'low hanging fruit', 'docs']
       )
ON CONFLICT (repo_owner, repo_name, number) DO 
UPDATE SET title = EXCLUDED.title, body = EXCLUDED.body, labels = EXCLUDED.labels;
