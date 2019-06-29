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

INSERT INTO issues (number, title, body, url, repo_owner, repo_name, labels)
VALUES ( 342
       , 'Fix docs'
       , 'Docs need to be fixed.'
       , 'https://api.github.com/repos/jgm/pandoc/issues/342'
       , 'jgm'
       , 'pandoc'
       , ARRAY['good first issue']
       )
       ,
       ( 20
       , 'Change increment function'
       , 'Increment function should increase by two instead of one.'
       , 'https://api.github.com/repos/john117/test1/issues/20'
       , 'john117'
       , 'test1'
       , ARRAY[] :: TEXT[]
       )
       ,
       ( 3
       , 'Update config file'
       , 'Update the server configuration file.'
       , 'https://api.github.com/repos/john117/test1/issues/3'
       , 'john117'
       , 'test1'
       , ARRAY['good first issue', 'help wanted']
       );
