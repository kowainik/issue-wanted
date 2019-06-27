-- To execute this file from SQL REPL:
-- \i sql/seed.sql

-----------------
-- BASE TABLES --
-----------------

INSERT INTO repos (name, owner, descr, categories)
VALUES ( 'pandoc'
       , 'jgm'
       , 'Universal markup converter'
       , ARRAY['Text']
       )
       ,
       ( 'test1'
       , 'john117'
       , 'A test repo'
       , ARRAY['Text', 'FFI']
       );

INSERT INTO issues (number, title, body, url, repo_name, labels)
VALUES ( 342
       , 'Fix docs'
       , 'Docs need to be fixed.'
       , 'https://api.github.com/repos/jgm/pandoc/issues/342'
       , 'pandoc'
       , ARRAY['good first issue', 'help wanted', 'docs']
       )
       ,
       ( 20
       , 'Change increment function'
       , 'Increment function should increase by two instead of one.'
       , 'https://api.github.com/repos/john117/test1/issues/20'
       , 'test1'
       , ARRAY['low hanging fruit']
       )
       ,
       ( 3
       , 'Update config file'
       , 'Update the server configuration file.'
       , 'https://api.github.com/repos/john117/test1/issues/3'
       , 'test1'
       , ARRAY['good first issue', 'help wanted']
       );
