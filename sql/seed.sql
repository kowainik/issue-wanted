-- To execute this file from SQL REPL:
-- \i sql/seed.sql

-----------------
-- BASE TABLES --
-----------------

INSERT INTO repos (name, owner, descr)
VALUES ( 'pandoc'
       , 'jgm'
       , 'Universal markup converter'
       )
       ,
       ( 'test1'
       , 'john117'
       , 'A test repo'
       );

INSERT INTO issues (number, title, body, url, repo_id)
VALUES ( 342
       , 'Fix docs'
       , 'Docs need to be fixed.'
       , 'https://api.github.com/repos/jgm/pandoc/issues/342'
       , 1
       )
       ,
       ( 20
       , 'Change increment function'
       , 'Increment function should increase by two instead of one.'
       , 'https://api.github.com/repos/john117/test1/issues/20'
       , 2
       )
       ,
       ( 3
       , 'Update config file'
       , 'Update the server configuration file.'
       , 'https://api.github.com/repos/john117/test1/issues/3'
       , 2
       );

INSERT INTO categories (name)
VALUES ('Text'), ('FFI');

INSERT INTO labels (name)
VALUES ('good first issue'), ('help wanted');

-----------------
-- JOIN TABLES --
-----------------

INSERT INTO repos_categories (repo_id, category_id)
VALUES (1, 1);

INSERT INTO issues_labels (issue_id, label_id)
VALUES (1, 2), (3,1), (2,2);
