-- To execute this file from SQL REPL:
-- \i sql/seed.sql

-----------------
-- BASE TABLES --
-----------------

INSERT INTO repos (id, repo_name, repo_desc, repo_url, open_issue_count, fork_count)
VALUES ( 1
       , 'pandoc'
       , 'Universal markup converter'
       , 'https://github.com/jgm/pandoc'
       , 495
       , 1788 
       );

INSERT INTO issues (id, issue_number, issue_title, issue_body, issue_url, repo_id)
VALUES ( 1
       , 342
       , 'Fix docs'
       , 'Docs need to be fixed.'
       , 'https://api.github.com/repos/jgm/pandoc/issues/342'
       , 1
       );

INSERT INTO categories (id, category_name)
VALUES (1, 'Text'), (2, 'FFI');

INSERT INTO labels (id, label_name, label_color)
VALUES (1, 'good first issue', '00FFFF'), (2, 'help wanted', 'F08080');

-----------------
-- JOIN TABLES --
-----------------

INSERT INTO repos_categories (repo_id, category_id)
VALUES (1, 1);

INSERT INTO issues_labels (issue_id, label_id)
VALUES (1, 2);
