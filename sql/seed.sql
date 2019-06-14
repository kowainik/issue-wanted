-- To execute this file from SQL REPL:
-- \i sql/seed.sql

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