-- To execute this file from SQL REPL:
-- \i sql/drop.sql

DROP TABLE IF EXISTS repos CASCADE;
DROP TABLE IF EXISTS issues CASCADE;
DROP TABLE IF EXISTS categories CASCADE;
DROP TABLE IF EXISTS labels CASCADE;
DROP TABLE IF EXISTS repos_categories CASCADE;
DROP TABLE IF EXISTS issues_labels CASCADE;