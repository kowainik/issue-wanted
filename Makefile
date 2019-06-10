ide:
	ghcid --command "stack ghci --ghci-options=-fno-code --main-is issue-wanted:lib issue-wanted:exe:issue-wanted-exe issue-wanted:test:issue-wanted-test"

postgres:
	docker run -p 5432\:5432 -e POSTGRES_USER=root -e POSTGRES_DB=issue-wanted postgres\:10.5-alpine

sql-repl:
	psql -h localhost -p 5432 -U root -d issue-wanted