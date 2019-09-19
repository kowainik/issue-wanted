FROM fpco/stack-build:lts-14.5
RUN apt-get update -y && apt-get install libpq-dev postgresql -y
