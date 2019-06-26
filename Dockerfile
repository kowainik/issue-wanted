FROM fpco/stack-build:lts-13.26
RUN apt-get update -y && apt-get install libpq-dev  postgresql -y
