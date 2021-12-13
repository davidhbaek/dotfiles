#!/bin/bash
function gob() {
	ROOT="$(git rev-parse --show-toplevel)"
	watchexec --filter '*.go' --filter '*.json' --clear --watch $ROOT '
	clear && printf "\033[3J"
	printf "$(date) running goimports...\n" && goimports -w . &&
	printf "$(date) running go build...\n" && go build -tags manual ./... &&  
	printf "$(date) cleaning test cache...\n" && go clean -testcache &&  
  printf "$(date) testing...\n" && go test ./... -v --run TestHandler
	printf "$(date) running golangci-lint...\n" && golangci-lint run --allow-parallel-runners &&
	printf "$(date) done\n"
'
}
gob