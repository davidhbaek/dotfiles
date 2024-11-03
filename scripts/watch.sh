
#!/bin/bash
function gob() {
	ROOT="$(git rev-parse --show-toplevel)"
	watchexec --filter '*.go' --filter '*.json' --clear --watch $ROOT '
	printf "$(date) running goimports...\n" && goimports -w . &&
	printf "$(date) running go build...\n" && go build -tags=gcp ./... &&  
	printf "$(date) running golangci-lint...\n" && golangci-lint run --allow-parallel-runners &&
	# printf "$(date) running go test...\n" && go test -v ./... -run TestStreamMessage
	printf "$(date) done\n"
'
}
gob
