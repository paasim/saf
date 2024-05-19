.PHONY: build check eval jit parse scan test

build:
	cargo build -r

check:
	.git/hooks/pre-commit

clean:
	unlink safi
	unlink safc
	rm -rf target

eval:
	cargo run -r --bin interpreter

jit:
	cargo run -r

parse:
	cargo run -r --bin interpreter -- --parse

scan:
	cargo run -r --bin interpreter -- --scan

test:
	cargo test
