.PHONY: build check eval jit parse scan test

build:
	cargo build -r
	ln -s ./target/release/interpreter safi
	ln -s ./target/release/compiler safc

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
