.PHONY: echo init test parse parse-dbg playground

echo:
	@echo make highlight: highlight example.saf
	@echo make test: run tree-sitter test

src/parser.c: grammar.js
	tree-sitter generate

highlight:
	tree-sitter highlight ../example.saf

test: src/parser.c
	tree-sitter test

parse: src/parser.c
	tree-sitter parse ../example.saf

parse-dbg: src/parser.c
	tree-sitter parse -D ../example.saf

tree-sitter-saf.wasm: src/parser.c
	tree-sitter build-wasm

playground: tree-sitter-saf.wasm
	tree-sitter playground
