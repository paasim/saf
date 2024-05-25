.PHONY: echo init test parse parse-dbg playground
EXAMPLE=../example.saf

echo:
	@echo make highlight: highlight ../example.saf
	@echo make parse: display parse tree for ../example.saf
	@echo make test: run tests
	@echo make playground: start tree-sitter playground

highlight: src/parser.c
	npm run highlight $(EXAMPLE)

install:
	npm install

parse: src/parser.c
	npm run parse $(EXAMPLE)

playground: tree-sitter-saf.wasm
	npm exec tree-sitter playground

src/parser.c: grammar.js
	npm run generate

test: src/parser.c
	npm test

tree-sitter-saf.wasm: src/parser.c
	npm exec tree-sitter build -- --wasm
