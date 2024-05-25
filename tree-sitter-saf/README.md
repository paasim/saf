# tree-sitter-saf

`saf` grammar for [tree-sitter](https://tree-sitter.github.io/tree-sitter/).

## testing

```bash
# display parse tree
npm run parse ../example.saf

# apply syntax highlighting
npm run highlight ../example.saf

# run tests
npm test
```

## installation

Following e.g. [neovim install instructions](https://github.com/nvim-treesitter/nvim-treesitter/#adding-parsers) would result in:
```lua
vim.filetype.add { extension = { saf = 'saf' } }
local parser_config = require('nvim-treesitter.parsers').get_parser_configs()
parser_config.saf = {
  install_info = {
    url = 'https://github.com/paasim/saf.git', -- local path or git repo
    files = { 'src/parser.c' },
    location = 'tree-sitter-saf',
    branch = 'main',
  },
}
```

Note, that in addition you might want to add the `queries` folder to inside your nvim config.
