package tree_sitter_saf_test

import (
	"testing"

	tree_sitter "github.com/smacker/go-tree-sitter"
	"github.com/tree-sitter/tree-sitter-saf"
)

func TestCanLoadGrammar(t *testing.T) {
	language := tree_sitter.NewLanguage(tree_sitter_saf.Language())
	if language == nil {
		t.Errorf("Error loading Saf grammar")
	}
}
