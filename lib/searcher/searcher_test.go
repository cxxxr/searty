package searcher

import (
	"testing"

	"github.com/cxxxr/searty/lib/database"
	"github.com/cxxxr/searty/lib/testutil"
	"github.com/cxxxr/searty/lib/tokenizer"
	"github.com/stretchr/testify/assert"
)

func Test_Search(t *testing.T) {
	databaseFile := testutil.DoIndex(t, "../indexer/test/testdata/cl-ppcre.json")
	// databaseFile := testutil.DoIndex(t, "testdata/1am.json")
	tokenizer := tokenizer.New()
	database := database.New(databaseFile)
	database.Connect()
	defer database.Close()
	searcher := New(tokenizer, database)
	results, err := searcher.Search("defun")
	assert.Nil(t, err)

	err = prettyPrintResults(results, database)
	assert.Nil(t, err)
}
