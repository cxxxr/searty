package searcher

import (
	"os"
	"testing"

	"github.com/cxxxr/searty/lib/database"
	"github.com/cxxxr/searty/lib/testutil"
	"github.com/cxxxr/searty/lib/tokenizer"
	"github.com/stretchr/testify/require"
)

func Test_Search(t *testing.T) {
	databaseFile := testutil.DoIndex(t, "../indexer/test/testdata/cl-ppcre.json")
	tokenizer := tokenizer.New()
	database := database.New(databaseFile)
	database.Connect()
	defer database.Close()
	searcher := New(tokenizer, database)
	results, err := searcher.Search("defun")
	require.Nil(t, err)

	file, err := os.Create("/tmp/searty.searchresult.txt")
	require.Nil(t, err)
	err = prettyPrintResults(results, database, file)
	require.Nil(t, err)
}
