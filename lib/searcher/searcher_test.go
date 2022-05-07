package searcher

import (
	"bytes"
	"testing"

	"github.com/cxxxr/searty/lib/database"
	"github.com/cxxxr/searty/lib/testutil"
	"github.com/cxxxr/searty/lib/tokenizer"
	"github.com/stretchr/testify/require"
)

func Test_Search(t *testing.T) {
	// Prepare index
	databaseFile := testutil.DoIndex(t, "../testdata/cl-ppcre.json")

	// Prepare database connection
	database := database.New(databaseFile)
	database.Connect()
	defer database.Close()

	// Do
	results, err := New(tokenizer.New(), database).Search("defun")
	require.Nil(t, err)

	// Post
	writer := bytes.NewBuffer(nil)
	err = prettyPrintResults(results, database, writer)
	require.Nil(t, err)

	testutil.Snapshot(t, writer.Bytes(), ".snapshot")
}
