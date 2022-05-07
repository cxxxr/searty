package searcher

import (
	"bytes"
	"fmt"
	"testing"

	"github.com/cxxxr/searty/lib/database"
	"github.com/cxxxr/searty/lib/testutil"
	"github.com/cxxxr/searty/lib/tokenizer"
	"github.com/stretchr/testify/require"
)

func Test_SearchPhrase(t *testing.T) {
	// Prepare index
	databaseFile := testutil.DoIndex(t, "../testdata/cl-ppcre.json")

	// Prepare db connection
	db := database.New(databaseFile)
	db.Connect()
	defer db.Close()

	// Do
	results, err := NewPhraseSearcher(tokenizer.New(), db).Search("defun")
	require.Nil(t, err)

	// Post
	writer := bytes.NewBuffer(nil)
	err = prettyPrintResults(results, db, writer)
	require.Nil(t, err)

	testutil.Snapshot(t, writer.Bytes(), ".snapshot")
}

func Test_XXX(t *testing.T) {
	databaseFile := testutil.DoIndex(t, "../testdata/1am.json")

	fmt.Println(databaseFile)
	db := database.New(databaseFile)
	db.Connect()
	defer db.Close()

	NewSymbolSearcher(db).Search("shuffle")
}
