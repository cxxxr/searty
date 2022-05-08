package searcher

import (
	"bytes"
	"testing"

	"github.com/cxxxr/searty/lib/database"
	"github.com/cxxxr/searty/lib/testutil"
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
	results, err := NewPhraseSearcher(db).Search("defun")
	require.Nil(t, err)

	// Post
	writer := bytes.NewBuffer(nil)
	err = PrintResults(results, db, writer)
	require.Nil(t, err)

	testutil.Snapshot(t, writer.Bytes(), ".snapshot.Test_SearchPhrase")
}

func Test_SearchSymbol(t *testing.T) {
	databaseFile := testutil.DoIndex(t, "../testdata/1am.json")

	db := database.New(databaseFile)
	db.Connect()
	defer db.Close()

	results, err := NewSymbolSearcher(db).Search("is")
	require.Nil(t, err)

	writer := bytes.NewBuffer(nil)
	err = PrintResults(results, db, writer)
	require.Nil(t, err)

	testutil.Snapshot(t, writer.Bytes(), ".snapshot.Test_SearchSymbol")
}
