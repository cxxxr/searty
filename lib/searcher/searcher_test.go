package searcher

import (
	"fmt"
	"testing"

	"github.com/cxxxr/searty/lib/database"
	"github.com/cxxxr/searty/lib/testutil"
	"github.com/cxxxr/searty/lib/tokenizer"
	"github.com/stretchr/testify/assert"
)

func Test_Search(t *testing.T) {
	// databaseFile := testutil.DoIndex(t, "../indexer/test/testdata/cl-ppcre.json")
	databaseFile := testutil.DoIndex(t, "../../build/1am.json")
	tokenizer := tokenizer.New()
	database := database.New(databaseFile)
	database.Connect()
	defer database.Close()
	results, err := New(tokenizer, database).Search("defun")
	fmt.Printf("%+v\n", err)
	assert.Nil(t, err)

	fmt.Println(len(results))
	for _, result := range results {
		fmt.Println(result.doc.Filename, result.start, result.end)
	}
}
