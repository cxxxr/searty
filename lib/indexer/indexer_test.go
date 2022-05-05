package indexer

import (
	"fmt"
	"os"
	"testing"

	"github.com/cxxxr/searty/lib/database"
	"github.com/cxxxr/searty/lib/primitive"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func Test_index(t *testing.T) {
	databaseFile, err := os.CreateTemp("", "searty.sqlite3.*")
	defer databaseFile.Close()
	require.Nil(t, err)
	indexer := New()
	err = indexer.Index("testdata/cl-ppcre.json", databaseFile.Name())
	require.Nil(t, err)

	database := database.New(databaseFile.Name())
	require.Nil(t, database.Connect())

	docs, err := database.ResolveAllDocuments()
	require.Nil(t, err)

	assert.Equal(t, len(docs), 17)

	for _, doc := range docs {
		println(doc.Filename)
	}

	tokenIds, err := database.ResolveAllTokenIds()
	assert.Nil(t, err)

	invertedIndex, err := database.ResolveInvertedIndex(tokenIds)
	assert.Nil(t, err)

	for _, tokenId := range tokenIds {
		postingList, _ := invertedIndex.Get(tokenId)
		fmt.Printf("%s: count = %d\n", tokenId, postingList.Count())
		invertedIndex.MapPostingList(
			tokenId,
			func(docId primitive.DocumentId, positions []int) error {
				fmt.Println(docId, positions)
				return nil
			},
		)
	}
}
