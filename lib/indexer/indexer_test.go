package indexer

import (
	"bytes"
	"fmt"
	"os"
	"sort"
	"testing"

	"github.com/cxxxr/searty/lib/database"
	"github.com/cxxxr/searty/lib/primitive"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func isExists(filename string) bool {
	_, err := os.Stat(filename)
	return err == nil
}

func Test_index(t *testing.T) {
	writer := bytes.NewBuffer(nil)

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
		fmt.Fprintln(writer, doc.Filename)
	}

	tokens, err := database.ResolveAllTokens()
	assert.Nil(t, err)

	tokenIds := make([]primitive.TokenId, len(tokens))
	for i, token := range tokens {
		tokenIds[i] = token.Id
	}

	invertedIndex, err := database.ResolveInvertedIndex(tokenIds)
	assert.Nil(t, err)

	sort.Slice(tokens, func(i, j int) bool {
		return tokens[i].Term < tokens[j].Term
	})
	for _, token := range tokens {
		postingList, _ := invertedIndex.Get(token.Id)
		fmt.Fprintf(writer, "%#v: count = %d\n", token.Term, postingList.Count())
		postingList.Map(func(docId primitive.DocumentId, positions []int) error {
			doc, err := database.ResolveDocumentById(docId)
			if err != nil {
				return err
			}
			fmt.Fprintln(writer, doc.Filename, positions)
			return nil
		})
	}

	if isExists(".dump") {
		data, err := os.ReadFile(".dump")
		assert.Nil(t, err)
		assert.Equal(t, data, writer.Bytes())
		return
	}

	file, err := os.Create(".dump")
	assert.Nil(t, err)
	file.Write(writer.Bytes())
}
