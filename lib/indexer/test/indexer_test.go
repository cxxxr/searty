package indexer

import (
	"bytes"
	"fmt"
	"os"
	"sort"
	"testing"

	"github.com/cxxxr/searty/lib/database"
	"github.com/cxxxr/searty/lib/invertedindex"
	"github.com/cxxxr/searty/lib/primitive"
	"github.com/cxxxr/searty/lib/testutil"
	"github.com/stretchr/testify/require"
)

func isExists(filename string) bool {
	_, err := os.Stat(filename)
	return err == nil
}

func resolveInvertedIndex(
	t *testing.T,
	database *database.Database,
	tokens []*database.Token,
) *invertedindex.InvertedIndex {
	tokenIds := make([]primitive.TokenId, len(tokens))
	for i, token := range tokens {
		tokenIds[i] = token.Id
	}

	invertedIndex, err := database.ResolveInvertedIndex(tokenIds)
	require.Nil(t, err)
	return invertedIndex
}

func resolveAllTokens(t *testing.T, database *database.Database) []*database.Token {
	tokens, err := database.ResolveAllTokens()
	require.Nil(t, err)

	sort.Slice(tokens, func(i, j int) bool {
		return tokens[i].Term < tokens[j].Term
	})

	return tokens
}

func Test_index(t *testing.T) {
	databaseFile := testutil.DoIndex(t, "testdata/cl-ppcre.json")

	// prepare database connection
	database := database.New(databaseFile)
	require.Nil(t, database.Connect())

	writer := bytes.NewBuffer(nil)

	// snapshot documents
	docs, err := database.ResolveAllDocuments()
	require.Nil(t, err)
	for _, doc := range docs {
		fmt.Fprintln(writer, doc.Filename)
	}

	// snapshot inverted index
	tokens := resolveAllTokens(t, database)
	invertedIndex := resolveInvertedIndex(t, database, tokens)
	for _, token := range tokens {
		postingList := invertedIndex.Get(token.Id)
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

	// snapshot test
	if isExists(".snapshot") {
		data, err := os.ReadFile(".snapshot")
		require.Nil(t, err)
		require.Equal(t, data, writer.Bytes())
		return
	}

	file, err := os.Create(".snapshot")
	require.Nil(t, err)
	file.Write(writer.Bytes())
}
