package indexer

import (
	"fmt"

	"github.com/cxxxr/searty/lib/database"
	"github.com/cxxxr/searty/lib/invertedindex"
	"github.com/cxxxr/searty/lib/primitive"
)

func DescribeInvertedIndex(dbFile string) {
	db := database.New(dbFile)
	if err := db.Connect(); err != nil {
		panic(err)
	}
	defer db.Close()

	inverted, _ := db.ResolveWholeInvertedIndex()
	inverted.Map(func(tokId primitive.TokenId, postinglist *invertedindex.PostingList) error {
		tok, _ := db.ResolveTokenById(tokId)
		fmt.Printf("--- %s %#v\n", tok.Id, tok.Term)
		postinglist.Map(func(docId primitive.DocumentId, positions []int) error {
			doc, _ := db.ResolveDocumentById(docId)
			fmt.Println(doc.Id, doc.Filename, positions)
			return nil
		})
		return nil
	})
}
