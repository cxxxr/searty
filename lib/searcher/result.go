package searcher

import "github.com/cxxxr/searty/lib/database"

type metadata map[string]string

type Result struct {
	Doc      *database.Document
	Start    int
	End      int
	Metadata metadata
}

func newResult(doc *database.Document, start, end int, metadata metadata) *Result {
	return &Result{doc, start, end, metadata}
}
