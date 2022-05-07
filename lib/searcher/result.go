package searcher

import "github.com/cxxxr/searty/lib/database"

type Result struct {
	doc   *database.Document
	start int
	end   int
}

func newResult(doc *database.Document, start, end int) *Result {
	return &Result{doc, start, end}
}
