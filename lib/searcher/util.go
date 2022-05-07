package searcher

import "github.com/cxxxr/searty/lib/primitive"

func resultDocIds(results []*Result) []primitive.DocumentId {
	ids := make([]primitive.DocumentId, len(results))
	for i, result := range results {
		ids[i] = result.doc.Id
	}
	return ids
}
