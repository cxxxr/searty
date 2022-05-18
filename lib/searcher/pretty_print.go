package searcher

import (
	"fmt"
	"io"

	"github.com/cxxxr/searty/lib/database"
	"github.com/cxxxr/searty/lib/primitive"
)

func searchLineStartBackward(text string, pos int) int {
	for {
		pos--
		if pos == 0 {
			return 0
		}
		if text[pos] == '\n' {
			return pos + 1
		}
	}
}

func searchLineStartForward(text string, pos int) int {
	for {
		pos++
		if pos >= len(text) {
			return pos
		}
		if text[pos] == '\n' {
			return pos
		}
	}
}

func printMatchedLine(result *Result, text string, writer io.Writer) {
	lineStart := searchLineStartBackward(text, result.Start)
	lineEnd := searchLineStartForward(text, result.Start)
	fmt.Fprintf(writer,
		"%s:%d:%d:%s\n",
		result.Doc.Filename,
		result.Start,
		result.End,
		text[lineStart:lineEnd],
	)
}

func uniqueDocumentIds(results []*Result) []primitive.DocumentId {
	docIdMap := make(map[primitive.DocumentId]struct{}, 0)
	for _, result := range results {
		docIdMap[result.Doc.Id] = struct{}{}
	}

	ids := make([]primitive.DocumentId, 0, len(docIdMap))
	for id := range docIdMap {
		ids = append(ids, id)
	}
	return ids
}

func PrintResults(results []*Result, db *database.Database, writer io.Writer) error {
	if len(results) == 0 {
		return nil
	}

	docIds := uniqueDocumentIds(results)
	docIdTextMap := make(map[primitive.DocumentId]string, len(docIds))
	for _, docId := range docIds {
		text, err := db.ResolveDocumentWithBodyById(docId)
		if err != nil {
			return err
		}
		docIdTextMap[docId] = text
	}

	for _, result := range results {
		printMatchedLine(result, docIdTextMap[result.Doc.Id], writer)
	}

	return nil
}
