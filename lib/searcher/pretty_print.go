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
	lineStart := searchLineStartBackward(text, result.start)
	lineEnd := searchLineStartForward(text, result.start)
	fmt.Fprintf(writer,
		"%s:%d:%d:%s\n",
		result.doc.Filename,
		result.start,
		result.end,
		text[lineStart:lineEnd],
	)
}

func PrintResults(results []*Result, db *database.Database, writer io.Writer) error {
	if len(results) == 0 {
		return nil
	}

	docIds := make([]primitive.DocumentId, 0, len(results))
	for _, result := range results {
		docIds = append(docIds, result.doc.Id)
	}

	docs, err := db.ResolveDocumentsWithBodyByIds(docIds)
	if err != nil {
		return err
	}

	docIdTextMap := make(map[primitive.DocumentId]string, len(docs))
	for _, doc := range docs {
		docIdTextMap[doc.Id] = doc.Body
	}

	for _, result := range results {
		printMatchedLine(result, docIdTextMap[result.doc.Id], writer)
	}

	return nil
}
