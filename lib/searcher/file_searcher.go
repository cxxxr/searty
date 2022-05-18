package searcher

import (
	"fmt"
	"strings"

	"github.com/cxxxr/searty/lib/database"
)

type FileSearcher struct {
	db *database.Database
}

func NewFileSearcher(db *database.Database) *FileSearcher {
	return &FileSearcher{
		db: db,
	}
}

func (s *FileSearcher) Search(query string) ([]*Result, error) {
	docs, err := s.db.ResolveAllDocuments()
	if err != nil {
		return nil, err
	}

	for _, doc := range docs {
		if strings.Contains(doc.Filename, query) {
			fmt.Println(doc.Filename)
		}
	}

	return nil, nil
}
