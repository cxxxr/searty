package searcher

import "github.com/cxxxr/searty/lib/database"

type FileSearcher struct {
	db *database.Database
}

func NewFileSearcher(db *database.Database) *FileSearcher {
	return &FileSearcher{
		db: db,
	}
}

func (s *FileSearcher) Search(query string) ([]*Result, error) {
}
