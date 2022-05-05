package searcher

import "github.com/cxxxr/searty/lib/tokenizer"

type Searcher struct {
	tokenizer *tokenizer.Tokenizer
}

func New() *Searcher {
	return &Searcher{}
}

func (s *Searcher) Search(query string) {
	terms := s.tokenizer.Tokenize(query)
	
}
