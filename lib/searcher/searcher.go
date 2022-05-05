package searcher

import (
	"github.com/cxxxr/searty/lib/database"
	"github.com/cxxxr/searty/lib/invertedindex"
	"github.com/cxxxr/searty/lib/primitive"
	"github.com/cxxxr/searty/lib/tokenizer"
)

type Searcher struct {
	tokenizer *tokenizer.Tokenizer
	database *database.Database
}

func New() *Searcher {
	return &Searcher{}
}

func makeTokenIds(tokens []database.Token) []primitive.TokenId {
	tokenIds := make([]primitive.TokenId, len(tokens))
	for i, token := range tokens {
		tokenIds[i] = token.Id
	}
	return tokenIds
}

type postingSlice []*invertedindex.Posting

func makePostingSlice(invertedIndex invertedindex.InvertedIndex) postingSlice {
	make([]*invertedindex.Posting, invertedIndex.Length())
}

func (s *Searcher) Search(query string) ([]Result, error) {
	terms := s.tokenizer.Tokenize(query)
	tokens, err := s.database.ResolveTokensByTerms(terms)
	if err != nil {
		return nil, err
	}

	if len(terms) != len(tokens) {
		return nil, nil
	}

	invertedIndex, err := s.database.ResolveInvertedIndex(makeTokenIds(tokens))
	if err != nil {
		return nil, err
	}

	postings := make([]*invertedindex.Posting, invertedIndex.Length())
	for findFinishPostingList(postings) {
	}
}

func findFinishPostingList(postingLists postingLists) bool {
}
