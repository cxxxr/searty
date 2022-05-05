package searcher

import (
	"github.com/cxxxr/searty/lib/database"
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

func (s *Searcher) Search(query string) error {
	terms := s.tokenizer.Tokenize(query)
	tokens, err := s.database.ResolveTokensByTerms(terms)
	if err != nil {
		return err
	}

	invertedIndex, err := s.database.ResolveInvertedIndex(makeTokenIds(tokens))
	if err != nil {
		return err
	}

	var _ = invertedIndex
	return nil
}
