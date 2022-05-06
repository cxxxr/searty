package searcher

import (
	"log"

	"github.com/cxxxr/searty/lib/database"
	"github.com/cxxxr/searty/lib/invertedindex"
	"github.com/cxxxr/searty/lib/primitive"
	"github.com/cxxxr/searty/lib/tokenizer"
)

type Searcher struct {
	tokenizer *tokenizer.Tokenizer
	database  *database.Database
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

func makePostings(ii *invertedindex.InvertedIndex, tokenIds []primitive.TokenId) postingSlice {
	postings := make([]*invertedindex.Posting, ii.Length())
	for i, tokenId := range tokenIds {
		postings[i] = ii.Get(tokenId).Posting()
	}
	return postings
}

func findFinishPosting(postings postingSlice) bool {
	for _, p := range postings {
		if p == nil {
			return true
		}
	}
	return false
}

func isEverythingSameDocument(postings postingSlice) bool {
	id := postings[0].DocumentId()
	for _, p := range postings[1:] {
		if p.DocumentId() != id {
			return false
		}
	}
	return true
}

func nextEachPosting(postings postingSlice) {
	for i, posting := range postings {
		postings[i] = posting.Next()
	}
}

func nextMinimumPosting(postings postingSlice) {
	minDoc := postings[0]
	MinIndex := 0
	for i, posting := range postings[1:] {
		if minDoc.DocumentId() > posting.DocumentId() {
			minDoc = posting
			MinIndex = i
		}
	}
	postings[MinIndex] = postings[MinIndex].Next()
}

type positions []int
type positionsSet []positions

func computeRelativePositions(posting *invertedindex.Posting, offset int) positions {
	absolutePositions := posting.Positions()
	relativePositions := make(positions, len(absolutePositions))
	for i, pos := range absolutePositions {
		relativePositions[i] = pos - offset
	}
	return relativePositions
}

func computeRelativePositionsSet(postings postingSlice) positionsSet {
	relativePositionsSet := make(positionsSet, len(postings))
	for offset, posting := range postings {
		relativePositionsSet[offset] = computeRelativePositions(posting, offset)
	}
	return relativePositionsSet
}

func intersectionPositionsSet(set positionsSet) positions {
	m := make(map[int]int, 0)
	for _, positions := range set {
		for _, pos := range positions {
			m[pos]++
		}
	}
	intersection := make(positions, len(m))
	for pos, count := range m {
		if count == len(set) {
			intersection = append(intersection, pos)
		}
	}
	return intersection
}

func matchPhrase(postings postingSlice) (positions, bool) {
	positions := intersectionPositionsSet(computeRelativePositionsSet(postings))
	if len(positions) == 0 {
		return nil, false
	}

	return positions, true
}

type result struct {
	docId primitive.DocumentId
	start int
	end   int
}

func extractMatched(postings postingSlice, positions positions) *result {
	// assert
	{
		pos0 := positions[0]
		for i, pos := range positions[1:] {
			if pos0 != pos+i {
				panic("assertion error")
			}
		}
	}

	docId := postings[0].DocumentId()
	start := positions[0]
	end := positions[len(positions)-1]
	return &result{docId, start, end}
}

func convertResults(results []*result, database *database.Database) ([]*Result, error) {
	ids := make([]primitive.DocumentId, len(results))
	for i, result := range results {
		ids[i] = result.docId
	}

	// REVIEW: 現状は含めていないがbodyを含めるべきか?
	docs, err := database.ResolveDocumentsByIds(ids)
	if err != nil {
		return nil, err
	}

	convertedResults := make([]*Result, len(results))
	for i, result := range results {
		convertedResults[i] = newResult(docs[i].Filename, result.start, result.end)
	}

	return convertedResults, nil
}

func (s *Searcher) Search(query string) ([]*Result, error) {
	terms := s.tokenizer.Tokenize(query)
	tokens, err := s.database.ResolveTokensByTerms(terms)
	if err != nil {
		return nil, err
	}

	if len(terms) != len(tokens) {
		return nil, nil
	}

	tokenIds := makeTokenIds(tokens)
	invertedIndex, err := s.database.ResolveInvertedIndex(tokenIds)
	if err != nil {
		return nil, err
	}

	results := make([]*result, 0)

	postings := makePostings(invertedIndex, tokenIds)
	for !findFinishPosting(postings) {
		if isEverythingSameDocument(postings) {
			if positions, ok := matchPhrase(postings); ok {
				result := extractMatched(postings, positions)
				results = append(results, result)
			}
			nextEachPosting(postings)
		} else {
			nextMinimumPosting(postings)
		}
	}

	return convertResults(results, s.database)
}
