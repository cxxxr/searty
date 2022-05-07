package searcher

import (
	"sort"

	"github.com/cxxxr/searty/lib/database"
	"github.com/cxxxr/searty/lib/invertedindex"
	"github.com/cxxxr/searty/lib/primitive"
	"github.com/cxxxr/searty/lib/tokenizer"
)

type PhraseSearcher struct {
	tokenizer *tokenizer.Tokenizer
	database  *database.Database
}

func NewPhraseSearcher(tokenizer *tokenizer.Tokenizer, database *database.Database) *PhraseSearcher {
	return &PhraseSearcher{tokenizer: tokenizer, database: database}
}

func makeTokenIds(tokens []*database.Token) []primitive.TokenId {
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
	minIndex := 0
	for i, posting := range postings[1:] {
		if minDoc.DocumentId() > posting.DocumentId() {
			minDoc = posting
			minIndex = i + 1
		}
	}
	postings[minIndex] = postings[minIndex].Next()
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
	intersection := make(positions, 0)
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

type resultsPerDocMap map[primitive.DocumentId][]*Result

func extractMatched(acc resultsPerDocMap, postings postingSlice, positions positions) {
	for offset, posting := range postings {
		for _, pos := range positions {
			r := newResult(
				&database.Document{Id: posting.DocumentId()},
				pos+offset,
				pos+offset+3,
			)
			k := posting.DocumentId()
			acc[k] = append(acc[k], r)
		}
	}
}

func resultDocIds(results []*Result) []primitive.DocumentId {
	ids := make([]primitive.DocumentId, len(results))
	for i, result := range results {
		ids[i] = result.doc.Id
	}
	return ids
}

func resolveResultDocument(results []*Result, db *database.Database) ([]*Result, error) {
	ids := resultDocIds(results)

	// REVIEW: 現状は含めていないがbodyを含めるべきか?
	docs, err := db.ResolveDocumentsByIds(ids)
	if err != nil {
		return nil, err
	}

	docMap := make(map[primitive.DocumentId]*database.Document, 0)
	for _, doc := range docs {
		docMap[doc.Id] = doc
	}

	for i, result := range results {
		results[i].doc = docMap[result.doc.Id]
	}

	return results, nil
}

func mergeRanges(results []*Result) []*Result {
	cursor := 0

	for cursor < len(results)-1 {
		current := results[cursor]
		next := results[cursor+1]
		if current.start <= next.start && next.start <= current.end {
			current.end = next.end
			results[cursor+1].start = current.start
			results[cursor] = nil
			cursor++
		} else {
			cursor++
		}
	}

	var acc []*Result
	for _, result := range results {
		if result != nil {
			acc = append(acc, result)
		}
	}
	return acc
}

func mergeRangesForResultsPerDoc(resultsPerDoc resultsPerDocMap) {
	for docId, results := range resultsPerDoc {
		sort.Slice(results, func(i, j int) bool {
			return results[i].start < results[j].start
		})
		resultsPerDoc[docId] = mergeRanges(results)
	}
}

func convertResultsPerDocToResults(resultsPerDoc resultsPerDocMap) []*Result {
	var acc []*Result
	for _, results := range resultsPerDoc {
		acc = append(acc, results...)
	}
	return acc
}

func (s *PhraseSearcher) Search(query string) ([]*Result, error) {
	// TODO: ngramのトークンをphrase検索するときにn-1文字ずつ探す

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

	resultsPerDoc := make(resultsPerDocMap, 0)
	postings := makePostings(invertedIndex, tokenIds)

	for !findFinishPosting(postings) {
		if isEverythingSameDocument(postings) {
			if positions, ok := matchPhrase(postings); ok {
				extractMatched(resultsPerDoc, postings, positions)
			}
			nextEachPosting(postings)
		} else {
			nextMinimumPosting(postings)
		}
	}

	mergeRangesForResultsPerDoc(resultsPerDoc)
	results := convertResultsPerDocToResults(resultsPerDoc)
	return resolveResultDocument(results, s.database)
}
