package invertedindex

import (
	"fmt"

	"github.com/cxxxr/searty/lib/primitive"
)

type InvertedIndex struct {
	table map[primitive.TokenId]*PostingList
}

func New() *InvertedIndex {
	return &InvertedIndex{
		table: make(map[primitive.TokenId]*PostingList),
	}
}

func (index *InvertedIndex) Set(tokenId primitive.TokenId, postingList *PostingList) {
	index.table[tokenId] = postingList
}

func (index *InvertedIndex) Get(tokenId primitive.TokenId) *PostingList {
	postingList, ok := index.table[tokenId]
	if !ok {
		return nil
	}
	return postingList
}

func (index *InvertedIndex) Length() int {
	return len(index.table)
}

func (index *InvertedIndex) Insert(tokenId primitive.TokenId, docId primitive.DocumentId, pos int) {
	postinglist, ok := index.table[tokenId]
	if !ok {
		postinglist = newPostingList()
		index.table[tokenId] = postinglist
	}
	postinglist.insert(pos, docId)
}

func (index *InvertedIndex) TokenIds() []primitive.TokenId {
	ids := make([]primitive.TokenId, 0)
	for tokenId := range index.table {
		ids = append(ids, tokenId)
	}
	return ids
}

func (index *InvertedIndex) EncodePostingList(tokenId primitive.TokenId) ([]byte, error) {
	postinglist, ok := index.table[tokenId]
	if !ok {
		return nil, fmt.Errorf("%v not found", tokenId)
	}
	return encode(postinglist)
}
