package invertedindex

import (
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

func (index *InvertedIndex) Insert(tokenId primitive.TokenId, value *Posting) {
	postinglist, ok := index.table[tokenId]
	if !ok {
		postinglist = newPostingList()
		index.table[tokenId] = postinglist
	}
	postinglist.insert(value)
}

func (index *InvertedIndex) Push(tokenId primitive.TokenId, value *Posting) {
	postinglist, ok := index.table[tokenId]
	if !ok {
		postinglist = newPostingList()
		index.table[tokenId] = postinglist
	}
	postinglist.push(value)
}

func (index *InvertedIndex) TokenIds() []primitive.TokenId {
	ids := make([]primitive.TokenId, 0)
	for tokenId := range index.table {
		ids = append(ids, tokenId)
	}
	return ids
}

func (index *InvertedIndex) Map(fn func(id primitive.TokenId, list *PostingList) error) error {
	for id, list := range index.table {
		err := fn(id, list)
		if err != nil {
			return err
		}
	}
	return nil
}
