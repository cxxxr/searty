package invertedindex

import (
	"fmt"

	"github.com/cxxxr/searty/lib/entity"
)

type InvertedIndex struct {
	table map[entity.TokenId]*PostingList
}

func New() *InvertedIndex {
	return &InvertedIndex{
		table: make(map[entity.TokenId]*PostingList),
	}
}

func (index *InvertedIndex) Insert(tokenId entity.TokenId, document *entity.Document, pos int) {
	postinglist, ok := index.table[tokenId]
	if !ok {
		postinglist = newPostingList()
		index.table[tokenId] = postinglist
	}
	postinglist.insert(pos, document.Id)
}

func (index *InvertedIndex) TokenIds() []entity.TokenId {
	ids := make([]entity.TokenId, 0)
	for tokenId := range index.table {
		ids = append(ids, tokenId)
	}
	return ids
}

func (index *InvertedIndex) EncodePostingList(tokenId entity.TokenId) ([]byte, error) {
	postinglist, ok := index.table[tokenId]
	if !ok {
		return nil, fmt.Errorf("%v not found", tokenId)
	}
	return encode(postinglist)
}
