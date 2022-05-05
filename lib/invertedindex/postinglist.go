package invertedindex

import (
	"bytes"
	"encoding/gob"

	"github.com/cxxxr/searty/lib/entity"
	"github.com/pkg/errors"
)

type posting struct {
	DocumentId entity.DocumentId
	Positions  []int
	Next       *posting // TODO: exportしたくない
}

func newPosting(docId entity.DocumentId, pos int, next *posting) *posting {
	return &posting{
		DocumentId: docId,
		Positions:  []int{pos},
		Next:       next,
	}
}

type PostingList struct {
	Head  *posting // TODO: exportしたくない
	Count int      // TODO: exportしたくない
}

func newPostingList() *PostingList {
	return &PostingList{Head: nil}
}

func (p *PostingList) insert(pos int, docId entity.DocumentId) {
	node := &p.Head

	for *node != nil {
		current := *node

		if current.DocumentId == docId {
			current.Positions = append(current.Positions, pos)
			return
		}
		if current.DocumentId < docId {
			p.Count++
			posting := newPosting(docId, pos, current)
			*node = posting
			return
		}

		node = &current.Next
	}

	p.Count++
	(*node) = newPosting(docId, pos, nil)
}

func (p *PostingList) Map(fn func(entity.DocumentId, []int) error) error {
	if p == nil {
		return nil
	}
	current := p.Head
	for current != nil {
		if err := fn(current.DocumentId, current.Positions); err != nil {
			return err
		}
		current = current.Next
	}
	return nil
}

func encode(p *PostingList) ([]byte, error) {
	writer := bytes.NewBuffer(nil)
	encoder := gob.NewEncoder(writer)
	if err := encoder.Encode(p); err != nil {
		return nil, errors.WithStack(err)
	}
	return writer.Bytes(), nil
}

func DecodePostingList(blob []byte) (*PostingList, error) {
	var postinglist PostingList

	decoder := gob.NewDecoder(bytes.NewReader(blob))
	if err := decoder.Decode(&postinglist); err != nil {
		return nil, err
	}

	return &postinglist, nil
}
