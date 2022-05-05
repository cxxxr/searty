package invertedindex

import (
	"bytes"
	"encoding/gob"

	"github.com/cxxxr/searty/lib/primitive"
	"github.com/pkg/errors"
)

type posting struct {
	documentId primitive.DocumentId
	positions  []int
	next       *posting
}

func newPosting(docId primitive.DocumentId, pos int, next *posting) *posting {
	return &posting{
		documentId: docId,
		positions:  []int{pos},
		next:       next,
	}
}

func (p *posting) GobEncode() ([]byte, error) {
	buf := bytes.NewBuffer(nil)
	type Alias posting
	_ = gob.NewEncoder(buf).Encode(struct {
		DocumentId primitive.DocumentId
		Positions  []int
		Next       *posting
	}{
		DocumentId: p.documentId,
		Positions:  p.positions,
		Next:       p.next,
	})
	return buf.Bytes(), nil
}

func (p *posting) GobDecode(data []byte) error {
	buf := bytes.NewBuffer(data)
	type Alias posting
	aux := struct {
		DocumentId primitive.DocumentId
		Positions  []int
		Next       *posting
	}{}
	_ = gob.NewDecoder(buf).Decode(&aux)
	p.documentId = aux.DocumentId
	p.positions = aux.Positions
	p.next = aux.Next
	return nil
}

type PostingList struct {
	head  *posting
	count int
}

func newPostingList() *PostingList {
	return &PostingList{head: nil}
}

func (p *PostingList) Count() int {
	return p.count
}

func (p *PostingList) GobEncode() ([]byte, error) {
	buf := bytes.NewBuffer(nil)
	type Alias PostingList
	_ = gob.NewEncoder(buf).Encode(struct {
		Head  *posting
		Count int
	}{
		Head:  p.head,
		Count: p.count,
	})
	return buf.Bytes(), nil
}

func (p *PostingList) GobDecode(data []byte) error {
	buf := bytes.NewBuffer(data)
	type Alias PostingList
	aux := struct {
		Head  *posting
		Count int
	}{}
	_ = gob.NewDecoder(buf).Decode(&aux)
	p.head = aux.Head
	p.count = aux.Count
	return nil
}

func (p *PostingList) insert(pos int, docId primitive.DocumentId) {
	node := &p.head

	for *node != nil {
		current := *node

		if current.documentId == docId {
			current.positions = append(current.positions, pos)
			return
		}
		if current.documentId < docId {
			p.count++
			posting := newPosting(docId, pos, current)
			*node = posting
			return
		}

		node = &current.next
	}

	p.count++
	(*node) = newPosting(docId, pos, nil)
}

func (p *PostingList) Map(fn func(primitive.DocumentId, []int) error) error {
	if p == nil {
		return nil
	}
	current := p.head
	for current != nil {
		if err := fn(current.documentId, current.positions); err != nil {
			return err
		}
		current = current.next
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
