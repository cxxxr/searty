package invertedindex

import (
	"bytes"
	"encoding/gob"

	"github.com/cxxxr/searty/lib/primitive"
	"github.com/pkg/errors"
)

type Posting struct {
	documentId primitive.DocumentId
	positions  []int
	next       *Posting
}

func newPosting(docId primitive.DocumentId, pos int, next *Posting) *Posting {
	return &Posting{
		documentId: docId,
		positions:  []int{pos},
		next:       next,
	}
}

func (p *Posting) DocumentId() primitive.DocumentId {
	return p.documentId
}

func (p *Posting) Next() *Posting {
	return p.next
}

func (p *Posting) Positions() []int {
	return p.positions
}

func (p *Posting) GobEncode() ([]byte, error) {
	buf := bytes.NewBuffer(nil)
	type Alias Posting
	_ = gob.NewEncoder(buf).Encode(struct {
		DocumentId primitive.DocumentId
		Positions  []int
		Next       *Posting
	}{
		DocumentId: p.documentId,
		Positions:  p.positions,
		Next:       p.next,
	})
	return buf.Bytes(), nil
}

func (p *Posting) GobDecode(data []byte) error {
	buf := bytes.NewBuffer(data)
	type Alias Posting
	aux := struct {
		DocumentId primitive.DocumentId
		Positions  []int
		Next       *Posting
	}{}
	_ = gob.NewDecoder(buf).Decode(&aux)
	p.documentId = aux.DocumentId
	p.positions = aux.Positions
	p.next = aux.Next
	return nil
}

type PostingList struct {
	head  *Posting
	count int
}

func newPostingList() *PostingList {
	return &PostingList{head: nil}
}

func (p *PostingList) Count() int {
	return p.count
}

func (p *PostingList) Posting() *Posting {
	return p.head
}

func (p *PostingList) GobEncode() ([]byte, error) {
	buf := bytes.NewBuffer(nil)
	type Alias PostingList
	_ = gob.NewEncoder(buf).Encode(struct {
		Head  *Posting
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
		Head  *Posting
		Count int
	}{}
	_ = gob.NewDecoder(buf).Decode(&aux)
	p.head = aux.Head
	p.count = aux.Count
	return nil
}

func (p *PostingList) insert(docId primitive.DocumentId, pos int) {
	node := &p.head

	for *node != nil {
		current := *node

		if current.documentId == docId {
			// これもソートされている必要があるが昇順にinsertされるので問題になってない
			current.positions = append(current.positions, pos)
			return
		}
		if docId < current.documentId {
			p.count++
			*node = newPosting(docId, pos, current)
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

func (p *PostingList) CheckCorruption() error {
	prevId := primitive.DocumentId(0)

	return p.Map(func(id primitive.DocumentId, _ []int) error {
		if prevId > id {
			return errors.New("assertion error")
		}
		prevId = id
		return nil
	})
}

func (p *PostingList) Encode() ([]byte, error) {
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
