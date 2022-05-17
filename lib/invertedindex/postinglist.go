package invertedindex

import (
	"github.com/cxxxr/searty/lib/primitive"
	"github.com/pkg/errors"
)

type Posting struct {
	documentId primitive.DocumentId
	positions  []int
	next       *Posting
}

func NewPosting(docId primitive.DocumentId, positions []int) *Posting {
	return &Posting{
		documentId: docId,
		positions:  positions,
		next:       nil,
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

func (p *PostingList) insert(newNode *Posting) {
	node := &p.head

	for *node != nil {
		current := *node

		if current.documentId == newNode.documentId {
			// これもソートされている必要があるが昇順にinsertされるので問題になってない
			current.positions = append(current.positions, newNode.positions...)
			return
		}
		if newNode.documentId < current.documentId {
			p.count++
			newNode.next = current
			*node = newNode
			return
		}

		node = &current.next
	}

	p.count++
	(*node) = newNode
}

func (p *PostingList) push(newNode *Posting) {
	newNode.next = p.head
	p.head = newNode
	p.count++
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

func (p *PostingList) Encode(isReverse bool) []byte {
	enc := newEncoder()
	enc.EncodePostingList(p, isReverse)
	return enc.Bytes()
}

func DecodePostingList(blob []byte) *PostingList {
	return newDecoder(blob).DecodePostingList()
}
