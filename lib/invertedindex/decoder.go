package invertedindex

import "github.com/cxxxr/searty/lib/primitive"

type decoder struct {
	buf []byte
	pos int
}

func newDecoder(blob []byte) *decoder {
	d := new(decoder)
	d.buf = blob
	d.pos = 0
	return d
}

func (dec *decoder) decodeUint() int {
	v := 0
	for {
		b := int(dec.buf[dec.pos])
		dec.pos++
		v <<= 7
		if (b >> 7) == 1 {
			v |= (b ^ 128)
		} else {
			v |= b
			break
		}
	}
	return v
}

func (dec *decoder) decodeUints() []int {
	n := dec.decodeUint()
	vs := make([]int, n)
	for i := 0; i < n; i++ {
		vs[i] = dec.decodeUint()
	}
	return vs
}

func (dec *decoder) decodePosting() *Posting {
	docId := primitive.DocumentId(dec.decodeUint())
	positions := dec.decodeUints()
	return NewPosting(docId, positions)
}

func (dec *decoder) DecodePostingList() *PostingList {
	count := dec.decodeUint()
	postinglist := newPostingList()
	for i := 0; i < count; i++ {
		postinglist.push(dec.decodePosting())
	}
	return postinglist
}
