package invertedindex

type encoder struct {
	buf []byte
}

func newEncoder() *encoder{
	return &encoder{}
}

func (enc *encoder) append(b byte) {
	enc.buf = append(enc.buf, b)
}

func (enc *encoder) EncodeUint(v int) {
	vs := make([]byte, 0)
	vs = append(vs, byte(v & 0x7f))
	for {
		v >>= 7
		if v == 0 {
			break
		}
		vs = append(vs, byte((v & 0x7f) + 0x80))
	}
	enc.buf = append(enc.buf, reverse(vs)...)
}

func (enc *encoder) EncodeUints(vs []int) {
	enc.EncodeUint(len(vs))
	for _, v := range vs {
		enc.EncodeUint(v)
	}
}

func (enc *encoder) EncodePosting(posting *Posting) {
	enc.EncodeUint(int(posting.documentId))
	enc.EncodeUints(posting.positions)
}

func reversePostingList(postinglist *PostingList) *PostingList {
	postinglist2 := newPostingList()
	p := postinglist.head
	for p != nil {
		// pushでp.nextが変更されるので一時変数nextに退避する
		next := p.next
		postinglist2.push(p)
		p = next
	}
	return postinglist2
}

func (enc *encoder) EncodePostingList(postinglist *PostingList, isReverse bool) {
	enc.EncodeUint(postinglist.count)

	if isReverse {
		// 既に逆順なので何もしない
	} else {
		// 各postingを逆順にencodeしている, decodeで順にpushしていけば元の順番に戻るため
		postinglist = reversePostingList(postinglist)
	}

	for p := postinglist.head; p != nil; p = p.next {
		enc.EncodePosting(p)
	}
}

func (enc *encoder) Bytes() []byte {
	return enc.buf
}

func reverse[T any](vs []T) []T {
	size := len(vs)
	for i := 0; i < size / 2; i++ {
		vs[i], vs[size - i - 1] = vs[size - i - 1], vs[i]
	}
	return vs
}
