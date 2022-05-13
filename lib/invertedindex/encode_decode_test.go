package invertedindex

import (
	"testing"

	"github.com/cxxxr/searty/lib/primitive"
	"github.com/stretchr/testify/require"
)

func Test_EncodeDecodeUint(t *testing.T) {
	cases := []struct {
		decoded int
		encoded []byte
	}{
		{
			decoded: 0,
			encoded: []byte{0},
		},
		{
			decoded: 1,
			encoded: []byte{1},
		},
		{
			decoded: 12,
			encoded: []byte{12},
		},
		{
			decoded: 127,
			encoded: []byte{127},
		},
		{
			decoded: 128,
			encoded: []byte{129, 0},
		},
		{
			decoded: 12345,
			encoded: []byte{224, 57},
		},
		{
			decoded: 12345678,
			encoded: []byte{133, 241, 194, 78},
		},
	}

	for _, tc := range cases {
		enc := newEncoder()
		enc.EncodeUint(tc.decoded)
		data := enc.Bytes()
		require.Equal(t, tc.encoded, data)
	}

	for _, tc := range cases {
		dec := newDecoder(tc.encoded)
		actual := dec.decodeUint()
		require.Equal(t, tc.decoded, actual)
	}
}

func Test_EncodePostingList(t *testing.T) {
	postinglist := newPostingList()
	postinglist.insert(NewPosting(primitive.DocumentId(1), []int{10, 20}))
	postinglist.insert(NewPosting(primitive.DocumentId(2), []int{3, 5, 8}))

	enc := newEncoder()
	enc.EncodePostingList(postinglist)
	data := enc.Bytes()

	postinglist = newDecoder(data).DecodePostingList()

	require.Equal(t, postinglist.count, postinglist.count)

	p := postinglist.head

	require.Equal(t, primitive.DocumentId(1), p.documentId)
	require.Equal(t, []int{10, 20}, p.positions)
	p = p.next

	require.Equal(t, primitive.DocumentId(2), p.documentId)
	require.Equal(t, []int{3, 5, 8}, p.positions)
	p = p.next

	require.Nil(t, p)
}
