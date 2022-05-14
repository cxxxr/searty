package tokenizer

import (
	"github.com/stretchr/testify/assert"
	"testing"
)

func Test_Tokenize(t *testing.T) {
	type tc struct {
		expected []string
		input    string
	}

	testcases := []tc{
		{
			expected: []string{"abc", "bcd", "cde", "def", "efg"},
			input:    "abcdefg",
		},
		{
			expected: []string{"あいう", "いうえ", "うえお"},
			input:    "あいうえお",
		},
	}

	for _, tc := range testcases {
		assert.Equal(t, tc.expected, New().Tokenize(tc.input))
	}
}
