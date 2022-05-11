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
		// {
		// 	expected: []string{"\x00a\x00"},
		// 	input:    "a",
		// },
		// {
		// 	expected: []string{"\x00ab", "ab\x00"},
		// 	input:    "ab",
		// },
		// {
		// 	expected: []string{"\x00ab", "abc", "bc\x00"},
		// 	input:    "abc",
		// },
	}

	for _, tc := range testcases {
		assert.Equal(t, tc.expected, New().Tokenize(tc.input))
	}
}
