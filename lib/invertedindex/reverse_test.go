package invertedindex

import (
	"testing"

	"github.com/stretchr/testify/require"
)

func Test_reverse(t *testing.T) {
	require.Equal(t, []int{1}, reverse([]int{1}))
	require.Equal(t, []int{2, 1}, reverse([]int{1, 2}))
	require.Equal(t, []int{3, 2, 1}, reverse([]int{1, 2, 3}))
	require.Equal(t, []int{4, 3, 2, 1}, reverse([]int{1, 2, 3, 4}))
	require.Equal(t, []int{5, 4, 3, 2, 1}, reverse([]int{1, 2, 3, 4, 5}))
}
