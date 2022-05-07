package testutil

import (
	"os"
	"testing"

	"github.com/stretchr/testify/require"
)

func isExists(filename string) bool {
	_, err := os.Stat(filename)
	return err == nil
}

func Snapshot(t *testing.T, data []byte, snapshotFile string) {
	if isExists(snapshotFile) {
		cached, err := os.ReadFile(snapshotFile)
		require.Nil(t, err)
		require.Equal(t, cached, data)
		return
	}

	file, err := os.Create(snapshotFile)
	require.Nil(t, err)
	file.Write(data)
}
