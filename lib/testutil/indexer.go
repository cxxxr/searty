package testutil

import (
	"os"
	"testing"

	"github.com/cxxxr/searty/lib/indexer"
	"github.com/stretchr/testify/require"
)

func createTestingDatabaseFile(t *testing.T) string {
	databaseFile, err := os.CreateTemp("", "searty.sqlite3.*")
	require.Nil(t, err)
	defer databaseFile.Close()
	return databaseFile.Name()
}

func DoIndex(t *testing.T, specFile string) string {
	databaseFile := createTestingDatabaseFile(t)
	err := indexer.NewIndexer().Index(specFile, databaseFile)
	require.Nil(t, err)
	return databaseFile
}
