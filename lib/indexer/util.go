package indexer

import (
	"log"
	"path/filepath"
)

func trimExt(file string) string {
	ext := filepath.Ext(file)
	return file[:len(file)-len(ext)]
}

func specToOutputName(specFile, baseDir, ext string) string {
	base := filepath.Base(specFile)
	abs, err := filepath.Abs(filepath.Join(baseDir, trimExt(base)+ext))
	if err != nil {
		log.Fatal(err)
	}
	return abs
}

func GetDatabaseFile(specFile, outputDir string) string {
	return specToOutputName(specFile, outputDir, ".sqlite3")
}
