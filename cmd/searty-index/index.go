package main

import (
	"flag"
	"log"

	"github.com/cxxxr/searty/lib/indexer"
)

var outputDir string

func init() {
	const usage = "Output distination directory"
	flag.StringVar(&outputDir, "output-dir", "", usage)
	flag.StringVar(&outputDir, "o", ".", usage)
}

func main() {
	flag.Parse()
	specFile := flag.Arg(0)

	log.Printf("outputDir: %s\n", outputDir)

	err := indexer.NewIndexer().Index(specFile, indexer.GetDatabaseFile(specFile, outputDir))
	if err != nil {
		log.Fatal(err)
	}
}
