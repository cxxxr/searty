package main

import (
	"flag"
	"log"

	"github.com/cxxxr/searty/lib/indexer"
)

var outputFile string

func init() {
	const usage = "Output database file"
	const value = "searty.db"
	flag.StringVar(&outputFile, "output", value, usage)
	flag.StringVar(&outputFile, "o", value, usage)
}

func main() {
	flag.Parse()
	args := flag.Args()

	err := indexer.MergeDatabases(args, outputFile)
	if err != nil {
		log.Fatalf("%+v", err)
	}
}
