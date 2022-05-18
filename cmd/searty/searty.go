package main

import (
	"bytes"
	"flag"
	"fmt"
	"log"
	"os"

	"github.com/cxxxr/searty/lib/database"
	"github.com/cxxxr/searty/lib/searcher"
)

var file string
var searchKind string

const (
	phraseSearch = "phrase"
	fileSearch = "file"
	definitionSearch = "definition"
)

func init() {
	flag.StringVar(&file, "d", "", "database file")
	flag.StringVar(&searchKind, "k", phraseSearch, "kind")
}

func newSearcher(kind string, db *database.Database) searcher.Searcher {
	switch kind {
	case phraseSearch:
		return searcher.NewPhraseSearcher(db)
	case fileSearch:
		panic("unimplemented")
	case definitionSearch:
		return searcher.NewSymbolSearcher(db)
	default:
		return nil
	}
}

func main() {
	flag.Parse()
	flag.Args()
	query := flag.Arg(0)

	if file == "" {
		flag.Usage()
		return
	}

	if _, err := os.Stat(file); err != nil {
		log.Printf("%s not found", file)
		return
	}

	if query == "" {
		return
	}

	db := database.New(file)
	if err := db.Connect(); err != nil {
		log.Fatal(err)
	}
	defer db.Close()

	results, err := newSearcher(searchKind, db).Search(query)
	if err != nil {
		log.Fatalf("%+v\n", err)
	}

	writer := bytes.NewBuffer(nil)
	if err := searcher.PrintResults(results, db, writer); err != nil {
		log.Fatalf("%+v\n", err)
	}

	fmt.Print(string(writer.Bytes()))
}
