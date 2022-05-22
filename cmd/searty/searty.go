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

var dbFile string

var (
	fileQuery   string
	symbolQuery string
)

func init() {
	flag.StringVar(&dbFile, "d", "", "database file")

	flag.StringVar(&fileQuery, "file", "", "")
	flag.StringVar(&symbolQuery, "symbol", "", "")
}

func main() {
	flag.Parse()

	if dbFile == "" {
		flag.Usage()
		return
	}

	if _, err := os.Stat(dbFile); err != nil {
		log.Printf("%s not found", dbFile)
		return
	}

	db := database.New(dbFile)
	if err := db.Connect(); err != nil {
		log.Fatal(err)
	}
	defer db.Close()

	results := make([]*searcher.Result, 0)

	if fileQuery != "" {
		// TODO
	}

	if symbolQuery != "" {
		results1, err := searcher.NewSymbolSearcher(db).Search(symbolQuery)
		if err != nil {
			log.Fatalf("%+v\n", err)
		}
		results = append(results, results1...)
	}

	for _, phraseQuery := range flag.Args() {
		results1, err := searcher.NewPhraseSearcher(db).Search(phraseQuery)
		if err != nil {
			log.Fatalf("%+v\n", err)
		}
		results = append(results, results1...)
	}

	writer := bytes.NewBuffer(nil)
	if err := searcher.PrintResults(results, db, writer); err != nil {
		log.Fatalf("%+v\n", err)
	}

	fmt.Print(string(writer.Bytes()))
}
