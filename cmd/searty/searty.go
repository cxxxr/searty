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

var (
	file = flag.String("d", "", "database file")
)

func main() {
	flag.Parse()
	flag.Args()
	query := flag.Arg(0)

	if *file == "" {
		flag.Usage()
		return
	}

	if _, err := os.Stat(*file); err != nil {
		log.Printf("%s not found", *file)
		return
	}

	if query == "" {
		return
	}

	db := database.New(*file)
	if err := db.Connect(); err != nil {
		log.Fatal(err)
	}
	defer db.Close()

	s := searcher.NewPhraseSearcher(db)
	results, err := s.Search(query)
	if err != nil {
		log.Fatalf("%+v\n", err)
	}

	writer := bytes.NewBuffer(nil)
	if err := searcher.PrintResults(results, db, writer); err != nil {
		log.Fatal(err)
	}

	fmt.Print(string(writer.Bytes()))
}
