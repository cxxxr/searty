package indexer

import (
	"log"
	"os"

	"github.com/google/uuid"
	"github.com/pkg/errors"

	"github.com/cxxxr/searty/lib/database"
	"github.com/cxxxr/searty/lib/invertedindex"
	"github.com/cxxxr/searty/lib/primitive"
	"github.com/cxxxr/searty/lib/spec"
	"github.com/cxxxr/searty/lib/tokenizer"
)

type Indexer struct {
	index *invertedindex.InvertedIndex
}

func New() *Indexer {
	return &Indexer{invertedindex.New()}
}

func (i *Indexer) flush(database *database.Database) error {
	for _, tokenId := range i.index.TokenIds() {
		blob, err := i.index.EncodePostingList(tokenId)
		if err != nil {
			return err
		}
		database.UpsertInvertedIndex(tokenId, blob)
	}
	return nil
}

func (i *Indexer) indexFile(file string, database *database.Database) error {
	data, err := os.ReadFile(file)
	if err != nil {
		return errors.WithStack(err)
	}
	text := string(data)

	if err := database.InsertDocument(file, text); err != nil {
		return err
	}

	doc, err := database.ResolveDocumentByFilename(file)
	if err != nil {
		return err
	}

	terms := tokenizer.New().Tokenize(text)

	// OPTIMIZE:
	// ResolveTokenIdがN+1になっている.
	// 現状はtokenはtermだけを持ってるが,
	// 付加情報があるとsqlのwhere inで一度にselectできない問題がある.
	// システム単位でデータベースが分割されている前提ならオンメモリでトークンテーブルを管理し,
	// 最後にbulk insertすれば良いかもしれない.
	for pos, term := range terms {
		token, err := database.ResolveTokenByTerm(term)
		if err != nil {
			return err
		}

		var tokenId primitive.TokenId
		if token == nil {
			id := primitive.TokenId(uuid.NewString())
			database.InsertToken(id, term)
			tokenId = id
		} else {
			tokenId = token.Id
		}
		i.index.Insert(tokenId, doc.Id, pos)
	}

	return nil
}

func (i *Indexer) Index(specFile, databaseFile string) error {
	prefix := log.Prefix()
	log.SetPrefix("Index: ")
	defer log.SetPrefix(prefix)

	database := database.New(databaseFile)

	if err := database.Clear(); err != nil {
		return err
	}

	if err := database.Connect(); err != nil {
		return err
	}
	defer database.Close()

	spec, err := spec.Read(specFile)
	if err != nil {
		return err
	}

	for _, file := range spec.Files {
		log.Println(file)
		if err := i.indexFile(file, database); err != nil {
			return err
		}
	}

	return i.flush(database)
}

// TODO
// - inverted_index
// - symbol_definition
// - package_definition
// - symbol
// - package
// - asd_system
