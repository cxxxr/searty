package indexer

import (
	"log"
	"os"
	"path"

	"github.com/google/uuid"
	"github.com/pkg/errors"

	"github.com/cxxxr/searty/lib/database"
	"github.com/cxxxr/searty/lib/invertedindex"
	"github.com/cxxxr/searty/lib/primitive"
	"github.com/cxxxr/searty/lib/spec"
	"github.com/cxxxr/searty/lib/tokenizer"
)

type Indexer struct {
	index         *invertedindex.InvertedIndex
	rootDirectory string
}

func New() *Indexer {
	return &Indexer{index: invertedindex.New()}
}

func computeRootDirectory(asdFile string) string {
	return path.Dir(path.Dir(asdFile))
}

func (i *Indexer) computeRelativePath(file string) string {
	return file[len(i.rootDirectory)+1:]
}

func (i *Indexer) flush(database *database.Database) error {
	for _, tokenId := range i.index.TokenIds() {
		blob, err := i.index.Get(tokenId).Encode()
		if err != nil {
			return err
		}
		database.UpsertInvertedIndex(tokenId, blob)
	}
	return nil
}

func createDocument(file, text string, db *database.Database) (*database.Document, error) {
	if err := db.InsertDocument(file, text); err != nil {
		return nil, err
	}

	doc, err := db.ResolveDocumentByFilename(file)
	if err != nil {
		return nil, err
	}

	return doc, nil
}

func (i *Indexer) indexFile(file string, db *database.Database) (*database.Document, error) {
	log.Println(file)

	data, err := os.ReadFile(file)
	if err != nil {
		return nil, errors.WithStack(err)
	}
	text := string(data)

	doc, err := createDocument(i.computeRelativePath(file), text, db)
	if err != nil {
		return nil, err
	}

	terms := tokenizer.New().Tokenize(text)

	// OPTIMIZE:
	// ResolveTokenIdがN+1になっている.
	// 現状はtokenはtermだけを持ってるが,
	// 付加情報があるとsqlのwhere inで一度にselectできない問題がある.
	// システム単位でデータベースが分割されている前提ならオンメモリでトークンテーブルを管理し,
	// 最後にbulk insertすれば良いかもしれない.
	for pos, term := range terms {
		token, err := db.ResolveTokenByTerm(term)
		if err != nil {
			return nil, err
		}

		var tokenId primitive.TokenId
		if token == nil {
			id := primitive.TokenId(uuid.NewString())
			db.InsertToken(id, term)
			tokenId = id
		} else {
			tokenId = token.Id
		}
		i.index.Insert(tokenId, doc.Id, pos)
	}

	return doc, nil
}

func (i *Indexer) Index(specFile, databaseFile string) error {
	prefix := log.Prefix()
	log.SetPrefix("Index: ")
	defer log.SetPrefix(prefix)

	db := database.New(databaseFile)

	if err := db.InitTables(); err != nil {
		return err
	}

	if err := db.Connect(); err != nil {
		return err
	}
	defer db.Close()

	spec, err := spec.Read(specFile)
	if err != nil {
		return err
	}

	i.rootDirectory = computeRootDirectory(spec.AsdFile)

	asdDoc, err := i.indexFile(spec.AsdFile, db)
	if err != nil {
		return err
	}
	err = db.InsertAsdSystem(
		&database.AsdSystem{
			Id: primitive.SystemId(uuid.NewString()),
			Name: spec.SystemName,
			DocumentId: asdDoc.Id,
			AnalyzedTime: spec.Time,
		},
	)
	if err != nil {
		return err
	}

	for _, file := range spec.Files {
		if _, err := i.indexFile(file, db); err != nil {
			return err
		}
	}

	return i.flush(db)
}

// TODO
// - symbol_definition
// - package_definition
// - symbol
// - package
// - asd_system
