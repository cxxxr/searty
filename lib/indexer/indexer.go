package indexer

import (
	"database/sql"
	"fmt"
	"os"
	"path"

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
	systemId      primitive.SystemId
	fileIdMap     map[string]primitive.DocumentId
}

func NewIndexer() *Indexer {
	return &Indexer{
		index:     invertedindex.New(),
		fileIdMap: make(map[string]primitive.DocumentId, 0),
	}
}

func computeRootDirectory(asdFile string) string {
	return path.Dir(path.Dir(asdFile))
}

func (i *Indexer) computeRelativePath(file string) (string, error) {
	n := len(i.rootDirectory)
	if file[:n] != i.rootDirectory {
		return "", errors.Errorf("invalid file: %s\n", file)
	}
	return file[n+1:], nil
}

func flush(index *invertedindex.InvertedIndex, db *database.Database) error {
	for _, tokenId := range index.TokenIds() {
		blob, err := index.Get(tokenId).Encode()
		if err != nil {
			return err
		}
		db.UpsertInvertedIndex(tokenId, blob)
	}
	return nil
}

func (i *Indexer) createDocument(file, text string, db *database.Database) (
	*database.Document,
	error,
) {
	relativePath, err := i.computeRelativePath(file)
	if err != nil {
		return nil, err
	}
	if err := db.InsertDocument(relativePath, text); err != nil {
		return nil, err
	}

	doc, err := db.ResolveDocumentByFilename(relativePath)
	if err != nil {
		return nil, err
	}

	i.fileIdMap[file] = doc.Id

	return doc, nil
}

func (i *Indexer) indexFile(file string, db *database.Database) (*database.Document, error) {
	data, err := os.ReadFile(file)
	if err != nil {
		return nil, errors.WithStack(err)
	}
	text := string(data)

	doc, err := i.createDocument(file, text, db)
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
			id := primitive.NewTokenId()
			db.InsertToken(id, term)
			tokenId = id
		} else {
			tokenId = token.Id
		}
		i.index.Insert(tokenId, doc.Id, pos)
	}

	return doc, nil
}

func (i *Indexer) makeLocation(loc spec.Location) database.Location {
	return database.Location{
		Specifier:  loc.Specifier,
		DocumentId: i.fileIdMap[loc.File],
		Position:   loc.Position,
	}
}

func (i *Indexer) indexDefinition(definition spec.Definition, db *database.Database) error {
	identifier := definition.Identifier
	switch identifier.Type {
	case "package":
		packageId := primitive.NewPackageId()
		err := db.InsertPackage(&database.Package{
			Id:       packageId,
			Name:     identifier.Name,
			SystemId: i.systemId,
		})
		if err != nil {
			return err
		}

		for _, loc := range definition.Locations {
			record := database.PackageDefinition{
				PackageId: packageId,
				Location:  i.makeLocation(loc),
			}
			err := db.InsertPackageDefinition(&record)
			if err != nil {
				return err
			}
		}
		return nil
	case "symbol":
		symbolId := primitive.NewSymbolId()
		err := db.InsertSymbol(&database.Symbol{
			Id:          symbolId,
			Name:        identifier.Name,
			PackageName: sql.NullString{String: identifier.Package, Valid: true},
		})
		if err != nil {
			return err
		}

		for _, loc := range definition.Locations {
			record := database.SymbolDefinition{
				SymbolId: symbolId,
				Location: i.makeLocation(loc),
			}
			err := db.InsertSymbolDefinition(&record)
			if err != nil {
				return err
			}
		}
		return nil
	default:
		return fmt.Errorf("unexpected identifier.Type: %v", identifier.Type)
	}
}

func (i *Indexer) indexDefinitions(definitions []spec.Definition, db *database.Database) error {
	for _, definition := range definitions {
		err := i.indexDefinition(definition, db)
		if err != nil {
			return err
		}
	}
	return nil
}

func (i *Indexer) Index(specFile, databaseFile string) error {
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

	systemId := primitive.NewSystemId()
	err = db.InsertAsdSystem(
		&database.AsdSystem{
			Id:           systemId,
			Name:         spec.SystemName,
			DocumentId:   asdDoc.Id,
			AnalyzedTime: spec.Time,
		},
	)
	if err != nil {
		return err
	}
	i.systemId = systemId

	for _, file := range spec.Files {
		if _, err := i.indexFile(file, db); err != nil {
			return err
		}
	}

	err = i.indexDefinitions(spec.Definitions, db)
	if err != nil {
		return err
	}

	return flush(i.index, db)
}
