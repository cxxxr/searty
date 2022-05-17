package indexer

import (
	"errors"
	"fmt"
	"os"

	"github.com/cxxxr/searty/lib/database"
	"github.com/cxxxr/searty/lib/invertedindex"
	"github.com/cxxxr/searty/lib/primitive"
)

type reducer struct {
	dstDB *database.Database
}

type documentIdMap map[primitive.DocumentId]primitive.DocumentId
type tokenIdMap map[primitive.TokenId]primitive.TokenId

func printProgress(desc string, n, deno int) {
	fmt.Fprintf(os.Stderr, "\r%s [%d/%d]", desc, n, deno)
}

func finishProgress() {
	fmt.Fprintln(os.Stderr)
}

func (rdr *reducer) mergeDocuments(file string) (documentIdMap, error) {
	srcDB := database.New(file)
	if err := srcDB.Connect(); err != nil {
		return nil, err
	}
	defer srcDB.Close()

	docs, err := srcDB.ResolveAllDocuments()
	if err != nil {
		return nil, err
	}

	docIdMap := make(documentIdMap, len(docs))

	filenames := make([]string, 0, len(docs))
	for _, doc := range docs {
		if err := rdr.dstDB.InsertDocument(doc.Filename, doc.Body); err != nil {
			return nil, err
		}
		filenames = append(filenames, doc.Filename)
	}

	dstDocs, err := rdr.dstDB.ResolveDocumentsByFilenames(filenames)
	if err != nil {
		return nil, err
	}

	for _, dstDoc := range dstDocs {
		for _, srcDoc := range docs {
			if dstDoc.Filename == srcDoc.Filename {
				docIdMap[srcDoc.Id] = dstDoc.Id
				break
			}
		}
	}

	return docIdMap, nil
}

func (rdr *reducer) mergeDocumentsPerDBs(inputFiles []string) (map[string]documentIdMap, error) {
	docIdMapPerDBs := make(map[string]documentIdMap, len(inputFiles))

	for progress, file := range inputFiles {
		docIdMap, err := rdr.mergeDocuments(file)
		if err != nil {
			return nil, err
		}
		docIdMapPerDBs[file] = docIdMap
		printProgress("merge document", progress+1, len(inputFiles))
	}
	finishProgress()

	return docIdMapPerDBs, nil
}

func (rdr *reducer) mergeTokens(
	file string,
	tokenIdMap tokenIdMap,
	termIdMap map[string]primitive.TokenId,
) error {
	srcDB := database.New(file)
	if err := srcDB.Connect(); err != nil {
		return err
	}
	defer srcDB.Close()

	srcTokens, err := srcDB.ResolveAllTokens()
	if err != nil {
		return err
	}

	for _, token := range srcTokens {
		dstId, ok := termIdMap[token.Term]
		if !ok {
			dstId = primitive.NewTokenId()
			err := rdr.dstDB.InsertToken(dstId, token.Term)
			if err != nil {
				return err
			}
			termIdMap[token.Term] = dstId
		}
		tokenIdMap[token.Id] = dstId
	}

	return nil
}

func (rdr *reducer) mergeTokensPerDBs(inputFiles []string) (tokenIdMap, error) {
	tokenIdMap := make(tokenIdMap, 0)
	termIdMap := make(map[string]primitive.TokenId, 0)

	for progress, file := range inputFiles {
		err := rdr.mergeTokens(file, tokenIdMap, termIdMap)
		if err != nil {
			return nil, err
		}
		printProgress("merge token", progress+1, len(inputFiles))
	}
	finishProgress()

	return tokenIdMap, nil
}

func (rdr *reducer) mergeInvertedIndex(
	dst, src *invertedindex.InvertedIndex,
	tokenIdMap tokenIdMap,
	docIdMap documentIdMap,
) error {
	return src.Map(func(
		srcId primitive.TokenId,
		postinglist *invertedindex.PostingList,
	) error {
		dstId, ok := tokenIdMap[srcId]
		if !ok {
			return errors.New("unexpected error")
		}

		postinglist.Map(func(srcDocId primitive.DocumentId, positions []int) error {
			dstDocId, ok := docIdMap[srcDocId]
			if !ok {
				return errors.New("unexpected error")
			}
			// dstDocIdは常に昇順なのでpostinglistの先頭に足していくと降順にソートされることになる
			// 通常、encode時に降順のpostinglistを昇順になるようreverseするが、このときから逆順になるようにしておき、
			// encodeではreverseしないようにする
			dst.Push(dstId, invertedindex.NewPosting(dstDocId, positions))
			return nil
		})

		return nil
	})
}

func (rdr *reducer) getInvertedIndex(file string) (
	*invertedindex.InvertedIndex, error,
) {
	srcDB := database.New(file)
	if err := srcDB.Connect(); err != nil {
		return nil, err
	}
	defer srcDB.Close()

	srcInvertedIndex, err := srcDB.ResolveWholeInvertedIndex()
	if err != nil {
		return nil, err
	}

	return srcInvertedIndex, nil
}

func (rdr *reducer) mergeInvertedIndexPerDBs(
	inputFiles []string,
	tokenIdMap tokenIdMap,
	docIdMapPerDBs map[string]documentIdMap,
) error {
	dstInvertedIndex := invertedindex.New()

	for progress, file := range inputFiles {
		srcInvertedIndex, err := rdr.getInvertedIndex(file)
		if err != nil {
			return err
		}

		err = rdr.mergeInvertedIndex(
			dstInvertedIndex, srcInvertedIndex, tokenIdMap, docIdMapPerDBs[file],
		)
		if err != nil {
			return err
		}

		printProgress("merge index", progress+1, len(inputFiles))
	}
	finishProgress()

	if err := flush(dstInvertedIndex, rdr.dstDB, true); err != nil {
		return err
	}

	return nil
}

func MergeDatabases(inputFiles []string, outputFile string) error {
	db := database.New(outputFile)

	if err := db.InitTables(); err != nil {
		return err
	}

	if err := db.Connect(); err != nil {
		return err
	}
	defer db.Close()

	rdr := reducer{dstDB: db}

	docIdMapPerDBs, err := rdr.mergeDocumentsPerDBs(inputFiles)
	if err != nil {
		return err
	}

	tokenIdMap, err := rdr.mergeTokensPerDBs(inputFiles)
	if err != nil {
		return err
	}

	err = rdr.mergeInvertedIndexPerDBs(inputFiles, tokenIdMap, docIdMapPerDBs)
	if err != nil {
		return err
	}

	// TODO
	// - sybmol
	// - package
	// - symbol_definition
	// - package_definition
	// - asd_system

	return nil
}
