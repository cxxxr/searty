package indexer

import (
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"sort"

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

func printDocIdMap(docIdMapPerDBs map[string]documentIdMap) {
	dbNames := make([]string, 0, len(docIdMapPerDBs))
	for k := range docIdMapPerDBs {
		dbNames = append(dbNames, k)
	}
	sort.Strings(dbNames)

	for _, databaseName := range dbNames {
		docIdMap := docIdMapPerDBs[databaseName]
		fmt.Printf("%s: ", trimExt(filepath.Base(databaseName)))
		first := true

		docIdPairs := make([][]primitive.DocumentId, 0)
		for src, dst := range docIdMap {
			docIdPairs = append(docIdPairs, []primitive.DocumentId{src, dst})
		}
		sort.Slice(docIdPairs, func(i, j int) bool {
			if docIdPairs[i][0] == docIdPairs[j][0] {
				return docIdPairs[i][1] < docIdPairs[j][1]
			}
			return docIdPairs[i][0] < docIdPairs[j][0]
		})

		for _, pair := range docIdPairs {
			if !first {
				fmt.Print(" ")
			}
			first = false
			fmt.Printf("%d:%d", pair[0], pair[1])
		}
		fmt.Println()
	}
}

func printInvertedIndex(dstDB *database.Database, dstInvertedIndex *invertedindex.InvertedIndex) {
	// 転置インデックスの整形表示
	type item struct {
		term     string
		postings []*invertedindex.Posting
	}
	items := make([]*item, 0)
	dstInvertedIndex.Map(func(tokenId primitive.TokenId, list *invertedindex.PostingList) error {
		token, err := dstDB.ResolveTokenById(tokenId)
		if err != nil {
			panic(err)
		}
		postings := make([]*invertedindex.Posting, 0, list.Count())
		list.Map(func(docId primitive.DocumentId, positions []int) error {
			postings = append(
				postings,
				invertedindex.NewPosting(docId, positions),
			)
			return nil
		})
		items = append(items, &item{token.Term, postings})
		return nil
	})
	sort.Slice(items, func(i, j int) bool {
		return items[i].term < items[j].term
	})
	for _, item := range items {
		fmt.Printf("%v ", []byte(item.term))
		for i, posting := range item.postings {
			if i > 0 {
				fmt.Print(" ")
			}
			doc, err := dstDB.ResolveDocumentById(posting.DocumentId())
			if err != nil {
				panic(err)
			}
			fmt.Printf("%s:%v", doc.Filename, posting.Positions())
		}
		fmt.Println()
	}
}

func printDocumentId(db *database.Database, inputFiles []string) {
	fmt.Println("\n--- src ---")
	for _, file := range inputFiles {
		fmt.Println("---", file)
		srcDB := database.New(file)
		if err := srcDB.Connect(); err != nil {
			panic(err)
		}
		defer srcDB.Close()

		docs, err := srcDB.ResolveAllDocuments()
		if err != nil {
			panic(err)
		}

		for _, doc := range docs {
			fmt.Println(doc.Id, doc.Filename)
		}
	}

	fmt.Println("\n--- dst ---")
	docs, err := db.ResolveAllDocuments()
	if err != nil {
		panic(err)
	}

	for _, doc := range docs {
		fmt.Println(doc.Id, doc.Filename)
	}
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

	if false {
		printInvertedIndex(rdr.dstDB, dstInvertedIndex)
	}

	if err := flush(dstInvertedIndex, rdr.dstDB, true); err != nil {
		return err
	}

	return nil
}

func (rdr *reducer) mergeSymbolsAndPackages(file string) error {
	srcDB := database.New(file)
	if err := srcDB.Connect(); err != nil {
		return err
	}
	defer srcDB.Close()

	symbols, err := srcDB.ResolveAllSymbols()
	if err != nil {
		return err
	}

	for _, symbol := range symbols {
		if err := rdr.dstDB.InsertSymbol(symbol); err != nil {
			return err
		}
	}

	packages, err := srcDB.ResolveAllPackages()
	if err != nil {
		return err
	}

	for _, pkg := range packages {
		if err := rdr.dstDB.InsertPackage(pkg); err != nil {
			return err
		}
	}

	return nil
}

func (rdr *reducer) mergeSymbolsAndPackagesPerDBs(inputFiles []string) error {
	for progress, file := range inputFiles {
		if err := rdr.mergeSymbolsAndPackages(file); err != nil {
			return err
		}
		printProgress("merge symbol and package", progress+1, len(inputFiles))
	}
	finishProgress()
	return nil
}

func (rdr *reducer) mergeDefinitions(file string) error {
	srcDB := database.New(file)
	if err := srcDB.Connect(); err != nil {
		return err
	}
	defer srcDB.Close()

	{
		defs, err := srcDB.ResolveAllSymbolDefinitions()
		if err != nil {
			return err
		}

		for _, def := range defs {
			err := rdr.dstDB.InsertSymbolDefinition(def)
			if err != nil {
				return err
			}
		}
	}

	{
		defs, err := srcDB.ResolveAllPackageDefinitions()
		if err != nil {
			return err
		}

		for _, def := range defs {
			err := rdr.dstDB.InsertPackageDefinition(def)
			if err != nil {
				return err
			}
		}
	}

	return nil
}

func (rdr *reducer) mergeDefinitionsPerDBs(inputFiles []string) error {
	for progress, file := range inputFiles {
		if err := rdr.mergeDefinitions(file); err != nil {
			return err
		}
		printProgress("merge definition", progress+1, len(inputFiles))
	}
	finishProgress()
	return nil
}

func (rdr *reducer) mergeAsdSystem(file string, idMap documentIdMap) error {
	srcDB := database.New(file)
	if err := srcDB.Connect(); err != nil {
		return err
	}
	defer srcDB.Close()

	asdSystems, err := srcDB.ResolveAllAsdSystem()
	if err != nil {
		return err
	}

	for _, asdSystem := range asdSystems {
		asdSystem.DocumentId = idMap[asdSystem.DocumentId]
		err := rdr.dstDB.InsertAsdSystem(asdSystem)
		if err != nil {
			return err
		}
	}

	return nil
}

func (rdr *reducer) mergeAsdSystemsPerDBs(
	inputFiles []string,
	docIdMapPerDBs map[string]documentIdMap,
) error {
	for progress, file := range inputFiles {
		if err := rdr.mergeAsdSystem(file, docIdMapPerDBs[file]); err != nil {
			return err
		}
		printProgress("merge asd system", progress+1, len(inputFiles))
	}
	finishProgress()
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

	if false {
		printDocIdMap(docIdMapPerDBs)
		printDocumentId(db, inputFiles)
	}

	if err := rdr.mergeSymbolsAndPackagesPerDBs(inputFiles); err != nil {
		return err
	}

	if err := rdr.mergeDefinitionsPerDBs(inputFiles); err != nil {
		return err
	}

	if err := rdr.mergeAsdSystemsPerDBs(inputFiles, docIdMapPerDBs); err != nil {
		return err
	}

	return nil
}
