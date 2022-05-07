package searcher

import (
	"database/sql"
	"regexp"
	"strings"

	"github.com/cxxxr/searty/lib/database"
	"github.com/cxxxr/searty/lib/primitive"
)

var (
	externalSymbolRegexp *regexp.Regexp
	internalSymbolRegexp *regexp.Regexp
)

func init() {
	externalSymbolRegexp = regexp.MustCompile("(.*):(.*)")
	internalSymbolRegexp = regexp.MustCompile("(.*)::(.*)")
}

type SymbolSearcher struct {
	db *database.Database
}

func NewSymbolSearcher(db *database.Database) *SymbolSearcher {
	return &SymbolSearcher{
		db: db,
	}
}

func makeSymbol(name, packageName string) *database.Symbol {
	valid := packageName != ""
	return &database.Symbol{
		Name: strings.ToUpper(name),
		PackageName: sql.NullString{
			String: strings.ToUpper(packageName),
			Valid:  valid,
		},
	}
}

func parseSymbol(s string) *database.Symbol {
	r := internalSymbolRegexp.FindAllStringSubmatch(s, -1)
	if r != nil {
		return makeSymbol(strings.ToUpper(r[0][2]), strings.ToUpper(r[0][1]))
	}

	r = externalSymbolRegexp.FindAllStringSubmatch(s, -1)
	if r != nil {
		return makeSymbol(strings.ToUpper(r[0][2]), strings.ToUpper(r[0][1]))
	}

	return makeSymbol(s, "")
}

func convertDefinitionsToResults(
	defs []*database.SymbolDefinition,
	db *database.Database,
	symbolMap map[primitive.SymbolId]*database.Symbol,
) ([]*Result, error) {
	docMap := make(map[primitive.DocumentId]*database.Document, 0)
	for _, def := range defs {
		docMap[def.DocumentId] = nil
	}

	docIds := make([]primitive.DocumentId, 0, len(docMap))
	for id := range docMap {
		docIds = append(docIds, id)
	}

	docs, err := db.ResolveDocumentsByIds(docIds)
	if err != nil {
		return nil, err
	}

	for _, doc := range docs {
		docMap[doc.Id] = doc
	}

	results := make([]*Result, 0, len(defs))

	for _, def := range defs {
		doc := docMap[def.DocumentId]

		symbol := symbolMap[def.SymbolId]

		m := metadata{
			"specifier": def.Specifier,
			"symbol_name": symbol.Name,
		}
		if symbol.PackageName.Valid {
			m["package_name"] = symbol.PackageName.String
		}

		results = append(results, newResult(doc, def.Position, def.Position, m))
	}

	return results, nil
}

func (s *SymbolSearcher) Search(query string) ([]*Result, error) {
	symbols, err := s.db.ResolveSymbols(parseSymbol(query))
	if err != nil {
		return nil, err
	}

	symbolMap := make(map[primitive.SymbolId]*database.Symbol, 0)

	defs := make([]*database.SymbolDefinition, 0)
	for _, symbol := range symbols {
		symbolMap[symbol.Id] = symbol
		defs1, err := s.db.ResolveSymbolDefinitionsBySymbolId(symbol.Id)
		if err != nil {
			return nil, err
		}
		defs = append(defs, defs1...)
	}

	return convertDefinitionsToResults(defs, s.db, symbolMap)
}
