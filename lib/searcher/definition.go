package searcher

import (
	"database/sql"
	"fmt"
	"regexp"
	"strings"

	"github.com/cxxxr/searty/lib/database"
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

func (s *SymbolSearcher) Search(query string) ([]*Result, error) {
	symbols, err := s.db.ResolveSymbols(parseSymbol(query))
	if err != nil {
		return nil, err
	}

	return nil, nil
}
