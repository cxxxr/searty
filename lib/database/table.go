package database

import (
	"database/sql"

	"github.com/cxxxr/searty/lib/primitive"
)

type Document struct {
	Id       primitive.DocumentId `db:"id"`
	Filename string               `db:"filename"`
	Body     string               `db:"body"`
}

type Token struct {
	Id   primitive.TokenId `db:"id"`
	Term string            `db:"term"`
	Kind sql.NullInt64     `db:"kind"`
}

type InvertedIndex struct {
	TokenId     primitive.TokenId `db:"token_id"`
	PostingList []byte            `db:"posting_list"`
}

type Symbol struct {
	Id          primitive.SymbolId `db:"id"`
	Name        string             `db:"name"`
	PackageName string             `db:"package_name"`
}

type Package struct {
	Id       primitive.PackageId `db:"id"`
	Name     string              `db:"name"`
	SystemId primitive.SystemId  `db:"system_id"`
}

type definition struct {
	Specifier  string               `db:"specifier"`
	DocumentId primitive.DocumentId `db:"document_id"`
	Position   int                  `db:"position"`
}

type SymbolDefinition struct {
	SymbolId   primitive.SymbolId   `db:"symbol_id"`
	*definition
}

type PackageDefinition struct {
	PackageId  primitive.PackageId  `db:"package_id"`
	*definition
}

type AsdSystem struct {
	Id           primitive.SystemId   `db:"system_id"`
	Name         string               `db:"name"`
	DocumentId   primitive.DocumentId `db:"document_id"`
	AnalyzedTime float64              `db:"analyzed_time"`
}
