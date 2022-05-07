package primitive

import "github.com/google/uuid"

type TokenId string

type DocumentId int

type SymbolId string
type PackageId string
type SystemId string

func NewTokenId() TokenId {
	return TokenId(uuid.NewString())
}

func NewSymbolId() SymbolId {
	return SymbolId(uuid.NewString())
}

func NewPackageId() PackageId {
	return PackageId(uuid.NewString())
}

func NewSystemId() SystemId {
	return SystemId(uuid.NewString())
}
