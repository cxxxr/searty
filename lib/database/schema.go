package database

import (
	_ "embed"
)

//go:embed schema.sql
var schema []byte
