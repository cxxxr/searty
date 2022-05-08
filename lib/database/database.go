package database

import (
	"context"

	"github.com/cxxxr/searty/lib/invertedindex"
	"github.com/cxxxr/searty/lib/primitive"
	"github.com/jmoiron/sqlx"
	_ "github.com/mattn/go-sqlite3"
	"github.com/pkg/errors"
)

type Database struct {
	databaseFile string
	db           *sqlx.DB
	tx           *sqlx.Tx
	prepareStatements
}

type prepareStatements struct {
	insertDocument            *sqlx.Stmt
	resolveDocumentByFilename *sqlx.Stmt
	resolveDocumentById       *sqlx.Stmt
	resolveAllDocuments       *sqlx.Stmt

	resolveTokenByTerm *sqlx.Stmt
	resolveTokenById   *sqlx.Stmt
	resolveAllTokenIds *sqlx.Stmt
	insertToken        *sqlx.Stmt

	upsertInvertedIndex *sqlx.Stmt

	insertAsdSystem *sqlx.NamedStmt

	insertSymbol                   *sqlx.NamedStmt
	resolveSymbolsByName           *sqlx.Stmt
	resolveSymbolsByPackage        *sqlx.Stmt
	resolveSymbolsByNameAndPackage *sqlx.Stmt

	insertPackage         *sqlx.NamedStmt
	resolvePackagesByName *sqlx.NamedStmt

	insertSymbolDefinition             *sqlx.NamedStmt
	resolveSymbolDefinitionsBySymbolId *sqlx.Stmt

	insertPackageDefinition *sqlx.NamedStmt
}

func New(databaseFile string) *Database {
	return &Database{databaseFile: databaseFile}
}

func connectSqlite3(databaseFile string) (*sqlx.DB, error) {
	db, err := sqlx.Connect("sqlite3", databaseFile)
	if err != nil {
		return nil, errors.Wrapf(err, "file: %s", databaseFile)
	}
	return db, err
}

func (d *Database) InitTables() error {
	db, err := connectSqlite3(d.databaseFile)
	if err != nil {
		return err
	}
	defer db.Close()
	db.MustExec(string(schema))

	return nil
}

func (d *Database) Connect() error {
	db, err := connectSqlite3(d.databaseFile)
	if err != nil {
		return err
	}
	d.db = db

	tx, err := db.Beginx()
	if err != nil {
		return errors.WithStack(err)
	}
	d.tx = tx

	if err := d.initializePrepareStatements(); err != nil {
		return err
	}

	return nil
}

func (d *Database) Close() error {
	err := d.tx.Commit()
	if err != nil {
		return err
	}
	return d.db.Close()
}

func (d *Database) initializePrepareStatements() error {
	ctx := context.Background()

	stmt, err := d.tx.PreparexContext(
		ctx,
		`INSERT INTO document (filename, body) VALUES (?, ?)`,
	)
	if err != nil {
		return errors.WithStack(err)
	}
	d.insertDocument = stmt

	stmt, err = d.tx.PreparexContext(
		ctx,
		`SELECT id, filename FROM document WHERE filename = ? LIMIT 1`,
	)
	if err != nil {
		return errors.WithStack(err)
	}
	d.resolveDocumentByFilename = stmt

	stmt, err = d.tx.PreparexContext(
		ctx,
		`SELECT id, filename FROM document WHERE id = ? LIMIT 1`,
	)
	if err != nil {
		return errors.WithStack(err)
	}
	d.resolveDocumentById = stmt

	stmt, err = d.tx.PreparexContext(
		ctx,
		`SELECT id, filename FROM document`,
	)
	if err != nil {
		return errors.WithStack(err)
	}
	d.resolveAllDocuments = stmt

	stmt, err = d.tx.PreparexContext(
		ctx,
		`SELECT id, term FROM token WHERE term = ? LIMIT 1`,
	)
	if err != nil {
		return errors.WithStack(err)
	}
	d.resolveTokenByTerm = stmt

	stmt, err = d.tx.PreparexContext(
		ctx,
		`SELECT id, term FROM token WHERE id = ? LIMIT 1`,
	)
	if err != nil {
		return errors.WithStack(err)
	}
	d.resolveTokenById = stmt

	stmt, err = d.tx.PreparexContext(
		ctx,
		`SELECT id, term FROM token`,
	)
	if err != nil {
		return errors.WithStack(err)
	}
	d.resolveAllTokenIds = stmt

	stmt, err = d.tx.PreparexContext(
		ctx,
		`INSERT INTO token (id, term) VALUES (?, ?)`,
	)
	if err != nil {
		return errors.WithStack(err)
	}
	d.insertToken = stmt

	stmt, err = d.tx.PreparexContext(
		ctx,
		`INSERT INTO inverted_index (token_id, posting_list) VALUES (?, ?)
ON CONFLICT(token_id) DO NOTHING`,
	)
	if err != nil {
		return errors.WithStack(err)
	}
	d.upsertInvertedIndex = stmt

	namedStmt, err := d.tx.PrepareNamedContext(
		ctx,
		`INSERT INTO asd_system (id, name, document_id, analyzed_time)
VALUES (:id, :name, :document_id, :analyzed_time)`,
	)
	if err != nil {
		return errors.WithStack(err)
	}
	d.insertAsdSystem = namedStmt

	namedStmt, err = d.tx.PrepareNamedContext(
		ctx,
		`INSERT INTO symbol (id, name, package_name)
VALUES (:id, :name, :package_name)`,
	)
	if err != nil {
		return errors.WithStack(err)
	}
	d.insertSymbol = namedStmt

	stmt, err = d.tx.PreparexContext(
		ctx,
		`SELECT id, name, package_name FROM symbol WHERE name = ?`,
	)
	if err != nil {
		return errors.WithStack(err)
	}
	d.resolveSymbolsByName = stmt

	stmt, err = d.tx.PreparexContext(
		ctx,
		`SELECT id, name, package_name FROM symbol WHERE package_name = ?`,
	)
	if err != nil {
		return errors.WithStack(err)
	}
	d.resolveSymbolsByPackage = stmt

	stmt, err = d.tx.PreparexContext(
		ctx,
		`SELECT id, name, package_name FROM symbol WHERE name = ? and package_name = ?`,
	)
	if err != nil {
		return errors.WithStack(err)
	}
	d.resolveSymbolsByNameAndPackage = stmt

	namedStmt, err = d.tx.PrepareNamedContext(
		ctx,
		`INSERT INTO package (id, name, system_id)
VALUES (:id, :name, :system_id)`,
	)
	if err != nil {
		return errors.WithStack(err)
	}
	d.insertPackage = namedStmt

	namedStmt, err = d.tx.PrepareNamedContext(
		ctx,
		`SELECT :id, :name, :system_id FROM symbol WHERE name = ?`,
	)
	if err != nil {
		return errors.WithStack(err)
	}
	d.resolvePackagesByName = namedStmt

	namedStmt, err = d.tx.PrepareNamedContext(
		ctx,
		`INSERT INTO symbol_definition (symbol_id, specifier, document_id, position)
VALUES (:symbol_id, :specifier, :document_id, :position)`,
	)
	if err != nil {
		return errors.WithStack(err)
	}
	d.insertSymbolDefinition = namedStmt

	stmt, err = d.tx.PreparexContext(
		ctx,
		`SELECT symbol_id, specifier, document_id, position
FROM symbol_definition
WHERE symbol_id = ?`,
	)
	if err != nil {
		return errors.WithStack(err)
	}
	d.resolveSymbolDefinitionsBySymbolId = stmt

	namedStmt, err = d.tx.PrepareNamedContext(
		ctx,
		`INSERT INTO package_definition (package_id, specifier, document_id, position)
VALUES (:package_id, :specifier, :document_id, :position)`,
	)
	if err != nil {
		return errors.WithStack(err)
	}
	d.insertPackageDefinition = namedStmt

	return nil
}

func (d *Database) InsertDocument(filename, body string) error {
	_, err := d.insertDocument.Exec(filename, body)
	if err != nil {
		return errors.WithStack(err)
	}
	return nil
}

func (d *Database) ResolveDocumentByFilename(filename string) (*Document, error) {
	var doc Document
	err := d.resolveDocumentByFilename.Get(&doc, filename)
	if err != nil {
		return nil, errors.WithStack(err)
	}
	return &doc, nil
}

func (d *Database) ResolveDocumentById(id primitive.DocumentId) (*Document, error) {
	var doc Document
	err := d.resolveDocumentById.Get(&doc, id)
	if err != nil {
		return nil, errors.WithStack(err)
	}
	return &doc, nil
}

func (d *Database) resolveDocumentsAux(query string, params []interface{}) ([]*Document, error) {
	var records []*Document
	if err := d.db.Select(&records, query, params...); err != nil {
		return nil, errors.WithStack(err)
	}
	return records, nil
}

func (d *Database) ResolveDocumentsByIds(ids []primitive.DocumentId) ([]*Document, error) {
	query, params, err := sqlx.In(`SELECT id, filename FROM document WHERE id in (?)`, ids)
	if err != nil {
		return nil, errors.WithStack(err)
	}
	return d.resolveDocumentsAux(query, params)
}

func (d *Database) ResolveDocumentsWithBodyByIds(ids []primitive.DocumentId) ([]*Document, error) {
	query, params, err := sqlx.In(`SELECT id, filename, body FROM document WHERE id in (?)`, ids)
	if err != nil {
		return nil, err
	}
	return d.resolveDocumentsAux(query, params)
}

func (d *Database) ResolveAllDocuments() ([]*Document, error) {
	var docs []*Document
	err := d.resolveAllDocuments.Select(&docs)
	if err != nil {
		return nil, errors.WithStack(err)
	}
	return docs, nil
}

func (d *Database) resolveToken(s *sqlx.Stmt, arg interface{}) (*Token, error) {
	var tokens []Token
	err := s.Select(&tokens, arg)
	if err != nil {
		return nil, errors.WithStack(err)
	}
	if len(tokens) == 0 {
		return nil, nil
	}
	return &tokens[0], nil
}

func (d *Database) ResolveTokenByTerm(term string) (*Token, error) {
	return d.resolveToken(d.resolveTokenByTerm, term)
}

func (d *Database) ResolveTokenById(id primitive.TokenId) (*Token, error) {
	return d.resolveToken(d.resolveTokenById, id)
}

func (d *Database) ResolveTokensByTerms(terms []string) ([]*Token, error) {
	query, params, err := sqlx.In(
		`SELECT id, term, kind FROM token WHERE term in (?)`,
		terms,
	)
	if err != nil {
		return nil, errors.WithStack(err)
	}

	var records []*Token
	if err := d.db.Select(&records, query, params...); err != nil {
		return nil, errors.WithStack(err)
	}

	return records, nil
}

func (d *Database) ResolveAllTokens() ([]*Token, error) {
	var tokens []*Token
	err := d.resolveAllTokenIds.Select(&tokens)
	if err != nil {
		return nil, errors.WithStack(err)
	}
	return tokens, nil
}

func (d *Database) InsertToken(tokenId primitive.TokenId, term string) error {
	_, err := d.insertToken.Exec(tokenId, term)
	if err != nil {
		return errors.WithStack(err)
	}
	return nil
}

func (d *Database) UpsertInvertedIndex(tokenId primitive.TokenId, blob []byte) error {
	_, err := d.upsertInvertedIndex.Exec(tokenId, blob)
	if err != nil {
		return errors.WithStack(err)
	}
	return nil
}

func (d *Database) ResolveInvertedIndex(tokenIds []primitive.TokenId) (
	*invertedindex.InvertedIndex,
	error,
) {
	query, params, err := sqlx.In(
		`SELECT token_id, posting_list FROM inverted_index WHERE token_id in (?)`,
		tokenIds,
	)
	if err != nil {
		return nil, errors.WithStack(err)
	}

	var records []*InvertedIndex
	if err := d.db.Select(&records, query, params...); err != nil {
		return nil, errors.WithStack(err)
	}

	invertedIndex := invertedindex.New()
	for _, record := range records {
		postingList, err := invertedindex.DecodePostingList(record.PostingList)
		if err != nil {
			return nil, err
		}
		invertedIndex.Set(record.TokenId, postingList)
	}
	return invertedIndex, nil
}

func (d *Database) InsertAsdSystem(record *AsdSystem) error {
	_, err := d.insertAsdSystem.Exec(record)
	if err != nil {
		return errors.WithStack(err)
	}
	return nil
}

func (d *Database) InsertSymbol(record *Symbol) error {
	_, err := d.insertSymbol.Exec(record)
	if err != nil {
		return errors.WithStack(err)
	}
	return nil
}

func (d *Database) ResolveSymbols(symbol *Symbol) ([]*Symbol, error) {
	var records []*Symbol
	var err error
	if symbol.PackageName.Valid {
		err = d.resolveSymbolsByNameAndPackage.Select(
			&records,
			symbol.Name,
			symbol.PackageName,
		)
	} else {
		err = d.resolveSymbolsByName.Select(&records, symbol.Name)
	}
	if err != nil {
		return nil, err
	}
	return records, nil
}

func (d *Database) InsertPackage(record *Package) error {
	_, err := d.insertPackage.Exec(record)
	if err != nil {
		return errors.WithStack(err)
	}
	return nil
}

func (d *Database) InsertSymbolDefinition(record *SymbolDefinition) error {
	_, err := d.insertSymbolDefinition.Exec(record)
	if err != nil {
		return errors.WithStack(err)
	}
	return nil
}

func (d *Database) ResolveSymbolDefinitionsBySymbolId(id primitive.SymbolId) (
	[]*SymbolDefinition, error,
) {
	var records []*SymbolDefinition
	err := d.resolveSymbolDefinitionsBySymbolId.Select(&records, id)
	if err != nil {
		return nil, err
	}
	return records, nil
}

func (d *Database) InsertPackageDefinition(record *PackageDefinition) error {
	_, err := d.insertPackageDefinition.Exec(record)
	if err != nil {
		return errors.WithStack(err)
	}
	return nil
}
