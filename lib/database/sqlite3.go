package database

import (
	"context"
	"database/sql"
	"os/exec"

	"github.com/cxxxr/searty/lib/entity"
	"github.com/cxxxr/searty/lib/invertedindex"
	_ "github.com/mattn/go-sqlite3"
	"github.com/pkg/errors"
)

type Database struct {
	databaseFile string
	conn         *sql.DB
	transaction  *sql.Tx
	prepareStatements
}

type prepareStatements struct {
	insertDocument      *sql.Stmt
	resolveDocument     *sql.Stmt
	resolveAllDocuments *sql.Stmt
	resolveToken        *sql.Stmt
	resolveAllTokenIds  *sql.Stmt
	insertToken         *sql.Stmt
	upsertInvertedIndex *sql.Stmt
	resolvePostingList  *sql.Stmt
}

func New(databaseFile string) *Database {
	return &Database{databaseFile: databaseFile}
}

func (d *Database) Clear() error {
	err := exec.Command("sqlite3", "-init", "../../schema.sql", d.databaseFile).Run()
	if err != nil {
		return errors.WithStack(err)
	}
	return nil
}

func (d *Database) Connect() error {
	conn, err := sql.Open("sqlite3", d.databaseFile)
	if err != nil {
		return errors.WithStack(err)
	}
	d.conn = conn

	tx, err := conn.Begin()
	if err != nil {
		return errors.WithStack(err)
	}
	d.transaction = tx

	if err := d.initializePrepareStatements(); err != nil {
		return err
	}

	return nil
}

func (d *Database) Close() error {
	err := d.transaction.Commit()
	if err != nil {
		return err
	}
	return d.conn.Close()
}

func (d *Database) initializePrepareStatements() error {
	ctx := context.Background()

	stmt, err := d.transaction.PrepareContext(
		ctx,
		`INSERT INTO document (filename, external_format, body) VALUES (?, ?, ?)`,
	)
	if err != nil {
		return errors.WithStack(err)
	}
	d.insertDocument = stmt

	stmt, err = d.transaction.PrepareContext(
		ctx,
		`SELECT id FROM document WHERE filename = ? LIMIT 1`,
	)
	if err != nil {
		return errors.WithStack(err)
	}
	d.resolveDocument = stmt

	stmt, err = d.transaction.PrepareContext(
		ctx,
		`SELECT id, filename FROM document`,
	)
	if err != nil {
		return errors.WithStack(err)
	}
	d.resolveAllDocuments = stmt

	stmt, err = d.transaction.PrepareContext(
		ctx,
		`SELECT id FROM token WHERE term = ? LIMIT 1`,
	)
	if err != nil {
		return errors.WithStack(err)
	}
	d.resolveToken = stmt

	stmt, err = d.transaction.PrepareContext(
		ctx,
		`SELECT id FROM token`,
	)
	if err != nil {
		return errors.WithStack(err)
	}
	d.resolveAllTokenIds = stmt

	stmt, err = d.transaction.PrepareContext(
		ctx,
		`INSERT INTO token (id, term) VALUES (?, ?)`,
	)
	if err != nil {
		return errors.WithStack(err)
	}
	d.insertToken = stmt

	stmt, err = d.transaction.PrepareContext(
		ctx,
		`INSERT INTO inverted_index (token_id, postinglist) VALUES (?, ?)
ON CONFLICT(token_id) DO NOTHING`,
	)
	if err != nil {
		return errors.WithStack(err)
	}
	d.upsertInvertedIndex = stmt

	stmt, err = d.transaction.PrepareContext(
		ctx,
		`SELECT postinglist FROM inverted_index WHERE token_id = ? LIMIT 1`,
	)
	if err != nil {
		return errors.WithStack(err)
	}
	d.resolvePostingList = stmt

	return nil
}

func (d *Database) InsertDocument(filename, externalFormat, body string) error {
	_, err := d.insertDocument.Exec(filename, externalFormat, body)
	if err != nil {
		return errors.WithStack(err)
	}
	return nil
}

func (d *Database) ResolveDocument(filename string) (*entity.Document, error) {
	rows, err := d.resolveDocument.Query(filename)
	if err != nil {
		return nil, err
	}
	defer rows.Close()

	if !rows.Next() {
		return nil, nil
	}

	var id entity.DocumentId
	if err := rows.Scan(&id); err != nil {
		return nil, errors.WithStack(err)
	}

	return entity.NewDocument(id, filename), nil
}

func (d *Database) ResolveAllDocuments() ([]*entity.Document, error) {
	rows, err := d.resolveAllDocuments.Query()
	if err != nil {
		return nil, errors.WithStack(err)
	}
	defer rows.Close()

	documents := make([]*entity.Document, 0)
	for rows.Next() {
		var id entity.DocumentId
		var filename string
		if err := rows.Scan(&id, &filename); err != nil {
			return nil, errors.WithStack(err)
		}
		doc := entity.NewDocument(id, filename)
		documents = append(documents, doc)
	}
	return documents, nil
}

func (d *Database) ResolveTokenId(term string) (entity.TokenId, error) {
	rows, err := d.resolveToken.Query(term)
	if err != nil {
		return entity.EmptyTokenId, errors.WithStack(err)
	}
	defer rows.Close()

	if !rows.Next() {
		return entity.EmptyTokenId, nil
	}

	var id string
	if err := rows.Scan(&id); err != nil {
		return entity.EmptyTokenId, errors.WithStack(err)
	}

	return entity.TokenId(id), nil
}

func (d *Database) ResolveAllTokenIds() ([]entity.TokenId, error) {
	rows, err := d.resolveAllTokenIds.Query()
	if err != nil {
		return nil, errors.WithStack(err)
	}
	defer rows.Close()

	tokenIds := make([]entity.TokenId, 0)
	for rows.Next() {
		var id string
		if err := rows.Scan(&id); err != nil {
			return nil, errors.WithStack(err)
		}
		tokenIds = append(tokenIds, entity.TokenId(id))
	}
	return tokenIds, nil
}

func (d *Database) InsertToken(tokenId entity.TokenId, term string) error {
	_, err := d.insertToken.Exec(tokenId, term)
	if err != nil {
		return errors.WithStack(err)
	}
	return nil
}

func (d *Database) UpsertInvertedIndex(tokenId entity.TokenId, blob []byte) error {
	_, err := d.upsertInvertedIndex.Exec(tokenId, blob)
	if err != nil {
		return errors.WithStack(err)
	}
	return nil
}

func (d *Database) ResolvePostingList(tokenId entity.TokenId) (*invertedindex.PostingList, error) {
	rows, err := d.resolvePostingList.Query(tokenId)
	if err != nil {
		return nil, errors.WithStack(err)
	}
	defer rows.Close()

	if !rows.Next() {
		return nil, nil
	}

	var blob []byte
	if err := rows.Scan(&blob); err != nil {
		return nil, errors.WithStack(err)
	}

	return invertedindex.DecodePostingList(blob)
}
