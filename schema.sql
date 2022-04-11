CREATE TABLE IF NOT EXISTS document (
  id BIGSERIAL NOT NULL,
  pathname TEXT NOT NULL,
  body TEXT NOT NULL
);

CREATE INDEX document_pathname_index ON document(pathname);

CREATE TYPE token_kind AS ENUM ('unknown', 'string', 'symbol', 'character', 'function-object', 'unintern-symbol', 'line-comment', 'block-comment');

CREATE TABLE IF NOT EXISTS token (
  id UUID PRIMARY KEY,
  term BYTEA,
  kind TOKEN_kind
);

CREATE INDEX token_term_index ON token(term, kind);

CREATE TABLE IF NOT EXISTS inverted_index (
  token_id UUID,
  document_id BIGINT,
  position INTEGER
);

CREATE INDEX inverted_index_index ON inverted_index(token_id, document_id);
