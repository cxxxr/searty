DROP TABLE IF EXISTS document;
CREATE TABLE document (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  pathname TEXT,
  external_format TEXT,
  body text
);

CREATE INDEX document_pathname_index ON document(pathname);

DROP TABLE IF EXISTS token;
CREATE TABLE token (
  id TEXT PRIMARY KEY,
  term BLOB,
  kind INT
);

CREATE INDEX token_term_index ON token(term, kind);

DROP TABLE IF EXISTS inverted_index;
CREATE TABLE inverted_index (
  token_id TEXT PRIMARY KEY,
  locations blob
);
