DROP TABLE IF EXISTS document;
CREATE TABLE document (
  id TEXT PRIMARY KEY,
  pathname TEXT,
  body TEXT
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
  locations_data_file text
);
