DROP TABLE IF EXISTS document;
CREATE TABLE document (
  id TEXT PRIMARY KEY,
  pathname TEXT,
  body TEXT
);

CREATE INDEX document_pathname_index ON document(pathname);

DROP TABLE IF EXISTS inverted_index;
CREATE TABLE inverted_index (
  token TEXT,
  kind INT,
  encoded_values BLOB
);

CREATE INDEX inverted_index_token_kind_index ON inverted_index(token, kind);
