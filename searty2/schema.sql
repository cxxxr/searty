DROP TABLE IF EXISTS document;
CREATE TABLE document (
  id TEXT PRIMARY KEY,
  pathname TEXT,
  body TEXT
);

CREATE INDEX document_pathname_index ON document(pathname);

DROP TABLE IF EXISTS inverted_index;
CREATE TABLE inverted_index (
  term TEXT,
  kind INT,
  encoded_values BLOB
);

CREATE INDEX inverted_index_term_kind_index ON inverted_index(term, kind);
