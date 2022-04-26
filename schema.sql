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

DROP TABLE IF EXISTS symbol;
CREATE TABLE symbol (
  id TEXT PRIMARY KEY,
  name TEXT,
  package TEXT
);

CREATE INDEX symbol_name_package_index ON symbol(name, package);

DROP TABLE IF EXISTS symbol_definition;
CREATE TABLE symbol_definition (
  symbol_id TEXT,
  specifier TEXT,
  filename TEXT,
  position INT
);

CREATE INDEX symbol_definition_symbol_id_index ON symbol_definition(symbol_id);
CREATE INDEX symbol_definition_symbol_filename_index ON symbol_definition(filename);

DROP TABLE IF EXISTS asd_system;
CREATE TABLE asd_system (
  id text PRIMARY KEY,
  name TEXT,
  filename TEXT,
  analyzed_time REAL
);
