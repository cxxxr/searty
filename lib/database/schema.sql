DROP TABLE IF EXISTS document;
CREATE TABLE document (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  filename TEXT,
  body text
);

CREATE INDEX document_filename_index ON document(filename);

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
  posting_list blob
);

DROP TABLE IF EXISTS symbol;
CREATE TABLE symbol (
  id TEXT PRIMARY KEY,
  name TEXT,
  package_name TEXT
);

DROP TABLE IF EXISTS package;
CREATE TABLE package (
  id TEXT PRIMARY KEY,
  name TEXT,
  system_id TEXT
);

CREATE INDEX package_name_index ON package(name);
CREATE INDEX package_system_id_index ON package(system_id);

CREATE INDEX symbol_name_package_index ON symbol(name, package_name);

DROP TABLE IF EXISTS symbol_definition;
CREATE TABLE symbol_definition (
  symbol_id TEXT,
  specifier TEXT,
  document_id INT,
  position INT
);

CREATE INDEX symbol_definition_symbol_id_index ON symbol_definition(symbol_id);
CREATE INDEX symbol_definition_symbol_document_id_index ON symbol_definition(document_id);

DROP TABLE IF EXISTS package_definition;
CREATE TABLE package_definition (
  package_id TEXT,
  specifier TEXT,
  document_id INT,
  position INT
);

CREATE INDEX package_definition_symbol_id_index ON package_definition(package_id);
CREATE INDEX package_definition_symbol_document_id_index ON package_definition(document_id);

DROP TABLE IF EXISTS asd_system;
CREATE TABLE asd_system (
  id TEXT PRIMARY KEY,
  name TEXT,
  document_id INT,
  analyzed_time REAL
);
