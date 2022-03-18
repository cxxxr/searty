DROP TABLE IF EXISTS document;
CREATE TABLE document (
	id TEXT PRIMARY KEY,
	pathname TEXT,
	text TEXT
);

DROP TABLE IF EXISTS token;
CREATE TABLE token (
	id TEXT PRIMARY KEY,
	term TEXT
);

DROP TABLE IF EXISTS inverted_index;
CREATE TABLE inverted_index (
	token_id TEXT PRIMARY KEY,
	encoded_values BLOB
);
