CREATE TABLE mismatching_primary_key (
    id INTEGER NOT NULL,
    x INTEGER PRIMARY KEY NOT NULL
);

CREATE TABLE nullable_primary_key (
    id INTEGER PRIMARY KEY
);

CREATE TABLE non_integer_primary_key (
    id TEXT NOT NULL
);

CREATE TABLE default_values (
    id INTEGER PRIMARY KEY NOT NULL,
    a TEXT NOT NULL,
    b TEXT,
    c TEXT NOT NULL DEFAULT ''
);
