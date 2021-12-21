CREATE TABLE mismatching_primary_key (
    id INTEGER NOT NULL,
    x INTEGER PRIMARY KEY NOT NULL
);

CREATE TABLE nullable_primary_key (
    id INTEGER PRIMARY KEY
);

CREATE TABLE non_integer_primary_key (
    id TEXT NOT NULL
)
