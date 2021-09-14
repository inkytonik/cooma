CREATE TABLE text_columns (
    id TEXT PRIMARY KEY,
    name TEXT NOT NULL
);

INSERT INTO text_columns (id, name) VALUES
('fvt', 'jlr'),
('bdt', 'mhu'),
('nvg', 'efl');


CREATE TABLE many_text_columns (
    id TEXT PRIMARY KEY,
    c1 TEXT NOT NULL,
    c2 TEXT NOT NULL,
    c3 TEXT NOT NULL,
    c4 TEXT NOT NULL
);

INSERT INTO many_text_columns (id, c1, c2, c3, c4) VALUES
('lyb', 'hwk', 'oqk', 'diy', 'rdu'),
('gbb', 'qfd', 'ryl', 'gut', 'qig');


CREATE TABLE nullable_columns (
    id TEXT PRIMARY KEY,
    name_1 TEXT,
    name_2 TEXT
);

INSERT INTO nullable_columns (id, name_1, name_2) VALUES
('rfn', 'iit', 'ics'),
('tjz', 'cxa', NULL),
('rfa', NULL, 'pdg'),
('rci', NULL, NULL);


CREATE TABLE integer_columns (
    id INTEGER PRIMARY KEY,
    x INTEGER NOT NULL,
    y INTEGER NOT NULL
);

INSERT INTO integer_columns (x, y) VALUES
(17, 30),
(90, 22);


CREATE TABLE numeric_columns (
    id INTEGER PRIMARY KEY,
    x NUMERIC NOT NULL,
    y NUMERIC NOT NULL,
    z NUMERIC NOT NULL
);

INSERT INTO numeric_columns (x, y, z) VALUES
(842, 68.7, 0.62);


CREATE TABLE boolean_columns (
    id INTEGER PRIMARY KEY,
    p BOOLEAN NOT NULL,
    q BOOLEAN NOT NULL
);

INSERT INTO boolean_columns (p, q) VALUES
(TRUE, FALSE);


CREATE TABLE mixed_columns (
    id INTEGER PRIMARY KEY,
    a TEXT NOT NULL,
    b TEXT,
    c INTEGER NOT NULL,
    d INTEGER,
    e NUMERIC NOT NULL,
    f NUMERIC,
    g BOOLEAN NOT NULL,
    h BOOLEAN
);

INSERT INTO mixed_columns (a, b, c, d, e, f, g, h) VALUES
('gws', 'oxd', 210, 574, 41.9, 8.63, TRUE, FALSE),
('gyn', NULL, 228, NULL, 9.34, NULL, FALSE, NULL);
