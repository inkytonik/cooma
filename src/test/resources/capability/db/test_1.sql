CREATE TABLE text_columns (
    id INTEGER PRIMARY KEY NOT NULL,
    name TEXT NOT NULL
);

INSERT INTO text_columns (name) VALUES
('jlr'),
('mhu'),
('efl');


CREATE TABLE many_text_columns (
    id INTEGER PRIMARY KEY NOT NULL,
    c1 TEXT NOT NULL,
    c2 TEXT NOT NULL,
    c3 TEXT NOT NULL,
    c4 TEXT NOT NULL
);

INSERT INTO many_text_columns (c1, c2, c3, c4) VALUES
('hwk', 'oqk', 'diy', 'rdu'),
('qfd', 'ryl', 'gut', 'qig');


CREATE TABLE nullable_columns (
    id INTEGER PRIMARY KEY NOT NULL,
    name_1 TEXT,
    name_2 TEXT
);

INSERT INTO nullable_columns (name_1, name_2) VALUES
('iit', 'ics'),
('cxa', NULL),
(NULL, 'pdg'),
(NULL, NULL);


CREATE TABLE integer_columns (
    id INTEGER PRIMARY KEY NOT NULL,
    x INTEGER NOT NULL,
    y INTEGER NOT NULL
);

INSERT INTO integer_columns (x, y) VALUES
(17, 30),
(90, 22);


CREATE TABLE numeric_columns (
    id INTEGER PRIMARY KEY NOT NULL,
    x NUMERIC NOT NULL,
    y NUMERIC NOT NULL,
    z NUMERIC NOT NULL
);

INSERT INTO numeric_columns (x, y, z) VALUES
(842, 68.7, 0.62);


CREATE TABLE boolean_columns (
    id INTEGER PRIMARY KEY NOT NULL,
    p BOOLEAN NOT NULL,
    q BOOLEAN NOT NULL
);

INSERT INTO boolean_columns (p, q) VALUES
(TRUE, FALSE);


CREATE TABLE mixed_columns (
    id INTEGER PRIMARY KEY NOT NULL,
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
