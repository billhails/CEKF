CREATE TABLE unicode (
    int_code INTEGER PRIMARY KEY NOT NULL, -- -1
    code TEXT UNIQUE NOT NULL,             --  0
    name TEXT NOT NULL,                    --  1
    gc TEXT,                               --  2
    cc INT,                                --  3
    bc TEXT,                               --  4
    decomposition TEXT,                    --  5
    nv_dec TEXT,                           --  6
    nv_dig TEXT,                           --  7
    nv_num TEXT,                           --  8
    bm TEXT,                               --  9
    alias TEXT,                            -- 10
    -- [obsolete]                          -- 11
    upper_case TEXT,                       -- 12
    lower_case TEXT,                       -- 13
    title_case TEXT                        -- 14
);
CREATE INDEX unicode_name ON unicode(name);
.import --csv UnicodeData.csv unicode
