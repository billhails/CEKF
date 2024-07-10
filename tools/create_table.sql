CREATE TABLE unicode (
    int_code INTEGER PRIMARY KEY NOT NULL, -- -1
    code TEXT UNIQUE NOT NULL,             --  0
    name TEXT NOT NULL,                    --  1
    gc TEXT,                               --  2
    cc INT,                                --  3
    bc TEXT,                               --  4
    decomposition TEXT,                    --  5
    nv_dec INT,                            --  6
    nv_dig INT,                            --  7
    nv_num INT,                            --  8
    bm TEXT,                               --  9
    alias TEXT,                            -- 10
    -- [obsolete]                          -- 11
    upper_case INT,                        -- 12
    lower_case INT,                        -- 13
    title_case INT                         -- 14
);
CREATE INDEX unicode_name ON unicode(name);
.import --csv UnicodeData.csv unicode
UPDATE unicode SET gc = NULL WHERE gc = "";
UPDATE unicode SET cc = NULL WHERE cc = -1;
UPDATE unicode SET bc = NULL WHERE bc = "";
UPDATE unicode SET decomposition = NULL WHERE decomposition = "";
UPDATE unicode SET nv_dec = NULL WHERE nv_dec = -1;
UPDATE unicode SET nv_dig = NULL WHERE nv_dig = -1;
UPDATE unicode SET nv_num = NULL WHERE nv_num = -1;
UPDATE unicode SET bm = NULL WHERE bm = "";
UPDATE unicode SET alias = NULL WHERE alias = "";
UPDATE unicode SET upper_case = NULL WHERE upper_case = -1;
UPDATE unicode SET lower_case = NULL WHERE lower_case = -1;
UPDATE unicode SET title_case = NULL WHERE title_case = -1;
