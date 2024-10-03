#! /usr/bin/env bash

TGT=generated/preamble.c
SRC=src/preamble.fn

echo -n 'const char *preamble = ' > $TGT

cat $SRC |
sed -e 's/\\/\\\\/g' -e 's/"/\\"/g' -e 's/^/"/' -e 's/$/\\n"/' >> $TGT

echo ';' >> $TGT
