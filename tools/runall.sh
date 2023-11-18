#! /usr/bin/env bash

set -e

test -d tmp_scm || mkdir tmp_scm
test -d out_scm || mkdir out_scm

for i in fn/*.fn ; do
    echo $i
    base=`basename $i .fn`
    if [ "$base" == "env" ] ; then continue ; fi
    if [ "$base" == "prototype" ] ; then continue ; fi
    ./cekf $i 2> tmp_scm/$base.scm
    pp tmp_scm/$base.scm > out_scm/$base.scm
done
