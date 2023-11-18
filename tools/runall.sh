#! /usr/bin/env bash

set -e

if [ ! -d tmp_scm ] ; then mkdir tmp_scm ; fi
if [ ! -d out_scm ] ; then  mkdir out_scm ; fi

for i in fn/*.fn ; do
    echo $i
    base=`basename $i .fn`
    if [ "$base" == "env" ] ; then continue ; fi
    if [ "$base" == "prototype" ] ; then continue ; fi
    ./cekf $i 2> tmp_scm/$base.scm
    pp tmp_scm/$base.scm > out_scm/$base.scm
done
