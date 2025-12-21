fnd () {
    grep -Irwn $1 src
}

fnd_all () {
    grep -Irwn $1 src generated tests
}

run_gdb () {
    gdb -q ./bin/fn core
}

cores_on () {
    sudo bash -c "echo 'core.%e' > /proc/sys/kernel/core_pattern"
    ulimit -c unlimited
}

cores_off () {
    ulimit -c 0
}

echo_gpl () {
    echo '/*'
    cat docs/gpl | sed -e 's/^/ * /'
    echo ' */'
}

watch_make () {
    inotifywait -q -e close_write -m ./src |
    while read -r directory events filename; do
        if [ -e ./src/$filename ] ; then
            make
        fi
    done
}

error () {
    echo 1>&2 "$*"
}

new_h () {
    if [ -z "$1" ] ; then
        error usage: new_h filename_without_extension
        return 1
    fi
    file="src/$1.h"
    if [ -e $file ] ; then
        error $file already exists
        return 1
    else
        define=`echo "cekf_${1}_h" | sed -e 's#/#_#g'`
        echo "#ifndef $define" > $file
        echo "#  define $define" >> $file
        echo_gpl >> $file
        echo "" >> $file
        echo "#endif" >> $file
    fi
}

new_c () {
    if [ -z "$1" ] ; then
        error usage: new_c filename_without_extension
        return 1
    fi
    file="src/$1.c"
    if [ -e $file ] ; then
        error $file already exists
        return 1
    else
        echo_gpl > $file
        echo "" >> $file
    fi
}

new_ch () {
    if [ -z "$1" ] ; then
        error usage: new_ch filename_without_extension
        return 1
    fi
    cfile="src/$1.c"
    if [ -e $cfile ] ; then
        error $cfile already exists
        return 1
    else
        new_c $1
        new_h $1
        echo "#include \"$1.h\"" >> $cfile
        echo "" >> $cfile
    fi
}

new_visitor () {
    if [ -z "$1" ] || [ -z "$2" ] ; then
        error usage: new_visitor name suffix
        return 1
    fi
    cfile="src/$1_$2.c"
    if [ -e $cfile ] ; then
        error $cfile already exists
        return 1
    fi
    new_h "$1_$2" &&
    python3 tools/generate.py --suffix "$2" src/$1.yaml visitor > $cfile
}