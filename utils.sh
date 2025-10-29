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
    exit 1
}

new_h () {
    file="src/$1.h"
    if [ -e $file ] ; then
        error $file already exists
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
    file="src/$1.c"
    if [ -e $file ] ; then
        error $file already exists
    else
        echo_gpl > $file
        echo "" >> $file
    fi
}

new_ch () {
    cfile="src/$1.c"
    if [ -e $cfile ] ; then
        error $cfile already exists
    else
        new_c $1
        new_h $1
        echo "#include \"$1.h\"" >> $cfile
        echo "" >> $cfile
    fi
}