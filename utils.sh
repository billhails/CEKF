fnd () {
    grep -Irwn $1 src generated tests
}

run_gdb () {
    gdb -q ./cekf `ls -rt1 /var/lib/apport/coredump/* | tail -1`
}

cores () {
    ulimit -c unlimited
}

no_cores () {
    ulimit -c 0
}

watch_make () {
    inotifywait -q -e close_write -m ./src |
    while read -r directory events filename; do
        if [ -e ./src/$filename ] ; then
            make
        fi
    done
}
