fnd () {
    grep -Irwn $1 src generated tests
}

run_gdb () {
    gdb -q ./cekf `ls -rt1 /var/lib/apport/coredump/* | tail -1`
}

cores () {
    ulimit -c unlimited
}
