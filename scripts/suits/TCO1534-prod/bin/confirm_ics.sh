#!/bin/bash

# Check if the ICs exist and if the file operation on them are compeleted

dstamp=${CYLC_TASK_CYCLE_POINT:0:8}
idir="$IC_DIR/gfs.${dstamp}"

files="gfs.t00z.sfcanl.nemsio gfs.t00z.nstanl.nemsio gfs.t00z.atmanl.nemsio"

for f in $files; do
    fpath=${idir}/${f}
    echo "Checking for $fpath $CYLC_TASK_CYCLE_POINT"
    if [[ ! -f "$fpath" ]]; then
        echo "File $fpath doesn't exist"
        exit 1
    fi

    date1=$(stat -c %y "$fpath")
    sleep 5
    date2=$(stat -c %y "$fpath")
    if [[ "$date1" != "$date2" ]]; then
        echo "File $fpath is still getting updated"
        exit 1
    fi
done
exit 0