#!/bin/bash

# Check if the ICs exist and if the file operation on them are compeleted

dstamp=${CYLC_TASK_CYCLE_POINT:0:8}
idir="$IC_DIR/gfs.${dstamp}"
rdir="/home/gfsprod/data/gdasv14/gdas/prod/gdas.${dstamp}"
server="iitmgefs@10.119.105.5"

# files="gfs.t00z.sfcanl.nemsio gfs.t00z.nstanl.nemsio gfs.t00z.atmanl.nemsio"

filesStr="sfcanl nstanl atmanl"

# ssh iitmgefs@10.119.105.5 stat -c %y ${rdir}/gdas1.t00z.atmanl.nemsio
mkdir -p $idir

for fs in $filesStr; do

    rfpath=${rdir}/gdas1.t00z.${fs}.nemsio
    echo "Checking for $server $rfpath"
    date1=$(ssh $server stat -c %y ${rfpath}) || (echo $date1; exit 1)
    sleep 5
    date2=$(ssh $server stat -c %y ${rfpath}) || (echo $date2; exit 1)
    if [[ "$date1" != "$date2" ]]; then
        echo "File $rfpath is still getting updated..."
        exit 1
    fi

done

for fs in $filesStr; do
    rfpath=${rdir}/gdas1.t00z.${fs}.nemsio
    ifpath=${idir}/gfs.t00z.${fs}.nemsio
    scp ${server}:${rfpath} ${ifpath} || (echo "Error while coping $rfpath"; exit 1) || exit 1
done

exit 0