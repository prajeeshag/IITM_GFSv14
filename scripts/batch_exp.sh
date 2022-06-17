#!/bin/bash

set -ex

ICs="\
/home/PANL/gefsopera/IC/GFS-IC/gfs.20220602
/home/PANL/gefsopera/IC/GFS-IC/gfs.20220603
/home/PANL/gefsopera/IC/GFS-IC/gfs.20220604
/home/PANL/gefsopera/IC/GFS-IC/gfs.20220605
/home/PANL/gefsopera/IC/GFS-IC/gfs.20220606

"

FHMAX=240

CURR_DIR=$(pwd)
for IC in $ICs; do
    echo $IC
    START_DATE=${IC: -8}
    echo $START_DATE
    ./create_exp.sh -o TCO1534_$START_DATE
    cd work/TCO1534_$START_DATE
    sed -i "s/_STARTDATE_/$START_DATE/g" submit_gfsv14.sh
    sed -i "s|_ICDIR_|$IC|g" submit_gfsv14.sh
    sed -i "s|_FHMAX_|$FHMAX|g" submit_gfsv14.sh
    ./submit_gfsv14.sh chgres_model
    cd $CURR_DIR
done
