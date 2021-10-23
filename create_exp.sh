#!/bin/bash
set -e

usage (){
	echo
	echo $0 -r model_resolution -e experiment_directory_name 
	echo
	echo options:
	echo -r model_resolution : available resolutions are T254
	echo -e experiment_directory_name 
	exit 1;
}


res="T254"
expname=""

if [[ -z "$@" ]]; then
	usage
fi

while getopts 'r:e:' flag; do
    case "${flag}" in
    e) expname=$OPTARG ;;
    r) res=$OPTARG ;;
	*) usage ;;
    esac
done

shift $(($OPTIND - 1))
opts=$@

if [[ -z "$expname" ]]; then
    usage
fi

case "${res}" in
    T254) jcap="254"; t="T"; nlon="512"; nlat="256";;
    *) usage;;
esac

echo '...............Setting up environment.....................'
if [ ! -f .env ]; then
	echo ".env file does not exist. Run init.sh first."
	exit
fi

source .env
source $rootdir/bin/env.$MACH

if [ ! -d "work/$expname" ]; then
    mkdir -p $rootdir/work/$expname
else
    echo "Experiment directory work/$expname already exist"
    exit 1
fi

fixdir=$rootdir/fix
scriptdir=$rootdir/scripts
expdir=$rootdir/work/$expname


cd $expdir/
cp $scriptdir/* $expdir/
cp $fixdir/nml/* $expdir/
sed -i "s/_NLON_/$nlon/g" *
sed -i "s/_NLAT_/$nlat/g" *
sed -i "s/_JCAP_/$jcap/g" *
sed -i "s/_EXPNAME_/$expname/g" *
sed -i "s|_ROOTDIR_|$rootdir|g" *
sed -i "s|_MACH_|$MACH|g" *

mkdir -p $expdir/INPUT
cp $fixdir/diag_table/* $expdir/
cp $fixdir/lonsperlat/lonsperlat.dat.$t$jcap $expdir/lonsperlat.dat
cp $fixdir/vcoord/ak_bk_64l.nc $expdir/INPUT/ak_bk.nc 


echo "Experiment directory created"