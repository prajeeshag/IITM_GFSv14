#!/bin/bash
set -e

usage()
{
  echo "Usage: $0 -o | --exp_name <experiment_name>
                [ -r | --resolution <resolution> default: TCO1534 ]
                [ -e | --exetype <experiment type> default: real ]
                [ --rundir <path_to_rundir> ]
                [ --force ]
        "
  exit 2
}

basedir=$(dirname "$0")

res="TCO1534"
expname=""
EXETYPE="real"
force="false"


VALID_ARGS=$(getopt -o o:r:e: --long exp_name:,resolution:,exp_type:,rundir:,force -- "$@")
if [[ $? -ne 0 ]]; then
    usage;
fi

eval set -- "$VALID_ARGS"
while [ : ]; do
  case "$1" in
    -o | --exp_name)
        expname=$2
        shift 2
        ;;
    -r | --resolution)
        res=$2
        shift 2
        ;;
    -e | --exp_type)
        EXETYPE=$2
        shift 2
        ;;
    --rundir)
        EXPDIR=$2
        shift 2
        ;;
    --force)
        force="true"
        shift 1
        ;;
    --) shift; 
        break 
        ;;
  esac
done


case "${EXETYPE}" in
	hs_forcing) EXETYPEdir="HS" ;;
	real) EXETYPEdir="real" ;;
	*) usage ;;
esac

if [[ -z "$expname" ]]; then
    usage
fi

case "${res}" in
    T254) JCAP="254"; t="T"; NLON="512"; NLAT="256";;
    TCO765) JCAP="765"; t="TCO"; NLON="3088"; NLAT="1536";;
    TCO1534) JCAP="1534"; t="TCO"; NLON="6156"; NLAT="3070";;
    *) usage;;
esac

echo '...............Setting up environment.....................'

if [ ! -f $basedir/.env ]; then
	echo ".env file does not exist. Run init.sh first."
	exit 1
fi

source $basedir/.env
source $rootdir/bin/env.$MACH

EXPDIR=${EXPDIR:-"$rootdir/work/$expname"}

if [[ ! -d "$EXPDIR" ]] || [[ "$force" == "true" ]]; then
    mkdir -p $EXPDIR
else
    echo "Experiment directory $EXPDIR already exist"
    exit 1
fi

nmldir=$rootdir/nml_tbl
scriptdir=$rootdir/scripts
EXE=$rootdir/exec/$EXETYPEdir/gfs/GSM/nems/gfs.exe
CHGRES_EXE=$rootdir/exec/preprocessing/chgres/chgres

cd $EXPDIR/
cp $scriptdir/submit_gfsv14.sh $EXPDIR/
cp $scriptdir/run_mppnccombine.sh $EXPDIR/
sed -i "s/_NLON_/$NLON/g" submit_gfsv14.sh 
sed -i "s/_NLAT_/$NLAT/g" submit_gfsv14.sh 
sed -i "s/_JCAP_/$JCAP/g" submit_gfsv14.sh
sed -i "s/_EXPNAME_/$expname/g" submit_gfsv14.sh
sed -i "s|_ROOTDIR_|$rootdir|g" submit_gfsv14.sh
sed -i "s|_EXE_|$EXE|g" submit_gfsv14.sh
sed -i "s|_CHGRESEXE_|$CHGRES_EXE|g" submit_gfsv14.sh
sed -i "s|_MACH_|$MACH|g" submit_gfsv14.sh
cp $nmldir/diag_table/* $EXPDIR/

mkdir -p $EXPDIR/INPUT
mkdir -p $EXPDIR/OUTPUT


echo "Experiment $expname created"
echo "Directory = $EXPDIR"
echo "Resolution = $res"