#!/bin/bash
set -e

usage (){
	echo
	echo $0 -r model_resolution -e exec_type -o experiment_directory_name 
	echo
	echo "options:"
	echo "-r model_resolution : available resolutions are T254 TCO765 TCO1534 [default: TCO1534]"
    echo "-e exec_type: real or hs_forcing [default: real]"
	echo "-o experiment_directory_name"
	exit 1;
}

. .env

res="TCO1534"
expname=""
EXETYPE="real"

if [[ -z "$@" ]]; then
	usage
fi


while getopts 'r:e:o:' flag; do
    case "${flag}" in
    o) expname=$OPTARG ;;
    r) res=$OPTARG ;;
    e) EXETYPE=$OPTARG ;;
	*) usage ;;
    esac
done

shift $(($OPTIND - 1))
opts=$@

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


nmldir=$rootdir/nml_tbl
scriptdir=$rootdir/scripts
EXPDIR=$rootdir/work/$expname
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