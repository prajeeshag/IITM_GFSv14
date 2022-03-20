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

cd $EXPDIR/
cp $scriptdir/submit.sh $EXPDIR/
cp $scriptdir/run_mppnccombine.sh $EXPDIR/
cp $nmldir/nml/* $EXPDIR/
sed -i "s/_NLON_/$NLON/g" *
sed -i "s/_NLAT_/$NLAT/g" *
sed -i "s/_JCAP_/$JCAP/g" *
sed -i "s/_EXPNAME_/$expname/g" *
sed -i "s|_ROOTDIR_|$rootdir|g" *
sed -i "s|_EXE_|$EXE|g" *
sed -i "s|_MACH_|$MACH|g" *
cp $nmldir/diag_table/* $EXPDIR/

mkdir -p $EXPDIR/INPUT
mkdir -p $EXPDIR/OUTPUT


ln -sf $FIXDIR/ak_bk_64l.nc $EXPDIR/INPUT/ak_bk.nc
ln -sf $FIXDIR/global_lonsperlat.t$JCAP.$NLON.$NLAT.txt $EXPDIR/lonsperlat.dat

if [[ "$EXETYPE" == "real" ]]; then
    ln -sf $FIXDIR/global_mtnvar.t$JCAP.$NLON.$NLAT.nc $EXPDIR/global_mtnvar.nc
    ln -sf $FIXDIR/global_orography.t$JCAP.$NLON.$NLAT.nc $EXPDIR/orography.nc
    ln -sf $FIXDIR/global_orography_uf.t$JCAP.$NLON.$NLAT.nc $EXPDIR/orography_uf.nc
    ln -sf $FIXDIR/global_o3prdlos.f77 $EXPDIR/global_o3prdlos.f77
    ln -sf $FIXDIR/global_climaeropac_global.txt $EXPDIR/aerosol.dat
    ln -sf $FIXDIR/global_solarconstant_noaa_an.txt $EXPDIR/solarconstant_noaa_an.txt
    ln -sf $FIXDIR/fix_co2_proj/global_co2historicaldata_2018.txt $EXPDIR/co2historicaldata_2018.txt

    ln -sf $FIXDIR/global_vegtype.igbp.t1534.3072.1536.rg.grb $EXPDIR/INPUT/global_vegtype.igbp.rg.grb
    ln -sf $FIXDIR/global_soiltype.statsgo.t1534.3072.1536.rg.grb $EXPDIR/INPUT/global_soiltype.statsgo.rg.grb
    ln -sf $FIXDIR/global_mxsnoalb.uariz.t1534.3072.1536.rg.grb $EXPDIR/INPUT/global_mxsnoalb.uariz.rg.grb
    ln -sf $FIXDIR/global_snowfree_albedo.bosu.t1534.3072.1536.rg.grb $EXPDIR/INPUT/global_snowfree_albedo.bosu.rg.grb
    ln -sf $FIXDIR/global_soilmgldas.t1534.3072.1536.grb $EXPDIR/INPUT/global_soilmgldas.grb
    ln -sf $FIXDIR/global_albedo4.1x1.grb $EXPDIR/INPUT/global_albedo4.1x1.grb
    ln -sf $FIXDIR/global_tg3clim.2.6x1.5.grb $EXPDIR/INPUT/global_tg3clim.2.6x1.5.grb
    ln -sf $FIXDIR/global_vegfrac.0.144.decpercent.grb $EXPDIR/INPUT/global_vegfrac.0.144.decpercent.grb
    ln -sf $FIXDIR/seaice_newland.grb $EXPDIR/INPUT/seaice_newland.grb
    ln -sf $FIXDIR/global_glacier.2x2.grb $EXPDIR/INPUT/global_glacier.2x2.grb
    ln -sf $FIXDIR/global_maxice.2x2.grb $EXPDIR/INPUT/global_maxice.2x2.grb
    ln -sf $FIXDIR/global_shdmin.0.144x0.144.grb $EXPDIR/INPUT/global_shdmin.0.144x0.144.grb
    ln -sf $FIXDIR/global_shdmax.0.144x0.144.grb $EXPDIR/INPUT/global_shdmax.0.144x0.144.grb
    ln -sf $FIXDIR/global_slope.1x1.grb $EXPDIR/INPUT/global_slope.1x1.grb
    ln -sf $FIXDIR/RTGSST.1982.2012.monthly.clim.grb $EXPDIR/INPUT/RTGSST.1982.2012.monthly.clim.grb
    ln -sf $FIXDIR/CFSR.SEAICE.1982.2012.monthly.clim.grb $EXPDIR/INPUT/CFSR.SEAICE.1982.2012.monthly.clim.grb
    ln -sf $FIXDIR/global_snoclim.1.875.grb $EXPDIR/INPUT/global_snoclim.1.875.grb
fi

echo "Experiment $expname created"
echo "Directory = $EXPDIR"
echo "Resolution = $res"