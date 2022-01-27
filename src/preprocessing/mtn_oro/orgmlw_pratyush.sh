#!/bin/bash 
#PBS -N create_grid 
#PBS -l walltime=1:00:00
##PBS -l nodes=3:ppn=36
##PBS -l select=2:ncpus=36:vntype=cray_compute -l place=scatter 
#PBS -l nodes=2:ppn=36
#PBS -q cccr
#PBS -V

cd $PBS_O_WORKDIR
set -x
export OMP_NUM_THREADS=1
export OMP_STACKSIZE=4G
export FORT_BUFFERED=true
export MKL_NUM_THREADS=1
export MKL_CBWR=AVX
export USEBULKXFER=NO
ulimit -c unlimited
ulimit -s unlimited
ulimit -a

#
# Script history log:
# 1999-05-01  Mark Iredell
# 2000-02-14  S    Moorthi
# 2001-12-14  J    Alpert  (*j*)
# 2004-05-12  J    Alpert  (*j*) fix for E-W gaussian grid pt shift
# 2004-12-06  J    Alpert  (*j*)  script input settings for spect filter
# 2005-03-21  J    Alpert  (*j*)  Added GrumbineICE to orog/slm...
#
# W/Lott & Miller terrain principal coord. (*j*)
#
#Usage: orgmlw.sh slmgb orogb mtnvar14 nlon nlat jcap filter1 filter2 mtnres
# Normally: filter1~1/3 ((jcap/3)-1))
# Normally: filter2~jcap+2))
# Normally: mtnres=8 minute only (do not use =4, =2 except at own risk)
#           now =1 is 30" others are turned off.  see below
# New run mtnlm7 for mtnres=1 set for 30"
#   script changed like ml4b for spect filter input, otherwise same as ml2b
#   Input script fortran  positional parameters:
#     1             output sea-land mask GRIB file
#     2             output orography GRIB file
#     3             output 14-field mountain variance file
#     4             number of Gaussian longitudes
#     5             number of Gaussian latitudes
#     6             spectral triangular truncation
#     7             Envelope orography factor
#     8             Begining latitude (used only for nongaussian grid -
#                                      used only for switching north/south)
#     9             Mountain data resolution
#
#   Imported Shell Variables:
#     WRKDIR        working directory
#                   defaults to a directory that is made, used and deleted
#     FIXDIR        fix directory
#                   defaults to /gloptmp/fix
#     TERRAINSORC   terrain source file
#                   defaults  
#                   now this defaults to the local dir
#     LONSPERLAT    input lonsperlat text file (if it exists)
#                   defaults to $FIXDIR/global_lonsperlat.t$6.txt
#     VERBOSE       verbose flag (YES or NO)
#                   defaults to NO
#
#   Modules and files referenced:
#
#     source     : ${TERRAINSORC} or 
#                  ops(20060822)w/GICE 
#
#     input data : /ptmp/wx23ja/terr05/markr/gtopo30_gg.fine output array
#                  /global/noscrub/wx23ja/terr05/markr/gtopo30_gg.fine
#                    about 2GB fort.235
#                  /gloptmp/fix/global_lonsperlat.t$6.txt
#
#     output data: $1
#                  $2
#                  $3
#
#     scratch    : ${WRKDIR}/terrain00.xd
#                  ${WRKDIR}/fort.11
#                  ${WRKDIR}/fort.12
#                  ${WRKDIR}/fort.13
#                  ${WRKDIR}/fort.14
#                  ${WRKDIR}/fort.20
#                  ${WRKDIR}/fort.27
#                  ${WRKDIR}/fort.51
#                  ${WRKDIR}/fort.52
#                  ${WRKDIR}/fort.53
#                  ${WRKDIR}/fort.54
#                  ${WRKDIR}/fort.55
#                  ${WRKDIR}/fort.56
#                  ${WRKDIR}/fort.57
#                  ${WRKDIR}/fort.71
#
# Remarks:
#
#   Condition codes
#      0 - no problem encountered
#     >0 - some problem encountered
#
# Attributes:
#   Language: POSIX shell
#   Machine: IBM SP
#
####
echo " ===== -----------   MTN BLOCKING/GWD/OROG ------------  "
echo " ===== first_dir=$first_dir "
echo " ===== local_dir=$local_dir "
echo " =====     jcap =$jcap  "
echo " =====      lonb=$lonb  "
echo " =====      latb=$latb  "
echo " =====     filt1=$filt1 "
echo " =====     filt2=$filt2 "


################################################################################
# Check arguments
#if [[ $# -ne 9 ]];then
# echo Usage: $0 slmgb orogb mtnvar14 IM JM NM filter1 filter2 MTNRES >&2
# exit 1
#fi
#
# VERBOSE = YES means debug mode
#
# export VERBOSE=${VERBOSE:-"NO"}
export VERBOSE=${VERBOSE:-"YES"}
if [[ "$VERBOSE" = "YES" ]];then
 echo $(date) EXECUTING $0 $* >&2
# set -x
fi
pwd=$(pwd)
echo $pwd
oc_slmgb=oc_slmgb
l1=$oc_slmgb;[[ $l1 = / || $l1 = ~ ]] || oc_slmgb=$pwd/$oc_slmgb
# --- $oc_slmgb is ocean grib land mask output
##typeset -L1 l1
#slmgb=$1
slmgb=slmgb${JCAP}
l1=$slmgb;[[ $l1 = / || $l1 = ~ ]] || slmgb=$pwd/$slmgb
echo " ===== slmgb=$slmgb   "
#orogb=$2
orogb=orogb${JCAP}
l1=$orogb;[[ $l1 = / || $l1 = ~ ]] || orogb=$pwd/$orogb
echo " ===== orogb=$orogb   "
#mtnvar14=$3
mtnvar14=mtnvar14_${JCAP}
l1=$mtnvar14;[[ $l1 = / || $l1 = ~ ]] || mtnvar14=$pwd/$mtnvar14
echo " ===== mtnvar14=$mtnvar14   "
#nlon=$4
nlon=$lonb
echo " =====      nlon=$nlon  "
#nlat=$5
nlat=$latb
echo " =====      nlat=$nlat  "
#jcap=$6
#jcap=574
#### efac=$7
#### blat=$8
efac=0
blat=0
#### mtnres=$9
#export mtnres=${9:-"8"}
export mtnres=1
#### export NF1=${NF1:-$(($jcap+1))}
#### export NF2=${NF2:-$(($jcap+2))}
#export NF1=${7:-$(($jcap+1))}
#export NF2=${8:-$(($jcap+2))}
export NF1=$filt1
export NF2=$filt2
echo " =====     NF1=$NF1 "
echo " =====     NF2=$NF2 "

NR=0
echo " ===== -----------   MTN BLOCKING/GWD/OROG ------------  "
echo
 echo "Usage: $0 $slmgb $orogb $mtnvar14 $nlon $nlat $jcap $NF1 $NF2  $MTNRES "
echo "   efac=$efac  blat=$blat  NF1=$NF1   NF2=$NF2 "
echo " _______________________________________________  "

#
#  file names for Prin Coord dataset grib output
#
thetagb=thetagb
l1=$thetagb;[[ $l1 = / || $l1 = ~ ]] || thetagb=$pwd/$thetagb
gammagb=gammagb
l1=$gammagb;[[ $l1 = / || $l1 = ~ ]] || gammagb=$pwd/$gammagb
sigmagb=sigmagb
l1=$sigmagb;[[ $l1 = / || $l1 = ~ ]] || sigmagb=$pwd/$sigmagb
vargb=vargb
l1=$vargb;[[ $l1 = / || $l1 = ~ ]] || vargb=$pwd/$vargb
elvmaxgb=elvmaxgb
l1=$elvmaxgb;[[ $l1 = / || $l1 = ~ ]] || elvmaxgb=$pwd/$elvmaxgb
#
export WRKDIR=/home/SSPMRES/rphani/test/mtnvar_to_nc/ptmp
export WRKDIR=${WRKDIR}

export FIXDIR=/home/SSPMRES/rphani/GFSv14_TCO/global_shared.v14.1.3/fix/fix_am
export LONSPERLAT=${FIXDIR}/global_lonsperlat.t${jcap}.${lonb}.${latb}.txt
# make sure, for now, that LONSPERLAT is null
LONSPERLAT=""
##export SORCDIR=/global/noscrub/Jordan.Alpert/terr/ocean_lm
 export SORCDIR=${pwd}
 export TERRAINSORC=${TERRAINSORC:-${SORCDIR}/mtnlm7_oclsm_grib.f}
 #export TERRAINSORC=${TERRAINSORC:-${SORCDIR}/mtnlm7_oclsm.f90}
echo " ===== TERRAINSORC=$TERRAINSORC "
################################################################################
# Make working directory
stmp=/home/SSPMRES/rphani/test/mtnvar_to_nc/ptmp
mkdir -p $WRKDIR
cd $WRKDIR
/bin/rm *.x
pwd
# --- hardwired ocean landsea mask ...  and use lonb, latb
# --- oc_landsea mask input file from ocean model netcdf convert to binary
ls -l a_ocean_mask${lonb}x${latb}.txt
#ln -fs /global/noscrub/Jordan.Alpert/terr/ocean_lm/testlm.bin fort.25
##ln -fs /global/noscrub/Jordan.Alpert/terr/landmask/landmask.txt fort.25
##ln -fs /global/noscrub/Jordan.Alpert/terr/landmask/a_ocean_mask${lonb}x${latb}.txt  fort.25
  ln -fs ${pwd}/a_ocean_mask${lonb}x${latb}.txt  fort.25
  echo  " ===== UMC 30 sec landcover fort.10=${pwd}/landcover30.fixed"
  ln -fs ${pwd}/landcover30.fixed  fort.10
#
##MTN_SLM=/global/noscrub/Jordan.Alpert/terrain/TOP8M_slm.80I1.asc
MTN_SLM=${pwd}/TOP8M_slm.80I1.asc
ln -fs $MTN_SLM       fort.14
##ln -fs /global/noscrub/Jordan.Alpert/terrain30/thirty.second.antarctic.new.bin fort.15
ln -fs ${pwd}/thirty.second.antarctic.new.bin fort.15
#
ln -fs ${FIXDIR}/global_lonsperlat.t${jcap}.${lonb}.${latb}.txt            fort.20
ln -fs SLM.T$jcap             fort.51
ln -fs ORO.T$jcap             fort.52
ln -sf mtnvar14_$jcap          fort.53
ln -fs ORS.T$jcap             fort.54
ln -fs ORU.T$jcap             fort.55
ln -fs OCLSM.T$jcap           fort.27
#ln -sf oc_$slmgb                 fort.27
ln -sf $slmgb                 fort.56
ln -sf $orogb                 fort.57
ln -sf $thetagb               fort.58
ln -sf $gammagb               fort.59
ln -sf $sigmagb               fort.60
ln -sf $vargb                 fort.61
ln -sf $elvmaxgb              fort.62
ln -sf THETA.T$jcap           fort.66
ln -sf GAMMA.T$jcap           fort.67
ln -sf SIGMA.T$jcap           fort.68
ln -sf mtn.T$jcap.ieee        fort.71

##ln -fs /global/noscrub/Jordan.Alpert/terr05/markr/gtopo30_gg.fine fort.235
ln -fs ${pwd}/gtopo30_gg.fine fort.235

#
module unload craype-sandybridge craype-ivybridge craype-haswell
module load craype-broadwell
module swap PrgEnv-cray PrgEnv-intel
module swap intel  intel/16.0.3.210
module load cray-netcdf
module load cray-hdf5
module unload craype-hugepages8M

CF=ifort 
#FFOPTS="-auto -O -r8 -openmp -convert big_endian "
FFOPTS="-qopenmp -convert big_endian -mcmodel=large"
#export W3EMC_LIBd=/home/apps/NWPROD/test/global_shared.v14.1.3/sorc/ncep_post.fd/nwprod/lib/libw3emc_d.a
#export W3EMC_INCd=/home/apps/NWPROD/test/global_shared.v14.1.3/sorc/ncep_post.fd/nwprod/lib/incmod/w3emc_d
#export W3NCO_LIBd=/home/apps/NWPROD/test/global_shared.v14.1.3/sorc/ncep_post.fd/nwprod/lib/libw3nco_d.a
export W3EMC_LIBd=/home/apps/CFS_T382/cfs.v2.1.18-rfp/lib/sorc/pmb/codes/nwprod/lib/libw3emc_4.a
export W3EMC_INCd=/home/apps/CFS_T382/cfs.v2.1.18-rfp/lib/sorc/pmb/codes/nwprod/lib/incmod/incmod/w3emc_d
export W3NCO_LIBd=/home/apps/CFS_T382/cfs.v2.1.18-rfp/lib/sorc/pmb/codes/nwprod/lib/libw3nco_4.a
export BACIO_LIB4=/home/apps/NWPROD/test/global_shared.v14.1.3/sorc/ncep_post.fd/nwprod/lib/libbacio_4.a
#export SP_LIB4d=/home/apps/NWPROD/test/global_shared.v14.1.3/sorc/ncep_post.fd/nwprod/lib/libsp_v2.0.2_d.a
#export SP_LIB4d=/home/apps/CFS_T382/cfs.v2.1.18-rfp/lib/sorc/pmb/codes/nwprod/lib/libsp_v2.0.2_d.a
export SP_LIB4d=/home/SSPMRES/rphani/test/mtnvar_to_nc/nceplibs/lib/sorc/libsp_v2.0.2_4.a
#export SP_LIB4d=/home/apps/NWPROD/test/global_shared.v14.1.3/sorc/ncep_post.fd/nwprod/lib/libsp_4.a
#export IP_LIBd=/home/apps/NWPROD/test/global_shared.v14.1.3/sorc/ncep_post.fd/nwprod/lib/libip.a
export IP_LIBd=/home/apps/NWPROD/test/global_shared.v14.1.3/sorc/ncep_post.fd/nwprod/lib/libip_4.a
#LIBS="${W3EMC_LIBd} ${W3NCO_LIBd} ${BACIO_LIB4} ${SP_LIB4d} ${IP_LIBd}"
#LIBS="${BACIO_LIB4} ${SP_LIB4d} ${IP_LIBd} ${W3EMC_LIBd} ${W3NCO_LIBd}"
LIBS="/home/apps/NWPROD/test/global_shared.v14.1.3/sorc/ncep_post.fd/nwprod/lib/libbacio_4.a /home/SSPMRES/rphani/lib/sp/v2.0.2/libsp_v2.0.2_4.a /home/apps/NWPROD/test/global_shared.v14.1.3/sorc/ncep_post.fd/nwprod/lib/libip.a /home/SSPMRES/rphani/lib/w3nco/v2.0.6/intel/libw3nco_v2.0.6_4.a /home/SSPMRES/rphani/lib/w3emc/v2.2.0/src/intel/libw3emc__4.a"
#LIBS="/home/apps/NWPROD/test/global_shared.v14.1.3/sorc/ncep_post.fd/nwprod/lib/libbacio_4.a /home/SSPMRES/rphani/lib/sp/v2.0.2/libsp_v2.0.2_d.a /home/SSPMRES/rphani/lib/w3nco/v2.0.6/intel/libw3nco_v2.0.6_d.a /home/SSPMRES/rphani/lib/w3emc/v2.2.0/src/intel/libw3emc__4.a"
f=$TERRAINSORC
x=$WRKDIR/ml01rg2.x
echo "$CF $FFOPTS $f $LIBS $LDOPTS -o $x"
$CF -traceback $FFOPTS -I${W3EMC_INCd} $f $LIBS $LDOPTS -o $x||exit 1
export MKL_NUM_THREADS=1
export MKL_CBWR=AVX
export MP_MPILIB=mpich2
export USEBULKXFER=NO
export MPICH_GNI_COLL_OPT_OFF=MPI_Alltoallv
export IOBUF_PARAMS=${IOBUF_PARAMS:-'*:size=8M:verbose'}
export OMP_NUM_THREADS=${OMP_NUM_THREADS:-24}

echo " mtnres nlon nlat jcap NR NF1 NF2 efac blat "
echo "     $mtnres $nlon $nlat $jcap $NR $NF1 $NF2 $efac $blat "
echo " exec located:  $x "
echo " EXECUTION BEGINS ... "
echo $mtnres $nlon $nlat $jcap $NR $NF1 $NF2 $efac $blat |aprun -n1 -d24 $x
ret=$?
 if [[ "$VERBOSE" = "YES" ]];then 
    echo ret=$ret
#   copy files from working dir to present
    echo  " =====  cp $WRKDIR/m* ${pwd}/. "
 fi
cp $WRKDIR/mtn*${jcap} ${pwd}/.
cp  ORO.T$jcap ${pwd}/.
cp  SLM.T$jcap ${pwd}/.
cp  OCLSM.T$jcap  ${pwd}/.
# this will get the mtnvar_14 and mtn.ieee file for grads use
# the ...gb files are present directly - from starting local dir.
# the other files working files are left to be copied by the user.
# Remove working directory
if [[ "$VERBOSE" = "YES" ]];then
     pwd
     ls -l
     echo "  orgmlw.sh: setting MKWRKDIR = NO  " 
     MKWRKDIR=NO
fi
# 
cd $pwd
[[ $MKWRKDIR = YES ]] && rm -rf $WRKDIR
set +x
if [[ "$VERBOSE" = "YES" ]];then
 echo " $(date) EXITING $0 with return code $ret >&2 "
fi
exit $ret
