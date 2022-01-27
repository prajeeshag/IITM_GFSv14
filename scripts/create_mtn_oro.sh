#!/bin/bash 
#PBS -N create_grid 
#PBS -l walltime=1:00:00
#PBS -l nodes=1:ppn=36
#PBS -q cccr
#PBS -V

mtnres=1
grid=TCO
jcap=1534
nlon=6156
nlat=3070
NF1=878
NF2=1536
NR=0
efac=0
blat=0

rootdir=_ROOTDIR_
. $rootdir/bin/env._MACH_
datadir=$rootdir/data
EXE=$rootdir/exec/preprocessing/mtn_oro/mtn_oro

cd $PBS_O_WORKDIR
set -x
export OMP_NUM_THREADS=1
export OMP_STACKSIZE=4G
export FORT_BUFFERED=true
export MKL_NUM_THREADS=1
export MKL_CBWR=AVX
export USEBULKXFER=NO
export MP_MPILIB=mpich2
export MPICH_GNI_COLL_OPT_OFF=MPI_Alltoallv
export IOBUF_PARAMS=${IOBUF_PARAMS:-'*:size=8M:verbose'}
export OMP_NUM_THREADS=${OMP_NUM_THREADS:-24}

ulimit -c unlimited
ulimit -s unlimited
ulimit -a

# ln -fs ${pwd}/a_ocean_mask${lonb}x${latb}.txt  fort.25
ln -fs ${datadir}/landcover30.fixed  fort.10
ln -fs ${datadir}/TOP8M_slm.80I1.asc fort.14
ln -fs ${datadir}/thirty.second.antarctic.new.bin fort.15
ln -fs ${datadir}/lonsperlat.dat_${grid}${jcap}  fort.20
ln -fs ${datadir}/gtopo30_gg.fine fort.235

ln -fs SLM.${grid}$jcap      fort.51
ln -fs ORO.${grid}$jcap      fort.52
ln -sf mtnvar14_${grid}$jcap fort.53
ln -fs ORS.${grid}$jcap      fort.54
ln -fs ORU.${grid}$jcap      fort.55
ln -fs OCLSM.${grid}$jcap    fort.27
ln -sf slmgb.${grid}${jcap}  fort.56
ln -sf orogb.${grid}${jcap}  fort.57
ln -sf thetagb.${grid}${jcap}  fort.58
ln -sf gammagb.${grid}${jcap}  fort.59
ln -sf sigmagb.${grid}${jcap}  fort.60
ln -sf vargb.${grid}${jcap}    fort.61
ln -sf elvmaxgb.${grid}${jcap} fort.62
ln -sf THETA.${grid}$jcap           fort.66
ln -sf GAMMA.${grid}$jcap           fort.67
ln -sf SIGMA.${grid}$jcap           fort.68
ln -sf mtn.${grid}$jcap.ieee        fort.71


echo " mtnres nlon nlat jcap NR NF1 NF2 efac blat "
echo "     $mtnres $nlon $nlat $jcap $NR $NF1 $NF2 $efac $blat "
echo " exec located:  $x "
echo " EXECUTION BEGINS ... "
echo $mtnres $nlon $nlat $jcap $NR $NF1 $NF2 $efac $blat | aprun -n1 -d24 $EXE
ret=$?
exit $ret
