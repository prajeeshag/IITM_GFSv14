#!/bin/bash 
#PBS -N create_grid 
#PBS -l walltime=1:00:00
#PBS -l nodes=1:ppn=36
#PBS -q cccr
#PBS -V

cd $PBS_O_WORKDIR

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

datadir=/scratch/cccr/prajeesh/GFSv14_aux/fix/fix_am/

. ../../.env 
. $rootdir/bin/env.${MACH}
EXE=$rootdir/exec/preprocessing/mtn_oro/mtn_oro

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
#ln -fs ${datadir}/global_lonsperlat.t${jcap}.${nlon}.${nlat}.txt  fort.20
ln -fs ${datadir}/gtopo30_gg.fine fort.235

echo " mtnres nlon nlat jcap NR NF1 NF2 efac blat "
echo "     $mtnres $nlon $nlat $jcap $NR $NF1 $NF2 $efac $blat "
echo " exec located:  $EXE "
echo " EXECUTION BEGINS for filtered orography ... "
echo $mtnres $nlon $nlat $jcap $NR $NF1 $NF2 $efac $blat | aprun -n1 -d24 $EXE >stdout.filtered 2>&1
ret=$?
echo $ret

rm fort.*
