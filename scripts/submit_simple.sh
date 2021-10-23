#!/bin/sh --login

#PBS -N GFSv14_low
###PBS -j oe
#PBS -l walltime=19:00:00
#PBS -l nodes=16:ppn=36 
#PBS -q cccr
#PBS -V

set -xe
export threads=1

WRT_GROUP=${WRT_GROUP:-4}
WRTPE_PER_GROUP=${WRTPE_PER_GROUP:-6}


#############################################################
# Load modules
###  Phani#############
#############################################################
. /opt/cray/pe/modules/3.2.10.6/init/ksh
module load pbs
module switch PrgEnv-cray/6.0.4 PrgEnv-intel
module load fftw
module load cray-netcdf
module load craype-broadwell
module unload cray-libsci
ulimit -c unlimited
ulimit -s unlimited
ulimit -a
module load craype-hugepages16M


#############################################################
# WCOSS_C environment settings
#############################################################
export KMP_AFFINITY=disabled
export OMP_STACKSIZE=1024m
export OMP_NUM_THREADS=$threads
export NTHREADS=$threads

cd $PBS_O_WORKDIR
rm -f OUTPUT.NEMS errfile.NEMS GFSv14_low.*
rm -f FLX.F* LOG.F* NST.F* SIG.F* SFC.F* PET*

#aprun -j1 -n 64 -N 4 -d 4 -cc depth ./gfs.exe 1> OUTPUT.NEMS 2> errfile.NEMS
aprun -j1 -n 64 -N 4 -cc depth ./gfs.exe 1> OUTPUT.NEMS 2> errfile.NEMS

