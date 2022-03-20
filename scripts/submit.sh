#!/bin/sh --login

#PBS -N _EXPNAME_
#PBS -j oe
#PBS -l walltime=19:00:00
#PBS -l nodes=16:ppn=36 
#PBS -q cccr
#PBS -V

set -xe

rootdir=_ROOTDIR_
export threads=6

. $rootdir/bin/env._MACH_

ulimit -c unlimited
ulimit -s unlimited
ulimit -a
module load craype-hugepages16M

export KMP_AFFINITY=disabled
export OMP_STACKSIZE=1024m
export OMP_NUM_THREADS=$threads
export NTHREADS=$threads

cd $PBS_O_WORKDIR

EXE=_EXE_

aprun -j1 -n 64 -N 4 -cc depth $EXE 1> OUTPUT.NEMS 2> errfile.NEMS
