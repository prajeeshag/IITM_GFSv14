#!/bin/sh --login

#PBS -N GFS-chgres
#PBS -l select=1:ncpus=16
#PBS -q cccr
#PBS -l walltime=1:00:00 

set -x
export cyc=00
export ICdir=/scratch/cccr/prajeesh/GFS_IC_SL/nemsio_20180810 # path to IC files to be regridded

export cycle=t${cyc}z
export SIGINP=$ICdir/gfs.$cycle.atmanl.nemsio  # input sig file
export SFCINP=$ICdir/gfs.$cycle.sfcanl.nemsio  # input sfc file
export NSTINP=$ICdir/gfs.$cycle.nstanl.nemsio  # input nst file



#############################################################
cd $PBS_O_WORKDIR
export NODES=1
export ntasks=1
export ptile=6
export threads=6
ROOTDIR=/scratch/cccr/prajeesh/ShortRange/GFSv14_HS/IITM_GFSv14

. $ROOTDIR/.env

export FIXgsm=$FIXDIR  # path to fix files
export RUN_ENVIR=para
export job=gfs_forecast_high_${cyc}
export pid=${pid:-$$}
export jobid=${job}.${pid}
export envir=para
export DATA=./


#############################################################
# Specify versions
#############################################################
export global_shared_ver=v14.1.3
export gfs_ver=v14.1.1
export grib_util_ver=1.0.3
export prod_util_ver=1.0.5


#############################################################
# Load modules
###  Phani#############
#############################################################
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


#############################################################
# Set user specific variables
#############################################################
export PARA_CONFIG=$ROOTDIR/scripts/chgres/para_config
export GFS_PARM=$ROOTDIR/scripts/chgres/gfs_forecast_low.parm
export GLOBAL_CHGRES=$ROOTDIR/scripts/chgres/global_chgres.sh
export CHGRESEXEC=$ROOTDIR/exec/preprocessing/chgres/chgres
export GFS_INPUT_NML=$ROOTDIR/nml_tbl/gfs_input.nml

export NWROOT=/home/SSPMRES/rphani/models/GSMv14
export NWPROD=$NWROOT
#############################################################
# Set user specific variables
#############################################################
export wave=1534
export JCAP=$wave
export LEVS=64
export LONB=6156
export LATB=3070
export NTRAC=3
export MTNRSL=${wave}.${LONB}.${LATB}
export semilag=.true.
export SLG=.true.
export TCO=.true.
export DELTIM=450
export DTPHYS=225
export LONR=$LONB            # Number of Physics Longitudes
export LATR=$LATB             # Number of Physics Latitudes
export LONF=$LONB
export LATG=$LATB

#############################################################
# Execute job
#############################################################
. $ROOTDIR/scripts/chgres/change_resol

exit

