#!/bin/sh --login

#PBS -N GFS-chgres
###PBS -l select=1:ncpus=16:mem=250GB
#PBS -l select=1:ncpus=16
#PBS -q cccr
#PBS -l walltime=1:00:00 

set -x
export NODES=1
export ntasks=1
export ptile=6
export threads=6

export CDATE=2018081000

#############################################################
# Specify whether the run is production or development
#############################################################
export RUN_ENVIR=para
export PDY=`echo $CDATE | cut -c1-8`
export cyc=`echo $CDATE | cut -c9-10`
export job=gfs_forecast_high_${cyc}
export pid=${pid:-$$}
export jobid=${job}.${pid}
export envir=para
export DATAROOT=/scratch/cccr/prajeesh/ShortRange/GFSv14_HS/IITM_GFSv14/work/ptmp


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
module list


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
export NWROOT=/home/SSPMRES/rphani/models/GSMv14
export NWPROD=$NWROOT
export PARA_CONFIG=/scratch/cccr/prajeesh/ShortRange/GFSv14_HS/IITM_GFSv14/src/preprocessing/chres/para_config
export PARMgfs=/home/SSPMRES/rphani/test/create_tco_IC
export JOBGLOBAL=$NWROOT/gfs.${gfs_ver}/jobs

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

export CHGRESEXEC=/home/SSPMRES/rphani/models/GSMv14/global_chgres.v14.1.1/sorc/global_chgres

#############################################################
# Execute job
#############################################################
#$JOBGLOBAL/JGFS_FORECAST_HIGH
. /scratch/cccr/prajeesh/ShortRange/GFSv14_HS/IITM_GFSv14/src/preprocessing/chres/change_resol

exit

