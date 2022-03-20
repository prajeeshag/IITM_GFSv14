#!/bin/sh --login
###################  Phani 2018  ###############################
#PBS -N GFS-TCO1534_post
###PBS -l nodes=24:ppn=36 
#PBS -l select=24:ncpus=36:vntype=cray_compute -l place=scatter
#PBS -q cccr
#PBS -l walltime=2:00:00 

set -x
###  Phani#############
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
export CDIR=/scratch/cccr/prajeesh/ShortRange/GFSv14_HS/IITM_GFSv14/work/realTCO1534_N/OUTPUT
export DATAROOT=${CDIR}/ptmp


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
#module load craype-hugepages16M
module load craype-hugepages256M
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
export NWROOT=/scratch/cccr/prajeesh
export NWPROD=/scratch/cccr/prajeesh
export PARA_CONFIG=${CDIR}/para_config
export JOBGLOBAL=$NWROOT/gfs.${gfs_ver}/jobs

export COMROOT=${CDIR}
############################################################
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
export DELTIM=450
export DTPHYS=225

 # Estimate the number of Hours the post to be done
  hourmax=120
  nlist=""
  hours=0
  while (( hours < hourmax ))
   do
      (( hours = hours + 6 ))
      if (( hours < 10 )); then
         hours=0$hours
      fi
      nlist="$nlist $hours"
   done
   echo nlist=$nlist 
 export post_times="$nlist"
#  export post_times="06"


#############################################################
# Execute job
#############################################################
/scratch/cccr/prajeesh/ShortRange/GFSv14_HS/IITM_GFSv14/src/postprocessing/ncep_post/JGFS_NCEPPOST_TCO

exit

