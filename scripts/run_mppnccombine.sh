#!/bin/bash
#USER DEFINED PARAMETERS

# queue - Queue name
queue="cccr"

# WCLOCK - Wall clock limit (in hours)
WCLOCK="240"

# END OF USER DEFINED PARAMETERS

#--------------------------------------------------------------------------------   
#--------------------------------------------------------------------------------   
#--------------------------------------------------------------------------------   
#--------------------------------------------------------------------------------   
#--------------------------------------------------------------------------------   
#-------------------------------------------------------------------------------- 

rootdir=/scratch/cccr/prajeesh/ShortRange/GFSv14_HS/IITM_GFSv14

# nproc - number of processors for postproc
nproc=36
while getopts 'p:n:' flag; do
    case "${flag}" in
    p) jobid=$OPTARG ;;
    n) nproc=$OPTARG ;;
    esac
done

JOBNAME='GFSv14_nccombine'
RUNNCCP2R=$rootdir/exec/mppncc_gfs/mppncc_gfs
WCLOCK=$((WCLOCK*2))
ppn=36
nnodes=$((nproc/ppn))
rem=$((nproc%ppn))
if [ "$rem" -gt "0" ]; then
	nnodes=$((nnodes+1))
fi

tfile=$(mktemp)

child_run=0
if [[ ! -z "$jobid" ]]; then
	child_run=1
	COND="PBS -W depend=after:$jobid"
fi

cat <<EOFx > 'mppncc.nml'
&opts_nml 
 child_run=$child_run 
/
EOFx


cat <<EOFy > $tfile
#!/bin/bash
#PBS -q $queue
#PBS -l nodes=$nnodes:ppn=$ppn
#PBS -l walltime=${WCLOCK}:00:00
#PBS -N ${JOBNAME}_post
#PBS -j oe

ulimit -c unlimited
set -xu
cd \$PBS_O_WORKDIR
source $rootdir/bin/env.pratyush_intel

aprun -n $nproc $RUNNCCP2R &> ${JOBNAME}.mppncc.out

EOFy

qsub < $tfile
