#!/bin/sh

# ---  qsub_mtnlsm190.384x190_128-192.sh
 set -x
# --- Create mtnvar14, orog and lsm files needed input into GFS, chgres, etc. 
echo -e " ===== Create mtnvar14, orog and lsm files needed input into
GFS and chgres. \ Can use Ocean lsm if present "
# ---  
echo -e " =====  Today's Date: `date` "
mac=$(hostname | cut -c1-1)
mac2=$(hostname | cut -c1-2)

echo " ===== mac=$mac "
#

 if [ $mac = z -o $mac = h -o $mac = f ] ; then # For ZEUS
 export machine=ZEUS
  ptmp=/scratch2/portfolios/NCEPDEV/ptmp/$LOGNAME
    export NCEPLIB=/contrib/nceplibs/test/lib
     export ESMFDIR=${ESMFDIR:-$LIBDIR}
 elif [ $mac = t -o $mac = e -o $mac = g ] ; then # For WCOSS
      export machine=WCOSS
       ptmp=ptmpp2
        export LIBDIR=/nwprod/lib
	 export NCEPLIB=/usrx/local/nceplibs
	  export ESMFDIR=/usrx/local/esmf-3.1.0rp5
	  fi


# --- /scratch2/portfolios/NCEPDEV/global/noscrub/Jordan.Alpert/terr/landmask/ocean_lm
# --- the Zeus qsub to create the orgmlw orog slm and mtn blocking files

#JASET   set machine
echo " ===== machine=$machine  "
#
# ---        OROG Resolution Settings
# for no filter, set filt1=jcap+1 and filt2=filt1+1 or switch with 0 0
#
export jcap=190
export latb=190
export lonb=384
#export filt1=0
export filt1=128
#export filt2=0
export filt2=192

          if [ $machine = ZEUS ]; then
#export first_dir=terr14_${jcap}.${lonb}.${latb}_${filt1}-${filt2}
first_dir=""
#export local_dir=/scratch2/portfolios/NCEPDEV/${ptmp}/${LOGNAME}/${first_dir}
local_dir=""
#mkdir -p ${local_dir}
#cd ${local_dir}
#echo " ===== change to directory: ${local_dir} "
pwd
echo " ===== ----------- ZEUS  QSUB   MTN BLOCKING/GWD/OROG SCRIPT -----  "

# --- (*j*) 20150422 ocean governed landsea mask - set for t62 192x94 hard wire
# ---                compiles new code mtnlm7_oclsm.f located locally

          elif [ $machine = WCOSS ]; then
#export first_dir=terr14_${jcap}.${lonb}.${latb}_${filt1}-${filt2}
first_dir=""
#export local_dir=/${ptmp}/${LOGNAME}/${first_dir}
local_dir=""
#echo " local_dir=$local_dir"
#mkdir -p ${local_dir}
#cd ${local_dir}
#echo " ===== change to directory: ${local_dir} "
pwd
echo " ===== ----------- WCOSS BSUB:  MTN BLOCKING/GWD/OROG ------------ "
          fi

          if [ $machine = ZEUS ]; then
#
qsub -V -F"slmgb${jcap} orogb${jcap} mtnvar14_${jcap} $lonb $latb ${jcap} $filt1 $filt2 1 " orgmlw_ZEUS.sh 
#
# submited batch job
echo " ===== ------ QSUB   MTN BLOCKING/GWD/OROG SCRIPT -----  "
echo " ===== ------ Submited batch job ...             -----  "
echo " ===== first_dir=$first_dir "
echo " ===== local_dir=$local_dir "
echo " =====      jcap=$jcap  "
echo " =====      lonb=$lonb  "
echo " =====      latb=$latb  "
echo " =====     filt1=$filt1 "
echo " =====     filt2=$filt2 "
echo " ===== ------------------------------------------------  "

#
exit

          elif [ $machine = WCOSS ]; then

export slmgb=slmgb${jcap}
export orogb=orogb${jcap}
export mtnvar14=mtnvar14_${jcap}
#
  bsub < orgmlw.sh > out_${jcap}.out
# --- There is no "-F" option on WCOSS bsub like there is on ZUES qsub ...

#
echo " ===== ----------- WCOSS BSUB:  MTN BLOCKING/GWD/OROG FILES --------  "
echo " ===== first_dir=$first_dir "
echo " ===== local_dir=$local_dir "
echo " =====      jcap=$jcap  "
echo " =====      lonb=$lonb  "
echo " =====      latb=$latb  "
echo " =====     filt1=$filt1 "
echo " =====     filt2=$filt2 "
echo " ===== ----------- WCOSS BSUB:  MTN BLOCKING/GWD/OROG FILES --------  "
exit
          fi
