#!/bin/bash


START_DATE=_STARTDATE_  # START DATE in YYYYMMDD FORMAT
cyc=00
FHMAX=240 # Model run hours
NLON=_NLON_
NLAT=_NLAT_
JCAP=_JCAP_
DO_CHGRES=true # whether to run change resolution for IC's
DO_MODEL=true # whether to run the model
DO_POST=true # whether to run the postprocessing 

ICdir=_ICDIR_ # Path to the Initial condition files








ROOTDIR=_ROOTDIR_
. $ROOTDIR/.env


START_YEAR=`echo $START_DATE | cut -c1-4`
START_MONTH=`echo $START_DATE | cut -c5-6`
START_DAY=`echo $START_DATE | cut -c7-8`

SIGINP=$ICdir/gfs.t${cyc}z.atmanl.nemsio  # IC sig file
SFCINP=$ICdir/gfs.t${cyc}z.sfcanl.nemsio  # IC sfc file
NSTINP=$ICdir/gfs.t${cyc}z.nstanl.nemsio  # IC nst file



FNGLAC=$FIXDIR/global_glacier.2x2.grb
FNMXIC=$FIXDIR/global_maxice.2x2.grb
FNTSFC=$FIXDIR/RTGSST.1982.2012.monthly.clim.grb
FNSNOC=$FIXDIR/global_snoclim.1.875.grb
FNZORC=igbp
FNALBC=$FIXDIR/global_snowfree_albedo.bosu.t1534.3072.1536.rg.grb
FNALBC2=$FIXDIR/global_albedo4.1x1.grb
FNAISC=$FIXDIR/CFSR.SEAICE.1982.2012.monthly.clim.grb
FNTG3C=$FIXDIR/global_tg3clim.2.6x1.5.grb
FNVEGC=$FIXDIR/global_vegfrac.0.144.decpercent.grb
FNVETC=$FIXDIR/global_vegtype.igbp.t1534.3072.1536.rg.grb
FNSOTC=$FIXDIR/global_soiltype.statsgo.t1534.3072.1536.rg.grb
FNSMCC=$FIXDIR/global_soilmgldas.t1534.3072.1536.grb
FNMSKH=$FIXDIR/seaice_newland.grb
FNVMNC=$FIXDIR/global_shdmin.0.144x0.144.grb
FNVMXC=$FIXDIR/global_shdmax.0.144x0.144.grb
FNSLPC=$FIXDIR/global_slope.1x1.grb
FNABSC=$FIXDIR/global_mxsnoalb.uariz.t1534.3072.1536.rg.grb

mkdir -p INPUT 
mkdir -p OUTPUT

ln -sf $FIXDIR/ak_bk_64l.nc INPUT/ak_bk.nc
ln -sf $FIXDIR/global_lonsperlat.t$JCAP.$NLON.$NLAT.txt lonsperlat.dat
ln -sf $FIXDIR/global_mtnvar.t$JCAP.$NLON.$NLAT.nc global_mtnvar.nc
ln -sf $FIXDIR/global_orography.t$JCAP.$NLON.$NLAT.nc orography.nc
ln -sf $FIXDIR/global_orography_uf.t$JCAP.$NLON.$NLAT.nc orography_uf.nc
ln -sf $FIXDIR/global_o3prdlos.f77 global_o3prdlos.f77
ln -sf $FIXDIR/global_climaeropac_global.txt aerosol.dat
ln -sf $FIXDIR/global_solarconstant_noaa_an.txt solarconstant_noaa_an.txt
ln -sf $FIXDIR/fix_co2_proj/global_co2historicaldata_2018.txt co2historicaldata_2018.txt

cat <<EOFF > gfs_input.nml
&data_override_nml
  debug_data_override=.false.,
  num_io_buffers=20 
/

&diag_manager_nml
  max_axes = 100,
  max_num_axis_sets = 100,
  max_input_fields = 699
  max_output_fields = 699
  mix_snapshot_average_fields=.false.
  issue_oor_warnings = .false.
  do_diag_field_log = .false.
/

&fms_io_nml
  threading_read='multi'
  threading_write='single'
  fileset_write='single'
  max_files_r = 200
  max_files_w = 200
/

&fms_nml
  clock_grain='loop' ! 'component' ! 'routine' !
  domains_stack_size = 8000000
  stack_size =0
/
EOFF

# CHRES ------------------

if [ "$DO_CHGRES" = true ] ; then

ln -sf $FIXDIR/global_slmask.t1534.6156.3070.nc slmask.nc
ln -sf $FIXDIR/global_hyblev.l64.txt chgres.inp.siglevel
ln -sf $FIXDIR/global_lonsperlat.t$JCAP.$NLON.$NLAT.txt chgres.inp.lonsperlat
ln -sf $FIXDIR/global_lonsperlat.t$JCAP.$NLON.$NLAT.txt chgres.inp.lpl3
ln -sf $FIXDIR/global_o3clim.txt chgres.inp.o3clim

ln -sf $SIGINP chgres.inp.sig
ln -sf $SFCINP chgres.inp.sfc
ln -sf $NSTINP chgres.inp.nst

ln -sf chgres.out.grd sig_ini
ln -sf chgres.out.nsn nst_ini
ln -sf chgres.out.sfn sfc_ini

cat <<EOFCH > chgres.nml

 &NAMSFC
  FNGLAC="$FNGLAC"
  FNMXIC="$FNMXIC"
  FNTSFC="$FNTSFC"
  FNSNOC="$FNSNOC"
  FNZORC="igbp"
  FNALBC="$FNALBC"
  FNALBC2="$FNALBC2"
  FNAISC="$FNAISC"
  FNTG3C="$FNTG3C"
  FNVEGC="$FNVEGC"
  FNVETC="$FNVETC"
  FNSOTC="$FNSOTC"
  FNSMCC="$FNSMCC"
  FNVMNC="$FNVMNC"
  FNVMXC="$FNVMXC"
  FNSLPC="$FNSLPC"
  FNABSC="$FNABSC"
  FNMSKH="$FNMSKH"
  FNTSFA=""
  FNACNA=""
  FNSNOA=""
  LDEBUG=.false.
  LANDICE=.true.
/ 

&soil_parameters
  soil_src_input = "statsgo"
  smclow_input  = 0.5
  smchigh_input = 6.0
  smcmax_input= 0.395, 0.421, 0.434, 0.476, 0.476, 0.439,
                0.404, 0.464, 0.465, 0.406, 0.468, 0.457,
                0.464, -9.99, 0.200, 0.421
  beta_input  = 4.05, 4.26, 4.74, 5.33, 5.33, 5.25, 
                6.77, 8.72, 8.17, 10.73, 10.39, 11.55,
                5.25, -9.99, 4.05, 4.26
  psis_input  = 0.0350, 0.0363, 0.1413, 0.7586, 0.7586, 0.3548,
                0.1349, 0.6166, 0.2630, 0.0977, 0.3236, 0.4677,
                0.3548, -9.99,  0.0350, 0.0363
  satdk_input = 1.7600e-4, 1.4078e-5, 5.2304e-6, 2.8089e-6, 2.8089e-6,
                3.3770e-6, 4.4518e-6, 2.0348e-6, 2.4464e-6, 7.2199e-6,
                1.3444e-6, 9.7384e-7, 3.3770e-6,     -9.99, 1.4078e-5,
                1.4078e-5
  soil_src_output = "statsgo"
  smclow_output  = 0.5
  smchigh_output = 6.0
  smcmax_output= 0.395, 0.421, 0.434, 0.476, 0.476, 0.439,
                 0.404, 0.464, 0.465, 0.406, 0.468, 0.457,
                 0.464, -9.99, 0.200, 0.421
  beta_output  = 4.05, 4.26, 4.74, 5.33, 5.33, 5.25, 
                 6.77, 8.72, 8.17, 10.73, 10.39, 11.55,
                 5.25, -9.99, 4.05, 4.26
  psis_output  = 0.0350, 0.0363, 0.1413, 0.7586, 0.7586, 0.3548,
                 0.1349, 0.6166, 0.2630, 0.0977, 0.3236, 0.4677,
                 0.3548, -9.99,  0.0350, 0.0363
  satdk_output = 1.7600e-4, 1.4078e-5, 5.2304e-6, 2.8089e-6, 2.8089e-6,
                 3.3770e-6, 4.4518e-6, 2.0348e-6, 2.4464e-6, 7.2199e-6,
                 1.3444e-6, 9.7384e-7, 3.3770e-6,     -9.99, 1.4078e-5,
                 1.4078e-5
/

 &veg_parameters
  veg_src_input = "igbp"
  veg_src_output = "igbp"
  salp_output= -999.
  snup_output= -999.
/

 &options
  CLIMO_FIELDS_OPT=3
  LANDICE_OPT=5
 /


  &NAMCHG 
  JCAP=$JCAP, LEVS=64, LONB=$NLON, LATB=$NLAT,
  NTRAC=3, IDVC=2, IDSL=1,
  LSOIL=4, IVSSFC=0, OUTTYP=1,
  IDRT=4, GRDFMT=bin4, IALB=1, ISOT=1,
  IVEGSRC=1, use_ufo=.true.,nst_anl=.true.,
  rdgrid=.true.,idvc=2,idvt=21,idsl=1,
  IDVM=0,OUTTYP=1,nopdpvv=.true.,
/

EOFCH

cat <<EOFCHA > _submit_chres.sh
#!/bin/sh --login

#PBS -N _EXPNAME_-chgres
#PBS -l select=1:ncpus=16
#PBS -q $QUEUE
#PBS -l walltime=1:00:00 

. $ROOTDIR/bin/env._MACH_

ulimit -c unlimited
ulimit -s unlimited
ulimit -a
threads=6

export KMP_AFFINITY=disabled
export OMP_STACKSIZE=1024m
export OMP_NUM_THREADS=\$threads
export NTHREADS=\$threads

cd \$PBS_O_WORKDIR

aprun -n 1 -N 1 -j 1 -d 24 -cc depth _CHGRESEXE_ 1>OUTPUT.chgres 2>ERROR.chgres

EOFCHA
echo "Submitting chgres..."
output=$(qsub < _submit_chres.sh)
echo $output
jobid=$(echo $output | awk -F "." '{print $1}')
if [ "$jobid" -eq "$jobid" ] 2>/dev/null; then
    echo "" 
else
  echo $jobid
    echo "Job not submitted" 
    exit 1
fi

COND="PBS -W depend=afterok:$jobid"

else

ln -sf $SIGINP sig_ini
ln -sf $SFCINP sfc_ini
ln -sf $NSTINP nst_ini
COND=""

fi




#MODEL RUN -------------

if [ "$DO_MODEL" = true ] ; then
cat <<EOF > atm_namelist
 &nam_dyn
  FHOUT=6, FHMAX=$FHMAX, IGEN=81, DELTIM=450,
  FHRES=264, FHROT=0, FHDFI=3, nsout=0,
  nxpt=1, nypt=2, jintmx=2, lonf=$NLON, latg=$NLAT,
  jcap=$JCAP, levs=64,  levr=64,
  ntrac=3, ntoz=2, ntcw=3, ncld=1,
  ngptc=8, hybrid=.true., tfiltc=0.85,
  gen_coord_hybrid=.false., zflxtvd=.false.,
  spectral_loop=2, explicit=.false.,
  ndslfv=.false.,mass_dp=.false.,process_split=.false.,
  reduced_grid=.true.,lsidea=.false.,
  wam_ipe_coupling=.false.,
  height_dependent_g=.false.,
  semi_implicit_temp_profile=.false.,
  thermodyn_id=0, sfcpress_id=1,
  dfilevs=64, 
  FHOUT_HF=6, FHMAX_HF=120,
  SHUM=0.0, -999., -999., -999, -999,SHUM_TAU=2.16E4, 1.728E5, 6.912E5, 7.776E6, 3.1536E7,SHUM_LSCALE=500.E3, 1000.E3, 2000.E3, 2000.E3, 2000.E3,ISEED_SHUM=0,
  SPPT=0.0,0.0,0.0,0.0,0.0,SPPT_TAU=21600,2592500,25925000,7776000,31536000,SPPT_LSCALE=500000,1000000,2000000,2000000,2000000,SPPT_LOGIT=.TRUE.,ISEED_SPPT=0,
  SKEB=0.0, -999., -999., -999, -999,SKEB_TAU=2.164E4, 1.728E5, 2.592E6, 7.776E6, 3.1536E7,SKEB_LSCALE=1000.E3, 1000.E3, 2000.E3, 2000.E3, 2000.E3,SKEB_VFILT=40,SKEB_DISS_SMOOTH=12,ISEED_SKEB=0,
  VC=0.0,VC_TAU=4.32E4, 1.728E5, 2.592E6, 7.776E6, 3.1536E7,VC_LSCALE=1000.E3, 1000.E3, 2000.E3, 2000.E3, 2000.E3,VCAMP=0.0, -999., -999., -999, -999,ISEED_VC=0,
  nemsio_in=.true.,nemsio_out=.true.,sigio_out=.false.,shuff_lats_a=.false.,hdif_fac=1.0,hdif_fac2=1.0,settls_dep3ds=.true.,settls_dep3dg=.true.,redgg_a=.true.,gg_tracers=.false.,sl_epsln=0.02,ref_temp=350.0,yhalo=10,phigs_d=60.0,ldfi_spect=.true.,cdamp=50000,2.0,k2o=32,ref_pres=100.0 /
 &nam_phy
  FHOUT=6, FHMAX=$FHMAX, IGEN=81, DELTIM=450,
  DTPHYS=225,
  FHRES=264, FHROT=0, FHCYC=24, FHDFI=3,
  FHZER=6, FHLWR=3600, FHSWR=3600,nsout=0,
  nxpt=1, nypt=2, jintmx=2, lonr=$NLON, latr=$NLAT,
  jcap=$JCAP, levs=64, levr=64, reduced_grid=.true.,
  ntrac=3, ntoz=2, ntcw=3, ncld=1,
  lsoil=4, nmtvr=14, lsidea=.false.,
  f107_kp_size=56,
  f107_kp_interval=10800,
  f107_kp_skip_size=0,
  ngptc=8, hybrid=.true., tfiltc=0.85,
  gen_coord_hybrid=.false.,
  thermodyn_id=0, sfcpress_id=1,
  FHOUT_HF=6, FHMAX_HF=120,
  nstf_name=2,0,1,0,5,NST_ANL=.true.,
  SHUM=0.0, -999., -999., -999, -999, SPPT=0.0,0.0,0.0,0.0,0.0,SKEB=0.0, -999., -999., -999, -999, VC=0.0,VCAMP=0.0, -999., -999., -999, -999,
  ialb=1,
  IEMS=1,ISOL=2,IAER=111,ICO2=2,nemsio_in=.true.,nemsio_out=.true.,sfcio_out=.false.,ras=.false.,zhao_mic=.true.,lsm=1,old_monin=.false.,imfshalcnv=2,imfdeepcnv=2,shal_cnv=.true.,shuff_lats_r=.false.,ialb=1,pre_rad=.false.,random_clds=.false.,iovr_lw=1,iovr_sw=1,ISOL=2,ICO2=2,IAER=111,ictm=1,nsout=0,use_ufo=.true.,ldiag3d=.false.,ncw=20,120,crtrh=0.90,0.90,0.90,flgmin=0.180,0.220,cnvgwd=.true.,cgwf=0.5,0.05,ctei_rm=10.0,10.0,mstrat=.false.,ccnorm=.false.,mom4ice=.false.,A2OI_OUT=.false.,CPLFLX=.false.,NGRID_A2OI=20,lgoc3d=.false.,trans_trac=.true.,cal_pre=.true.,bkgd_vdif_m=1.0,bkgd_vdif_h=1.0,bkgd_vdif_s=1.0,climate=.false.,psautco=6.0e-4,3.0e-4,prautco=1.0e-4,1.0e-4,evpco=2.0e-5,wminco=1.0e-5,1.0e-5,CCWF=1.0,1.0,dlqf=0.0,0.0,cdmbgwd=2.0,0.25,grid_aldata=.false.,semilag=.true.,redrag=.true.,hybedmf=.true.,dspheat=.true.,cnvcld=.true.,pdfcld=.false.,shcnvcw=.false.,isubc_lw=2,isubc_sw=2,fixtrc=.false.,.true.,.false.,gg_tracers=.false.,h2o_phys=.false.,isot=1,ivegsrc=1  /
 &TRACER_CONSTANT
  RI=286.05,   461.50, 173.2247,    0.0,,CPI=1004.6,   1846.0, 820.2391,    0.0,, /
 &SOIL_VEG
  LPARAM = .FALSE./
 &NAMSFC
  FNGLAC="$FNGLAC",
  FNMXIC="$FNMXIC",
  FNTSFC="$FNTSFC",
  FNSNOC="$FNSNOC",
  FNZORC="igbp",
  FNALBC= "$FNALBC",
  FNALBC2="$FNALBC2",
  FNAISC= "$FNAISC",
  FNTG3C= "$FNTG3C",
  FNVEGC= "$FNVEGC",
  FNVETC= "$FNVETC",
  FNSOTC= "$FNSOTC",
  FNSMCC= "$FNSMCC",
  FNMSKH= "$FNMSKH",
  FNVMNC="$FNVMNC",
  FNVMXC="$FNVMXC",
  FNSLPC="$FNSLPC",
  FNABSC="$FNABSC",
  FNTSFA="",
  FNACNA="",
  FNSNOA="",
  LDEBUG=.false.,
  FSMCL(2)=99999,
  FSMCL(3)=99999,
  FSMCL(4)=99999,
  FTSFS=90.0,
  FAISS=99999,
  FSNOL=99999,
  FSICL=99999,
  FTSFL=99999,
  FAISL=99999,
  FVETL=99999,
  FSOTL=99999,
  FvmnL=99999,
  FvmxL=99999,
  FSLPL=99999,
  FABSL=99999,
  FSNOS=99999,
  FSICS=99999,

    /
 &NAMPGB
   /
EOF


cat <<EOFA > atm_namelist.rc
core: gfs
print_esmf:     .false.

#nam_atm +++++++++++++++++++++++++++
nlunit:                  35
deltim:                  450.0
fhrot:                   0
namelist:                atm_namelist
total_member:            1
grib_input:              0
PE_MEMBER01:             0
PE_MEMBER02:             
PE_MEMBER03:             
PE_MEMBER04:             
PE_MEMBER05:             
PE_MEMBER06:             
PE_MEMBER07:             
PE_MEMBER08:             
PE_MEMBER09:             
PE_MEMBER10:             
PE_MEMBER11:             
PE_MEMBER12:             
PE_MEMBER13:             
PE_MEMBER14:             
PE_MEMBER15:             
PE_MEMBER16:             
PE_MEMBER17:             
PE_MEMBER18:             
PE_MEMBER19:             
PE_MEMBER20:             
PE_MEMBER21:             

# For stachastic purturbed runs -  added by Dhou and Wyang
  --------------------------------------------------------
#  ENS_SPS, logical control for application of stochastic perturbation scheme
#  HH_START, start hour of forecast, and modified ADVANCECOUNT_SETUP
#  HH_INCREASE and HH_FINAL are fcst hour increment and end hour of forecast
#  ADVANCECOUNT_SETUP is an integer indicating the number of time steps between
#  integrtion_start and the time when model state is saved for the _ini of the
#  GEFS_Coupling, currently is 0h.

HH_INCREASE:             24
HH_FINAL:                24
HH_START:                0
ADVANCECOUNT_SETUP:      0

ENS_SPS:                 .false.
HOUTASPS:                10000

#ESMF_State_Namelist +++++++++++++++

RUN_CONTINUE:            .false.

#
dt_int:                  450
dt_num:                  0
dt_den:                  1
start_year:              $START_YEAR
start_month:             $START_MONTH
start_day:               $START_DAY
start_hour:              0
start_minute:            0
start_second:            0
nhours_fcst:             $FHMAX
restart:                 .false.
nhours_fcst1:            $FHMAX
im:                      $NLON
jm:                      $NLAT
global:                  .true.
nhours_dfini:            3
adiabatic:               .false.
lsoil:                   4
passive_tracer:          .false.
dfilevs:                 64
ldfiflto:                .true.
ldfi_grd:                .false.
num_tracers:             3
lwrtgrdcmp:              .true.
nemsio_in:               .true.

#jwstart added quilt
###############################
#### Specify the I/O tasks ####
###############################


quilting:                .true.   #For asynchronous quilting/history writes
read_groups:             0
read_tasks_per_group:    0
write_groups:            1
write_tasks_per_group:   1

num_file:                4                   #
filename_base:           'SIG.F' 'SFC.F' 'FLX.F' 'NST.F'
file_io_form:            'bin4' 'bin4' 'bin4' 'bin4'                     
file_io:                 'DEFERRED' 'DEFERRED' 'DEFERRED' 'DEFERRED'  #
write_dopost:            .false.          # True--> run do on quilt
post_gribversion:        grib1      # True--> grib version for post output files
gocart_aer2post:         .false.
write_nemsioflag:        .true.      # True--> Write nemsio run history files
write_fsyncflag:         .false.       # True--> check if output files synced to disk
nfhout:                  6
nfhout_hf:               6
nfhmax_hf:               120
nsout:                   0

io_recl:                 100
io_position:             ' '
io_action:               'WRITE'
io_delim:                ' '
io_pad:                  ' '

#jwend

EOFA

cat <<EOFB > dyn_namelist.rc
core: gfs
print_esmf:     .false.

#nam_atm +++++++++++++++++++++++++++
nlunit:                  35
deltim:                  450.0
fhrot:                   0
namelist:                atm_namelist
total_member:            1
grib_input:              0
PE_MEMBER01:             0
PE_MEMBER02:             
PE_MEMBER03:             
PE_MEMBER04:             
PE_MEMBER05:             
PE_MEMBER06:             
PE_MEMBER07:             
PE_MEMBER08:             
PE_MEMBER09:             
PE_MEMBER10:             
PE_MEMBER11:             
PE_MEMBER12:             
PE_MEMBER13:             
PE_MEMBER14:             
PE_MEMBER15:             
PE_MEMBER16:             
PE_MEMBER17:             
PE_MEMBER18:             
PE_MEMBER19:             
PE_MEMBER20:             
PE_MEMBER21:             

# For stachastic purturbed runs -  added by Dhou and Wyang
  --------------------------------------------------------
#  ENS_SPS, logical control for application of stochastic perturbation scheme
#  HH_START, start hour of forecast, and modified ADVANCECOUNT_SETUP
#  HH_INCREASE and HH_FINAL are fcst hour increment and end hour of forecast
#  ADVANCECOUNT_SETUP is an integer indicating the number of time steps between
#  integrtion_start and the time when model state is saved for the _ini of the
#  GEFS_Coupling, currently is 0h.

HH_INCREASE:             24
HH_FINAL:                24
HH_START:                0
ADVANCECOUNT_SETUP:      0

ENS_SPS:                 .false.
HOUTASPS:                10000

#ESMF_State_Namelist +++++++++++++++

RUN_CONTINUE:            .false.

#
dt_int:                  450
dt_num:                  0
dt_den:                  1
start_year:              $START_YEAR
start_month:             $START_MONTH
start_day:               $START_DAY
start_hour:              0
start_minute:            0
start_second:            0
nhours_fcst:             $FHMAX
restart:                 .false.
nhours_fcst1:            $FHMAX
im:                      $NLON
jm:                      $NLAT
global:                  .true.
nhours_dfini:            3
adiabatic:               .false.
lsoil:                   4
passive_tracer:          .false.
dfilevs:                 64
ldfiflto:                .true.
ldfi_grd:                .false.
num_tracers:             3
lwrtgrdcmp:              .true.
nemsio_in:               .true.

#jwstart added quilt
###############################
#### Specify the I/O tasks ####
###############################


quilting:                .true.   #For asynchronous quilting/history writes
read_groups:             0
read_tasks_per_group:    0
write_groups:            1
write_tasks_per_group:   1

num_file:                4                   #
filename_base:           'SIG.F' 'SFC.F' 'FLX.F' 'NST.F'
file_io_form:            'bin4' 'bin4' 'bin4' 'bin4'                     
file_io:                 'DEFERRED' 'DEFERRED' 'DEFERRED' 'DEFERRED'  #
write_dopost:            .false.          # True--> run do on quilt
post_gribversion:        grib1      # True--> grib version for post output files
gocart_aer2post:         .false.
write_nemsioflag:        .true.      # True--> Write nemsio run history files
write_fsyncflag:         .false.       # True--> check if output files synced to disk
nfhout:                  6
nfhout_hf:               6
nfhmax_hf:               120
nsout:                   0

io_recl:                 100
io_position:             ' '
io_action:               'WRITE'
io_delim:                ' '
io_pad:                  ' '

#jwend


SLG_FLAG:                        .true.

#ESMF_State_Namelist +++++++++++++++
idate1_import:                    1
z_import:                         1
ps_import:                        1
div_import:                       0
vor_import:                       0
u_import:                         1
v_import:                         1
temp_import:                      1
tracer_import:                    1
p_import:                         1
dp_import:                        1
dpdt_import:                      0

idate1_export:                    1
z_export:                         1
ps_export:                        1
div_export:                       0
vor_export:                       0
u_export:                         1
v_export:                         1
temp_export:                      1
tracer_export:                    1
p_export:                         1
dp_export:                        1
dpdt_export:                      1
sppt_wts_export:                  0
shum_wts_export:                  0
skeb_wts_export:                  0
vc_wts_export:                    0
EOFB

cat <<EOFC > atmos.configure
 core: gfs
 atm_model:                  gsm
 atm_coupling_interval_sec:
EOFC

cat <<EOFD > nems.configure
 EARTH_component_list: ATM
 ATM_model:            gsm
 runSeq::
   ATM
 ::
EOFD


cat <<EOFE > phy_namelist.rc
core: gfs
print_esmf:     .true.

#nam_atm +++++++++++++++++++++++++++
nlunit:                  35
deltim:                  450.0
fhrot:                   0
namelist:                atm_namelist
total_member:            1
grib_input:              0
PE_MEMBER01:             0
PE_MEMBER02:             
PE_MEMBER03:             
PE_MEMBER04:             
PE_MEMBER05:             
PE_MEMBER06:             
PE_MEMBER07:             
PE_MEMBER08:             
PE_MEMBER09:             
PE_MEMBER10:             
PE_MEMBER11:             
PE_MEMBER12:             
PE_MEMBER13:             
PE_MEMBER14:             
PE_MEMBER15:             
PE_MEMBER16:             
PE_MEMBER17:             
PE_MEMBER18:             
PE_MEMBER19:             
PE_MEMBER20:             
PE_MEMBER21:             

# For stachastic purturbed runs -  added by Dhou and Wyang
  --------------------------------------------------------
#  ENS_SPS, logical control for application of stochastic perturbation scheme
#  HH_START, start hour of forecast, and modified ADVANCECOUNT_SETUP
#  HH_INCREASE and HH_FINAL are fcst hour increment and end hour of forecast
#  ADVANCECOUNT_SETUP is an integer indicating the number of time steps between
#  integrtion_start and the time when model state is saved for the _ini of the
#  GEFS_Coupling, currently is 0h.

HH_INCREASE:             24
HH_FINAL:                24
HH_START:                0
ADVANCECOUNT_SETUP:      0

ENS_SPS:                 .false.
HOUTASPS:                10000

#ESMF_State_Namelist +++++++++++++++

RUN_CONTINUE:            .false.

#
dt_int:                  450
dt_num:                  0
dt_den:                  1
start_year:              $START_YEAR
start_month:             $START_MONTH
start_day:               $START_DAY
start_hour:              0
start_minute:            0
start_second:            0
nhours_fcst:             $FHMAX
restart:                 .false.
nhours_fcst1:            $FHMAX
im:                      $NLON
jm:                      $NLAT
global:                  .true.
nhours_dfini:            3
adiabatic:               .false.
lsoil:                   4
passive_tracer:          .false.
dfilevs:                 64
ldfiflto:                .true.
ldfi_grd:                .false.
num_tracers:             3
lwrtgrdcmp:              .true.
nemsio_in:               .true.

#jwstart added quilt
###############################
#### Specify the I/O tasks ####
###############################


quilting:                .true.   #For asynchronous quilting/history writes
read_groups:             0
read_tasks_per_group:    0
write_groups:            1
write_tasks_per_group:   1

num_file:                4                   #
filename_base:           'SIG.F' 'SFC.F' 'FLX.F' 'NST.F'
file_io_form:            'bin4' 'bin4' 'bin4' 'bin4'                     
file_io:                 'DEFERRED' 'DEFERRED' 'DEFERRED' 'DEFERRED'  #
write_dopost:            .false.          # True--> run do on quilt
post_gribversion:        grib1      # True--> grib version for post output files
gocart_aer2post:         .false.
write_nemsioflag:        .true.      # True--> Write nemsio run history files
write_fsyncflag:         .false.       # True--> check if output files synced to disk
nfhout:                  3
nfhout_hf:               1
nfhmax_hf:               120
nsout:                   0

io_recl:                 100
io_position:             ' '
io_action:               'WRITE'
io_delim:                ' '
io_pad:                  ' '

#jwend


#Upper_Air_State_Namelist +++++++++++++++
idate1_import:                    1
z_import:                         1
ps_import:                        1
div_import:                       0
vor_import:                       0
u_import:                         1
v_import:                         1
temp_import:                      1
tracer_import:                    1
p_import:                         1
dp_import:                        1
dpdt_import:                      1                 
sppt_wts_import:                  0
shum_wts_import:                  0
skeb_wts_import:                  0
vc_wts_import:                    0

idate1_export:                    1
z_export:                         1
ps_export:                        1
div_export:                       0
vor_export:                       0
u_export:                         1
v_export:                         1
temp_export:                      1
tracer_export:                    1
p_export:                         1
dp_export:                        1
dpdt_export:                      1

# Surface state.
#---------------
orography_import:                 1
t_skin_import:                    1
soil_mois_import:                 1
snow_depth_import:                1
soil_t_import:                    1
deep_soil_t_import:               1
roughness_import:                 1
conv_cloud_cover_import:          1
conv_cloud_base_import:           1
conv_cloud_top_import:            1
albedo_visible_scattered_import:  1
albedo_visible_beam_import:       1
albedo_nearir_scattered_import:   1
albedo_nearir_beam_import:        1
sea_level_ice_mask_import:        1
vegetation_cover_import:          1
canopy_water_import:              1
m10_wind_fraction_import:         1
vegetation_type_import:           1
soil_type_import:                 1
zeneith_angle_facsf_import:       1
zeneith_angle_facwf_import:       1
uustar_import:                    1
ffmm_import:                      1
ffhh_import:                      1
sea_ice_thickness_import:         1
sea_ice_concentration_import:     1
tprcp_import:                     1
srflag_import:                    1
actual_snow_depth_import:         1
liquid_soil_moisture_import:      1
vegetation_cover_min_import:      1
vegetation_cover_max_import:      1
slope_type_import:                1
snow_albedo_max_import:           1

orography_export:                 1
t_skin_export:                    1
soil_mois_export:                 1
snow_depth_export:                1
soil_t_export:                    1
deep_soil_t_export:               1
roughness_export:                 1
conv_cloud_cover_export:          1
conv_cloud_base_export:           1
conv_cloud_top_export:            1
albedo_visible_scattered_export:  1
albedo_visible_beam_export:       1
albedo_nearir_scattered_export:   1
albedo_nearir_beam_export:        1
sea_level_ice_mask_export:        1
vegetation_cover_export:          1
canopy_water_export:              1
m10_wind_fraction_export:         1
vegetation_type_export:           1
soil_type_export:                 1
zeneith_angle_facsf_export:       1
zeneith_angle_facwf_export:       1
uustar_export:                    1
ffmm_export:                      1
ffhh_export:                      1
sea_ice_thickness_export:         1
sea_ice_concentration_export:     1
tprcp_export:                     1
srflag_export:                    1
actual_snow_depth_export:         1
liquid_soil_moisture_export:      1
vegetation_cover_min_export:      1
vegetation_cover_max_export:      1
slope_type_export:                1
snow_albedo_max_export:           1
EOFE


rm -f _submit.sh
cat <<EOFG > _submit.sh

#!/bin/sh --login

#PBS -N _EXPNAME_
#PBS -j oe
#PBS -l walltime=19:00:00
#PBS -l select=251:ncpus=36:vntype=cray_compute -l place=scatter
#PBS -q $QUEUE
#PBS -V
#$COND

set -e

threads=6

. $ROOTDIR/bin/env._MACH_

ulimit -c unlimited
ulimit -s unlimited
ulimit -a
module load craype-hugepages16M

export KMP_AFFINITY=disabled
export OMP_STACKSIZE=1024m
export OMP_NUM_THREADS=\$threads
export NTHREADS=\$threads

cd \$PBS_O_WORKDIR

EXE=_EXE_

aprun -j1 -n 1001 -N 4 -cc depth \$EXE 1> OUTPUT.NEMS 2> ERROR.NEMS
EOFG

echo "Submiting model run..."
output=$(qsub < _submit.sh)
echo $output
jobid=$(echo $output | awk -F "." '{print $1}')
if [ "$jobid" -eq "$jobid" ] 2>/dev/null; then
    echo "" 
else
  echo $jobid
    echo "Job not submitted" 
    exit 1
fi
COND="PBS -W depend=afterok:$jobid"

else

COND=""

fi


#POST-PROCESSING -------------

if [ "$DO_POST" = true ] ; then

NDATE=$ROOTDIR/src/postprocessing/ncep_post/ndate
RUNDIR=$(pwd)
mkdir -p POST

fhr=0
while (( fhr < FHMAX ))
do
    (( fhr = fhr + 6 ))
    if (( fhr < 10 )); then
       fhr=0$fhr
    fi
    VDATE=`$NDATE +${fhr} ${START_DATE}${cyc}`
    YY=`echo $VDATE | cut -c1-4`
    MM=`echo $VDATE | cut -c5-6`
    DD=`echo $VDATE | cut -c7-8`
    HH=`echo $VDATE | cut -c9-10`

    ofiles="pgbf flxf"
    for ofile in $ofiles; do
        OUTFILE=${ofile}_${VDATE}
        PTMP=POST/ptmp/$VDATE/$ofile
        mkdir -p $PTMP
        cd $PTMP
        ln -sf $FIXDIR/global_hyblev.l64.txt global_hyblev.txt
        ln -sf $FIXDIR/global_lonsperlat.t$JCAP.$NLON.$NLAT.txt lonsperlat.dat
        ln -sf $ROOTDIR/nml_tbl/nam_micro_lookup.dat eta_micro_lookup.dat
        ln -sf $ROOTDIR/nml_tbl/params_grib2_tbl_new .
        ln -sf $ROOTDIR/nml_tbl/postxconfig-NT-$ofile.txt postxconfig-NT.txt
        ln -sf $RUNDIR/SFC.F$fhr sfcfile 
        ln -sf $RUNDIR/SIG.F$fhr sigfile
        ln -sf $RUNDIR/FLX.F$fhr flxfile

cat <<EOF > itag
sigfile
binarynemsiompiio
grib2
$YY-$MM-$DD_$HH:00:00
GFS
flxfile

&NAMPGB
    KPO=47,PO=1000.,975.,950.,925.,900.,875.,850.,825.,800.,775.,750.,725.,700.,675.,650.,625.,600.,575.,550.,525.,500.,475.,450.,425.,400.,375.,350.,325.,300.,275.,250.,225.,200.,175.,150.,125.,100.,70.,50.,30.,20.,10.,7.,5.,3.,2.,1.,
 /
EOF


cat <<EOF > _submit_post.sh
#!/bin/sh --login
#PBS -N ${OUTFILE}-_EXPNAME_
#PBS -l select=24:ncpus=36:vntype=cray_compute -l place=scatter
#PBS -q $QUEUE
#PBS -l walltime=2:00:00
#$COND 

cd \$PBS_O_WORKDIR

export threads=6
export KMP_AFFINITY=disabled
export OMP_STACKSIZE=1024m
export OMP_NUM_THREADS=\$threads
export NTHREADS=\$threads

export PGBOUT=$OUTFILE
export SFCINPUT=sfcfile
export IDRT=0
export LATB=$NLAT
export LONB=$NLON

aprun -j 1 -n 24 -N 1 -d 1 -cc depth $ROOTDIR/exec/postprocessing/ncep_post/src/ncep_post > OUTPUT.POST 2> ERROR.POST 

mv $OUTFILE $RUNDIR/POST/
EOF

        qsub _submit_post.sh

        cd $RUNDIR

    done

done

fi