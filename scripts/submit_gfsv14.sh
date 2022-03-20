START_YEAR=2018
START_MONTH=8
START_DAY=10
FHMAX=240 # Model run hours
NLON=6156
NLAT=3070
JCAP=1534

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
  FNGLAC="INPUT/global_glacier.2x2.grb",
  FNMXIC="INPUT/global_maxice.2x2.grb",
  FNTSFC="INPUT/RTGSST.1982.2012.monthly.clim.grb",
  FNSNOC="INPUT/global_snoclim.1.875.grb",
  FNZORC="igbp",
  FNALBC= "INPUT/global_snowfree_albedo.bosu.rg.grb",
  FNALBC2="INPUT/global_albedo4.1x1.grb",
  FNAISC= "INPUT/CFSR.SEAICE.1982.2012.monthly.clim.grb",
  FNTG3C= "INPUT/global_tg3clim.2.6x1.5.grb",
  FNVEGC= "INPUT/global_vegfrac.0.144.decpercent.grb",
  FNVETC= "INPUT/global_vegtype.igbp.rg.grb",
  FNSOTC= "INPUT/global_soiltype.statsgo.rg.grb",
  FNSMCC= "INPUT/global_soilmgldas.grb",
  FNMSKH= "INPUT/seaice_newland.grb",
  FNTSFA="",
  FNACNA="",
  FNSNOA="",
  FNVMNC="INPUT/global_shdmin.0.144x0.144.grb",
  FNVMXC="INPUT/global_shdmax.0.144x0.144.grb",
  FNSLPC="INPUT/global_slope.1x1.grb",
  FNABSC="INPUT/global_mxsnoalb.uariz.rg.grb",
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


    