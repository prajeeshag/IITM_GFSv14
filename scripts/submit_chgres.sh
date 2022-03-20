
cat <<EOF > chgres.nml

 &NAMSFC
  FNGLAC="INPUT/global_glacier.2x2.grb"
  FNMXIC="INPUT/global_maxice.2x2.grb"
  FNTSFC="INPUT/RTGSST.1982.2012.monthly.clim.grb"
  FNSNOC="INPUT/global_snoclim.1.875.grb"
  FNZORC="igbp"
  FNALBC="INPUT/global_snowfree_albedo.bosu.rg.grb"
  FNALBC2="INPUT/global_albedo4.1x1.grb"
  FNAISC="INPUT/CFSR.SEAICE.1982.2012.monthly.clim.grb"
  FNTG3C="INPUT/global_tg3clim.2.6x1.5.grb"
  FNVEGC="INPUT/global_vegfrac.0.144.decpercent.grb"
  FNVETC="INPUT/global_vegtype.igbp.rg.grb"
  FNSOTC="INPUT/global_soiltype.statsgo.rg.grb"
  FNSMCC="INPUT/global_soilmgldas.grb"
  FNVMNC="INPUT/global_shdmin.0.144x0.144.grb"
  FNVMXC="INPUT/global_shdmax.0.144x0.144.grb"
  FNSLPC="INPUT/global_slope.1x1.grb"
  FNABSC="INPUT/global_mxsnoalb.uariz.rg.grb"
  FNMSKH="INPUT/seaice_newland.grb"
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

EOF

aprun -n 1 -N 1 -j 1 -d 24 -cc depth $ROOTDIR/exec/preprocessing/chgres/chgres 1>OUTPUT.chgres 2>ERROR.chgres