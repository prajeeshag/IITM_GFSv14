!
! !MODULE: gfs_physics_sfc_flx_mod  ---      Definition of the surface
!                                            fields in the ESMF internal state.
!
! !DESCRIPTION: gfs_physics_sfc_flx_mod ---    Define the surfacee  variables
!                                              in the ESMF internal state.
!---------------------------------------------------------------------------
! !REVISION HISTORY:
!
!  March 2007      S.Moorthi    Initial code.
!  March 2008      Y.-T. Hou    add Sunshine_Duration (suntim) to Flx_Var_Data
!  Jan 2009        S.Moorthi      add Ho Chun's changes
!  Apr 2009        Y.-T. Hou    add surface lw emissivity (sfcemis)
!  Nov 2009        Sarah Lu,    add rain and rainc
!  Sep 2010        Sarah Lu,    add wet1
!  June 2013       H Chuang     add sr
!  Oct 2013        Xingren Wu,  add DUSFCI/DVSFCI
!  Jun 2016        Fanglin Yang removed pointers for fixing digital filter
!  Jul 2016        S.Moorthi    made it compact
!
! !INTERFACE:
!
 MODULE gfs_physics_sfc_flx_mod

 use machine , only : kind_phys

 IMPLICIT none

 TYPE Sfc_Var_Data
    real(kind=kind_phys),allocatable, dimension(:,:)   ::        &
         tsea,   weasd,  sncovr, tg3,    zorl,  cv,    cvb, cvt, &
         alvsf,  alvwf,  alnsf,  alnwf,  slmsk, vfrac, canopy,   &
         f10m,   t2m,    q2m,    vtype,  stype, facsf, facwf,    &
         uustar, ffmm,   ffhh,   hice,   fice,  tisfc, tprcp,    &
         srflag, snwdph, shdmin, shdmax, slope, snoalb,          &
         oro,    oro_uf
    real(kind=kind_phys),allocatable, dimension(:,:,:) ::        &
         smc, stc, slc
 end type Sfc_Var_Data
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 TYPE Flx_Var_Data
    real(kind=kind_phys),allocatable, dimension(:,:) ::           &
         SFCDSW,  COSZEN,   TMPMIN,   TMPMAX,  SPFHMIN, SPFHMAX,   &
         DUSFC,   DVSFC,    DTSFC,    DQSFC,   DLWSFC,  ULWSFC,    &
         GFLUX,   RUNOFF,   EP,       CLDWRK,  DUGWD,   DVGWD,     &
         PSMEAN,  totprcp,  cnvprcp,  SFCNSW,  SFCDLW,  TSFLW,     &
         PSURF,   U10M,     V10M,     HPBL,    PWAT,    CHH,       &
         CMM,     EPI,      DLWSFCI,  ULWSFCI, USWSFCI, DSWSFCI,   &
         DUSFCI,  DVSFCI,   DTSFCI,   DQSFCI,  GFLUXI,  SRUNOFF,   &
         T1,      Q1,       U1,       V1,      ZLVL,    EVBSA,     &
         EVCWA,   TRANSA,   SBSNOA,   SNOWCA,  SOILM,   SNOHFA,    &
         SMCWLT2, SMCREF2,  suntim,   sfcemis, RAIN,    RAINC,     &
         WET1,    sr

 end type Flx_Var_Data
 END MODULE gfs_physics_sfc_flx_mod
