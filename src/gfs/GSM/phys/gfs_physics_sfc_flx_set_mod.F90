!
! !MODULE: gfs_physics_sfc_flx_mod  ---      Definition of the surface
!                                            fields in the ESMF internal state.
!
! !DESCRIPTION: gfs_physics_sfc_flx_mod ---    Define the surfacee  variables
!                                              in the ESMF internal state.
!---------------------------------------------------------------------------
! !REVISION HISTORY:
!
!  March 2007      Shrinivas Moorthi Initial code.
!  March 2008      Y.-T. Hou    add Sunshine_Duration (suntim) to Flx_Var_Data
!  Jan 2009        Moorthi      add Ho Chun's changes
!  Apr 2009        Y.-T. Hou    add surface lw emissivity (sfcemis)
!  Nov 2009        Sarah Lu,    add rain and rainc
!  Sep 2010        Sarah Lu,    add wet1
!  Nov 2011        Sarah Lu,    init wet1
!  Oct 2012        S. Moorthi   add oro_uf
!  Aug 2013        S. Moorthi - Adding Huiya's addition - sr variable from gfs
!  Oct 2013        Xingren Wu   add flx_fld%DUSFCI/DVSFCI
!  Aug 2015        S. Moorthi - add subroutine sfc_init
!  Jun 2016        F. Yang, Change pointer to allocatable for DFI (dfi_fixwr)
!  Jun 2016        X. Li      - Add instantanious flux variables initialization in flxvar_aldata
!
! !INTERFACE:
!
 MODULE gfs_physics_sfc_flx_set_mod

 use machine , only : kind_phys

 IMPLICIT none

    contains
    subroutine sfcvar_aldata(dim1, dim2, dim3, sfc_fld, iret)

    USE gfs_physics_sfc_flx_mod, ONLY: Sfc_Var_Data
    implicit none

    TYPE(Sfc_Var_Data), INTENT(inout) :: sfc_fld 
    integer, intent(in)               :: dim1, dim2, dim3

    integer, intent(out)             :: iret
!
allocate(                                  &
           sfc_fld%tsea   (dim1,dim2),     &
           sfc_fld%smc    (dim3,dim1,dim2),&
           sfc_fld%weasd  (dim1,dim2),     &
           sfc_fld%sncovr (dim1,dim2),     &
           sfc_fld%stc    (dim3,dim1,dim2),&
           sfc_fld%tg3    (dim1,dim2),     &
           sfc_fld%zorl   (dim1,dim2),     &
           sfc_fld%cv     (dim1,dim2),     &
           sfc_fld%cvb    (dim1,dim2),     &
           sfc_fld%cvt    (dim1,dim2),     &
           sfc_fld%alvsf  (dim1,dim2),     &
           sfc_fld%alvwf  (dim1,dim2),     &
           sfc_fld%alnsf  (dim1,dim2),     &
           sfc_fld%alnwf  (dim1,dim2),     &
           sfc_fld%slmsk  (dim1,dim2),     &
           sfc_fld%vfrac  (dim1,dim2),     &
           sfc_fld%canopy (dim1,dim2),     &
           sfc_fld%f10m   (dim1,dim2),     &
           sfc_fld%t2m    (dim1,dim2),     &
           sfc_fld%q2m    (dim1,dim2),     &
           sfc_fld%vtype  (dim1,dim2),     &
           sfc_fld%stype  (dim1,dim2),     &
           sfc_fld%facsf  (dim1,dim2),     &
           sfc_fld%facwf  (dim1,dim2),     &
           sfc_fld%uustar (dim1,dim2),     &
           sfc_fld%ffmm   (dim1,dim2),     &
           sfc_fld%ffhh   (dim1,dim2),     &
           sfc_fld%hice   (dim1,dim2),     &
           sfc_fld%fice   (dim1,dim2),     &
           sfc_fld%tisfc  (dim1,dim2),     &
           sfc_fld%tprcp  (dim1,dim2),     &
           sfc_fld%srflag (dim1,dim2),     &
           sfc_fld%snwdph (dim1,dim2),     &
           sfc_fld%slc    (dim3,dim1,dim2),&
           sfc_fld%shdmin (dim1,dim2),     &
           sfc_fld%shdmax (dim1,dim2),     &
           sfc_fld%slope  (dim1,dim2),     &
           sfc_fld%snoalb (dim1,dim2),     &
           sfc_fld%oro    (dim1,dim2),     &
           sfc_fld%oro_uf (dim1,dim2),     &
           stat=iret)
    if(iret.ne.0) iret=-3
    return
  end subroutine
    subroutine sfcvar_axdata(sfc_fld)

    USE gfs_physics_sfc_flx_mod, ONLY: Sfc_Var_Data
    implicit none

    TYPE(Sfc_Var_Data), INTENT(inout) :: sfc_fld
    deallocate(                                                            &
           sfc_fld%tsea,   sfc_fld%smc,    sfc_fld%weasd,  sfc_fld%sncovr, &
           sfc_fld%stc,    sfc_fld%tg3,    sfc_fld%zorl,   sfc_fld%cv,     &
           sfc_fld%cvb,    sfc_fld%cvt,    sfc_fld%alvsf,  sfc_fld%alvwf,  &
           sfc_fld%alnsf,  sfc_fld%alnwf,  sfc_fld%slmsk,  sfc_fld%vfrac,  &
           sfc_fld%canopy, sfc_fld%f10m,   sfc_fld%t2m,    sfc_fld%q2m,    &
           sfc_fld%vtype,  sfc_fld%stype,  sfc_fld%facsf,  sfc_fld%facwf,  &
           sfc_fld%uustar, sfc_fld%ffmm,   sfc_fld%ffhh,   sfc_fld%hice,   & 
           sfc_fld%fice,   sfc_fld%tisfc,  sfc_fld%tprcp,  sfc_fld%srflag, &
           sfc_fld%snwdph, sfc_fld%slc,    sfc_fld%shdmin, sfc_fld%shdmax, &
           sfc_fld%slope,  sfc_fld%snoalb, sfc_fld%oro,    sfc_fld%oro_uf )
    return
    end subroutine sfcvar_axdata
    subroutine flxvar_aldata(dim1, dim2, flx_fld, iret)

    USE gfs_physics_sfc_flx_mod, ONLY: Flx_Var_Data
    implicit none
    TYPE(Flx_Var_Data), INTENT(inout) :: flx_fld 
    integer, intent(in)               :: dim1, dim2

    integer, intent(out)              :: iret
    real, parameter                   :: flxval = -999.0
    integer                           :: i, j
!
    allocate(                          &
          flx_fld%SFCDSW  (dim1,dim2), &
          flx_fld%COSZEN  (dim1,dim2), &
          flx_fld%TMPMIN  (dim1,dim2), &
          flx_fld%TMPMAX  (dim1,dim2), &
          flx_fld%SPFHMIN (dim1,dim2), &
          flx_fld%SPFHMAX (dim1,dim2), &
          flx_fld%DUSFC   (dim1,dim2), &
          flx_fld%DVSFC   (dim1,dim2), &
          flx_fld%DTSFC   (dim1,dim2), &
          flx_fld%DQSFC   (dim1,dim2), &
          flx_fld%DLWSFC  (dim1,dim2), &
          flx_fld%ULWSFC  (dim1,dim2), &
          flx_fld%GFLUX   (dim1,dim2), &
          flx_fld%RUNOFF  (dim1,dim2), &
          flx_fld%EP      (dim1,dim2), &
          flx_fld%CLDWRK  (dim1,dim2), &
          flx_fld%DUGWD   (dim1,dim2), &
          flx_fld%DVGWD   (dim1,dim2), &
          flx_fld%PSMEAN  (dim1,dim2), &
          flx_fld%totprcp (dim1,dim2), &
          flx_fld%cnvprcp (dim1,dim2), &
          flx_fld%SFCNSW  (dim1,dim2), &
          flx_fld%SFCDLW  (dim1,dim2), &
          flx_fld%TSFLW   (dim1,dim2), &
          flx_fld%PSURF   (dim1,dim2), &
          flx_fld%U10M    (dim1,dim2), &
          flx_fld%V10M    (dim1,dim2), &
          flx_fld%HPBL    (dim1,dim2), &
          flx_fld%PWAT    (dim1,dim2), &
          flx_fld%CHH     (dim1,dim2), &
          flx_fld%CMM     (dim1,dim2), &
          flx_fld%EPI     (dim1,dim2), &
          flx_fld%DLWSFCI (dim1,dim2), &
          flx_fld%ULWSFCI (dim1,dim2), &
          flx_fld%USWSFCI (dim1,dim2), &
          flx_fld%DSWSFCI (dim1,dim2), &
          flx_fld%DUSFCI  (dim1,dim2), &
          flx_fld%DVSFCI  (dim1,dim2), &
          flx_fld%DTSFCI  (dim1,dim2), &
          flx_fld%DQSFCI  (dim1,dim2), &
          flx_fld%GFLUXI  (dim1,dim2), &
          flx_fld%SRUNOFF (dim1,dim2), &
          flx_fld%T1      (dim1,dim2), &
          flx_fld%Q1      (dim1,dim2), &
          flx_fld%U1      (dim1,dim2), &
          flx_fld%V1      (dim1,dim2), &
          flx_fld%ZLVL    (dim1,dim2), &
          flx_fld%EVBSA   (dim1,dim2), &
          flx_fld%EVCWA   (dim1,dim2), &
          flx_fld%TRANSA  (dim1,dim2), &
          flx_fld%SBSNOA  (dim1,dim2), &
          flx_fld%SNOWCA  (dim1,dim2), &
          flx_fld%SOILM   (dim1,dim2), &
          flx_fld%SNOHFA  (dim1,dim2), &
          flx_fld%SMCWLT2 (dim1,dim2), &
          flx_fld%SMCREF2 (dim1,dim2), &
          flx_fld%suntim  (dim1,dim2), &                
          flx_fld%sfcemis (dim1,dim2), &               
          flx_fld%RAIN    (dim1,dim2), &
          flx_fld%RAINC   (dim1,dim2), &
          flx_fld%WET1    (dim1,dim2), &
          flx_fld%sr      (dim1,dim2), &
          stat=iret)

          flx_fld%SFCDSW  = 0.0
          flx_fld%COSZEN  = 0.0
          flx_fld%PWAT    = 0.0
          flx_fld%SFCNSW  = 0.0
          flx_fld%SFCDLW  = 0.0
          flx_fld%TSFLW   = 0.0
          flx_fld%PSURF   = 0.0
          flx_fld%U10M    = 0.0
          flx_fld%V10M    = 0.0
          flx_fld%HPBL    = 0.0
          flx_fld%CHH     = 0.0
          flx_fld%CMM     = 0.0
          flx_fld%EPI     = 0.0
          flx_fld%DLWSFCI = 0.0
          flx_fld%ULWSFCI = 0.0
          flx_fld%USWSFCI = 0.0
          flx_fld%DSWSFCI = 0.0
          flx_fld%DUSFCI  = 0.0
          flx_fld%DVSFCI  = 0.0
          flx_fld%DTSFCI  = 0.0
          flx_fld%DQSFCI  = 0.0
          flx_fld%GFLUXI  = 0.0
          flx_fld%T1      = 0.0
          flx_fld%Q1      = 0.0
          flx_fld%U1      = 0.0
          flx_fld%V1      = 0.0
          flx_fld%ZLVL    = 0.0
          flx_fld%SOILM   = 0.0
          flx_fld%SMCWLT2 = 0.0
          flx_fld%SMCREF2 = 0.0
          flx_fld%sfcemis = 0.0
          flx_fld%RAIN    = 0.0
          flx_fld%RAINC   = 0.0

   
    if(iret /= 0) iret = -4

!    Initialize to zero the first time
!
!$omp parallel do private(i,j)
    do j=1,dim2
      do i=1,dim1
        flx_fld%SFCDSW(i,j)  = flxval
        flx_fld%COSZEN(i,j)  = flxval
        flx_fld%TMPMIN(i,j)  = flxval 
        flx_fld%TMPMAX(i,j)  = flxval 
        flx_fld%SPFHMIN(i,j) = flxval
        flx_fld%SPFHMAX(i,j) = flxval
        flx_fld%DUSFC(i,j)   = flxval 
        flx_fld%DVSFC(i,j)   = flxval 
        flx_fld%DTSFC(i,j)   = flxval
        flx_fld%DQSFC(i,j)   = flxval
        flx_fld%DLWSFC(i,j)  = flxval 
        flx_fld%ULWSFC(i,j)  = flxval
        flx_fld%GFLUX(i,j)   = flxval
        flx_fld%RUNOFF(i,j)  = flxval 
        flx_fld%EP(i,j)      = flxval    
        flx_fld%CLDWRK(i,j)  = flxval 
        flx_fld%DUGWD(i,j)   = flxval 
        flx_fld%DVGWD(i,j)   = flxval 
        flx_fld%PSMEAN(i,j)  = flxval
        flx_fld%totprcp(i,j) = flxval 
        flx_fld%cnvprcp(i,j) = flxval 
        flx_fld%SFCNSW(i,j)  = flxval 
        flx_fld%SFCDLW(i,j)  = flxval 
        flx_fld%TSFLW(i,j)   = flxval 
        flx_fld%PSURF(i,j)   = flxval  
        flx_fld%U10M(i,j)    = flxval  
        flx_fld%V10M(i,j)    = flxval 
        flx_fld%HPBL(i,j)    = flxval 
        flx_fld%PWAT(i,j)    = flxval
        flx_fld%CHH(i,j)     = flxval 
        flx_fld%CMM(i,j)     = flxval 
        flx_fld%EPI(i,j)     = flxval 
        flx_fld%DLWSFCI(i,j) = flxval 
        flx_fld%ULWSFCI(i,j) = flxval 
        flx_fld%USWSFCI(i,j) = flxval 
        flx_fld%DSWSFCI(i,j) = flxval 
        flx_fld%DUSFCI(i,j)  = flxval 
        flx_fld%DVSFCI(i,j)  = flxval 
        flx_fld%DTSFCI(i,j)  = flxval 
        flx_fld%DQSFCI(i,j)  = flxval 
        flx_fld%GFLUXI(i,j)  = flxval
        flx_fld%SRUNOFF(i,j) = flxval
        flx_fld%T1(i,j)      = flxval 
        flx_fld%Q1(i,j)      = flxval
        flx_fld%U1(i,j)      = flxval
        flx_fld%V1(i,j)      = flxval
        flx_fld%ZLVL(i,j)    = flxval 
        flx_fld%EVBSA(i,j)   = flxval 
        flx_fld%EVCWA(i,j)   = flxval 
        flx_fld%TRANSA(i,j)  = flxval
        flx_fld%SBSNOA(i,j)  = flxval
        flx_fld%SNOWCA(i,j)  = flxval
        flx_fld%SOILM(i,j)   = flxval 
        flx_fld%SNOHFA(i,j)  = flxval 
        flx_fld%SMCWLT2(i,j) = flxval
        flx_fld%SMCREF2(i,j) = flxval
        flx_fld%suntim(i,j)  = flxval 
        flx_fld%sfcemis(i,j) = flxval 
        flx_fld%RAIN(i,j)    = flxval 
        flx_fld%RAINC(i,j)   = flxval
        flx_fld%WET1(i,j)    = flxval
        flx_fld%sr(i,j)      = flxval 
      enddo
    enddo
    return
  end subroutine
   subroutine flxvar_axdata(flx_fld)

    USE gfs_physics_sfc_flx_mod, ONLY: Flx_Var_Data
    implicit none
    TYPE(Flx_Var_Data), INTENT(inout) :: flx_fld
    deallocate(                                              &
          flx_fld%SFCDSW,  flx_fld%COSZEN,  flx_fld%TMPMIN,  &
          flx_fld%TMPMAX,  flx_fld%SPFHMIN, flx_fld%SPFHMAX, &
          flx_fld%DUSFC,   flx_fld%DVSFC,   flx_fld%DTSFC,   &
          flx_fld%DQSFC,   flx_fld%DLWSFC,  flx_fld%ULWSFC,  &
          flx_fld%GFLUX,   flx_fld%RUNOFF,  flx_fld%EP,      &
          flx_fld%CLDWRK,  flx_fld%DUGWD,   flx_fld%DVGWD,   &  
          flx_fld%PSMEAN,  flx_fld%totprcp, flx_fld%cnvprcp, &
          flx_fld%SFCNSW,  flx_fld%SFCDLW,  flx_fld%TSFLW,   &  
          flx_fld%PSURF,   flx_fld%U10M,    flx_fld%V10M,    & 
          flx_fld%HPBL,    flx_fld%PWAT,    flx_fld%CHH,     &  
          flx_fld%CMM,     flx_fld%EPI,     flx_fld%DLWSFCI, & 
          flx_fld%ULWSFCI, flx_fld%USWSFCI, flx_fld%DSWSFCI, & 
          flx_fld%DUSFCI,  flx_fld%DVSFCI,  flx_fld%DTSFCI,  & 
          flx_fld%DQSFCI,  flx_fld%GFLUXI,  flx_fld%SRUNOFF, &
          flx_fld%T1,      flx_fld%Q1,      flx_fld%U1,      &    
          flx_fld%V1,      flx_fld%ZLVL,    flx_fld%EVBSA,   &  
          flx_fld%EVCWA,   flx_fld%TRANSA,  flx_fld%SBSNOA,  & 
          flx_fld%SNOWCA,  flx_fld%SOILM,   flx_fld%SNOHFA,  & 
          flx_fld%SMCWLT2, flx_fld%SMCREF2, flx_fld%suntim,  &  
          flx_fld%sfcemis, flx_fld%RAIN,    flx_fld%RAINC,   &  
          flx_fld%WET1,    flx_fld%sr )

    return
  end subroutine flxvar_axdata

    subroutine flx_init(flx_fld, iret)

    USE gfs_physics_sfc_flx_mod, ONLY: Flx_Var_Data
    implicit none
    TYPE(Flx_Var_Data), INTENT(inout) :: flx_fld 

    integer, intent(out)             :: iret
!
    flx_fld%TMPMIN  = 1.e4
    flx_fld%TMPMAX  = 0.
    flx_fld%SPFHMIN = 1.e10
    flx_fld%SPFHMAX = 0.
    flx_fld%totprcp = 0.
    flx_fld%cnvprcp = 0.
    flx_fld%DUSFC   = 0.
    flx_fld%DVSFC   = 0.
    flx_fld%DTSFC   = 0.
    flx_fld%DQSFC   = 0.
    flx_fld%DLWSFC  = 0.
    flx_fld%ULWSFC  = 0.
    flx_fld%GFLUX   = 0.
    flx_fld%RUNOFF  = 0.
    flx_fld%EP      = 0.
    flx_fld%CLDWRK  = 0.
    flx_fld%DUGWD   = 0.
    flx_fld%DVGWD   = 0.
    flx_fld%PSMEAN  = 0.
    flx_fld%EVBSA   = 0.
    flx_fld%EVCWA   = 0.
    flx_fld%TRANSA  = 0.
    flx_fld%SBSNOA  = 0.
    flx_fld%SNOWCA  = 0.
    flx_fld%SRUNOFF = 0.
    flx_fld%SNOHFA  = 0.
    flx_fld%SUNTIM  = 0.
!for gocart
    flx_fld%wet1    = 0.
    flx_fld%sr      = 0.
    flx_fld%SOILM   = 0.

     iret = 0

     return
  end subroutine
    subroutine sfc_init(sfc_fld, iret)

    USE gfs_physics_sfc_flx_mod, ONLY: Sfc_Var_Data
    implicit none

    TYPE(Sfc_Var_Data), INTENT(inout) :: sfc_fld 

    integer, intent(out)             :: iret
!
    sfc_fld%tsea   = 0.0
    sfc_fld%smc    = 0.0
    sfc_fld%weasd  = 0.0
    sfc_fld%sncovr = 0.0
    sfc_fld%stc    = 0.0
    sfc_fld%tg3    = 0.0
    sfc_fld%zorl   = 0.0
    sfc_fld%cv     = 0.0
    sfc_fld%cvb    = 0.0
    sfc_fld%cvt    = 0.0
    sfc_fld%alvsf  = 0.0
    sfc_fld%alvwf  = 0.0
    sfc_fld%alnsf  = 0.0
    sfc_fld%alnwf  = 0.0
    sfc_fld%slmsk  = 0.0
    sfc_fld%vfrac  = 0.0
    sfc_fld%canopy = 0.0
    sfc_fld%f10m   = 0.0
    sfc_fld%t2m    = 0.0
    sfc_fld%q2m    = 0.0
    sfc_fld%vtype  = 0.0
    sfc_fld%stype  = 0.0
    sfc_fld%facsf  = 0.0
    sfc_fld%facwf  = 0.0
    sfc_fld%uustar = 0.0
    sfc_fld%ffmm   = 0.0
    sfc_fld%ffhh   = 0.0
    sfc_fld%hice   = 0.0
    sfc_fld%fice   = 0.0
    sfc_fld%tisfc  = 0.0
    sfc_fld%tprcp  = 0.0
    sfc_fld%srflag = 0.0
    sfc_fld%snwdph = 0.0
    sfc_fld%slc    = 0.0
    sfc_fld%shdmin = 0.0
    sfc_fld%shdmax = 0.0
    sfc_fld%slope  = 0.0
    sfc_fld%snoalb = 0.0
    sfc_fld%oro    = 0.0
    sfc_fld%oro_uf = 0.0
    if(iret.ne.0) iret=-4
    return
  end subroutine
 END MODULE gfs_physics_sfc_flx_set_mod
