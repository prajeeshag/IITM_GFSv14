      subroutine dfi_fixwr(iflag,sfc_fld,flx_fld,nst_fld)

!***********************************************************************
!     PURPOSE:
!      save or retrieve fixed fields in digifilt
!
!     REVISION HISOTRY:
!     2011-12-05  J.Wang    Adopted from fixwr in /nwprod/sorc/global_fcst.fd
!     1014-03-00  F.Yang    Added flx fields and simplified using derived type in GFS
!     2014-09-18  S.Moorthi cleaned up and made nst model an option
!     2016-03-29  S.Moorthi deallocted allocated arrays after digital filter
!     2016-06-01  X. Li/F.Yang Modified flx fields for NEMS/GSM
!                            and deleted moorthi's changes
!     2016-07-07  S. Moorthi - added nstf_name back, added routines to deallocate
!                              sfc_flds, flx_flds and nst_flds
!
!***********************************************************************
      use resol_def
      use namelist_physics_def, only : nstf_name
      use layout1
      use gfs_physics_sfc_flx_mod,     ONLY: sfc_var_data, flx_var_data
      use gfs_physics_sfc_flx_set_mod, ONLY: sfcvar_aldata,flxvar_aldata
     &,                                      flx_init, sfcvar_axdata
     &,                                                flxvar_axdata
      use gfs_physics_nst_var_mod,     ONLY: nst_var_data, nstvar_aldata
     &,                                      nstvar_axdata
      implicit none
      integer iflag, ierr
      type(sfc_var_data)        :: sfc_fld, sfc_flds
      type(flx_var_data)        :: flx_fld, flx_flds
      type(nst_var_data)        :: nst_fld, nst_flds

      logical first
      data first/.true./
      save  first, sfc_flds, flx_flds, nst_flds
      if (first) then
        call sfcvar_aldata(lonr,lats_node_r,lsoil,sfc_flds,ierr)
        call flxvar_aldata(lonr,lats_node_r,flx_flds,ierr)
        call flx_init(flx_flds,ierr)
        if (nstf_name(1) > 0) then
          call nstvar_aldata(lonr,lats_node_r,nst_flds,ierr)
        endif
        first = .false.
      endif
      if(iflag == 1) then
        sfc_flds = sfc_fld
        flx_flds = flx_fld
        if (nstf_name(1) > 0) then
          nst_flds = nst_fld
        endif
      elseif(iflag == 2) then
        sfc_fld = sfc_flds
        flx_fld = flx_flds
        call sfcvar_axdata(sfc_flds)
        call flxvar_axdata(flx_flds)
        if (nstf_name(1) > 0) then
          nst_fld = nst_flds
          call nstvar_axdata(nst_flds)
        endif
      endif

      return
      end subroutine dfi_fixwr

