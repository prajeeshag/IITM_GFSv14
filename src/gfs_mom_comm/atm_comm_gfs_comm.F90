module atm_comm_gfs_comm_mod
#ifdef use_GFS_COMM
  use gfs_comm_mod, only: gfs_comm_isend, gfs_comm_irecv
  use gfs_comm_mod, only: set_nfrecv, set_nfsend, deactivated
  use interpred_mod, only: interp2regular, interp2reduced
  use ocean_mod, only: ocn
  use land_model_mod, only: lnd 
  implicit none
  private
 
  logical :: debug=.false.
  
  public :: atm_getsstice_gfs_comm, atm_sendfluxes_gfs_comm 
  contains 
  
  subroutine atm_getsstice_gfs_comm
    integer :: loc(2)
    if (deactivated) return 
    call atm_tiles_recv_gfs_comm(ocn%fice)
    call atm_tiles_recv_gfs_comm(ocn%hice)
    call atm_tiles_recv_gfs_comm(ocn%hsnow)
    call atm_tiles_recv_gfs_comm(ocn%tice)
    call atm_tiles_recv_gfs_comm(ocn%twater)
    where(lnd%ofrac(:,:)>0.0)
      ocn%twater(:,:)=ocn%twater(:,:)/lnd%ofrac(:,:)
      ocn%fice(:,:)=ocn%fice(:,:)/lnd%ofrac(:,:)
      ocn%hice(:,:)=ocn%hice(:,:)/lnd%ofrac(:,:)
      ocn%hsnow(:,:)=ocn%hsnow(:,:)/lnd%ofrac(:,:)
      ocn%tice(:,:)=ocn%tice(:,:)/lnd%ofrac(:,:)
    elsewhere
      !ocn%twater(:,:)=0.0
      ocn%fice(:,:)=0.0
      ocn%hice(:,:)=0.0
      ocn%hsnow(:,:)=0.0
      ocn%tice(:,:)=0.0
    endwhere

    ocn%hsnow(:,:)=ocn%hsnow(:,:)
    ocn%fwater(:,:)=0.0
    where(lnd%ofrac>0.0) ocn%fwater(:,:)=1.0-ocn%fice(:,:)
    
    where(ocn%fice(:,:)>0.0)
      ocn%tice(:,:)=ocn%tice(:,:)/ocn%fice(:,:)
      ocn%hice(:,:)=ocn%hice(:,:)/ocn%fice(:,:)
      ocn%hsnow(:,:)=ocn%hsnow(:,:)/ocn%fice(:,:)
    elsewhere
      ocn%tice(:,:)=0.0
      ocn%hice(:,:)=0.0
      ocn%hsnow(:,:)=0.0
    endwhere

    where(ocn%fwater(:,:)>0.0)
      ocn%twater(:,:)=ocn%twater(:,:)/ocn%fwater(:,:)
    elsewhere
      !ocn%twater(:,:)=0.0
    endwhere
    
    if(debug) then
      print *, 'atm fice:', minval(ocn%fice), maxval(ocn%fice)
      print *, 'atm twater:', minval(ocn%twater,mask=ocn%fwater>0.0), &   
           maxval(ocn%twater)
      loc(:) = minloc(ocn%twater,mask=ocn%fwater>0.0)
      print *, 'atm fwater(min(twater)):', ocn%fwater(loc(1),loc(2))
      print *, 'atm lfrac(min(twater)):', lnd%frac(loc(1),loc(2))
      print *, 'atm ofrac(min(twater)):', lnd%ofrac(loc(1),loc(2))
      print *, 'atm fice(min(twater)):', ocn%fice(loc(1),loc(2))
      print *, 'atm fwater:', minval(ocn%fwater), maxval(ocn%fwater)
      print *, 'atm hice:', minval(ocn%hice), maxval(ocn%hice)
      print *, 'atm hsnow:', minval(ocn%hsnow), maxval(ocn%hsnow)
      print *, 'atm tice:', minval(ocn%tice, mask=ocn%fice>0.0), maxval(ocn%tice)
    endif
  end subroutine atm_getsstice_gfs_comm

  subroutine atm_tiles_recv_gfs_comm(f)
    implicit none
    real, intent(out) :: f(:,:)
    real :: f1(size(f,1),size(f,2))

    call gfs_comm_irecv(f1)
    call interp2reduced(f1,f)
  end subroutine atm_tiles_recv_gfs_comm

  subroutine atm_sendfluxes_gfs_comm()
    if (deactivated) return 
    call set_nfsend(14)
    call atm_sendflux_gfs_com(ocn%nsw_i)
    call atm_sendflux_gfs_com(ocn%nsw_o)
    call atm_sendflux_gfs_com(ocn%dlw)
    call atm_sendflux_gfs_com(ocn%shflx_i)
    call atm_sendflux_gfs_com(ocn%shflx_o)
    call atm_sendflux_gfs_com(ocn%lhflx_i)
    call atm_sendflux_gfs_com(ocn%lhflx_o)
    call atm_sendflux_gfs_com(ocn%lprec)
    call atm_sendflux_gfs_com(ocn%fprec)
    call atm_sendflux_gfs_com(ocn%drdt)
    call atm_sendflux_gfs_com(ocn%dhdt)
    call atm_sendflux_gfs_com(ocn%dedt)
    call atm_sendflux_gfs_com(ocn%uflux)
    call atm_sendflux_gfs_com(ocn%vflux)
  end subroutine atm_sendfluxes_gfs_comm

  subroutine atm_sendflux_gfs_com(f)
    real, intent(in) :: f(:,:)
    real, dimension(size(f,1),size(f,2)):: f1
    call interp2regular(f,f1)
              ! <- interpolation from reduced grid (i.e. with # of
              ! longitudes varying from lat. circle to lat. circle)
              ! to full grid
    call gfs_comm_isend(f1)
  end subroutine atm_sendflux_gfs_com
#endif
end module atm_comm_gfs_comm_mod
