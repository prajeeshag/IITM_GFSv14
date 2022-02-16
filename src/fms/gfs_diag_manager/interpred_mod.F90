module interpred_mod

  use constants_mod, only : con_pi=>PI
  use mpp_mod, only : handle_error=>mpp_error, FATAL, WARNING
  use horiz_interp_mod, only: horiz_interp_type, horiz_interp_init, horiz_interp, horiz_interp_new
  use diag_data_mod, only: fill_value

  implicit none

  private

  public :: init_interpred, interp2regular_fms

  integer :: lats_node_r, lonr, latr
  logical :: debug=.false., initialized_this_module=.false.
  character (len=32) :: reg2red_interp_method='conservative', red2reg_interp_method='bilinear'
  type(horiz_interp_type), allocatable :: interp2reg(:), interp2red(:)
  integer, allocatable :: nlon(:)
  integer :: verbose=0

  interface interp2regular_fms
     module procedure interp2regular1_fms
     module procedure interp2regular2_fms
  end interface interp2regular_fms

  namelist/interpred_mod_nml/debug
  
  contains
  
  subroutine init_interpred(ipt_lats_node_r, xlon, xlonf, global_lats_r, lonsperlar)
    implicit none
    integer, intent(in) :: ipt_lats_node_r
    integer, intent(in) :: global_lats_r(:), lonsperlar(:)
    real, intent(in) :: xlon(:,:), xlonf(:)

    real :: red_dx, reg_dx
    integer :: j, i, lat
    real :: lon_in(size(xlon,1)+1), lon_out(size(xlonf,1)+1), dlon_in, dlon_out
    integer :: istat

    lats_node_r = size(xlon,2)
    lonr = size(xlon,1)
    latr = size(global_lats_r,1) 
    
    if(initialized_this_module) return
    
    call horiz_interp_init()

    allocate(interp2reg(lats_node_r), interp2red(lats_node_r))
    allocate(nlon(lats_node_r))
    dlon_out = xlonf(2)-xlonf(1)
    lon_out(1) = xlonf(1)-(dlon_out*0.5)

    do i = 2, size(lon_out)
       lon_out(i) = lon_out(i-1) + dlon_out
    enddo

    do j=1,lats_node_r
      lat = global_lats_r(ipt_lats_node_r-1+j)
      nlon(j) = lonsperlar(lat)
      
      dlon_in = xlon(2,j)-xlon(1,j)
      lon_in(1) = xlon(1,j)-(dlon_in*0.5)
      do i = 2, nlon(j)+1
         lon_in(i) = lon_in(i-1) + dlon_in
      enddo

      call horiz_interp_new (lon_in=lon_in(1:nlon(j)+1), lat_in=(/0.0,0.01/), lon_out=lon_out(:), lat_out=(/0.0,0.01/),  &
           verbose=verbose, interp_method=trim(red2reg_interp_method), src_modulo=.true., interp=interp2reg(j))
      call horiz_interp_new (lon_in=lon_out(:), lat_in=(/0.0,0.01/), lon_out=lon_in(1:nlon(j)+1), lat_out=(/0.0,0.01/),  &
           verbose=verbose, interp_method=trim(reg2red_interp_method), src_modulo=.true., interp=interp2red(j))
    enddo
        
    initialized_this_module=.true.
  end subroutine init_interpred

 subroutine interp2regular2_fms(fi, f, imask)
   implicit none
   real,intent(in):: fi(:,:)
   real,intent(out):: f(:,:)
   real, intent(in), optional :: imask(:,:)
   
   logical :: mask(size(fi,1),size(fi,2))
   real :: rmask(size(fi,1),size(fi,2))
   integer :: j,lons,lat

    if(.not.initialized_this_module) &
         call handle_error(FATAL, 'Module not initialized : interp2regular')
    
    rmask(:,:)=1.0

    if (present(imask)) rmask=imask
    
    do j = 1, lats_node_r
       lons=nlon(j)
       call horiz_interp ( interp2reg(j), fi(1:lons,j:j), f(1:lonr,j:j), mask_in=rmask(1:lons,j:j), missing_value=fill_value)
    enddo
   
 end subroutine interp2regular2_fms
 
 subroutine interp2regular1_fms(fi, f, lan, imask)
   implicit none
   real,intent(in):: fi(:)
   real,intent(out):: f(:,:)
   integer, intent(in) :: lan
   real, intent(in), optional :: imask(:)
    
   integer :: j,lons,lat
   real :: fi1(size(fi),1), f1(size(f),1), imask1(size(fi1),1)

   if(.not.initialized_this_module) &
        call handle_error(FATAL, 'Module not initialized : interp2regular')
    
   j = lan
   lons=nlon(j)
   fi1(:,1) = fi
   imask1 = 1.0
   if(present(imask)) imask1(:,1) = imask(:)
   call horiz_interp ( interp2reg(j), fi1(:,:), f(:,:), mask_in=imask1(:,:), missing_value=fill_value)
   
 end subroutine interp2regular1_fms

end module interpred_mod
