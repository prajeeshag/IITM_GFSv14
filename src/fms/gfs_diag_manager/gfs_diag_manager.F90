module gfs_diag_manager_mod

   use mpp_mod, only: handle_error=>mpp_error, FATAL, WARNING, NOTE, &
                      mpp_pe, mpp_root_pe
   use mpp_io_mod, only: mpp_open, mpp_close, mpp_write_meta, MPP_OVERWR, MPP_SINGLE, &
                         axistype, MPP_NETCDF, mpp_write, MPP_ASCII
   use fms_io_mod,       only: fms_io_init, write_data
   use constants_mod,    only: RAD_TO_DEG, PI
   use time_manager_mod, only: time_type, print_date, set_date, set_time, print_time
   use diag_manager_mod, only: diag_manager_init, diag_manager_end, diag_send_complete
   use diag_manager_mod, only: diag_axis_init, register_static_field, diag_manager_set_time_end
   use diag_manager_mod, only: register_diag_field, send_data
   use diag_manager_mod, only: set_diag_global_att
   use diag_data_mod,    only: fill_value=>CMOR_MISSING_VALUE
   use interpred_mod,    only: init_interpred

   implicit none
   private

   integer, public :: id_pr=0, id_ts=0, id_tas=0
   integer, public :: id_ta=0, id_ua=0, id_va=0, id_ps=0

   integer :: lon_id, lat_id, lev_id
   integer :: lonr, latr, lats_node_r
   integer :: max_nlevs=100, n_nlevs=0, total_eles=0
   integer, allocatable :: nlevs(:,:)
   type(time_type) :: currtime, time_step
  
   public :: init_gfs_diag_manager, end_gfs_diag_manager, register_var, &
             update_opdata, register_static, set_current_time, gfs_diag_send_complete

   interface update_opdata
      module procedure update_opdata_2d_o
      module procedure update_opdata_3d_o
      module procedure update_opdata_2d
      module procedure update_opdata_3d
   end interface update_opdata
  
  
   contains

   subroutine init_gfs_diag(levs)
      integer, intent(in) :: levs

      id_pr = register_var('pr', 'Railfall', '?')
      id_ts = register_var('ts', 'Surface Temperature', 'K')
      id_tas = register_var('tas', 'Near-Surface Air Temperature', 'K')
      id_ta = register_var('ta', 'Temperature', 'K', levs=levs)
      id_ua = register_var('ua', 'Temperature', 'm/s', levs=levs)
      id_va = register_var('va', 'Temperature', 'm/s', levs=levs)
      id_ps = register_var('ps', 'Surface Pressure', 'Pascals')

   end subroutine init_gfs_diag



   subroutine write_diag_post_nml(startdate, enddate, deltim, calendar_type)
      integer, intent(in) :: startdate(6), enddate(6), deltim, calendar_type

      integer :: ounit

      namelist/diag_post_nml/ startdate, calendar_type, deltim, enddate

      if (mpp_pe()==mpp_root_pe()) then
         call mpp_open(ounit,'diag_post.nml',action=MPP_OVERWR, &
               form=MPP_ASCII,threading=MPP_SINGLE)
         write(ounit,nml=diag_post_nml)
         call mpp_close(ounit)
      endif

   end subroutine write_diag_post_nml

   subroutine set_current_time(yy,mm,dd,h,m,s)
      integer, intent(in) :: yy, mm, dd, h, m, s 

      currtime = set_date(yy, mm, dd, h, m, s)

      ! if (mpp_pe() == mpp_root_pe()) &
      !    call print_date(currtime, 'FMS diag manager current time:')

   end subroutine set_current_time

   subroutine init_gfs_diag_manager(ipt_lats_node_r, xlat, xlon, ak5, bk5, &
                     global_lats_r, lonsperlar, dt_sec, curr_itime, stop_itime, &
                     calendar_type)
      integer, intent(in) :: ipt_lats_node_r
      real, intent(in) :: xlat(:), xlon(:,:)
      real, intent(in) :: ak5(:), bk5(:)
      integer, intent(in) :: global_lats_r(:), lonsperlar(:)
      integer, intent(in) :: curr_itime(6), stop_itime(6), calendar_type, dt_sec


      real :: xlonf(size(xlon,1)), dlon
    
      integer :: i, lev_id, id, k
      integer :: tmp(size(xlat,1)+1), nk, ounit
      Character (len=32) :: tmpc
      logical :: used
      type(axistype) :: ak_axis, bk_axis
      real :: rtmp(size(ak5,1))

      nk = size(ak5,1)
      if (mpp_pe()==mpp_root_pe()) then
         call mpp_open(ounit,'ak_bk_out.nc',action=MPP_OVERWR, &
               form=MPP_NETCDF,threading=MPP_SINGLE)
         rtmp = [(ak5(k),k=nk,1,-1)] * 1000. ! cb -> Pascal
         call mpp_write_meta(ounit, ak_axis, 'ak', 'Pascal', 'coef_a', data=rtmp)
         rtmp = [(bk5(k),k=nk,1,-1)]
         call mpp_write_meta(ounit, bk_axis, 'bk', '1', 'coef_b', data=rtmp)
         call mpp_write(ounit,ak_axis)
         call mpp_write(ounit,bk_axis)
         call mpp_close(ounit)
      endif

      lonr = size(xlon,1)
      lats_node_r = size(xlat,1)
      latr = size(global_lats_r,1)

      dlon = 2. * PI / lonr
      xlonf = 0.

      do i = 2, lonr
         xlonf(i) = xlonf(i-1) + dlon
      end do

      call diag_manager_init()
      call init_interpred(ipt_lats_node_r, xlon, xlonf, global_lats_r, lonsperlar)

      lon_id = diag_axis_init(name='lon', data=xlonf(:)*RAD_TO_DEG, &
                  units='degrees_east' , cart_name='X', long_name='longitude')
      lat_id = diag_axis_init(name='lat', data=xlat(:)*RAD_TO_DEG, &
                  units='degrees_north' , cart_name='Y', long_name='latitude')

      allocate(nlevs(2,max_nlevs))

      tmp(1)=latr
      total_eles = 0
      do i=1,lats_node_r
         tmp(i+1)=global_lats_r(ipt_lats_node_r-1+i)
         total_eles = total_eles + lonsperlar(tmp(i+1))
      enddo
    
      call set_diag_global_att(decomp=tmp)

      call set_current_time(curr_itime(1), curr_itime(2), curr_itime(3), &
                            curr_itime(4), curr_itime(5), curr_itime(6))

      call print_date(currtime, 'gfs diag manager start time')

      call diag_manager_set_time_end(set_date(stop_itime(1), stop_itime(2), stop_itime(3), &
                                 stop_itime(4), stop_itime(5), stop_itime(6) ))
      call write_diag_post_nml(curr_itime, stop_itime, dt_sec, calendar_type)
      time_step = set_time(dt_sec)

      call init_gfs_diag(nk-1)

   end subroutine init_gfs_diag_manager


   subroutine gfs_diag_send_complete()
      call diag_send_complete(time_step)
   end subroutine gfs_diag_send_complete

   integer function register_var(name, long_name, units, range, standard_name, levs, mask, wgt)

      character (len=*), intent(in) :: name
      character (len=*), intent(in), optional :: long_name, units
      real, intent(in), optional :: range(2)
      character (len=*), intent(in), optional :: standard_name
      integer, intent(in), optional :: levs
      real, intent(in), optional :: mask(:,:)
      logical, intent(in), optional :: wgt

      character (len=32) :: tmp
      integer :: n, i
      logical :: wgt1

      wgt1=.false.
      if(present(wgt)) wgt1=wgt

      if (.not.present(levs)) then
         register_var = register_diag_field('gfs', trim(name), (/lon_id, lat_id/), &
              currtime, long_name, units, fill_value, range, wgt1, standard_name, &
              mask=mask, red2reg=.true., total_elements=total_eles)
      else
         n=0
         do i = 1, n_nlevs
            if (levs==nlevs(1,i)) then
               n=i
               exit
            endif
         enddo

         if (n==0) then
            n_nlevs=n_nlevs+1
            if (n_nlevs > max_nlevs) call handle_error(fatal, 'register_var: n_nlevs>max_nlevs, increase max_nlevs!')
            n=n_nlevs
            nlevs(1,n) = levs
            write(tmp,*) levs
            tmp=trim(adjustl(tmp))
            nlevs(2,n) = diag_axis_init(name='lev'//trim(tmp), data=(/(real(i),i=1,levs)/), units='', &
                                        cart_name='Z', long_name='levels')
         endif

         lev_id = nlevs(2,n)

         register_var = register_diag_field('gfs', name, (/lon_id, lat_id, lev_id/), &
              currtime, long_name, units, fill_value, range, wgt1, standard_name, &
              mask=mask, red2reg=.true., total_elements=total_eles*levs)

      endif

   end function register_var

  integer function register_static(name, long_name, units, range, standard_name, levs, mask, wgt)
    character (len=*), intent(in) :: name
    character (len=*), intent(in), optional :: long_name, units
    real, intent(in), optional :: range(2)
    character (len=*), intent(in), optional :: standard_name
    integer, intent(in), optional :: levs
    real, intent(in), optional :: mask(:,:)
    logical, intent(in), optional :: wgt 

    character (len=32) :: tmp
    integer :: n, i

    if (.not.present(levs)) then
       register_static = register_static_field('gfs', trim(name), (/lon_id, lat_id/), &
            long_name, units, fill_value, range, wgt, standard_name, mask=mask, red2reg=.true., &
            total_elements=total_eles)
    else
       n=0
       do i = 1, n_nlevs
          if (levs==nlevs(1,i)) then
             n=i
             exit
          endif
       enddo

       if (n==0) then
          n_nlevs=n_nlevs+1
          if (n_nlevs > max_nlevs) call handle_error(fatal, 'register_var: n_nlevs>max_nlevs, increase max_nlevs!')
          n=n_nlevs
          nlevs(1,n) = levs
          write(tmp,*) levs
          tmp=trim(adjustl(tmp))
          nlevs(2,n) = diag_axis_init(name='lev'//trim(tmp), data=(/(real(i),i=1,levs)/), units='', &
                                      cart_name='Z', long_name='levels')
       endif

       lev_id = nlevs(2,n)

       register_static = register_static_field('gfs', name, (/lon_id, lat_id, lev_id/), &
            long_name, units, fill_value, range, wgt, standard_name, mask=mask, red2reg=.true., &
            total_elements=total_eles*levs)
       
    endif
          
  end function register_static

  subroutine update_opdata_2d_o(field_id, field, istrt, im, lan, wgt)
    integer, intent(in) :: field_id
    real, intent(in) :: field(:)
    integer, intent(in) :: lan, istrt, im
    real, intent(in), optional :: wgt(:)
    !local
    real :: field1(im,1)
    real :: wgt1(im,1)
    logical :: mask(im,1)
    logical :: used
    integer :: is, ie 

    if (field_id<=0) return

    is = istrt
    ie = is+im-1

    field1(1:im,1)=field(:)

    if (.not.present(wgt)) then
       used = send_data(field_id, field1(:,:), currtime, js_in=lan, je_in=lan, is_in=is, ie_in=ie)
    else
       wgt1=0.
       wgt1(1:im,1)=wgt
       mask=.false.
       where (wgt1>0.0) mask=.true.
       used = send_data(field_id, field1(:,:), currtime, js_in=lan, je_in=lan, is_in=is, ie_in=ie, mask=mask, weight=wgt1)
    endif
          
  end subroutine update_opdata_2d_o

  subroutine update_opdata_3d_o(field_id, field, istrt, im, lan, wgt)
    integer, intent(in) :: field_id
    real, intent(in) :: field(:,:)
    integer, intent(in) :: lan, im, istrt
    real, intent(in), optional :: wgt(:,:)
    !local
    real :: field2(im,1,size(field,2))
    real :: wgt1(im,1,size(field,2))
    logical :: mask(im,1,size(field,2))
    logical :: used
    integer :: k, is, ie

    if (field_id<=0) return

    is = istrt
    ie = is+im-1

    field2(1:im,1,:) = field(:,:)

    if (.not.present(wgt)) then
       used = send_data(field_id, field2(:,:,:), currtime, js_in=lan, je_in=lan, is_in=is, ie_in=ie)
    else
       wgt1=0.
       wgt1(1:im,1,:)=wgt(:,:)
       mask=.false.
       where (wgt1>0.0) mask=.true.
       used = send_data(field_id, field2(:,:,:), currtime, js_in=lan, je_in=lan, is_in=is, ie_in=ie, mask=mask, weight=wgt1)
    endif
    
  end subroutine update_opdata_3d_o

  subroutine update_opdata_2d(field_id, field, wgt)
    integer, intent(in) :: field_id
    real, intent(in) :: field(:,:)
    real, intent(in), optional :: wgt(:,:)
    !local
    logical :: used
    logical :: mask(size(field,1),size(field,2))

    if (field_id<=0) return

    if (.not.present(wgt)) then
       used = send_data(field_id, field(:,:), currtime)
    else
       mask = .false.
       where(wgt>0.0) mask=.true.
       used = send_data(field_id, field(:,:), currtime, mask=mask, weight=wgt)
    endif
          
  end subroutine update_opdata_2d

  subroutine update_opdata_3d(field_id, field, wgt)
    integer, intent(in) :: field_id
    real, intent(in) :: field(:,:,:)
    real, intent(in), optional :: wgt(:,:,:)
    !local
    logical :: used
    logical :: mask(size(field,1),size(field,2),size(field,3))

    if (field_id<=0) return

    if(.not.present(wgt)) then
       used = send_data(field_id, field(:,:,:), currtime)
    else
       mask =.false.
       where(wgt>0.0)mask=.true.
       used = send_data(field_id, field(:,:,:), currtime, mask=mask, weight=wgt)
    endif
    
  end subroutine update_opdata_3d

  subroutine end_gfs_diag_manager()
    call diag_manager_end(currtime)
  end subroutine end_gfs_diag_manager

end module gfs_diag_manager_mod
