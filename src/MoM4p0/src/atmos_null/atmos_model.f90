!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!                                                                   !!
!!                   GNU General Public License                      !!
!!                                                                   !!
!! This file is part of the Flexible Modeling System (FMS).          !!
!!                                                                   !!
!! FMS is free software; you can redistribute it and/or modify       !!
!! it and are expected to follow the terms of the GNU General Public !!
!! License as published by the Free Software Foundation.             !!
!!                                                                   !!
!! FMS is distributed in the hope that it will be useful,            !!
!! but WITHOUT ANY WARRANTY; without even the implied warranty of    !!
!! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     !!
!! GNU General Public License for more details.                      !!
!!                                                                   !!
!! You should have received a copy of the GNU General Public License !!
!! along with FMS; if not, write to:                                 !!
!!          Free Software Foundation, Inc.                           !!
!!          59 Temple Place, Suite 330                               !!
!!          Boston, MA  02111-1307  USA                              !!
!! or see:                                                           !!
!!          http://www.gnu.org/licenses/gpl.txt                      !!
!!                                                                   !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
module atmos_model_mod

use mpp_mod, only : mpp_npes, mpp_pe, mpp_error, FATAL
use mpp_domains_mod, only : domain2d
use time_manager_mod, only : time_type
use gfs_comm_mod, only : set_gfs_connector

implicit none
private

type land_ice_atmos_boundary_type
!variables of this type are declared by coupler_main, allocated by flux_exchange_init
!quantities going from land+ice to atmos
!       t         = surface temperature for radiation calculations
!       albedo    = surface albedo for radiation calculations
!       land_frac = fraction amount of land in a grid box
!       dt_t      = temperature tendency at the lowest level
!       dt_q      = specific humidity tendency at the lowest level
!       u_flux    = zonal wind stress
!       v_flux    = meridional wind stress
!       dtaudv    = derivative of wind stress w.r.t. the lowest level wind speed
!       u_star    = friction velocity
!       b_star    = bouyancy scale
!       q_star    = moisture scale
!       rough_mom = surface roughness (used for momentum
   real, dimension(:,:), pointer :: t =>NULL(), albedo =>NULL(), land_frac =>NULL()
   real, dimension(:,:), pointer :: albedo_vis_dir =>NULL(), albedo_nir_dir =>NULL()
   real, dimension(:,:), pointer :: albedo_vis_dif =>NULL(), albedo_nir_dif =>NULL()
   real, dimension(:,:), pointer :: dt_t =>NULL(), dt_q =>NULL()
   real, dimension(:,:), pointer :: u_flux =>NULL(), v_flux =>NULL(), &
                                    dtaudu =>NULL(), dtaudv =>NULL(), &
                                    u_star =>NULL(), b_star =>NULL(), q_star =>NULL(), &
                                    rough_mom =>NULL()
   real, dimension(:,:,:), pointer :: data =>NULL() !collective field for "named" fields above
   integer :: xtype             !REGRID, REDIST or DIRECT
end type land_ice_atmos_boundary_type

type surf_diff_type
  real, pointer, dimension(:,:) :: dtmass  =>NULL(),   &
                                   dflux_t =>NULL(),   & 
                                   dflux_q =>NULL(),   & 
                                   delta_t =>NULL(),   &
                                   delta_q =>NULL(),   &
                                   delta_u =>NULL(),   &
                                   delta_v =>NULL()
end type surf_diff_type

type atmos_data_type
   type (domain2d)               :: domain
   integer                       :: axes(4)
   real, pointer, dimension(:)   :: glon_bnd =>NULL(), glat_bnd =>NULL(),  &
                                    lon_bnd =>NULL(),  lat_bnd =>NULL()
   real, pointer, dimension(:,:) :: t_bot =>NULL(), q_bot =>NULL(), &
                                    z_bot =>NULL(), p_bot =>NULL(),  &
                                    u_bot =>NULL(), v_bot =>NULL(), &
                                    p_surf =>NULL(), gust =>NULL(),  &
                                    coszen =>NULL(), flux_sw =>NULL(), &
                                    flux_lw =>NULL(), lprec =>NULL(), fprec =>NULL(), &
                                    flux_sw_dir =>NULL(), &
                                    flux_sw_dif =>NULL(), &
                                    flux_sw_down_vis_dir =>NULL(), &
                                    flux_sw_down_vis_dif =>NULL(), &
                                    flux_sw_down_total_dir =>NULL(), &
                                    flux_sw_down_total_dif =>NULL(), &
                                    flux_sw_vis =>NULL(), &
                                    flux_sw_vis_dir =>NULL(), &
                                    flux_sw_vis_dif =>NULL()
   type (surf_diff_type)         :: Surf_diff
   type (time_type)              :: Time, Time_step, Time_init
   integer, pointer              :: pelist(:) =>NULL()
   logical                       :: pe
end type atmos_data_type
  
!quantities going from land alone to atmos (none at present)
type land_atmos_boundary_type
!   private
   real, dimension(:,:), pointer :: data =>NULL()
end type land_atmos_boundary_type

!quantities going from ice alone to atmos (none at present)
type ice_atmos_boundary_type
!   private
   real, dimension(:,:), pointer :: data =>NULL()
end type ice_atmos_boundary_type
  
public atmos_model_init, atmos_model_end, &
     update_atmos_model_down,           &
     update_atmos_model_up,             &
     atmos_data_type, surf_diff_type,   &
     land_ice_atmos_boundary_type, &
     land_atmos_boundary_type, &
     ice_atmos_boundary_type

contains


subroutine atmos_model_init (Atmos, Time_init, Time, Time_step)

  use fms_mod, only : read_data, field_size
  use mpp_domains_mod, only : mpp_define_layout, mpp_define_domains,&
                              CYCLIC_GLOBAL_DOMAIN, mpp_get_data_domain,&
                              mpp_get_compute_domain
  use diag_manager_mod, only : diag_axis_init
  use diag_integral_mod, only : diag_integral_init
  use constants_mod, only : cp_air, hlv
  
type (atmos_data_type), intent(inout) :: Atmos
type (time_type), intent(in) :: Time_init, Time, Time_step

real, dimension(:), allocatable :: glon, glat
integer, dimension(4) :: siz
integer, dimension(2) :: layout
integer :: is, ie, js, je, j

!---- set the atmospheric model time ------

   Atmos % Time_init = Time_init
   Atmos % Time      = Time
   Atmos % Time_step = Time_step
 
call field_size('INPUT/grid_spec','AREA_ATM',siz)
allocate(Atmos%glon_bnd(siz(1)+1))
allocate(Atmos%glat_bnd(siz(2)+1))
allocate(glon(siz(1)),glat(siz(2)))
call read_data('INPUT/grid_spec','xba',Atmos%glon_bnd)
call read_data('INPUT/grid_spec','yba',Atmos%glat_bnd)
call read_data('INPUT/grid_spec','xta',glon)
call read_data('INPUT/grid_spec','yta',glat)

Atmos%glon_bnd = Atmos%glon_bnd*atan(1.0)/45.0
Atmos%glat_bnd = Atmos%glat_bnd*atan(1.0)/45.0

call mpp_define_layout((/1,siz(1),1,siz(2)/), mpp_npes(), layout)
layout=[1,mpp_npes()] !-> prajeesh, GFS coupling
call mpp_define_domains((/1,siz(1),1,siz(2)/), layout, Atmos%domain, &
     xflags = CYCLIC_GLOBAL_DOMAIN)

call mpp_get_compute_domain(Atmos%domain,is,ie,js,je)

allocate(Atmos%lon_bnd(ie-is+2),&
         Atmos%lat_bnd(je-js+2))

Atmos%lon_bnd(:) = Atmos%glon_bnd(is:ie+1)
Atmos%lat_bnd(:) = Atmos%glat_bnd(js:je+1)

Atmos%axes(1) = diag_axis_init('lon',glon,'degrees_E','X','longitude',&
       set_name='atmos',domain2 = Atmos%domain)

Atmos%axes(2) = diag_axis_init('lat',glat,'degrees_N','Y','latitude',&
     set_name='atmos',domain2 = Atmos%domain)  

call set_gfs_connector(siz(2),siz(1),[(j,j=js,je)]) !-> prajeesh, GFS coupling

allocate(Atmos%t_bot(is:ie,js:je),&
         Atmos%q_bot(is:ie,js:je), &
         Atmos%z_bot(is:ie,js:je), &
         Atmos%p_bot(is:ie,js:je), &
         Atmos%u_bot(is:ie,js:je), &
         Atmos%v_bot(is:ie,js:je), &
         Atmos%p_surf(is:ie,js:je), &
         Atmos%gust(is:ie,js:je), &
         Atmos%coszen(is:ie,js:je), &
         Atmos%flux_sw(is:ie,js:je), &
         Atmos % flux_sw_dir (is:ie,js:je), &
         Atmos % flux_sw_dif (is:ie,js:je), &
         Atmos % flux_sw_down_vis_dir (is:ie,js:je), &
         Atmos % flux_sw_down_vis_dif (is:ie,js:je), &
         Atmos % flux_sw_down_total_dir (is:ie,js:je), &
         Atmos % flux_sw_down_total_dif (is:ie,js:je), &
         Atmos % flux_sw_vis (is:ie,js:je), &
         Atmos % flux_sw_vis_dir (is:ie,js:je), &
         Atmos % flux_sw_vis_dif(is:ie,js:je), &
         Atmos%flux_lw(is:ie,js:je), &
         Atmos%lprec(is:ie,js:je), &
         Atmos%fprec(is:ie,js:je))

Atmos%t_bot=273.0
Atmos%q_bot = 0.0
Atmos%z_bot = 10.0
Atmos%p_bot = 1.e5
Atmos%u_bot = 0.0
Atmos%v_bot = 0.0
Atmos%p_surf = 1.e5
Atmos%gust = 0.0
Atmos%coszen = 0.0
Atmos%flux_sw = 0.0
Atmos%flux_lw = 0.0
Atmos % flux_sw_dir = 0.0
Atmos % flux_sw_dif = 0.0 
Atmos % flux_sw_down_vis_dir = 0.0 
Atmos % flux_sw_down_vis_dif = 0.0 
Atmos % flux_sw_down_total_dir = 0.0
Atmos % flux_sw_down_total_dif = 0.0
Atmos % flux_sw_vis = 0.0 
Atmos % flux_sw_vis_dir = 0.0 
Atmos % flux_sw_vis_dif = 0.0
Atmos%lprec = 0.0
Atmos%fprec = 0.0

allocate(Atmos%Surf_diff%dtmass(is:ie, js:je) , &
         Atmos%Surf_diff%dflux_t(is:ie, js:je) , &
         Atmos%Surf_diff%dflux_q(is:ie, js:je) , &
         Atmos%Surf_diff%delta_t(is:ie, js:je) , &
         Atmos%Surf_diff%delta_q(is:ie, js:je) , &
         Atmos%Surf_diff%delta_u(is:ie, js:je) , &
         Atmos%Surf_diff%delta_v(is:ie, js:je) )

Atmos%Surf_diff%dflux_t = 0.0
Atmos%Surf_diff%dflux_q = 0.0
Atmos%Surf_diff%dtmass = 0.0
Atmos%Surf_diff%delta_t = 0.0
Atmos%Surf_diff%delta_q = 0.0
Atmos%Surf_diff%delta_u = 0.0
Atmos%Surf_diff%delta_v = 0.0

!------ initialize global integral package ------

    call diag_integral_init (Time_init, Time,  &
                             Atmos % lon_bnd,   Atmos % lat_bnd)
    
return

end subroutine atmos_model_init

subroutine atmos_model_end (Atmos)

type (atmos_data_type), intent(inout) :: Atmos

return

end subroutine atmos_model_end



subroutine update_atmos_model_down( Surface_boundary, Atmos )
!-----------------------------------------------------------------------
!                       atmospheric driver
!    performs radiation, damping, and vertical diffusion of momentum,
!    tracers, and downward heat/moisture
!
!-----------------------------------------------------------------------

  type(land_ice_atmos_boundary_type), intent(inout) :: Surface_boundary
  type (atmos_data_type), intent(inout) :: Atmos

  return

end subroutine update_atmos_model_down

subroutine update_atmos_model_up( Surface_boundary, Atmos )
!-----------------------------------------------------------------------
!                       atmospheric driver
!    performs upward vertical diffusion of heat/moisture and
!    moisture processes
!
!-----------------------------------------------------------------------

   type(land_ice_atmos_boundary_type), intent(in) :: Surface_boundary
   type (atmos_data_type), intent(inout) :: Atmos
   
   return

end subroutine update_atmos_model_up

end module atmos_model_mod
