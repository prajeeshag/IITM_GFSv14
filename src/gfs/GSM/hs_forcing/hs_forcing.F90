
module hs_forcing_mod

implicit none
private

real, parameter :: KAPPA=2./7.
real, parameter :: CP_AIR=287.04/KAPPA
real, parameter :: GRAV=9.8

!-----------------------------------------------------------------------
!---------- interfaces ------------

   public :: hs_forcing, hs_forcing_init

!-----------------------------------------------------------------------
!-------------------- namelist -----------------------------------------

   logical :: no_forcing = .false.

   real :: t_zero=315., t_strat=200., delh=60., delv=10., eps=0., sigma_b=0.7
   real :: P00 = 1.e5

   real :: ka = -40. !  negative values are damping time in days
   real :: ks =  -4., kf = -1.

   logical :: do_conserve_energy = .true.

   real :: trflux = 1.e-5   !  surface flux for optional tracer
   real :: trsink = -4.     !  damping time for tracer

!-----------------------------------------------------------------------

   namelist /hs_forcing_nml/  no_forcing, t_zero, t_strat, delh, delv, eps, &
                              sigma_b, ka, ks, kf, do_conserve_energy, &
                              trflux, trsink

!-----------------------------------------------------------------------

   character(len=128) :: version='$Id: hs_forcing.F90,v 14.0 2007/03/15 22:04:00 fms Exp $'
   character(len=128) :: tagname='$Name: riga_prerelease2 $'

   real :: tka, tks, vkf
   real :: trdamp

   integer :: id_teq, id_tdt, id_udt, id_vdt,  &
              id_tdt_diss, id_diss_heat
   real    :: missing_value = -1.e10
   character(len=14) :: mod_name = 'hs_forcing'

   logical :: module_is_initialized = .false.

!-----------------------------------------------------------------------

contains

!#######################################################################

 subroutine hs_forcing ( im, levs, dt, lat, ps, p_full, &
                         u, v, t, udt, vdt, tdt)

!-----------------------------------------------------------------------
   integer, intent(in)                             :: im
   real, intent(in)                                :: dt
   real, intent(in),    dimension(1:im,1)          :: lat
   real, intent(in),    dimension(1:im,1,1:levs)   :: p_full
   real, intent(in),    dimension(1:im,1)          :: ps
   real, intent(in),    dimension(1:im,1,1:levs)   :: u, v, t
   real, intent(inout), dimension(1:im,1,1:levs)   :: udt, vdt, tdt

!-----------------------------------------------------------------------
   real, dimension(size(t,1),size(t,2))            :: diss_heat
   real, dimension(size(t,1),size(t,2),size(t,3))  :: ttnd, utnd, vtnd, teq, pmass
   real, dimension(size(r,1),size(r,2),size(r,3))  :: rst, rtnd

   integer :: i, j, k, kb, n, num_tracers
   logical :: used
   real    :: flux, sink, value
   character(len=128) :: scheme, params

!-----------------------------------------------------------------------
     if (no_forcing) return

     if (.not.module_is_initialized) then
       print *, 'hs_forcing : hs_forcing_init has not been called'
     endif

!-----------------------------------------------------------------------
!     rayleigh damping of wind components near the surface

      call rayleigh_damping ( ps, p_full, u, v, utnd, vtnd)

      udt = udt + utnd
      vdt = vdt + vtnd


!-----------------------------------------------------------------------
!     thermal forcing for held & suarez (1994) benchmark calculation

      call newtonian_damping ( lat, ps, p_full, t, ttnd, teq )

      tdt = tdt + ttnd

 end subroutine hs_forcing

!#######################################################################

 subroutine hs_forcing_init ()
!-----------------------------------------------------------------------
!
!           routine for initializing the model with an
!              initial condition at rest (u & v = 0)
!
!-----------------------------------------------------------------------
!     ----- read namelist -----
      if (no_forcing) return
!     ----- compute coefficients -----
      if (ka < 0.) ka = -86400.*ka
      if (ks < 0.) ks = -86400.*ks
      if (kf < 0.) kf = -86400.*kf

      tka = 0.; if (ka > 0.) tka = 1./ka
      tks = 0.; if (ks > 0.) tks = 1./ks
      vkf = 0.; if (kf > 0.) vkf = 1./kf
!     ----- for tracers -----
      if (trsink < 0.) trsink = -86400.*trsink
      trdamp = 0.; if (trsink > 0.) trdamp = 1./trsink

      module_is_initialized  = .true.
!-----------------------------------------------------------------------
 end subroutine hs_forcing_init



 subroutine hs_forcing_end 

!-----------------------------------------------------------------------
!
!       routine for terminating held-suarez benchmark module
!             (this routine currently does nothing)
!
!-----------------------------------------------------------------------
 module_is_initialized = .false.

 end subroutine hs_forcing_end

!#######################################################################

 subroutine newtonian_damping ( lat, ps, p_full, t, tdt, teq, mask )

!-----------------------------------------------------------------------
!
!   routine to compute thermal forcing for held & suarez (1994)
!   benchmark calculation.
!
!-----------------------------------------------------------------------

real, intent(in),  dimension(:,:)   :: lat, ps
real, intent(in),  dimension(:,:,:) :: p_full, t
real, intent(out), dimension(:,:,:) :: tdt, teq
real, intent(in),  dimension(:,:,:), optional :: mask

!-----------------------------------------------------------------------

          real, dimension(size(t,1),size(t,2)) :: &
     sin_lat, sin_lat_2, cos_lat_2, t_star, cos_lat_4, &
     tstr, sigma, the, tfactr, rps, p_norm

       real, dimension(size(t,1),size(t,2),size(t,3)) :: tdamp

       integer :: k
       real    :: tcoeff, pref

!-----------------------------------------------------------------------
!------------latitudinal constants--------------------------------------

      sin_lat  (:,:) = sin(lat(:,:))
      sin_lat_2(:,:) = sin_lat(:,:)*sin_lat(:,:)
      cos_lat_2(:,:) = 1.0-sin_lat_2(:,:)
      cos_lat_4(:,:) = cos_lat_2(:,:)*cos_lat_2(:,:)

      t_star(:,:) = t_zero - delh*sin_lat_2(:,:) - eps*sin_lat(:,:)
      tstr  (:,:) = t_strat - eps*sin_lat(:,:)

!-----------------------------------------------------------------------

      tcoeff = (tks-tka)/(1.0-sigma_b)
      pref = P00
      rps  = 1./ps

      do k = 1, size(t,3)

!  ----- compute equilibrium temperature (teq) -----
         p_norm(:,:) = p_full(:,:,k)/pref
         the   (:,:) = t_star(:,:) - delv*cos_lat_2(:,:)*log(p_norm(:,:))
         teq(:,:,k) = the(:,:)*(p_norm(:,:))**KAPPA
         teq(:,:,k) = max( teq(:,:,k), tstr(:,:) )

!  ----- compute damping -----
         sigma(:,:) = p_full(:,:,k)*rps(:,:)
         where (sigma(:,:) <= 1.0 .and. sigma(:,:) > sigma_b)
           tfactr(:,:) = tcoeff*(sigma(:,:)-sigma_b)
           tdamp(:,:,k) = tka + cos_lat_4(:,:)*tfactr(:,:)
         elsewhere
           tdamp(:,:,k) = tka
         endwhere

      enddo

!*** note: if the following loop uses vector notation for all indices
!          then the code will not run ??????

      do k=1,size(t,3)
         tdt(:,:,k) = -tdamp(:,:,k)*(t(:,:,k)-teq(:,:,k))
      enddo

      if (present(mask)) then
         tdt = tdt * mask
         teq = teq * mask
      endif

!-----------------------------------------------------------------------

 end subroutine newtonian_damping

!#######################################################################

 subroutine rayleigh_damping ( ps, p_full, u, v, udt, vdt, mask )

!-----------------------------------------------------------------------
!
!           rayleigh damping of wind components near surface
!
!-----------------------------------------------------------------------

real, intent(in),  dimension(:,:)   :: ps
real, intent(in),  dimension(:,:,:) :: p_full, u, v
real, intent(out), dimension(:,:,:) :: udt, vdt
real, intent(in),  dimension(:,:,:), optional :: mask

!-----------------------------------------------------------------------

real, dimension(size(u,1),size(u,2)) :: sigma, vfactr, rps

integer :: i,j,k
real    :: vcoeff

!-----------------------------------------------------------------------
!----------------compute damping----------------------------------------

      vcoeff = -vkf/(1.0-sigma_b)
      rps = 1./ps

      do k = 1, size(u,3)

         sigma(:,:) = p_full(:,:,k)*rps(:,:)

         where (sigma(:,:) <= 1.0 .and. sigma(:,:) > sigma_b)
            vfactr(:,:) = vcoeff*(sigma(:,:)-sigma_b)
            udt(:,:,k)  = vfactr(:,:)*u(:,:,k)
            vdt(:,:,k)  = vfactr(:,:)*v(:,:,k)
         elsewhere
            udt(:,:,k) = 0.0
            vdt(:,:,k) = 0.0
         endwhere
      enddo

      if (present(mask)) then
          udt = udt * mask
          vdt = vdt * mask
      endif

!-----------------------------------------------------------------------

 end subroutine rayleigh_damping

!#######################################################################

 subroutine tracer_source_sink ( flux, damp, p_half, r, rdt, kbot )

!-----------------------------------------------------------------------
      real, intent(in)  :: flux, damp, p_half(:,:,:), r(:,:,:)
      real, intent(out) :: rdt(:,:,:)
   integer, intent(in), optional :: kbot(:,:)
!-----------------------------------------------------------------------
      real, dimension(size(r,1),size(r,2),size(r,3)) :: source, sink
      real, dimension(size(r,1),size(r,2))           :: pmass

      integer :: i, j, kb
      real    :: rdamp
!-----------------------------------------------------------------------

      rdamp = damp
      if (rdamp < 0.) rdamp = -86400.*rdamp   ! convert days to seconds
      if (rdamp > 0.) rdamp = 1./rdamp

!------------ simple surface source and global sink --------------------

      source(:,:,:)=0.0

   if (present(kbot)) then
      do j=1,size(r,2)
      do i=1,size(r,1)
         kb = kbot(i,j)
         pmass (i,j)    = p_half(i,j,kb+1) - p_half(i,j,kb)
         source(i,j,kb) = flux/pmass(i,j)
      enddo
      enddo
   else
         kb = size(r,3)
         pmass (:,:)    = p_half(:,:,kb+1) - p_half(:,:,kb)
         source(:,:,kb) = flux/pmass(:,:)
   endif

     sink(:,:,:) = rdamp*r(:,:,:)
     rdt(:,:,:) = source(:,:,:)-sink(:,:,:)

!-----------------------------------------------------------------------

 end subroutine tracer_source_sink

!#######################################################################

end module hs_forcing_mod
