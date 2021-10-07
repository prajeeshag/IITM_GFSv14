      module slgshr
      implicit none
!
      save
      REAL RA                 ! RECIPROCAL OF RADIUS OF EARTH METERS
      REAL, allocatable ::
     &     LAMMP(:,:,:),      ! TRAJECTORY MID-POINT LONG. COORDINATE
     &     PHIMP(:,:,:),      ! TRAJECTORY MID-POINT LAT   COORDINATE
     &     SIGMP(:,:,:),      ! TRAJECTORY MID-POINT SIGMA COORDINATE
     &     PHI(:),            ! LATITUDE  COORDINATES OF MODEL GRID
     &     DPHI(:),           ! LATITUDINAL GRID INCREMENTS
     &     LBASDY(:,:,:),     ! BASIS FUNCTIONS FOR LAT DERIV EST.
     &     LBASIY(:,:,:),     ! BASIS FUNCTIONS FOR LAGRANGE INTERP
     &     dlam(:), rdlam(:), rdlam6(:), lam(:,:), dphii(:)

     &,    lbasdz(:,:,:)      ! basis funcs for vert deriv est. at level
     &,    lbasiz(:,:,:)      ! lagrange cubic interp
     &,    detam(:)           ! delta eta at levels
     &,    detami(:)          ! one / detam
      integer, allocatable :: nlonex(:), kdpmpf(:)
      end module slgshr

