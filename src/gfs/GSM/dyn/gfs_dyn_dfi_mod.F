       module gfs_dyn_dfi_mod
!
!*** jw: set up alocatables for digital filter variables
!May 12 2016 S Moorthi changed pointer to allocatable
!
        use gfs_dyn_machine, only:kind_io8
        implicit none
!
        type gfs_dfi_grid_gr

          integer z_imp, ps_imp, temp_imp, u_imp, v_imp, tracer_imp     &
     &,           p_imp, dp_imp, dpdt_imp
          real(kind=kind_io8),allocatable, dimension(:,:,:) :: hs, ps, t&
     &,                                   u, v, tracer, p, dp, dpdt

        end type gfs_dfi_grid_gr
!
       end module gfs_dyn_dfi_mod
