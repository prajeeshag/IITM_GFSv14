module diag_register_gsmphys_mod

    use gfs_diag_manager_mod, only: register_var

    implicit none
    private

    integer, public :: id_ta, id_tas

    public:: register_diag_gsmphys

    contains

    subroutine register_diag_gsmphys()
        ! id_ta = register_var('ta', 'Temperature', 'K', levs=64)
        id_tas = register_var('tas', 'Surface Temperature', 'K')
    end subroutine register_diag_gsmphys

end module diag_register_gsmphys_mod