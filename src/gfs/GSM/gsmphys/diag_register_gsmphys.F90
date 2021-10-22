module diag_register_gsmphys_mod

    use gfs_diag_manager_mod, only: register_var

    implicit none
    private

    integer, public :: id_ta, id_tas, id_ua, id_va, id_ps

    public:: register_diag_gsmphys

    contains

    subroutine register_diag_gsmphys()
        id_ta = register_var('ta', 'Temperature', 'K', levs=64)
        id_ua = register_var('ua', 'Temperature', 'm/s', levs=64)
        id_va = register_var('va', 'Temperature', 'm/s', levs=64)
        id_ps = register_var('ps', 'Surface Pressure', 'Pascals')
        id_tas = register_var('tas', 'Surface Temperature', 'K')
    end subroutine register_diag_gsmphys

end module diag_register_gsmphys_mod