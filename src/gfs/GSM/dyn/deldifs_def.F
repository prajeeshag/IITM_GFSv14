      module deldifs_def
      use gfs_dyn_machine
      implicit none
      save
      real(kind=kind_evod),allocatable :: dne(:),dno(:),
     &                                    sf(:),rtrd(:),rthk(:),
     &                                    bkly(:),ckly(:)
     &,                                   dneh(:), dnoh(:),cthk(:)
     &,                                   rfacte(:,:), rfacto(:,:)
     &,                                   facte(:,:), facto(:,:)
!     &,                                   rfacte(:), rfacto(:)
!     &,                                   facte(:), facto(:)
    
      real(kind=kind_evod) rtnp
      integer jdel
      logical :: wrt_rfacte=.true.
      end module deldifs_def
