 module module_timers
   use module_include
   use module_gfs_mpi_def,only : mc_comp,mpi_comm_comp,my_taskid
   implicit none
   integer,parameter :: number_of_general_timers = 12
   integer,parameter :: number_of_dynamics_timers = 7
   integer,parameter :: number_of_physics_timers = 8
   integer,parameter :: max_number_of_timers = &
                        number_of_general_timers &
                      + number_of_dynamics_timers &
                      + number_of_physics_timers
   integer,parameter :: output_unit=2796
   public timef,initialize_timers,print_timers
   type :: timer_information
    character(len=22) :: name
    real(kind=8) :: elapsed
   end type timer_information
   type (timer_information),dimension(number_of_general_timers) :: general_timer
   type (timer_information),dimension(number_of_dynamics_timers) :: dynamics_timer
   type (timer_information),dimension(number_of_physics_timers) :: physics_timer
   integer :: i
    contains
     function timef()
      real(kind=8):: timef,mpi_wtime
      timef=MPI_Wtime()
     end function timef
     subroutine initialize_timers
!
      general_timer(1)%name='gfs_initialize        '
      general_timer(2)%name='gfs_run               '
      general_timer(3)%name='                      '
      general_timer(4)%name=' gfs_integrate        '
      general_timer(5)%name='  gc_gfs_dyn          '
      general_timer(6)%name='  filter block        '
      general_timer(7)%name='  gc_gfs_cpl exp_dyn  '
      general_timer(8)%name='  gc_gfs_phy          '
      general_timer(9)%name='  gc_gfs_cpl exp_phy  '
      general_timer(10)%name='  write_async_gfs     '
      general_timer(11)%name='   ESMF_VMBarrier     '
      general_timer(12)%name='gfs_finalize          '
      do i = 1,number_of_general_timers
       general_timer(i)%elapsed = 0.0
      enddo
      dynamics_timer(1)%name='gfs_dyn_initialize    '
      dynamics_timer(2)%name='gfs_dyn_run           '
      dynamics_timer(3)%name=' gfs_dynamics_imp2int '
      dynamics_timer(4)%name=' gfs_dynamics_run     '
      dynamics_timer(5)%name='  do_dynamics_slg_loop'
      dynamics_timer(6)%name='   gloopa             '
      dynamics_timer(7)%name='gfs_dyn_finalize      '
      do i = 1,number_of_dynamics_timers
       dynamics_timer(i)%elapsed = 0.0
      enddo
      physics_timer(1)%name='gfs_phy_initialize    '
      physics_timer(2)%name='gfs_phy_run           '
      physics_timer(3)%name='gfs_phy_finalize      '
      physics_timer(4)%name=' do_physics_one_step  '
      physics_timer(5)%name='  gloopr              '
      physics_timer(6)%name='   radiation          '
      physics_timer(7)%name='  gloopb              '
      physics_timer(8)%name='   gbphys             '
      do i = 1,number_of_physics_timers
       physics_timer(i)%elapsed = 0.0
      enddo
!
     end subroutine initialize_timers
     subroutine print_timers
      real(kind=8) :: timer_send(2,max_number_of_timers),       &
                      min_timer_recv(2,max_number_of_timers),   &
                      max_timer_recv(2,max_number_of_timers)
      integer :: ierr,imin,imax
      open(file='timing.summary',unit=output_unit,iostat=ierr)
      if (ierr /= 0) then
       write(6,*)' Error opening timing summary-> ',ierr
      endif
!
! general timers
!
      do i = 1,number_of_general_timers
       timer_send(1,i) = general_timer(i)%elapsed
       timer_send(2,i) = my_taskid
      enddo

!
! get minimum and location for general timers
!
      call mpi_reduce(timer_send,min_timer_recv,number_of_general_timers, &
                      MPI_2DOUBLE_PRECISION, MPI_MINLOC,0, mpi_comm_comp,ierr)
!
! get maximum and location for general timers
!
      call mpi_reduce(timer_send,max_timer_recv,number_of_general_timers, &
                      MPI_2DOUBLE_PRECISION, MPI_MAXLOC,0, mpi_comm_comp,ierr)
!
     if (my_taskid == 0) then
      write(output_unit,9003)
      do i = 1,number_of_general_timers
       if (max_timer_recv(1,i) > 0.05) then
        imin = min_timer_recv(2,i)
        imax = max_timer_recv(2,i)
        write(output_unit,9004) general_timer(i)%name,imin,min_timer_recv(1,i),     &
                      imax,max_timer_recv(1,i)
       endif
      enddo
     endif
!
! dynamics timers
!
      do i = 1,number_of_dynamics_timers
       timer_send(1,i) = dynamics_timer(i)%elapsed
       timer_send(2,i) = my_taskid
      enddo
!
! get minimum and location for dynamics timers
!
      call mpi_reduce(timer_send,min_timer_recv,number_of_dynamics_timers, &
                      MPI_2DOUBLE_PRECISION, MPI_MINLOC,0, mc_comp,ierr)
!
! get maximum and location for dynamics timers
!
      call mpi_reduce(timer_send,max_timer_recv,number_of_dynamics_timers, &
                      MPI_2DOUBLE_PRECISION, MPI_MAXLOC,0, mc_comp,ierr)
     if (my_taskid == 0) then
      write(output_unit,9005)
      write(output_unit,9003)
      do i = 1,number_of_dynamics_timers
       if (max_timer_recv(1,i) > 0.05) then
        imin = min_timer_recv(2,i)
        imax = max_timer_recv(2,i)
        write(output_unit,9004) dynamics_timer(i)%name,imin,min_timer_recv(1,i),     &
                      imax,max_timer_recv(1,i)
       endif
      enddo
     endif
!
! physics timers
!
      do i = 1,number_of_physics_timers
       timer_send(1,i) = physics_timer(i)%elapsed
       timer_send(2,i) = my_taskid
      enddo
!
! get minimum and location for physics timers
!
      call mpi_reduce(timer_send,min_timer_recv,number_of_physics_timers, &
                      MPI_2DOUBLE_PRECISION, MPI_MINLOC,0, mc_comp,ierr)
!
! get maximum and location for physics timers
!
      call mpi_reduce(timer_send,max_timer_recv,number_of_physics_timers, &
                      MPI_2DOUBLE_PRECISION, MPI_MAXLOC,0, mc_comp,ierr)
!
     if (my_taskid == 0) then
      write(output_unit,9005)
      write(output_unit,9003)
      do i = 1,number_of_physics_timers
       if (max_timer_recv(1,i) > 0.05) then
        imin = min_timer_recv(2,i)
        imax = max_timer_recv(2,i)
        write(output_unit,9004) physics_timer(i)%name,imin,min_timer_recv(1,i),     &
                      imax,max_timer_recv(1,i)
       endif
      enddo
     endif
!
9003 format('    routine             min pe     min time   max pe    max time')
9004 format(3x,a22,1x,i4,1x,f12.4,3x,i4,1x,f12.4)
9005 format(' ')
     end subroutine print_timers
 end module module_timers
