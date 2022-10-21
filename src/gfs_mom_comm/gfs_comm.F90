module gfs_comm_mod
#ifdef GFS_MOM_COUPLED
  use mpi
  implicit none
  private

  integer, parameter :: ATM=111, OCN=222
  integer :: mpi_rkind, component=0, exchange_pe=.false., deactivated=.false.
  integer :: global_comm, exchange_comm, exchange_local_comm, local_comm
  integer :: global_npes, exchange_npes, exchange_local_npes, local_npes
  integer :: global_rank, exchange_rank, exchange_local_rank, local_rank
  integer :: nlatg, nlatl
  integer :: nlong, nlonl
  real :: tolerance=10.0e-6
 
  logical :: exchange_comm_set=.false., initilized_local_comm=.false.
  logical :: gfs_connector_set=.false.
  logical :: debug=.true., use_blocking_recv=.false.

  real, allocatable :: rsndbuf(:,:,:), rrcvbuf(:,:,:)
  integer, allocatable :: rsndreq(:), rrcvreq(:)
  integer :: nfrecv=-1, nfsend=-1, nrcv=0, nsnd=0
  logical :: nfsend_set=.false., nfrecv_set=.false., called_ready_to_recv=.false.

  type gfs_connect_type
    integer :: pe
    integer :: tag
    integer :: request
    real :: lat
  end type

  type (gfs_connect_type), allocatable :: con(:)

  public :: init_local_comm, set_gfs_exchange_communicator, set_gfs_connector
  public :: gfs_comm_send, gfs_comm_recv, deactivated, end_gfs_comm
  public :: gfs_comm_isend, gfs_comm_irecv, set_nfsend, set_nfrecv, ready_to_recv
  
  interface gfs_comm_send
    module procedure gfs_comm_send_r
    module procedure gfs_comm_send_i
  end interface gfs_comm_send

  interface gfs_comm_recv
    module procedure gfs_comm_recv_r
    module procedure gfs_comm_recv_i
  end interface gfs_comm_recv

  namelist/gfs_comm_mod_nml/tolerance, debug, use_blocking_recv
  
  contains
  
  subroutine init_local_comm(comp,localcomm)
    ! Calls mpi_init, and splits MPI_COMM_WORLD in ocn and atm pesets
    character(len=*), intent(in) :: comp
    integer, intent(out) :: localcomm
    integer :: ierr, color
    real :: tmp   
    logical :: nmlexist

    call mpi_init(ierr)

    inquire(file='gfs_comm.nml',exist=nmlexist)
    if (nmlexist) then
      open(10,file='gfs_comm.nml')
      read(10,nml=gfs_comm_mod_nml,iostat=ierr)
      close(10)
    end if

    global_comm = MPI_COMM_WORLD

    call mpi_comm_size (global_comm, global_npes, ierr)
    call mpi_comm_rank (global_comm, global_rank, ierr)

    if (debug) then
      print *, "gfs_comm: global_rank: ", global_rank
    endif

    if (global_rank==0) then
      write(*,nml=gfs_comm_mod_nml)
      if (debug) then
        print *, "gfs_comm: global_npes: ", global_npes
      endif
    endif

    select case (kind(tmp))
      case (8)
        mpi_rkind=MPI_DOUBLE
      case (4)
        mpi_rkind=MPI_FLOAT
      case default
        print *, 'Error: gfs_comm_mod: Unrecognised MPI real type'
        call mpi_abort(MPI_COMM_WORLD,-1,ierr)
    end select
    
    select case (comp)
      case ('ATM')
        color=ATM 
        component=ATM 
      case ('OCN')
        color=OCN 
        component=OCN 
      case default
        print *, 'Error: Unrecognised component'
        call mpi_abort(MPI_COMM_WORLD,-1,ierr)
    end select
    
    call mpi_comm_split(MPI_COMM_WORLD, color, global_rank, local_comm, ierr)
    call mpi_comm_size (local_comm, local_npes, ierr)
    call mpi_comm_rank (local_comm, local_rank, ierr)

    if (debug) then
      print *, "gfs_comm: global_rank, local_rank: ", global_rank, local_rank
    endif

    if (local_rank==0) then
      write(*,nml=gfs_comm_mod_nml)
      if (debug) then
        print *, "gfs_comm:", trim(comp)," npes: ", local_npes
      endif
    endif

    localcomm=local_comm
    initilized_local_comm=.true.
  end subroutine init_local_comm


  subroutine set_gfs_exchange_communicator(color)
    integer, optional :: color
    integer :: colorr, pe1, pe2, pe3, pe4
    integer :: ierr, key

    if (exchange_comm_set) return

    if (.not.initilized_local_comm) then
      print *, 'gfs_comm: Error: set_gfs_exchange_communicator: &
                local_comm not initialized'
      call mpi_abort(MPI_COMM_WORLD, -1, ierr)
    endif

    if(debug) print *, 'gfs_comm: Entering set_gfs_exchange_communicator, pe:', global_rank

    !This is required because not all processors of GFS participate in exchange like IO pes
    colorr=0
    if(present(color))colorr=color
    if(colorr==0) exchange_pe=.true.

    key=global_rank
    call mpi_comm_split (global_comm, colorr, key, exchange_comm, ierr)
    call mpi_comm_size (exchange_comm, exchange_npes, ierr)
    call mpi_comm_rank (exchange_comm, exchange_rank, ierr)

    if(debug) print *, 'gfs_comm: after exchange_comm split, pe:', global_rank

    call mpi_comm_split (exchange_comm, component, key, exchange_local_comm, ierr)
    call mpi_comm_size (exchange_local_comm, exchange_local_npes, ierr)
    call mpi_comm_rank (exchange_local_comm, exchange_local_rank, ierr)

    if(debug) print *, 'gfs_comm: after exchange_local_comm split, pe:', global_rank

    if (debug) then
      print *,'gfs_comm: rank in g lc ec elc :', global_rank, local_rank,  &
                            exchange_rank, exchange_local_rank
    endif
    if (exchange_npes==exchange_local_npes) deactivated=.true.
    exchange_comm_set=.true.
    if(debug) print *, 'gfs_comm: leaving set_gfs_exchange_communicator, pe:', global_rank
  end subroutine set_gfs_exchange_communicator

  subroutine set_gfs_connector(nlat_global, nlon, latlist)
    integer, intent(in) :: nlat_global, nlon
    integer, intent(in) :: latlist(:)
    integer :: ldom(4,size(latlist))
    integer :: rbuf2d(4,2*nlat_global)
    integer :: ierr, ibuf, i, j
    integer :: ibuf2(3), ibuf2d(3,exchange_npes)
    integer :: recvcount(exchange_npes), offset(exchange_npes)
    integer :: stat(MPI_STATUS_SIZE), srcdest

    if (deactivated) return

    if (.not.exchange_comm_set) then
      print *, 'gfs_comm: Error: set_gfs_connector: Call set_gfs_exchange_communicator first'
      call mpi_abort(MPI_COMM_WORLD,-1,ierr)
    end if

    if(.not.exchange_pe) return

    if(debug) print *, 'gfs_comm: Entering set_gfs_connector, pe:', global_rank  

    nlatg=nlat_global  
    nlatl=size(latlist)

    nlong=nlon
    nlonl=nlon

    allocate(con(nlatl))
    con(:)%pe=-1; con(:)%tag=-1
    con(:)%lat=latlist


    if(debug) print *, 'gfs_comm: entering mpi_allreduce, in set_gfs_connector pe:', global_rank  
    call mpi_allreduce(nlatl,ibuf,1,MPI_INTEGER,MPI_SUM,exchange_comm,ierr)

    if (ibuf/=2*nlatg) then
      print *,"gfs_comm: Error: sum of nlat from all pes in exchange is /= 2*nlatg (nlat global)"
      call mpi_abort(MPI_COMM_WORLD, -1, ierr)
    endif
  
    ldom(1,:)=component; ldom(2,:)=exchange_rank
    forall(i=1:size(ldom,2)) ldom(3,i)=i
    ldom(4,:)=latlist(:)
   
    rbuf2d=0

    ibuf=size(ldom)
    call mpi_allgather(ibuf,1,MPI_INTEGER,recvcount,1,MPI_INTEGER, &
            exchange_comm,ierr)

    offset(1)=0
    do i=2,size(recvcount)
      offset(i)=offset(i-1)+recvcount(i-1)
    enddo
    
    if(debug) print *, 'gfs_comm: entering mpi_allgatherv, in set_gfs_connector pe:', global_rank  
    call mpi_allgatherv(ldom,size(ldom),MPI_INTEGER,rbuf2d(:,:),recvcount,offset,&
            mpi_rkind,exchange_comm,ierr)
    if(debug) print *, 'gfs_comm: leaving mpi_allgatherv, in set_gfs_connector pe:', global_rank  

    if (debug.and.exchange_rank==0) then
      do i = 1, size(rbuf2d,2)
        write(*,'(a,3(I4.4,2x),F15.6)')'gfs_connector: ', &
            rbuf2d(1,i),rbuf2d(2,i),rbuf2d(3,i),rbuf2d(4,i)
      enddo
    endif
    
    do i=1,size(rbuf2d,2)
      if (rbuf2d(1,i)==component) cycle
      do j=1,size(con)
        if ( con(j)%lat==rbuf2d(4,i) ) then
          if (con(j)%pe>-1) then
            print *, 'gfs_comm: Error: set_gfs_connector: got a second connection.'
            call mpi_abort(MPI_COMM_WORLD,-1,ierr)
          endif
          con(j)%pe=rbuf2d(2,i)
          con(j)%tag=rbuf2d(3,i)
        endif
      enddo
    enddo

    if(any(con(:)%pe<=-1)) then
      print *, 'gfs_comm: Error: set_gfs_connector: Some pes didnt get any connection'
      call mpi_abort(MPI_COMM_WORLD,-1,ierr)
    endif 
    gfs_connector_set = .true.
    if(debug) print *, 'gfs_comm: Leaving set_gfs_connector' 

  end subroutine set_gfs_connector


  subroutine gfs_comm_send_r(data2d)

    real, intent(in) :: data2d(:,:)
    integer :: i
    integer :: stat(MPI_STATUS_SIZE), ierr

    if (deactivated) return
    if (.not.exchange_pe) then
      print *, 'gfs_comm: Warning: gfs_comm_send/recv called from a non  exchange pe - global rank: ', &
                global_rank
      return
    endif

    if (.not. gfs_connector_set) then
      print *, 'gfs_comm: Error: gfs_connector not set, call set_gfs_connector'
      call mpi_abort(MPI_COMM_WORLD,-1,ierr)
    endif

    do i = 1, nlatl
      call mpi_isend(data2d(:,i), nlonl, mpi_rkind, con(i)%pe, &
            con(i)%tag, exchange_comm, con(i)%request,ierr)
    enddo
    
    do i=1,nlatl
      call mpi_wait(con(i)%request, stat, ierr)
    enddo

  end subroutine gfs_comm_send_r

  subroutine gfs_comm_send_i(data2d)
    integer, intent(in) :: data2d(:,:)
    integer :: i
    integer :: stat(MPI_STATUS_SIZE), ierr

    if (deactivated) return
    if (.not.exchange_pe) then
      print *, 'gfs_comm: Warning: gfs_comm_send/recv called from a non  exchange pe - global rank: ', &
                global_rank
      return
    endif

    if (.not. gfs_connector_set) then
      print *, 'gfs_comm: Error: gfs_connector not set, call set_gfs_connector'
      call mpi_abort(MPI_COMM_WORLD,-1,ierr)
    endif

    do i = 1, nlatl
      call mpi_isend(data2d(:,i), nlonl, MPI_INTEGER, con(i)%pe, &
            con(i)%tag, exchange_comm, con(i)%request, ierr)
    enddo
    
    do i=1,nlatl
      call mpi_wait(con(i)%request, stat, ierr)
    enddo
  end subroutine gfs_comm_send_i

  subroutine gfs_comm_recv_r(data2d)
    real, intent(out) :: data2d(:,:)
    integer :: i
    integer :: stat(MPI_STATUS_SIZE), ierr

    if (deactivated) return
    if (.not.exchange_pe) then
      print *, 'gfs_comm: Warning: gfs_comm_send/recv called from a non  exchange pe - global rank: ', &
                global_rank
      return
    endif
    
    if (.not. gfs_connector_set) then
      print *, 'gfs_comm: Error: gfs_connector not set, call set_gfs_connector'
      call mpi_abort(MPI_COMM_WORLD,-1,ierr)
    endif

    if (use_blocking_recv) then
      do i = 1, nlatl
        call mpi_recv(data2d(:,i), nlonl, mpi_rkind, con(i)%pe, &
              i, exchange_comm, stat, ierr)
      enddo
    else 
      do i = 1, nlatl
        call mpi_irecv(data2d(:,i), nlonl, mpi_rkind, con(i)%pe, & 
              i, exchange_comm, con(i)%request, ierr)
      enddo

      do i=1,nlatl
        call mpi_wait(con(i)%request, stat, ierr)
      enddo
    endif
  end subroutine gfs_comm_recv_r

  subroutine gfs_comm_recv_i(data2d)
    integer, intent(out) :: data2d(:,:)
    integer :: i
    integer :: stat(MPI_STATUS_SIZE), ierr

    if (deactivated) return
    if (.not.exchange_pe) then
      print *, 'gfs_comm: Warning: gfs_comm_send/recv called from a non  exchange pe - global rank: ', &
                global_rank
      return
    endif
    
    if (.not. gfs_connector_set) then
      print *, 'gfs_comm: Error: gfs_connector not set, call set_gfs_connector'
      call mpi_abort(MPI_COMM_WORLD,-1,ierr)
    endif

    do i = 1, nlatl
      call mpi_irecv(data2d(:,i), nlonl, MPI_INTEGER, con(i)%pe, &
            i, exchange_comm, con(i)%request, ierr)
    enddo

    do i=1,nlatl
      call mpi_wait(con(i)%request, stat, ierr)
    enddo
  end subroutine gfs_comm_recv_i

  subroutine gfs_comm_isend(data2d)

    real, intent(in) :: data2d(:,:)
    integer :: i, ierr
    integer :: stat(MPI_STATUS_SIZE)

    if(.not.exchange_pe) return
    if (deactivated) return
    
    if(.not.nfsend_set) then
      print *, 'gfs_comm: Error: nfsend is not set'
      call mpi_abort(MPI_COMM_WORLD,-1,ierr)
    endif
    
    nsnd=nsnd+1
  
    do i=1, nlatl
      rsndbuf(:,nsnd,i)=data2d(:,i)
    enddo
    
    if (nsnd==nfsend) then
      do i=1, nlatl
        call mpi_isend(rsndbuf(:,:,i), nlonl*nfsend, mpi_rkind, con(i)%pe, &
          con(i)%tag, exchange_comm, con(i)%request, ierr)
      enddo
      do i=1,nlatl
       call mpi_wait(con(i)%request, stat, ierr)
      enddo
      nsnd=0
    elseif (nsnd>nfsend) then
      print *, 'gfs_comm: Error nsnd > nfsend'
      call mpi_abort(MPI_COMM_WORLD, -1, ierr)
    endif
    
  end subroutine gfs_comm_isend

  subroutine ready_to_recv()
    integer :: ierr, i
    
    if(.not.exchange_pe) return

    if (deactivated) return

    if(.not.nfrecv_set) then
      print *, 'gfs_comm: Error: nfrecv is not set'
      call mpi_abort(MPI_COMM_WORLD,-1,ierr)
    endif

      do i=1, nlatl
        call mpi_irecv(rrcvbuf(:,:,i), nlonl*nfrecv, mpi_rkind, con(i)%pe, &
          i, exchange_comm, rrcvreq(i), ierr)
      enddo

    called_ready_to_recv=.true.
    
  end subroutine ready_to_recv

  subroutine gfs_comm_irecv(data2d)

    real, intent(out) :: data2d(:,:)
    integer :: i, ierr
    integer :: stat(MPI_STATUS_SIZE)

    if (deactivated) return

    if(.not.exchange_pe) return
    
    if(.not.nfrecv_set) then
      print *, 'gfs_comm: Error: nfrecv is not set'
      call mpi_abort(MPI_COMM_WORLD,-1,ierr)
    endif
    
    nrcv=nrcv+1

    if (nrcv>nfrecv) nrcv=1
  
    if (nrcv==1) then
      if (.not.called_ready_to_recv) then
        do i=1, nlatl
          call mpi_irecv(rrcvbuf(:,:,i), nlonl*nfrecv, mpi_rkind, con(i)%pe, &
            i, exchange_comm, rrcvreq(i), ierr)
        enddo
      endif

      do i=1,nlatl
       call mpi_wait(rrcvreq(i), stat, ierr)
      enddo
    end if

    do i=1, nlatl
      data2d(:,i)=rrcvbuf(:,nrcv,i)
    enddo

    if (nrcv==nfrecv) called_ready_to_recv=.false.

  end subroutine gfs_comm_irecv

  subroutine set_nfsend(n)

    integer, intent(in) :: n
    integer :: ierr
   
    if(nfsend_set) return
    if (deactivated) return

    if (.not.exchange_pe) then
      print *, 'gfs_comm: Warning: gfs_comm_send/recv called from a non  exchange pe - global rank: ', &
                global_rank
      return
    endif
    
    if (.not. gfs_connector_set) then
      print *, 'gfs_comm: Error: gfs_connector not set, call set_gfs_connector'
      call mpi_abort(MPI_COMM_WORLD,-1,ierr)
    endif
     
    nfsend = n
    allocate(rsndbuf(nlonl,nfsend,nlatl))
    allocate(rsndreq(nlatl))

    nfsend_set = .true.
    
  end subroutine

  subroutine set_nfrecv(n)
    integer, intent(in) :: n
    integer :: ierr

    if(nfrecv_set) return
    if (deactivated) return
   
    if (.not.exchange_pe) then
      print *, 'gfs_comm: Warning: gfs_comm_send/recv called from a non  exchange pe - global rank: ', &
                global_rank
      return
    endif
    
    if (.not. gfs_connector_set) then
      print *, 'gfs_comm: Error: gfs_connector not set, call set_gfs_connector'
      call mpi_abort(MPI_COMM_WORLD,-1,ierr)
    endif
     
    nfrecv = n
    allocate(rrcvbuf(nlonl,nfrecv,nlatl))
    allocate(rrcvreq(nlatl))

    nfrecv_set=.true.
    
  end subroutine

  subroutine end_gfs_comm()
    integer :: ierr
    call mpi_barrier(MPI_COMM_WORLD,ierr)
    call mpi_finalize(ierr)
  end subroutine end_gfs_comm
#endif
end module gfs_comm_mod
