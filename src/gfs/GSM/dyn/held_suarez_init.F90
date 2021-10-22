module held_suarez_init_mod
  use gfs_dyn_resol_def, only: ivsupa, jcap, thermodyn_id, g_gz, &    
      g_qm, g_ttm, g_uum, g_vvm, g_p, g_dpm, g_rm, g_q, g_tt, &    
      g_uu, g_vv, g_rq, lnt2, lonf, lotgr, levs, ntrac, levh, &    
      levp1, latg2, latg
  use gfs_dyn_layout1, only: lats_node_a, nodes, ls_dim, &    
      lats_node_a_max, len_trie_ls, len_trio_ls, ls_max_node, &    
      ipt_lats_node_a, me 
  use gfs_dyn_coordinate_def, only: vcoord, idvc, ck, bkl, &    
      dbk, bk5, ak5, thref, vertcoord_id, ck5 
  use gfs_dyn_io_header, only: z, z_r, iensi, icen, idusr, itrun, &    
      idrun, idpp, iens, icen2, latb, lonb, irealf, iorder, ncldt, &    
      ienst
  use namelist_dynamics_def, only: fhrot, fhini, gen_coord_hybrid, &    
      hybrid, igen
  use gfs_dyn_vert_def, only: sl, slk, si, sik 
  use gfs_dyn_mpi_def, only: num_pes_fcst, kind_grid, &    
      kind_io4, kind_evod, kind_io8
  use gfs_dyn_physcons, rerth => con_rerth, grav => con_g, &                      
      rkap  => con_rocp,  cpd  => con_cp
      ! use nemsio_module
  use nemsio_def, only: nemsio_open, gfile_in, nemsio_getfilehead, &    
      nemsio_getheadvar, nemsio_readrecv, nemsio_readrecvw34, &    
      nemsio_close
  use gfs_dyn_date_def,      only : fhour
  use gfs_dyn_tracer_config, ONLY : gfs_dyn_tracer 

  use mpp_mod, only: mpp_error, FATAL, NOTE, WARNING
  use fms_io_mod, only: read_data, field_size

  implicit none

  private

  real(kind=kind_evod), parameter :: pa2cb=0.001
  real(kind=kind_evod), parameter :: rkapi=1.0/rkap, rkapp1=1.0+rkap

  character(len=32) :: vcoordfile='ak_bk.nc'

  public :: held_suarez_init

  contains
      
      
  subroutine held_suarez_init(IDATE,tlev,restart_run, &
        gze,qe,tee,die,zee,rqe,gzo,qo,teo,dio,zeo,rqo, &
        grid_gr,ls_node,ls_nodes,max_ls_nodes,pdryini,iprint, &
        global_lats_a,lats_nodes_a,lonsperlat, & 
        epse,epso,plnew_a,plnow_a,snnp1ev,snnp1od)

    integer, intent(out) ::     idate(4)
    integer :: idate7(7),levsi, jcapi, latgi, lonfi, tlev
    logical, intent(in) :: restart_run
    real(kind=kind_grid), intent(out) :: grid_gr(lonf,lats_node_a_max,lotgr)
    real(kind=kind_evod), dimension(len_trie_ls,2), intent(out) :: gze, qe
    real(kind=kind_evod), dimension(len_trio_ls,2), intent(out) :: gzo, qo
    real(kind=kind_evod), dimension(len_trie_ls,2,levs), intent(out) :: tee,die,zee
    real(kind=kind_evod), dimension(len_trio_ls,2,levs), intent(out) :: teo,dio,zeo
    real(kind=kind_evod), dimension(len_trie_ls,2,levs,ntrac), intent(out) :: rqe
    real(kind=kind_evod), dimension(len_trio_ls,2,levs,ntrac), intent(out) :: rqo

    real(kind=kind_evod), dimension(len_trie_ls), intent(in) :: epse, snnp1ev
    real(kind=kind_evod), dimension(len_trio_ls), intent(in) :: epso, snnp1od
    real(kind=kind_evod), dimension(len_trie_ls,latg2), intent(in) :: plnew_a
    real(kind=kind_evod), dimension(len_trio_ls,latg2), intent(in) :: plnow_a
    integer, intent(in) :: ls_node(ls_dim,3)

    integer, dimension(nodes), intent(in) :: max_ls_nodes, lats_nodes_a
    integer, intent(in) :: ls_nodes(ls_dim,nodes), iprint

    integer :: j,k,l,locl,n,lv,kk,w3rlkind,w3ikind, &
        i,lan,lat,iblk,lons_lat,il,lon,njeff,nn, &
        indev,indod,indev1,indev2,indod1,indod2, &
        nfhour,nfminute,nfsecondn,nfsecondd 

    real(kind=kind_evod) :: xi(levp1),sikp1(levp1),xl(levs)
    real(kind=kind_io4) ::  vtid,runid4,pdryini4,xncld,xgf, &
        trun,waves,xlayers

    real(kind=kind_grid) ::  pdryini
    real(kind=kind_io4), allocatable :: vcoord4(:,:,:)

    integer :: nreci
    character(len=8) :: vname
    character(len=8), allocatable  :: recnamei(:), reclevtypi(:)
    integer, allocatable  :: reclevi(:)

    integer :: iret, num_dta, ijm, tlmeta
    real(kind=kind_evod) :: ga2, psurfff, pressk, tem

    integer kmsk(lonf,latg), global_lats_a(latg), lonsperlat(latg)
    real(kind=kind_io8), dimension(lonf,lats_node_a) ::  buffo, buff2
    real(kind=kind_evod) teref(levp1),ck5p(levp1)
    real (kind=kind_io4), allocatable ::  nemsio_data(:)
    real(kind=kind_grid), dimension(lonf,lats_node_a) :: zsg, psg, pwat, ptot
    real(kind=kind_grid), dimension(lonf,lats_node_a,levs) :: uug, vvg, ttg
    real(kind=kind_grid) :: rqg(lonf,lats_node_a,levh)
    real(kind=8) :: timef,stime,etime
    integer :: indlsev,jbasev,indlsod,jbasod, siz(4)

    namelist/hs_init_nml/ivsupa,idate,fhour,idvc,ncldt,iorder,irealf, &          
      igen,lonb,latb,icen2, iens, iensi, ienst, idpp, idrun, itrun, &          
      idusr,pdryini,vcoord

    INCLUDE 'function2'
!------------------------------------------------------------------------
!
    ivsupa = 200809
    idvc = 2
    ncldt = 1
    idate(:) = [0, 1, 1, 2000]
    iorder = -9999
    irealf = -9999
    igen = -9999
    lonb = lonf
    latb = latg
    icen2 = 0
    iens = 0
    idpp = -9999
    idrun = -9999
    itrun = 1
    idusr = -9999
    pdryini4 = 0.

    call field_size(vcoordfile,'ak',siz)
    if (siz(1)/=levp1) then
      call mpp_error(FATAL, 'The nlevs and size of ak, bk does not match')
    endif
    allocate(vcoord(levp1,3))
    vcoord = 0.
    call read_data(vcoordfile,'ak',vcoord(:,1))
    call read_data(vcoordfile,'bk',vcoord(:,2))

    if (hybrid .and. idvc .eq. 2) then
      psurfff = 101.3
      ck5=0.
      do k=1,levp1
        ak5(k) = vcoord(levp1+1-k,1)/1000.
        bk5(k) = vcoord(levp1+1-k,2)
        pressk = ak5(k) + bk5(k)*psurfff
      enddo
      do k=1,levs
        dbk(k) = bk5(k+1)-bk5(k)
        bkl(k) = (bk5(k+1)+bk5(k))*0.5
        ck(k)  = ak5(k+1)*bk5(k)-ak5(k)*bk5(k+1)
      enddo
      do k=1,levs+1
        si(levs+2-k) = ak5(k)/psurfff + bk5(k) !ak(k) bk(k) go top to bottom
      enddo
      do k=1,levs
        sl(k) = 0.5*(si(k)+si(k+1))
      enddo
    else
      print *,' Non compatible Initial state IDVC=',idvc 
      call MPI_QUIT(560)
    endif

    FHOUR = 0 

    if (tlev == 1) fhini = fhour
    itrun       = itrun
    icen        = 7
    icen2       = icen2
    igen        = igen
    ienst       = iens(1)
    iensi       = iens(2)
    pdryini4 = 0.0
    if (pdryini == 0.0) pdryini = pdryini4

    do k=1,levh
      do j=1,lats_node_a
        do i=1,lonf
          rqg(i,j,k) = 0.0
        enddo
      enddo
    enddo

    if(.not. me < num_pes_fcst) return

    zsg = 0. ! topography
    psg = 100000. ! Surface pressure 1000hpa
    uug = 0. ! U-velocity
    vvg = 0. ! V-velocity
    ttg = 273.15 ! Temperature
    rqg = 0. ! Tracers

    if (tlev == 1) then
      do j = 1, lats_node_a
        lat      = global_lats_a(ipt_lats_node_a-1+j)
        lons_lat = lonsperlat(lat)
        do i=1,lons_lat
          grid_gr(i,j,g_gz)  = zsg(i,j)
          grid_gr(i,j,g_qm ) = log(psg(i,j)*pa2cb)
        enddo
      enddo
      do k=1,levs
        do j = 1, lats_node_a
          lat      = global_lats_a(ipt_lats_node_a-1+j)
          lons_lat = lonsperlat(lat)
          do i=1,lons_lat
            grid_gr(i,j,g_ttm+k-1) = ttg(i,j,k)
            grid_gr(i,j,g_uum+k-1) = uug(i,j,k)
            grid_gr(i,j,g_vvm+k-1) = vvg(i,j,k)
          enddo
        enddo
      enddo
      do lan=1,lats_node_a
        lat      = global_lats_a(ipt_lats_node_a-1+lan)
        lons_lat = lonsperlat(lat)
        call hyb2press(lons_lat, lonf, grid_gr(1:lonf,lan,g_qm), &
                       grid_gr(1:lonf,lan,g_p:g_p+levs-1), &
                       grid_gr(1:lonf,lan,g_dpm:g_dpm+levs-1))
      enddo
      do k=1,levh
        do j = 1, lats_node_a
          do i=1,lonf
            grid_gr(i,j,g_rm+k-1) = rqg(i,j,k)
          enddo
        enddo
      enddo
    endif

    if (.not. restart_run .and. fhini == fhrot) then
      do j = 1, lats_node_a
        do i=1,lonf
          grid_gr(i,j,g_q ) = grid_gr(i,j,g_qm)
        enddo
      enddo
      do k=1,levs
        do j = 1, lats_node_a
          do i=1,lonf
            grid_gr(i,j,g_tt+k-1) = ttg(i,j,k)
            grid_gr(i,j,g_uu+k-1) = uug(i,j,k)
            grid_gr(i,j,g_vv+k-1) = vvg(i,j,k)
          enddo
        enddo
      enddo
      do k=1,levh
        do j = 1, lats_node_a
          do i=1,lonf
            grid_gr(i,j,g_rq+k-1) = rqg(i,j,k)
          enddo
        enddo
      enddo
    elseif (tlev == 2) then
      do j = 1, lats_node_a
        do i=1,lonf
          grid_gr(i,j,g_gz) = zsg(i,j)
          grid_gr(i,j,g_q ) = log(psg(i,j)*pa2cb)
        enddo
      enddo
      do k=1,levs
        do j = 1, lats_node_a
          do i=1,lonf
            grid_gr(i,j,g_tt+k-1) = ttg(i,j,k)
            grid_gr(i,j,g_uu+k-1) = uug(i,j,k)
            grid_gr(i,j,g_vv+k-1) = vvg(i,j,k)
          enddo
        enddo
      enddo
      do k=1,levh
        do j = 1, lats_node_a
          do i=1,lonf
            grid_gr(i,j,g_rq+k-1) = rqg(i,j,k)
          enddo
        enddo
      enddo
    endif

    call grid_to_spect_inp_slg &
            (zsg,psg,uug,vvg,ttg,rqg, &
            GZE,QE,TEE,DIE,ZEE,RQE, &
            GZO,QO,TEO,DIO,ZEO,RQO, & 
            ls_node,ls_nodes,max_ls_nodes, &
            lats_nodes_a,global_lats_a,lonsperlat, &
            epse,epso,plnew_a,plnow_a)

    call triseof(gze,gzo,z_r,1,ls_node)
    do i=1,lnt2
      z(i) = z_r(i)
    enddo

    !convert to laplacian of terrain for divergence
    ga2 = grav/(rerth*rerth)
    do locl=1,ls_max_node
            L = ls_node(locl,1)
       jbasev = ls_node(locl,2)
       indev1 = indlsev(L,L)
       if (mod(L,2) == mod(jcap+1,2)) then
          indev2 = indlsev(jcap+1,L)
       else
          indev2 = indlsev(jcap  ,L)
       endif
       do indev = indev1 , indev2
         GZE(INDEV,1) = GZE(INDEV,1)*SNNP1EV(INDEV)*GA2
         GZE(INDEV,2) = GZE(INDEV,2)*SNNP1EV(INDEV)*GA2
       end do
    end do
    do locl=1,ls_max_node
            L = ls_node(locl,1)
       jbasod = ls_node(locl,3)
       indod1 = indlsod(L+1,L)
       if (mod(L,2) == mod(jcap+1,2)) then
          indod2 = indlsod(jcap  ,L)
       else
          indod2 = indlsod(jcap+1,L)
       endif
       do indod = indod1 , indod2
         GZO(INDOD,1) = GZO(INDOD,1)*SNNP1OD(INDOD)*GA2
         GZO(INDOD,2) = GZO(INDOD,2)*SNNP1OD(INDOD)*GA2
       enddo
    enddo

    if (me==0) then
      write(*,nml=hs_init_nml)
    endif

    return
  end subroutine held_suarez_init

end module held_suarez_init_mod
