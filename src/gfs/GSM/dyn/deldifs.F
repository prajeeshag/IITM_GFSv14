      subroutine deldifs(rte,we,qme,xe,ye,teme,
     &                   rto,wo,qmo,xo,yo,temo,deltim,sl,
     &                   ls_node,coef00,k_level,hybrid,gen_coord_hybrid)
!
! Code Revision
! 14/02/2011 Sarah Lu, modify FSHK (to be consistent with CFS)
! 2011 02 20 : Henry Juang, updated code for ndsl implementation
! 2012 10 18 : S. Moorthi Updating to match current GFS - adding hdfac etc
! 2014 07 21 : S. Moorthi - merge with GFS trunk version
!

      use gfs_dyn_machine       , only : kind_evod
      use gfs_dyn_resol_def     , only : jcap,levr,levs,ntrac,adiabatic
      use namelist_dynamics_def , only : hdif_fac, hdif_fac2, slrd0,
     &                                   cdamp, k2o
      use gfs_dyn_layout1       , only : len_trie_ls,len_trio_ls,
     &                                   ls_dim,ls_max_node,me
      use gfs_dyn_coordinate_def, only : bk5,ck5,thref
      use deldifs_def           , only : bkly,ckly,dne,dno,rthk,rtrd,sf,
     &                                   rtnp,jdel,dneh,dnoh,cthk,
     &                                   rfacto, rfacte, facte, facto
      use gfs_dyn_physcons      , only : rerth => con_rerth,
     &                                   rd => con_rd,
     &                                   cp => con_cp
      use omp_lib
!
      implicit none
!
      logical hybrid, gen_coord_hybrid
      real(kind=kind_evod), dimension(len_trie_ls,2,ntrac) :: rte
      real(kind=kind_evod), dimension(len_trie_ls,2) :: we, xe
     &,                                                 ye, teme
      real(kind=kind_evod), dimension(len_trie_ls,2) :: qme
!
      real(kind=kind_evod), dimension(len_trio_ls,2,ntrac) :: rto
      real(kind=kind_evod), dimension(len_trio_ls,2) :: wo, xo
     &,                                                 yo, temo
      real(kind=kind_evod), dimension(len_trio_ls,2) :: qmo
      real(kind=kind_evod) :: pe1, pe2, po1, po2, deltim, sl(levs)
!
      integer              ls_node(ls_dim,3)
!
!cmr  ls_node(1,1) ... ls_node(ls_max_node,1) : values of l
!cmr  ls_node(1,2) ... ls_node(ls_max_node,2) : values of jbasev
!cmr  ls_node(1,3) ... ls_node(ls_max_node,3) : values of jbasod
!
      real(kind=kind_evod) coef00(levs,ntrac)
!
      integer              k_level
      integer              jdelh,k,kd,ku,l,locl,n,n0,nd,np,npd,it
      integer              indev, indod, indev1, indev2, indod1,indod2
!
      real(kind=kind_evod) dn1,realval,fact
     &,                    rfact,rfactrd,rtrd1,fshk,wrk1,wrk2,wrk3
!
      real(kind=kind_evod), parameter :: rkappa = cp / rd
      real(kind=kind_evod), parameter :: cons0=0.0, cons1=1.0, cons2=2.0
!
      integer              indlsev,jbasev,indlsod,jbasod
      integer           :: N_cutoff
      real(kind=kind_evod) :: realval1
      real(kind=kind_evod) :: rfact1, rfact2
!
      include 'function_indlsev'
      include 'function_indlsod'
!
!......................................................................
      N_cutoff = 0
      realval1 = jcap*(jcap+1)

!
      if (k_level == 0) then
        allocate ( bkly(levs) )        					! hmhj
        allocate ( ckly(levs) )        					! hmhj
        do k=1,levs
          bkly(k) = 1.0
          ckly(k) = 0.0
        enddo
        do  k=1,levs
          bkly(k) = 0.5*(bk5(levs-k+1)+bk5(levs-k+2))/sl(k)
        enddo
        allocate(rfacte(len_trie_ls,levs))
        allocate(rfacto(len_trio_ls,levs))
        allocate(facte(len_trie_ls,levs))
        allocate(facto(len_trio_ls,levs))
        rfacte(:,:) = 0.98
        rfacto(:,:) = 0.98
        facte(:,:) = -1. + 1./0.98
        facto(:,:) = -1. + 1./0.98

        do  k=1,levs
         do locl=1,ls_max_node
          l = ls_node(locl,1)
          jbasev = ls_node(locl,2)
          indev  = indlsev(l,l)
          do n=l,jcap,2
            if (n>N_cutoff) then
              realval     = n*(n+1)
              rfact1  = (1./(2.*jcap))*(realval/realval1)**2.
              rfact2 = exp( -0.5*( (n-jcap)/(n-N_cutoff))**2.)
              facte(indev,k) = rfact1*rfact2
              rfacte(indev,k)  = 1./(1.+rfact1*rfact2)
              indev       = indev + 1
             endif
           enddo
          enddo

         do locl=1,ls_max_node
           l = ls_node(locl,1)
           jbasev = ls_node(locl,2)
           if (mod(l,2) == mod(jcap+1,2)) then
             rfacte(indlsev(jcap+1,l),k)  = 1. ! set the even (n-l) terms of the top row to zero
             facte(indlsev(jcap+1,l),k)  = 0. ! set the even (n-l) terms of the top row to zero
           endif
          enddo

         do locl=1,ls_max_node
            l = ls_node(locl,1)
            jbasod = ls_node(locl,3)
            indod  = indlsod(l+1,l)
            do n=l+1,jcap,2
              if (n>N_cutoff) then
                realval     = n*(n+1)
                rfact1  = (1./(2.*jcap))*(realval/realval1)**2.
                rfact2 = exp( -0.5*( (n-jcap)/(n-N_cutoff))**2.)
                facto(indod,k)  = rfact1*rfact2
                rfacto(indod,k)  = 1./(1.+rfact1*rfact2)
                indod       = indod + 1
               endif
             enddo
          enddo
!
          do locl=1,ls_max_node
             l = ls_node(locl,1)
             jbasod = ls_node(locl,3)
             if (mod(l,2) /= mod(jcap+1,2)) then
                rfacto(indlsod(jcap+1,l),k)  = 1. ! set the odd (n-l) terms of the top row to zero
                facto(indlsod(jcap+1,l),k)  = 0.
             endif
          enddo

        enddo   ! k loop ends


      endif 

        n0   = 0            ! maximum wavenumber for zero diffusion
!......................................................................
!
      k = k_level
!!
!
!
      do locl=1,ls_max_node
              l = ls_node(locl,1)
         jbasev = ls_node(locl,2)
         if (l == 0) then
            n0 = 2
         else
            n0 = l
         endif
         indev1 = indlsev(n0,l)
         if (mod(l,2) == mod(jcap+1,2)) then
           indev2 = indlsev(jcap+1,l)
         else
           indev2 = indlsev(jcap  ,l)
         endif
!!       do n = n0, jcap+1, 2
         do indev = indev1 , indev2
 
           we(indev,1)      = we(indev,1)*rfacte(indev,k)
           we(indev,2)      = we(indev,2)*rfacte(indev,k)
 
           xe(indev,1)      = xe(indev,1)*rfacte(indev,k)
           xe(indev,2)      = xe(indev,2)*rfacte(indev,k)
 
           rte(indev,1,1) = rte(indev,1,1)*rfacte(indev,k)
           rte(indev,2,1) = rte(indev,2,1)*rfacte(indev,k)

           pe1 = bkly(k)*qme(indev,1) + ckly(k)*teme(indev,1)        	! hmhj
           pe2 = bkly(k)*qme(indev,2) + ckly(k)*teme(indev,2)        	! hmhj
 
           ye(indev,1)      = ( ye(indev,1)
     &         +   facte(indev,k)*coef00(k,1)* pe1 )*rfacte(indev,k)   	! hmhj
 
           ye(indev,2)      = ( ye(indev,2)
     &         +   facte(indev,k)*coef00(k,1)* pe2 )*rfacte(indev,k)    	! hmhj
 
         enddo
       enddo
!
!......................................................................
!
!      do l = 0, jcap
       do locl=1,ls_max_node
              l = ls_node(locl,1)
         jbasod = ls_node(locl,3)
         indod1 = indlsod(l+1,l)
         if (mod(l,2) == mod(jcap+1,2)) then
           indod2 = indlsod(jcap  ,l)
         else
           indod2 = indlsod(jcap+1,l)
         endif
         do indod = indod1 , indod2
 
           wo(indod,1)      = wo(indod,1)*rfacto(indod,k)
           wo(indod,2)      = wo(indod,2)*rfacto(indod,k)
 
           xo(indod,1)      = xo(indod,1)*rfacto(indod,k)
           xo(indod,2)      = xo(indod,2)*rfacto(indod,k)

           rto(indod,1,1) = rto(indod,1,1)*rfacto(indod,k)
           rto(indod,2,1) = rto(indod,2,1)*rfacto(indod,k)

           po1 = bkly(k)*qmo(indod,1) + ckly(k)*temo(indod,1)       	! hmhj
           po2 = bkly(k)*qmo(indod,2) + ckly(k)*temo(indod,2)       	! hmhj
 
           yo(indod,1)      = ( yo(indod,1)
     &        +   facto(indod,k)*coef00(k,1)* po1)*rfacto(indod,k)   		! hmhj
 
           yo(indod,2)      = ( yo(indod,2)
     &        +   facto(indod,k)*coef00(k,1)* po2 )*rfacto(indod,k)        	! hmhj
 
         enddo
       enddo
!
!......................................................................
!
      if (ntrac  >  1) then
        do it=2,ntrac
!
           do locl=1,ls_max_node
                  l = ls_node(locl,1)
             jbasev = ls_node(locl,2)
             if (l == 0) then
                n0 = 2
             else
                n0 = l
             endif
             indev1 = indlsev(n0,l)
             if (mod(l,2) == mod(jcap+1,2)) then
               indev2 = indlsev(jcap+1,l)
             else
               indev2 = indlsev(jcap  ,l)
             endif
             do indev = indev1 , indev2
 
               pe1 = bkly(k)*qme(indev,1) + ckly(k)*teme(indev,1)    	! hmhj
               pe2 = bkly(k)*qme(indev,2) + ckly(k)*teme(indev,2)     	! hmhj

               rte(indev,1,it) = ( rte(indev,1,it)
     &              + facte(indev,k)*coef00(k,it)* pe1 )*rfacte(indev,k) 	! hmhj
 
               rte(indev,2,it) = ( rte(indev,2,it)
     &              + facte(indev,k)*coef00(k,it)* pe2 )*rfacte(indev,k) 	! hmhj
 
             enddo
           enddo
!
!......................................................................
!
!         do l = 0, jcap
           do locl=1,ls_max_node
                  l = ls_node(locl,1)
             jbasod = ls_node(locl,3)
             indod1 = indlsod(l+1,l)
             if (mod(l,2) == mod(jcap+1,2)) then
               indod2 = indlsod(jcap  ,l)
             else
               indod2 = indlsod(jcap+1,l)
             endif
!            do n = l+1, jcap+1, 2
             do indod = indod1 , indod2
 
               po1 = bkly(k)*qmo(indod,1) + ckly(k)*temo(indod,1)	! hmhj
               po2 = bkly(k)*qmo(indod,2) + ckly(k)*temo(indod,2)   	! hmhj

               rto(indod,1,it) = ( rto(indod,1,it)
     &          +   facto(indod,k)*coef00(k,it)* po1 )*rfacto(indod,k)  	! hmhj
 
               rto(indod,2,it) = ( rto(indod,2,it)
     &          +   facto(indod,k)*coef00(k,it)* po2 )*rfacto(indod,k)   	! hmhj
 
             enddo
           enddo
        enddo                       ! tracer do loop end
      endif
!

!!
      return
      end
