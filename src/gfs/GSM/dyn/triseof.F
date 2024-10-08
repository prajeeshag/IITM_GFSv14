      subroutine triseof(triev,triod,trisca,levels,ls_node)
!
!02-23-2015 S . Moorthi - original created base on triseori
! 
      use gfs_dyn_resol_def
      use gfs_dyn_layout1
      implicit none
      integer              levels

      real(kind=kind_ior)   trisca(lnt2,levels)
      real(kind=kind_evod)  triev(len_trie_ls,2,levels)
      real(kind=kind_evod)  triod(len_trio_ls,2,levels)
!
      integer              ls_node(ls_dim,3)
!
!cmr  ls_node(1,1) ... ls_node(ls_max_node,1) : values of L
!cmr  ls_node(1,2) ... ls_node(ls_max_node,2) : values of jbasev
!cmr  ls_node(1,3) ... ls_node(ls_max_node,3) : values of jbasod
cc
!cmr    copy real even elements of scalar complex even-odd triangles
!cmr    to   real even triangles.  set top rows to zeros.
!cmr    copy imaginary even elements of scalar complex even-odd triangles
!cmr    to   imaginary even triangles.  set top rows to zeros.
!cmr    copy real odd elements of scalar complex even-odd triangles
!cmr    to   real odd triangles.  set top rows to zeros.
!cmr    copy imaginary odd elements of scalar complex even-odd triangles
!cmr    to   imaginary odd triangles.  set top rows to zeros.
!
!    local scalars
!    -------------
!
      integer   indsca, indev, indod, indev1,indev2, indod1, indod2
     &,         n, l, locl, k, i
!
!    statement functions
!    -------------------
!
!    offsca is scalar complex even-odd triangle offset in words.
!
      integer   offsca

      offsca(n,l) = (jcap+1)*(jcap+2) - (jcap-l+1)*(jcap-l+2) + 2*(n-l)
!
      integer   indlsev, jbasev, indlsod, jbasod
!
      include 'function2'
!
!     real(kind=kind_evod), parameter :: cons0=0.d0
!......................................................................
!
      do k = 1, levels
!
         do i=1,lnt2
           trisca(i,k) = 0.0
         enddo
         do locl=1,ls_max_node
                 l=ls_node(locl,1)
            jbasev=ls_node(locl,2)
            indev1 = indlsev(L,L)
            if (mod(L,2) == mod(jcap+1,2)) then
               indev2 = indlsev(jcap-1,L)
            else
               indev2 = indlsev(jcap  ,L)
            endif
!          copy the even (n-l) terms for each level
            indsca = offsca(l,l)
            do indev = indev1 , indev2
               trisca(indsca+1,k) = triev(indev,1,k) ! real part
               trisca(indsca+2,k) = triev(indev,2,k) ! imaginary part
               indsca = indsca+4
            end do
         end do
!......................................................................

         do locl=1,ls_max_node
                 l=ls_node(locl,1)
           if (l /= jcap) then
              jbasod = ls_node(locl,3)
              indod1 = indlsod(L+1,L)
              if (mod(L,2) == mod(jcap+1,2)) then
                 indod2 = indlsod(jcap  ,L)
              else
                 indod2 = indlsod(jcap-1,L)
              endif
!          copy the odd (n-l) terms for each level
              indsca   = offsca(l+1,l)
              do indod = indod1 , indod2
                 trisca(indsca+1,k) = triod(indod,1,k) ! real part
                 trisca(indsca+2,k) = triod(indod,2,k) ! imaginary part
                 indsca = indsca+4
              end do
 
           endif
         end do
!
      end do
!
      return
      end
