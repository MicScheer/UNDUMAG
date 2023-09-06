*CMZ :  2.04/14 06/09/2023  07.41.20  by  Michael Scheer
*CMZ :  2.04/13 04/09/2023  12.00.02  by  Michael Scheer
*-- Author :    Michael Scheer   04/09/2023
      subroutine clcmag_magnet_facet(tmag,iv,kface,ison,hulltiny)

      use magnets_structure

      implicit none

      double precision hulltiny
      integer :: iv,kface,ison,ipoi,npoi,ifac,k,l,iface
c      integer :: ical=0,lund

      Type(T_Magnet) tmag

      double precision p(3),v(3),dot,gcen(3),gvcen(3)

c      if (tmag%kmag.gt.1) then
c        stop "Ende in clcmag_magnet_facet"
c      endif

      gcen=tmag%gcen
      gvcen=tmag%t_voxels(iv)%gcen
c      if(tmag%cnam.eq.'polLSF1'.and.tmag%nface.eq.5.and.iv.eq.2.and.kface.eq.3) then
c        print*,tmag%cnam,tmag%nface,iv,kface
c      endif

c      if (ical.eq.0) then
c        call clcmag_dump_hull_of_magnet(tmag%kmag,lund)
c      endif

      ison=0

      do ifac=1,tmag%nface
        if (tmag%kmag.eq.1) write(65,*)ifac,tmag%fcen(1:3,ifac)
        l=1
        iface=1
        do k=1,tmag%t_voxels(iv)%kfacelast
          if (iface.eq.kface) exit
          iface=iface+1
          l=l+tmag%t_voxels(iv)%kface(l)+1
        enddo
        ison=0
        npoi=tmag%t_voxels(iv)%kface(l)
        do ipoi=1,npoi
          l=l+1
c          p(1)=tmag%xhull(tmag%t_voxels(iv)%kface(l))
c          p(2)=tmag%yhull(tmag%t_voxels(iv)%kface(l))
c          p(3)=tmag%zhull(tmag%t_voxels(iv)%kface(l))
          p(1)=tmag%t_voxels(iv)%xhull(tmag%t_voxels(iv)%kface(l))+gvcen(1)
          p(2)=tmag%t_voxels(iv)%yhull(tmag%t_voxels(iv)%kface(l))+gvcen(2)
          p(3)=tmag%t_voxels(iv)%zhull(tmag%t_voxels(iv)%kface(l))+gvcen(3)
          if (tmag%kmag.eq.1) write(66,*)ifac,iv,ipoi,p
          v=p-tmag%fcen(1:3,ifac)-gcen(:)
          v=v/norm2(v)
          dot=dot_product(v,tmag%fnorm(1:3,ifac))
          if(abs(dot).le.hulltiny) then
            ison=ison+1
          endif
c          print*,ifac,dot,ison
        enddo

        if (ison.eq.npoi) then
          ison=1
          return
        endif
      enddo

c      ical=ical+1

      return
      end
