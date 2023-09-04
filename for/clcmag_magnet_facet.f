*CMZ :  2.04/13 04/09/2023  12.00.02  by  Michael Scheer
*-- Author :    Michael Scheer   04/09/2023
      subroutine clcmag_magnet_facet(tmag,iv,kface,ison,hulltiny)

      use magnets_structure

      implicit none

      double precision hulltiny
      integer iv,kface,ison,ipoi,npoi,ifac,k,l,iface

      Type(T_Magnet) tmag

      double precision p(3),v(3),dot,gcen(3)

      gcen=tmag%gcen
      ison=0

      do ifac=1,tmag%nface
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
          p(1)=tmag%xhull(tmag%t_voxels(iv)%kface(l))
          p(2)=tmag%yhull(tmag%t_voxels(iv)%kface(l))
          p(3)=tmag%zhull(tmag%t_voxels(iv)%kface(l))
          v=p-tmag%fcen(1:3,ifac)
          v=v/norm2(v)
          dot=dot_product(v,tmag%fnorm(1:3,ifac))
          if(abs(dot).le.hulltiny) then
            ison=ison+1
          endif
        enddo
        if (ison.eq.npoi) then
          ison=1
          return
        endif
      enddo

      return
      end
