*CMZ :  2.04/17 13/09/2023  16.13.11  by  Michael Scheer
*CMZ :  2.04/14 06/09/2023  07.41.20  by  Michael Scheer
*CMZ :  2.04/13 04/09/2023  12.00.02  by  Michael Scheer
*-- Author :    Michael Scheer   04/09/2023
      subroutine clcmag_magnet_facet(tmag,iv,ivface,ison,hulltiny)

      use magnets_structure

      implicit none

      double precision hulltiny
      integer :: iv,ivface,ison,l,iface

      integer :: idebug=0,ipoi,k

      Type(T_Magnet) tmag

      double precision p(3),distpc,distpm1,gcen(3),gvcen(3),dp(3),dpn,pm1(3),
     &  pc(3),vnor(3)

      gcen=tmag%gcen
      gvcen=tmag%t_voxels(iv)%gcen

      if (idebug.ne.0) then
        do iface=1,tmag%nface
          l=tmag%lface(iface)
          do k=1,tmag%kface(l)
            ipoi=tmag%kface(l+k)
            write(44,*)
     &        tmag%xhull(tmag%khull(ipoi)),
     &        tmag%yhull(tmag%khull(ipoi)),
     &        tmag%zhull(tmag%khull(ipoi)),
     &        ipoi,iface
          enddo
        enddo
      endif

      ison=0

      p=tmag%t_voxels(iv)%vcen(:,ivface)+gvcen
      vnor=tmag%t_voxels(iv)%vnorm(:,ivface)
      if (idebug.ne.0) then
        write(33,*)iv,ivface,p,vnor
      endif

      ison=0
      do iface=1,tmag%nface

        l=tmag%lface(iface)

        pc=tmag%fcen(:,iface)+gcen

        pm1=[
     &    tmag%xhull(tmag%kface(l+1)),
     &    tmag%yhull(tmag%kface(l+1)),
     &    tmag%zhull(tmag%kface(l+1))
     &    ]+gcen

        dp=p-pc
        dpn=norm2(dp)

        if (dpn.le.hulltiny) then
          distpc=0.0d0
        else
          distpc=abs(dot_product(tmag%fnorm(:,iface),dp/dpn))
        endif

        distpm1=abs(abs(dot_product(tmag%fnorm(:,iface),vnor))-1.0d0)

        if (distpm1.le.hulltiny.and.distpc.le.hulltiny) then
          ison=1
        endif

        if (idebug.ne.0) then
          write(55,*)iface,iv,ivface,p,distpm1,
     &      pm1,
     &      tmag%fnorm(:,iface),
     &      ison
        endif

        if (iv.eq.1.and.iface.eq.1) then
          print*,iface,sngl(p),sngl(pc),sngl(vnor),sngl(tmag%fnorm(:,iface)),ison
        endif

        if (ison.eq.1) return

      enddo !iface

      return
      end
