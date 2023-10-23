*CMZ :  2.05/01 11/10/2023  17.11.14  by  Michael Scheer
*-- Author :    Michael Scheer   02/10/2023
      subroutine util_hull_check_convex(nverts,verts,
     &  khull,kedge,kface,nhull,nedge,nface,kfacelast,
     &  nconcave,lconcave,hulltiny,kfail)

      implicit none

      double precision, dimension (:), allocatable :: xp,yp,zp

      double precision verts(3,nverts),hulltiny,p1(3),p2(3),p3(3),q(3),dist,
     &  vnor(3)

      integer i,nconcave,nverts,nface,nedge,nhull,lconcave(nverts),kfail,
     &  kfacelast,khull(nverts),kedge(4,*),kface(*),l,iover

*KEEP,hulldim.
      integer lenhull,lenedge,lenface,nverhullmax
      common/uhullc/lenhull,lenedge,lenface,nverhullmax
*KEND.

      integer :: nallo=1000,kloc(3),iface,k,ipoi,npoi

      nconcave=0
      nallo=2*(nverts+1)*nverts

      allocate(xp(nallo),yp(nallo),zp(nallo))

      lenhull=nallo
      lenedge=nallo
      lenface=nallo
      nverhullmax=nallo

      xp(1:nverts)=verts(1,1:nverts)
      yp(1:nverts)=verts(2,1:nverts)
      zp(1:nverts)=verts(3,1:nverts)

      call util_convex_hull_3d(
     &  nverts,xp,yp,zp,khull,kedge,kface,
     &  nhull,nedge,nface,kfacelast,hulltiny,kfail)

      if (nhull.eq.nverts) return

      do i=1,nverts
        kloc=findloc(khull,i,1)
        if (kloc(1).eq.0) then
          nconcave=nconcave+1
          lconcave(nconcave)=i
        endif
      enddo

      k=0
      do iface=1,nface
        k=k+1
        if (k.gt.kfacelast) exit
        npoi=kface(k)
        do ipoi=1,npoi
          k=k+1
          p1=[xp(ipoi),yp(ipoi),zp(ipoi)]
          k=k+1
          p2=[xp(ipoi),yp(ipoi),zp(ipoi)]
          k=k+1
          p3=[xp(ipoi),yp(ipoi),zp(ipoi)]
        enddo
        do l=1,nconcave
          ipoi=lconcave(l)
          q=[xp(ipoi),yp(ipoi),zp(ipoi)]
          call util_plane_tiny(p1,p2,p3,q,vnor,dist,hulltiny,iover,kfail)
          if(abs(dist).ne.hulltiny) then
            lconcave(l)=-lconcave(l)
          endif
        enddo
      enddo

      deallocate(xp,yp,zp)

      return
      end
