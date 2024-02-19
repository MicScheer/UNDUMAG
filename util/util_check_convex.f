*CMZ :          04/10/2023  10.14.51  by  Michael Scheer
*-- Author :    Michael Scheer   02/10/2023
      subroutine util_chek_to_convex(nverts,verts,nconcave,lconcave,hulltiny)

      implicit none


      double precision, dimension (:), allocatable :: xp,yp,zp

      double precision verts(3,nverts),x,y,z,hulltiny

      integer, dimension (:,:), allocatable :: kedge
      integer, dimension (:), allocatable :: khull,kface

      integer i,nconcave,nverts,nface,nedge,nhull,lconcave(nverts),kfail,
     &  kfacelast

*KEEP,hulldim.
      integer lenhull,lenedge,lenface,nverhullmax
      common/uhullc/lenhull,lenedge,lenface,nverhullmax
*KEND.

      integer :: ipos(2,1000),nwords,istat,nallo=1000,nfacemax,npoimax,kloc(3)

      nconcave=0
      nallo=2*(nverts+1)*nverts

      allocate(xp(nallo),yp(nallo),zp(nallo))
      allocate(khull(nallo),kface(nallo),kedge(4,nallo))

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

      deallocate(khull,kedge,kface,xp,yp,zp)

      return
      end
