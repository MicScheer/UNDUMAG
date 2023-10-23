*CMZ :  2.05/01 10/10/2023  10.34.41  by  Michael Scheer
*-- Author :    Michael Scheer   02/10/2023
      subroutine util_hull_check_convex_2d(nverts,verts,khull,nhull,
     &  nconcave,lconcave,tiny,kfail)

      implicit none


      double precision, dimension (:), allocatable :: xp,yp

      double precision verts(2,nverts),tiny

      integer i,nconcave,nverts,nhull,lconcave(*),kfail,khull(*)

      integer :: nallo=1000,kloc(3)

      nconcave=0
      nallo=2*(nverts+1)*nverts

      allocate(xp(nallo),yp(nallo))

      xp(1:nverts)=verts(1,1:nverts)
      yp(1:nverts)=verts(2,1:nverts)

      call util_convex_hull_2d(nverts,xp,yp,nhull,khull,tiny,kfail)

      khull(nhull)=0
      nhull=nhull-1

      if (nhull.eq.nverts) goto 9999

      do i=1,nverts
        kloc=findloc(khull(1:nhull),i,1)
        if (kloc(1).eq.0) then
          nconcave=nconcave+1
          lconcave(nconcave)=i
        endif
      enddo

9999  deallocate(xp,yp)

      return
      end
