*CMZ :  2.05/01 10/10/2023  10.33.44  by  Michael Scheer
*-- Author :    Michael Scheer   07/10/2023
      subroutine util_concave_2d(nverts,verts,ivera,nparts,nvera,tiny,kfail)

      implicit none

      double precision, dimension(:,:), allocatable :: work
      integer, dimension(:), allocatable :: khull,lconcave,kveto

      double precision :: verts(2,nverts),tiny
      integer :: ivera(nverts*nverts)

      integer nverts,nhull,nconcave,kfail,l,i,n2,nparts,nvera(nverts),nv

      allocate(work(2,nverts),khull(nverts),kveto(nverts),lconcave(nverts))

      call util_hull_check_convex_2d(nverts,verts,khull,nhull,
     &  nconcave,lconcave,tiny,kfail)

      nparts=1
      kfail=0

      if (nconcave.eq.0) then
        nparts=1
        do i=1,nverts
          ivera(i)=i
          nvera(1)=nverts
        enddo
        goto 9999
      endif

      nv=0
      kveto=0
      do while(nconcave.gt.0)

        l=lconcave(nconcave)

        nv=nv+1
        ivera(nv)=l
        l=l+1
        if (l.gt.nverts) l=1
        do while(kveto(l).ne.0)
          l=l+1
        enddo
        nv=nv+1
        ivera(nv)=l
        kveto(l)=1
        l=l+1
        if (l.gt.nverts) l=1
        do while(kveto(l).ne.0)
          l=l+1
        enddo
        nv=nv+1
        ivera(nv)=l
        nvera(nparts)=3

        nparts=nparts+1
        n2=0
        do i=1,nverts
          if(kveto(i).ne.0) cycle
          n2=n2+1
          nv=nv+1
          ivera(nv)=i
          work(:,n2)=verts(:,i)
        enddo

        call util_hull_check_convex_2d(n2,work,khull,nhull,
     &    nconcave,lconcave,tiny,kfail)

        nv=nv-n2

      enddo

      nvera(nparts)=n2

9999  deallocate(work,khull,kveto,lconcave)

      return
      end
