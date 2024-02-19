*CMZ :          10/10/2023  09.20.22  by  Michael Scheer
*-- Author :    Michael Scheer   07/10/2023
      subroutine util_triangle_2d(nverts,verts,ivera,ntri,tiny,kfail)

      implicit none

      double precision :: verts(2,nverts),tiny
      integer ivera(nverts*nverts)
      integer, dimension(:), allocatable :: nvera,iwera

      integer nverts,kfail,ipart,nparts,ntri,nv,mparts,i1,i2,i,nw

      allocate(nvera(nverts))
      call util_concave_2d(nverts,verts,ivera,nparts,nvera,tiny,kfail)
      allocate(iwera(nverts*nverts))

      iwera=ivera

      ntri=0
      nw=0
      nv=0
      mparts=nparts
      do ipart=1,mparts
        if (nvera(ipart).eq.3) then
          ntri=ntri+1
          nv=nv+1
          nw=nw+1
          ivera(nv)=iwera(nw)
          nv=nv+1
          nw=nw+1
          ivera(nv)=iwera(nw)
          nv=nv+1
          nw=nw+1
          ivera(nv)=iwera(nw)
        else
          ntri=ntri+1
          nv=nv+1
          nw=nw+1
          ivera(nv)=iwera(nw)
          i1=nw
          nv=nv+1
          nw=nw+1
          ivera(nv)=iwera(nw)
          nv=nv+1
          nw=nw+1
          i2=nw
          ivera(nv)=iwera(nw)
          do i=4,nvera(ipart)
            ntri=ntri+1
            nv=nv+1
            ivera(nv)=iwera(i1)
            nv=nv+1
            ivera(nv)=iwera(i2)
            nv=nv+1
            nw=nw+1
            ivera(nv)=iwera(nw)
            i2=nw
          enddo
        endif
      enddo

      deallocate(nvera,iwera)

      return
      end
