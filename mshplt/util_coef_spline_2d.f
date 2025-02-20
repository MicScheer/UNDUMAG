*CMZ :  1.04/00 19/03/2024  11.06.49  by  Michael Scheer
*-- Author :    Michael Scheer   17/03/2024

! +PATCH,//UTIL/FOR
! +DECK,util_coef_spline_2d.
! *-- Author :    Michael Scheer   17/03/2024
      subroutine util_coef_spline_2d(nx,ny,f,coef,istat)

      implicit none

      integer nx,ny

      real*8 f(nx,ny),coef(4,4,nx,ny),
     &  fx(nx,ny),fy(nx,ny),fxy(nx,ny)

      real*8, dimension(16) :: a16=[
     &  1.0d0,0.0d0,-3.0d0,2.0d0,
     &  0.0d0,0.0d0,3.0d0,-2.0d0,
     &  0.0d0,1.0d0,-2.0d0,1.0d0,
     &  0.0d0,0.0d0,-1.0d0,1.0d0
     &  ]

      real*8, dimension(4,4) :: ar,al,fm,a
      equivalence (al,a16)

      real*8, dimension(:), allocatable :: t
      real*8 ::
     &  p(max(nx,ny)),p1(max(nx,ny)),p2(max(nx,ny))

      integer :: istat,ix,iy,ifail,nxyo=0,nxy,i,j

      save nxyo,t

      istat=0
      ifail=0

      nxy=max(nx,ny)
      if (nxy.gt.nxyo) then
        deallocate(t,stat=istat)
        allocate(t(nxy))
        do i=1,nxy
          t(i)=dble(i)
        enddo
      endif

      do iy=1,ny
        p(1:nx)=f(1:nx,iy)
        call util_coef_spline(nx,t,p,0.0d0,0.0d0,p1,p2,istat)
        if (istat.ne.0) ifail=ifail+1
        fx(1:nx,iy)=p1(1:nx)
      enddo

      do ix=1,nx
        p(1:ny)=f(ix,1:ny)
        call util_coef_spline(ny,t,p,0.0d0,0.0d0,p1,p2,istat)
        if (istat.ne.0) ifail=ifail+1
        fy(ix,1:ny)=p1(1:ny)
      enddo

      do ix=1,nx
        p(1:ny)=fx(ix,1:ny)
        call util_coef_spline(ny,t,p,0.0d0,0.0d0,p1,p2,istat)
        if (istat.ne.0) ifail=ifail+1
        fxy(ix,1:ny)=p1(1:ny)
      enddo

      ar=transpose(al)

      do ix=1,nx-1
        do iy=1,ny-1

          fm(1,1:2)=f(ix,iy:iy+1)
          fm(1,3:4)=fy(ix,iy:iy+1)

          fm(2,1:2)=f(ix+1,iy:iy+1)
          fm(2,3:4)=fy(ix+1,iy:iy+1)

          fm(3,1:2)=fx(ix,iy:iy+1)
          fm(3,3:4)=fxy(ix,iy:iy+1)

          fm(4,1:2)=fx(ix+1,iy:iy+1)
          fm(4,3:4)=fxy(ix+1,iy:iy+1)

          a=matmul(fm,ar)
          coef(1:4,1:4,ix,iy)=matmul(al,a)

        enddo
      enddo
      coef(1:4,1:4,nx,1:ny)=-1.0d0
      coef(1:4,1:4,1:nx,ny)=-2.0d0
      do ix=1,nx
        do i=1,4
          do j=1,4
            coef(i,j,ix,ny)=2.0d0*coef(i,j,ix,ny-1)-coef(i,j,ix,ny-2)
c            if (coef(i,j,ix,ny).ne.0.0d0) then
c              print*,ix,i,j
c            endif
          enddo
        enddo
        coef(1,1,ix,ny)=f(ix,ny)
      enddo

      do iy=1,ny
        do i=1,4
          do j=1,4
            coef(i,j,nx,iy)=2.0d0*coef(i,j,nx-1,iy)-coef(i,j,nx-2,iy)
          enddo
        enddo
        coef(1,1,nx,iy)=f(nx,iy)
      enddo

      do i=1,4
        do j=1,4
          coef(i,j,nx,ny)=2.0d0*coef(i,j,nx-1,ny-1)-coef(i,j,nx-2,ny-2)
        enddo
      enddo
      coef(1,1,nx,ny)=f(nx,ny)

c      do iy=1,ny
c        do ix=1,nx
c          do i=1,4
c            do j=1,4
c              print*,ix,iy,i,j,coef(i,j,ix,iy)
c            enddo
c          enddo
c        enddo
c      enddo
c
c      stop

      nxyo=nxy
      istat=ifail

      return
      end
