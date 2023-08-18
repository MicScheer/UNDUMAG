*CMZ :  2.04/09 15/08/2023  13.33.25  by  Michael Scheer
*CMZ :  2.04/02 25/02/2023  17.18.44  by  Michael Scheer
*CMZ :  2.04/00 13/01/2023  11.54.35  by  Michael Scheer
*CMZ :  2.03/00 31/07/2022  18.33.07  by  Michael Scheer
*CMZ :  2.02/01 19/01/2022  10.38.36  by  Michael Scheer
*-- Author :    Michael Scheer   20/04/2021
      subroutine clcmag_mothers

      use undumagf90m
      use commandlinef90m
      use magnets_structure

      implicit none

      double precision, dimension (:), allocatable :: xh,yh,zh
      double precision, dimension (:,:), allocatable :: hull
      double precision :: tol=1.0d-6,gcen(3),x,y,z

      integer, dimension (:,:), allocatable :: kedge
      integer, dimension (:), allocatable :: kface,khull

      integer im,moth,imag,nm,nmag,nhull,npoi,nh,nedge,nface,ifail,
     &  kfacelast,i,l,nhmag

      character(32) cnam,cmoth

      allocate(t_mothers(nmag_t+nspecmag_t))

      tol=hulltiny

      do imag=1,nmag_t+nspecmag_t
        cmoth=t_magnets(imag)%cmoth
        cnam=t_magnets(imag)%cnam
        moth=-1
        do im=1,nmoth_t
          if (t_mothers(im)%cmoth.eq.cmoth) then
            t_mothers(im)%nmagnets=t_mothers(im)%nmagnets+1
            t_mothers(im)%cmoth=cmoth
            moth=im
            exit
          endif
        enddo
        if (moth.eq.-1) then
          nmoth_t=nmoth_t+1
          im=nmoth_t
          t_mothers(im)%nmagnets=t_mothers(im)%nmagnets+1
          t_mothers(im)%cmoth=cmoth
        endif
      enddo !nmag_t

      do im=1,nmoth_t
        nm=t_mothers(im)%nmagnets
        allocate(t_mothers(im)%magnets(nm))
        t_mothers(im)%nhull=0
      enddo

      do imag=1,nmag_t+nspecmag_t
        cmoth=t_magnets(imag)%cmoth
        nmag=0
        nhmag=t_magnets(imag)%nhull
        do moth=1,nmoth_t
          if (t_mothers(moth)%cmoth.eq.cmoth) then
            nmag=nmag+1
            t_mothers(moth)%magnets(nmag)=imag
            t_mothers(moth)%nhull=t_mothers(moth)%nhull+nhmag
            nhull=max(nhull,t_mothers(moth)%nhull)
            exit
          endif
        enddo
      enddo !nmag_t

      allocate(hull(3,nhull))
      allocate(xh(nhull),yh(nhull),zh(nhull))
      allocate(khull(nhull))
      allocate(kedge(4,2*nhull-2),kface((nhull+1)*nhull))

      do moth=1,nmoth_t

        npoi=0
        nmag=t_mothers(moth)%nmagnets

        do imag=1,nmag
          x=t_magnets(imag)%xyz(1)
          y=t_magnets(imag)%xyz(2)
          z=t_magnets(imag)%xyz(3)
          do i=1,t_magnets(imag)%nhull
            npoi=npoi+1
            xh(npoi)=t_magnets(imag)%xhull(i)+x
            yh(npoi)=t_magnets(imag)%yhull(i)+y
            zh(npoi)=t_magnets(imag)%zhull(i)+z
          enddo
        enddo

        call util_convex_hull_3d_overwrite(npoi,xh,yh,zh,khull,kedge,kface,
     &    nh,nedge,nface,kfacelast,tol,ifail)

        if (ifail.ne.0) then
          print*,""
          print*,"*** Error in clcmag_mothers, bad return from util_convex_hull_3d for ",cmoth," ***"
          print*,""
          stop "*** UNDUAMG aborted ***"
        endif

        t_mothers(moth)%nhull=nh
        allocate(
     &    t_mothers(moth)%xhull(nh),
     &    t_mothers(moth)%yhull(nh),
     &    t_mothers(moth)%zhull(nh),
     &    t_mothers(moth)%khull(nh),
     &    t_mothers(moth)%kedge(4,2*nh-2),
     &    t_mothers(moth)%kface(kfacelast)
     &    )

        t_mothers(moth)%kfacelast=kfacelast
        t_mothers(moth)%kface(1:kfacelast)=kface(1:kfacelast)
        t_mothers(moth)%khull(1:nh)=khull(1:nh)
        t_mothers(moth)%kedge(1:4,1:nedge)=kedge(1:4,1:nedge)

        l=0
        gcen=0.0d0
        do i=1,nh
          t_mothers(moth)%xhull(i)=xh(i)
          t_mothers(moth)%yhull(i)=yh(i)
          t_mothers(moth)%zhull(i)=zh(i)
          gcen=gcen+[x,y,z]
        enddo

        gcen=gcen/nh
        t_mothers(moth)%xyz=gcen
        t_mothers(moth)%gcen=gcen

      enddo

      return
      end
