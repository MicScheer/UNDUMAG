*CMZ :  2.02/00 21/10/2020  09.46.44  by  Michael Scheer
*CMZ :  1.13/03 16/03/2017  09.36.56  by  Michael Scheer
*CMZ :  1.04/02 15/09/2016  15.11.30  by  Michael Scheer
*CMZ :  0.00/13 28/07/2016  12.38.30  by  Michael Scheer
*CMZ :  0.00/09 25/06/2016  14.02.31  by  Michael Scheer
*CMZ :  0.00/07 22/06/2016  14.17.39  by  Michael Scheer
*CMZ :  1.17/07 03/04/2016  19.29.14  by  Michael Scheer
*CMZ :  1.17/06 01/04/2016  12.58.26  by  Michael Scheer
*CMZ :  1.17/04 22/03/2016  15.36.44  by  Michael Scheer
*CMZ :  1.17/03 21/03/2016  09.58.20  by  Michael Scheer
*CMZ :  1.17/02 11/03/2016  16.48.04  by  Michael Scheer
*-- Author :    Michael Scheer   07/03/2016
      subroutine undumag_relax_rec_lin

      use bpolyederf90m
      use undumagf90m
      use commandlinef90m

!      bpebc(1:3,imag) contains position of magnet imag
!      bpebc(4:6,imag) contains normalized magnetization vector of magnet imag

      implicit none

      double precision bn,bx,by,bz,hx,hy,hz,bc(3),hpar,hper,parmag,permag,
     &  x,y,z,tr(3,3),ti(3,3),bcrot(3),hrot(3),hold,h

      integer ifail,imag,iter,i,j

      hold=0.0d0
      niterrec=0

      if (perksi.eq.0.0d0) then

        kinside=-1
        do imag=1,nrec
          ! bpolyeder returns H, if inside magnet!
          call undumag_field(
     &      bpebc(1,imag)/1000.0d0,bpebc(2,imag)/1000.0d0,bpebc(3,imag)/1000.0d0,
     &      bc0(4,imag),bc0(5,imag),bc0(6,imag),ifail)
          hpar=
     &      bc0(1,imag)*bc0(4,imag)+
     &      bc0(2,imag)*bc0(5,imag)+
     &      bc0(3,imag)*bc0(6,imag)
          parmag=1.0d0/(1.0d0-parksi*hpar)
          bc0(7:9,imag)=bc0(7:9,imag)*parmag
        enddo !nrec

        bpebc(4:6,1:nrec)=bc0(7:9,1:nrec)
        kconvrec=1

      else !perksi.eq.0

        do iter=1,maxiterrec

          bc0(4:6,1:nrec)=bpebc(4:6,1:nrec)

          h=0.0d0
          kinside=-1
          do imag=1,nrec

            x=bpebc(1,imag)/1000.0d0
            y=bpebc(2,imag)/1000.0d0
            z=bpebc(3,imag)/1000.0d0

            ! bpolyeder returns H, if inside magnet!
            call undumag_field(x,y,z,hx,hy,hz,ifail)

c            write(lun6,*)imag,bc0(4:6,imag)
            call undumag_rotate(bc0(4:6,imag),tr,ti) ! tr rotates vector to z-axis

            !rotate such, that easy axis becomes z-axis

c            bcrot(1:3)=tr(1:3,1)*bc0(4,imag)+
c     &        tr(1:3,2)*bc0(5,imag)+
c     &        tr(1:3,3)*bc0(6,imag)

            hrot(1:3)=tr(1:3,1)*hx+tr(1:3,2)*hy+tr(1:3,3)*hz

            bcrot(1:2)=hrot(1:2)*perksi
            bcrot(3)=bc0(10,imag)+hrot(3)*parksi !bc0(10,imag) is bc norm.

            !rotate back
            bc0(4:6,imag)=ti(1:3,1)*bcrot(1)+
     &        ti(1:3,2)*bcrot(2)+
     &        ti(1:3,3)*bcrot(3)

c            write(lun6,*)imag
c            write(lun6,*)ti

          enddo !nrec

          bpebc(4:6,1:nrec)=bc0(4:6,1:nrec)

          niterrec=iter
          kinside=0

          hold=h
          h=0.0d0
          do i=1,nxconv
            call undumag_field(xconv(i),yconv,zconv,hx,hy,hz,ifail)
            h=h+hx**2+hy**2+hz**2
          enddo
          h=sqrt(h/nxconv)

          if (hold.ne.0.0d0) then
            if (abs(h/hold-1.0d0).lt.hconva) then
              kconvrec=1
              goto 9999
            endif
          endif

        enddo !iter

      endif !perksi.eq.0

9999  return
      end
