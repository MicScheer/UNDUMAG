*CMZ :  1.25/00 22/08/2023  09.03.52  by  Michael Scheer
*CMZ :  1.23/02 18/09/2017  15.02.37  by  Michael Scheer
*CMZ :  1.14/00 21/03/2017  16.59.02  by  Michael Scheer
*CMZ :  1.11/03 16/01/2017  12.22.22  by  Michael Scheer
*CMZ :  1.10/02 28/11/2016  13.27.01  by  Michael Scheer
*CMZ :  1.04/02 15/09/2016  11.48.57  by  Michael Scheer
*CMZ :  1.04/01 14/09/2016  15.01.00  by  Michael Scheer
*CMZ :  0.00/04 13/05/2016  14.48.27  by  Michael Scheer
*CMZ :  0.00/03 04/05/2016  08.41.15  by  Michael Scheer
*CMZ :  0.00/02 02/05/2016  10.21.48  by  Michael Scheer
*CMZ :  1.17/10 04/04/2016  14.20.09  by  Michael Scheer
*CMZ :  1.17/08 04/04/2016  08.57.43  by  Michael Scheer
*CMZ :  1.17/07 03/04/2016  10.22.45  by  Michael Scheer
*CMZ :  1.17/06 31/03/2016  09.26.44  by  Michael Scheer
*CMZ :  1.17/05 27/03/2016  12.16.01  by  Michael Scheer
*CMZ :  1.17/03 21/03/2016  18.38.48  by  Michael Scheer
*-- Author :    Michael Scheer   02/12/2003
      subroutine undumag_bconv(h)

      use bpolyederf90m
      use undumagf90m

      implicit none

      double precision h,hx,hy,hz
      integer mag,ix

      h=0.0d0

      do ix=1,nxconv
        hx=0.0d0
        hy=0.0d0
        hz=0.0d0
        do mag=1,nmag
          hx=hx+convmat(1,1,ix,mag)*bpebc(4,mag)
          hy=hy+convmat(2,1,ix,mag)*bpebc(5,mag)
          hz=hz+convmat(3,1,ix,mag)*bpebc(6,mag)
          hx=hx+convmat(1,2,ix,mag)*bpebc(4,mag)
          hy=hy+convmat(2,2,ix,mag)*bpebc(5,mag)
          hz=hz+convmat(3,2,ix,mag)*bpebc(6,mag)
          hx=hx+convmat(1,3,ix,mag)*bpebc(4,mag)
          hy=hy+convmat(2,3,ix,mag)*bpebc(5,mag)
          hz=hz+convmat(3,3,ix,mag)*bpebc(6,mag)
        enddo
        if (kbextern.ne.0) then
          hx=hx+bxex
          hy=hy+byex
          hz=hz+bzex
        endif
        if (ncwires.gt.0) then
          hx=hx+bxconvw(ix)
          hy=hy+byconvw(ix)
          hz=hz+bzconvw(ix)
        endif
        h=h+hx**2+hy**2+hz**2
      enddo

      h=sqrt(h/nxconv)

      return
      end
