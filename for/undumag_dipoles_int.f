*CMZ :  1.23/01 09/08/2017  08.36.02  by  Michael Scheer
*CMZ :  1.22/02 04/08/2017  09.22.23  by  Michael Scheer
*CMZ :  1.22/01 20/07/2017  14.55.33  by  Michael Scheer
*CMZ :  1.22/00 05/07/2017  10.09.09  by  Michael Scheer
*CMZ :  1.20/03 29/06/2017  15.38.45  by  Michael Scheer
*CMZ :  1.20/01 22/06/2017  13.35.35  by  Michael Scheer
*CMZ :  1.20/00 21/06/2017  16.39.29  by  Michael Scheer
*CMZ :  1.19/00 20/06/2017  12.15.48  by  Michael Scheer
*CMZ :  1.18/03 14/06/2017  09.22.03  by  Michael Scheer
*CMZ :  1.18/01 07/06/2017  16.12.25  by  Michael Scheer
*CMZ :  1.18/00 02/06/2017  09.38.06  by  Michael Scheer
*CMZ :  1.17/08 30/05/2017  15.20.49  by  Michael Scheer
*CMZ :  1.17/07 24/05/2017  08.36.20  by  Michael Scheer
*CMZ :  1.17/06 18/05/2017  22.30.31  by  Michael Scheer
*CMZ :  1.15/12 04/05/2017  15.55.38  by  Michael Scheer
*CMZ :  1.15/11 24/04/2017  17.57.38  by  Michael Scheer
*CMZ :  1.15/10 19/04/2017  12.15.17  by  Michael Scheer
*CMZ :  1.15/09 07/04/2017  14.51.07  by  Michael Scheer
*CMZ :  1.15/08 06/04/2017  09.01.37  by  Michael Scheer
*CMZ :  1.15/07 04/04/2017  13.49.11  by  Michael Scheer
*CMZ :  1.15/05 03/04/2017  13.40.53  by  Michael Scheer
*CMZ :  1.15/04 03/04/2017  12.57.38  by  Michael Scheer
*CMZ :  1.15/03 02/04/2017  15.45.35  by  Michael Scheer
*CMZ :  1.15/02 02/04/2017  09.38.56  by  Michael Scheer
*CMZ :  1.15/01 28/03/2017  15.25.06  by  Michael Scheer
*CMZ :  1.15/00 27/03/2017  15.13.40  by  Michael Scheer
*CMZ :  1.14/00 21/03/2017  14.39.06  by  Michael Scheer
*CMZ :  1.13/03 10/03/2017  12.16.46  by  Michael Scheer
*CMZ :  1.13/01 08/03/2017  16.26.29  by  Michael Scheer
*CMZ :  1.11/05 22/02/2017  12.53.52  by  Michael Scheer
*CMZ :  1.11/04 23/01/2017  17.22.15  by  Michael Scheer
*CMZ :  1.11/03 17/01/2017  14.50.28  by  Michael Scheer
*CMZ :  1.11/00 07/12/2016  12.48.13  by  Michael Scheer
*CMZ :  1.10/02 24/11/2016  10.22.30  by  Michael Scheer
*CMZ :  1.10/01 18/11/2016  15.52.38  by  Michael Scheer
*CMZ :  1.10/00 18/11/2016  09.18.26  by  Michael Scheer
*CMZ :  1.09/01 06/10/2016  14.12.02  by  Michael Scheer
*CMZ :  1.07/03 27/09/2016  13.45.21  by  Michael Scheer
*CMZ :  1.07/02 25/09/2016  13.30.02  by  Michael Scheer
*CMZ :  1.07/01 25/09/2016  11.49.21  by  Michael Scheer
*CMZ :  1.07/00 24/09/2016  14.57.45  by  Michael Scheer
*CMZ :  1.06/01 21/09/2016  15.31.52  by  Michael Scheer
*CMZ :  1.06/00 20/09/2016  14.13.49  by  Michael Scheer
*CMZ :  1.02/01 09/09/2016  15.30.08  by  Michael Scheer
*CMZ :  1.02/00 29/08/2016  12.45.07  by  Michael Scheer
*CMZ :  1.01/00 21/08/2016  12.03.06  by  Michael Scheer
*CMZ :  1.00/00 19/08/2016  18.37.39  by  Michael Scheer
*CMZ :  0.00/13 16/08/2016  11.57.51  by  Michael Scheer
*CMZ :  0.00/11 18/07/2016  09.18.11  by  Michael Scheer
*CMZ :  0.00/10 13/07/2016  14.57.35  by  Michael Scheer
*CMZ :  0.00/09 28/06/2016  15.42.08  by  Michael Scheer
*CMZ :  0.00/06 16/06/2016  14.14.37  by  Michael Scheer
*CMZ :  0.00/05 13/06/2016  12.45.02  by  Michael Scheer
*CMZ :  0.00/02 30/04/2016  13.12.13  by  Michael Scheer
*CMZ :  0.00/01 26/04/2016  14.02.34  by  Michael Scheer
*CMZ :  1.17/11 05/04/2016  15.51.06  by  Michael Scheer
*CMZ :  1.17/08 04/04/2016  08.57.43  by  Michael Scheer
*CMZ :  1.17/07 03/04/2016  19.38.32  by  Michael Scheer
*CMZ :  1.17/06 01/04/2016  12.58.26  by  Michael Scheer
*CMZ :  1.17/05 27/03/2016  12.40.54  by  Michael Scheer
*CMZ :  1.17/04 24/03/2016  15.11.21  by  Michael Scheer
*CMZ :  1.17/03 22/03/2016  07.59.20  by  Michael Scheer
*CMZ :  1.17/02 07/03/2016  16.32.54  by  Michael Scheer
*-- Author :    Michael Scheer   07/03/2016
      subroutine undumag_dipoles_int(imag,y,z,byis,bzis,ifail)

      use bpolyederf90m
      use undumagf90m

      implicit none

*KEEP,seqdebug.
      include 'seqdebug.cmn'
*KEND.

      double precision  y,z,byis,bzis
      double precision  dip2y,dip3z,y2z2,py,pz,pryz,byi,bzi

      integer ifail,kmag,imag

      byis=0.0d0
      bzis=0.0d0
      byi=0.0d0
      bzi=0.0d0

      ifail=0
      if (bpebc(17,imag).lt.0.0d0) return
      ifail=1

      dip2y=dipoles(2,imag)-y*1000.0d0
      dip3z=dipoles(3,imag)-z*1000.0d0

      y2z2=dip2y**2+dip3z**2
      py=dipoles(6,imag)*dipoles(4,imag)
      pz=dipoles(7,imag)*dipoles(4,imag)
      pryz=py*dip2y+pz*dip3z

      if (y2z2.ne.0.0d0) then
        byi=3.0d0*pryz*dip2y*2.0d0/3.0d0/y2z2**2*2.0d0
     &    -2.0d0/y2z2*py
        bzi=3.0d0*pryz*dip3z*2.0d0/3.0d0/y2z2**2*2.0d0
     &    -2.0d0/y2z2*pz
        ifail=0
        byis=byis+byi
        bzis=bzis+bzi
      else
        ifail=1
        goto 999
      endif

      if (nmag.ne.ndipoles) then

        kmag=nmag+(imag-1)*(ndipoles/nmag-1)

        if (ixsym.ne.0.and.iysym.ne.0.and.izsym.ne.0) then
          kmag=kmag+1 !xsym
          dip2y=dipoles(2,kmag)-y*1000.0d0
          dip3z=dipoles(3,kmag)-z*1000.0d0
          y2z2=dip2y**2+dip3z**2
          py=dipoles(6,kmag)*dipoles(4,kmag)
          pz=dipoles(7,kmag)*dipoles(4,kmag)
          pryz=py*dip2y+pz*dip3z
          if (y2z2.ne.0.0d0) then
            byi=3.0d0*pryz*dip2y*2.0d0/3.0d0/y2z2**2*2.0d0
     &        -2.0d0/y2z2*py
            bzi=3.0d0*pryz*dip3z*2.0d0/3.0d0/y2z2**2*2.0d0
     &        -2.0d0/y2z2*pz
            ifail=0
            byis=byis+byi
            bzis=bzis+bzi
          else
            ifail=1
            goto 999
          endif
          kmag=kmag+1 !ysym
          dip2y=dipoles(2,kmag)-y*1000.0d0
          dip3z=dipoles(3,kmag)-z*1000.0d0
          y2z2=dip2y**2+dip3z**2
          py=dipoles(6,kmag)*dipoles(4,kmag)
          pz=dipoles(7,kmag)*dipoles(4,kmag)
          pryz=py*dip2y+pz*dip3z
          if (y2z2.ne.0.0d0) then
            byi=3.0d0*pryz*dip2y*2.0d0/3.0d0/y2z2**2*2.0d0
     &        -2.0d0/y2z2*py
            bzi=3.0d0*pryz*dip3z*2.0d0/3.0d0/y2z2**2*2.0d0
     &        -2.0d0/y2z2*pz
            ifail=0
            byis=byis+byi
            bzis=bzis+bzi
          else
            ifail=1
            goto 999
          endif
          kmag=kmag+1 !zsym
          dip2y=dipoles(2,kmag)-y*1000.0d0
          dip3z=dipoles(3,kmag)-z*1000.0d0
          y2z2=dip2y**2+dip3z**2
          py=dipoles(6,kmag)*dipoles(4,kmag)
          pz=dipoles(7,kmag)*dipoles(4,kmag)
          pryz=py*dip2y+pz*dip3z
          if (y2z2.ne.0.0d0) then
            byi=3.0d0*pryz*dip2y*2.0d0/3.0d0/y2z2**2*2.0d0
     &        -2.0d0/y2z2*py
            bzi=3.0d0*pryz*dip3z*2.0d0/3.0d0/y2z2**2*2.0d0
     &        -2.0d0/y2z2*pz
            ifail=0
            byis=byis+byi
            bzis=bzis+bzi
          else
            ifail=1
            goto 999
          endif
          kmag=kmag+1 !ix + iy
          dip2y=dipoles(2,kmag)-y*1000.0d0
          dip3z=dipoles(3,kmag)-z*1000.0d0
          y2z2=dip2y**2+dip3z**2
          py=dipoles(6,kmag)*dipoles(4,kmag)
          pz=dipoles(7,kmag)*dipoles(4,kmag)
          pryz=py*dip2y+pz*dip3z
          if (y2z2.ne.0.0d0) then
            byi=3.0d0*pryz*dip2y*2.0d0/3.0d0/y2z2**2*2.0d0
     &        -2.0d0/y2z2*py
            bzi=3.0d0*pryz*dip3z*2.0d0/3.0d0/y2z2**2*2.0d0
     &        -2.0d0/y2z2*pz
            ifail=0
            byis=byis+byi
            bzis=bzis+bzi
          else
            ifail=1
            goto 999
          endif
          kmag=kmag+1 !ix + iz
          dip2y=dipoles(2,kmag)-y*1000.0d0
          dip3z=dipoles(3,kmag)-z*1000.0d0
          y2z2=dip2y**2+dip3z**2
          py=dipoles(6,kmag)*dipoles(4,kmag)
          pz=dipoles(7,kmag)*dipoles(4,kmag)
          pryz=py*dip2y+pz*dip3z
          if (y2z2.ne.0.0d0) then
            byi=3.0d0*pryz*dip2y*2.0d0/3.0d0/y2z2**2*2.0d0
     &        -2.0d0/y2z2*py
            bzi=3.0d0*pryz*dip3z*2.0d0/3.0d0/y2z2**2*2.0d0
     &        -2.0d0/y2z2*pz
            ifail=0
            byis=byis+byi
            bzis=bzis+bzi
          else
            ifail=1
            goto 999
          endif
          kmag=kmag+1 !iy + iz
          dip2y=dipoles(2,kmag)-y*1000.0d0
          dip3z=dipoles(3,kmag)-z*1000.0d0
          y2z2=dip2y**2+dip3z**2
          py=dipoles(6,kmag)*dipoles(4,kmag)
          pz=dipoles(7,kmag)*dipoles(4,kmag)
          pryz=py*dip2y+pz*dip3z
          if (y2z2.ne.0.0d0) then
            byi=3.0d0*pryz*dip2y*2.0d0/3.0d0/y2z2**2*2.0d0
     &        -2.0d0/y2z2*py
            bzi=3.0d0*pryz*dip3z*2.0d0/3.0d0/y2z2**2*2.0d0
     &        -2.0d0/y2z2*pz
            ifail=0
            byis=byis+byi
            bzis=bzis+bzi
          else
            ifail=1
            goto 999
          endif
          kmag=kmag+1 !iy + iy + iz
          dip2y=dipoles(2,kmag)-y*1000.0d0
          dip3z=dipoles(3,kmag)-z*1000.0d0
          y2z2=dip2y**2+dip3z**2
          py=dipoles(6,kmag)*dipoles(4,kmag)
          pz=dipoles(7,kmag)*dipoles(4,kmag)
          pryz=py*dip2y+pz*dip3z
          if (y2z2.ne.0.0d0) then
            byi=3.0d0*pryz*dip2y*2.0d0/3.0d0/y2z2**2*2.0d0
     &        -2.0d0/y2z2*py
            bzi=3.0d0*pryz*dip3z*2.0d0/3.0d0/y2z2**2*2.0d0
     &        -2.0d0/y2z2*pz
            ifail=0
            byis=byis+byi
            bzis=bzis+bzi
          else
            ifail=1
            goto 999
          endif
        else if (ixsym.ne.0.and.iysym.ne.0) then
          kmag=kmag+1 !xsym
          dip2y=dipoles(2,kmag)-y*1000.0d0
          dip3z=dipoles(3,kmag)-z*1000.0d0
          y2z2=dip2y**2+dip3z**2
          py=dipoles(6,kmag)*dipoles(4,kmag)
          pz=dipoles(7,kmag)*dipoles(4,kmag)
          pryz=py*dip2y+pz*dip3z
          if (y2z2.ne.0.0d0) then
            byi=3.0d0*pryz*dip2y*2.0d0/3.0d0/y2z2**2*2.0d0
     &        -2.0d0/y2z2*py
            bzi=3.0d0*pryz*dip3z*2.0d0/3.0d0/y2z2**2*2.0d0
     &        -2.0d0/y2z2*pz
            ifail=0
            byis=byis+byi
            bzis=bzis+bzi
          else
            ifail=1
            goto 999
          endif
          kmag=kmag+1 !ysym
          dip2y=dipoles(2,kmag)-y*1000.0d0
          dip3z=dipoles(3,kmag)-z*1000.0d0
          y2z2=dip2y**2+dip3z**2
          py=dipoles(6,kmag)*dipoles(4,kmag)
          pz=dipoles(7,kmag)*dipoles(4,kmag)
          pryz=py*dip2y+pz*dip3z
          if (y2z2.ne.0.0d0) then
            byi=3.0d0*pryz*dip2y*2.0d0/3.0d0/y2z2**2*2.0d0
     &        -2.0d0/y2z2*py
            bzi=3.0d0*pryz*dip3z*2.0d0/3.0d0/y2z2**2*2.0d0
     &        -2.0d0/y2z2*pz
            ifail=0
            byis=byis+byi
            bzis=bzis+bzi
          else
            ifail=1
            goto 999
          endif
          kmag=kmag+1 !ix + iy
          dip2y=dipoles(2,kmag)-y*1000.0d0
          dip3z=dipoles(3,kmag)-z*1000.0d0
          y2z2=dip2y**2+dip3z**2
          py=dipoles(6,kmag)*dipoles(4,kmag)
          pz=dipoles(7,kmag)*dipoles(4,kmag)
          pryz=py*dip2y+pz*dip3z
          if (y2z2.ne.0.0d0) then
            byi=3.0d0*pryz*dip2y*2.0d0/3.0d0/y2z2**2*2.0d0
     &        -2.0d0/y2z2*py
            bzi=3.0d0*pryz*dip3z*2.0d0/3.0d0/y2z2**2*2.0d0
     &        -2.0d0/y2z2*pz
            ifail=0
            byis=byis+byi
            bzis=bzis+bzi
          else
            ifail=1
            goto 999
          endif
        else if (ixsym.ne.0.and.izsym.ne.0) then
          kmag=kmag+1 !xsym
          dip2y=dipoles(2,kmag)-y*1000.0d0
          dip3z=dipoles(3,kmag)-z*1000.0d0
          y2z2=dip2y**2+dip3z**2
          py=dipoles(6,kmag)*dipoles(4,kmag)
          pz=dipoles(7,kmag)*dipoles(4,kmag)
          pryz=py*dip2y+pz*dip3z
          if (y2z2.ne.0.0d0) then
            byi=3.0d0*pryz*dip2y*2.0d0/3.0d0/y2z2**2*2.0d0
     &        -2.0d0/y2z2*py
            bzi=3.0d0*pryz*dip3z*2.0d0/3.0d0/y2z2**2*2.0d0
     &        -2.0d0/y2z2*pz
            ifail=0
            byis=byis+byi
            bzis=bzis+bzi
          else
            ifail=1
            goto 999
          endif
          kmag=kmag+1 !zsym
          dip2y=dipoles(2,kmag)-y*1000.0d0
          dip3z=dipoles(3,kmag)-z*1000.0d0
          y2z2=dip2y**2+dip3z**2
          py=dipoles(6,kmag)*dipoles(4,kmag)
          pz=dipoles(7,kmag)*dipoles(4,kmag)
          pryz=py*dip2y+pz*dip3z
          if (y2z2.ne.0.0d0) then
            byi=3.0d0*pryz*dip2y*2.0d0/3.0d0/y2z2**2*2.0d0
     &        -2.0d0/y2z2*py
            bzi=3.0d0*pryz*dip3z*2.0d0/3.0d0/y2z2**2*2.0d0
     &        -2.0d0/y2z2*pz
            ifail=0
            byis=byis+byi
            bzis=bzis+bzi
          else
            ifail=1
            goto 999
          endif
          kmag=kmag+1 !ix + iz
          dip2y=dipoles(2,kmag)-y*1000.0d0
          dip3z=dipoles(3,kmag)-z*1000.0d0
          y2z2=dip2y**2+dip3z**2
          py=dipoles(6,kmag)*dipoles(4,kmag)
          pz=dipoles(7,kmag)*dipoles(4,kmag)
          pryz=py*dip2y+pz*dip3z
          if (y2z2.ne.0.0d0) then
            byi=3.0d0*pryz*dip2y*2.0d0/3.0d0/y2z2**2*2.0d0
     &        -2.0d0/y2z2*py
            bzi=3.0d0*pryz*dip3z*2.0d0/3.0d0/y2z2**2*2.0d0
     &        -2.0d0/y2z2*pz
            ifail=0
            byis=byis+byi
            bzis=bzis+bzi
          else
            ifail=1
            goto 999
          endif
        else if (iysym.ne.0.and.izsym.ne.0) then
          kmag=kmag+1 !ysym
          dip2y=dipoles(2,kmag)-y*1000.0d0
          dip3z=dipoles(3,kmag)-z*1000.0d0
          y2z2=dip2y**2+dip3z**2
          py=dipoles(6,kmag)*dipoles(4,kmag)
          pz=dipoles(7,kmag)*dipoles(4,kmag)
          pryz=py*dip2y+pz*dip3z
          if (y2z2.ne.0.0d0) then
            byi=3.0d0*pryz*dip2y*2.0d0/3.0d0/y2z2**2*2.0d0
     &        -2.0d0/y2z2*py
            bzi=3.0d0*pryz*dip3z*2.0d0/3.0d0/y2z2**2*2.0d0
     &        -2.0d0/y2z2*pz
            ifail=0
            byis=byis+byi
            bzis=bzis+bzi
          else
            ifail=1
            goto 999
          endif
          kmag=kmag+1 !zsym
          dip2y=dipoles(2,kmag)-y*1000.0d0
          dip3z=dipoles(3,kmag)-z*1000.0d0
          y2z2=dip2y**2+dip3z**2
          py=dipoles(6,kmag)*dipoles(4,kmag)
          pz=dipoles(7,kmag)*dipoles(4,kmag)
          pryz=py*dip2y+pz*dip3z
          if (y2z2.ne.0.0d0) then
            byi=3.0d0*pryz*dip2y*2.0d0/3.0d0/y2z2**2*2.0d0
     &        -2.0d0/y2z2*py
            bzi=3.0d0*pryz*dip3z*2.0d0/3.0d0/y2z2**2*2.0d0
     &        -2.0d0/y2z2*pz
            ifail=0
            byis=byis+byi
            bzis=bzis+bzi
          else
            ifail=1
            goto 999
          endif
          kmag=kmag+1 !iy + iz
          dip2y=dipoles(2,kmag)-y*1000.0d0
          dip3z=dipoles(3,kmag)-z*1000.0d0
          y2z2=dip2y**2+dip3z**2
          py=dipoles(6,kmag)*dipoles(4,kmag)
          pz=dipoles(7,kmag)*dipoles(4,kmag)
          pryz=py*dip2y+pz*dip3z
          if (y2z2.ne.0.0d0) then
            byi=3.0d0*pryz*dip2y*2.0d0/3.0d0/y2z2**2*2.0d0
     &        -2.0d0/y2z2*py
            bzi=3.0d0*pryz*dip3z*2.0d0/3.0d0/y2z2**2*2.0d0
     &        -2.0d0/y2z2*pz
            ifail=0
            byis=byis+byi
            bzis=bzis+bzi
          else
            ifail=1
            goto 999
          endif
        else if (ixsym.ne.0) then
          kmag=kmag+1 !xsym
          dip2y=dipoles(2,kmag)-y*1000.0d0
          dip3z=dipoles(3,kmag)-z*1000.0d0
          y2z2=dip2y**2+dip3z**2
          py=dipoles(6,kmag)*dipoles(4,kmag)
          pz=dipoles(7,kmag)*dipoles(4,kmag)
          pryz=py*dip2y+pz*dip3z
          if (y2z2.ne.0.0d0) then
            byi=3.0d0*pryz*dip2y*2.0d0/3.0d0/y2z2**2*2.0d0
     &        -2.0d0/y2z2*py
            bzi=3.0d0*pryz*dip3z*2.0d0/3.0d0/y2z2**2*2.0d0
     &        -2.0d0/y2z2*pz
            ifail=0
            byis=byis+byi
            bzis=bzis+bzi
          else
            ifail=1
            goto 999
          endif
        else if (iysym.ne.0) then
          kmag=kmag+1 !ysym
          dip2y=dipoles(2,kmag)-y*1000.0d0
          dip3z=dipoles(3,kmag)-z*1000.0d0
          y2z2=dip2y**2+dip3z**2
          py=dipoles(6,kmag)*dipoles(4,kmag)
          pz=dipoles(7,kmag)*dipoles(4,kmag)
          pryz=py*dip2y+pz*dip3z
          if (y2z2.ne.0.0d0) then
            byi=3.0d0*pryz*dip2y*2.0d0/3.0d0/y2z2**2*2.0d0
     &        -2.0d0/y2z2*py
            bzi=3.0d0*pryz*dip3z*2.0d0/3.0d0/y2z2**2*2.0d0
     &        -2.0d0/y2z2*pz
            ifail=0
            byis=byis+byi
            bzis=bzis+bzi
          else
            ifail=1
            goto 999
          endif
        else if (izsym.ne.0) then
          kmag=kmag+1 !zsym
          dip2y=dipoles(2,kmag)-y*1000.0d0
          dip3z=dipoles(3,kmag)-z*1000.0d0
          y2z2=dip2y**2+dip3z**2
          py=dipoles(6,kmag)*dipoles(4,kmag)
          pz=dipoles(7,kmag)*dipoles(4,kmag)
          pryz=py*dip2y+pz*dip3z
          if (y2z2.ne.0.0d0) then
            byi=3.0d0*pryz*dip2y*2.0d0/3.0d0/y2z2**2*2.0d0
     &        -2.0d0/y2z2*py
            bzi=3.0d0*pryz*dip3z*2.0d0/3.0d0/y2z2**2*2.0d0
     &        -2.0d0/y2z2*pz
            ifail=0
            byis=byis+byi
            bzis=bzis+bzi
          else
            ifail=1
            goto 999
          endif
        endif
      endif !(nmag.ne.ndipoles) then

999   byis=byis/1000.0d0
      bzis=bzis/1000.0d0

      return
      end
