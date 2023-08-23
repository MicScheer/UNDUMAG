*CMZ :  1.22/02 22/08/2023  09.03.52  by  Michael Scheer
*CMZ :  1.17/08 24/05/2017  15.33.47  by  Michael Scheer
*CMZ :  1.17/07 23/05/2017  15.39.11  by  Michael Scheer
*CMZ :  1.17/02 08/03/2016  16.00.02  by  Michael Scheer
*CMZ :  1.15/01 24/04/2008  11.51.05  by  Michael Scheer
*CMZ :  1.15/00 26/10/2007  13.11.55  by  Michael Scheer
*CMZ :  1.12/16 01/06/2007  11.17.50  by  Michael Scheer
*CMZ :  1.12/02 14/07/2005  10.19.00  by  Michael Scheer
*CMZ :  1.10/03 19/08/2004  15.32.19  by  Michael Scheer
*CMZ :  1.10/02 19/08/2004  14.03.28  by  Michael Scheer
*CMZ :  1.10/01 17/08/2004  14.15.54  by  Michael Scheer
*CMZ :  2.00/00 17/08/2004  09.38.52  by  Michael Scheer
*CMZ :  1.02/02 11/08/2004  09.13.49  by  Michael Scheer
*CMZ :  1.02/01 09/08/2004  14.45.40  by  Michael Scheer
*CMZ :  1.02/00 28/07/2004  16.57.47  by  Michael Scheer
*-- Author :    Michael Scheer   27/07/2004
      subroutine undumag_field_int(imag,xint,yint,zint,
     &  vxint,vyint,vzint,
     &  bxint,byint,bzint,ifail)

      use undumagf90m

      implicit none

      double precision xint,yint,zint,
     &  vxint,vyint,vzint,
     &  bxint,byint,bzint,bxi,byi,bzi

      integer imag,ifail,kfail

      bxint=0.0d0
      byint=0.0d0
      bzint=0.0d0

      kfail=0

      call  undumag_bpolyint(imag,xint,yint,zint,
     &  vxint,vyint,vzint,
     &  bxi,byi,bzi,ifail)
      if (ifail.ne.0) kfail=ifail

      bxint=bxint+bxi
      byint=byint+byi
      bzint=bzint+bzi

      if (iysym.ne.0) then
        call  undumag_bpolyint(imag,xint,-yint,zint,
     &    vxint,vyint,vzint,
     &    bxi,byi,bzi,ifail)
        if (ifail.ne.0) kfail=ifail
        bxint=bxint-bxi
        byint=byint+byi
        bzint=bzint-bzi
      endif

      if (izsym.ne.0) then
        call  undumag_bpolyint(imag,xint,yint,-zint,
     &    vxint,vyint,vzint,
     &    bxi,byi,bzi,ifail)
        if (ifail.ne.0) kfail=ifail
        bxint=bxint+bxi
        byint=byint+byi
        bzint=bzint-bzi
      endif

      if (iysym.ne.0.and.izsym.ne.0) then
        call  undumag_bpolyint(imag,xint,-yint,-zint,
     &    vxint,vyint,vzint,
     &    bxi,byi,bzi,ifail)
        if (ifail.ne.0) kfail=ifail
        bxint=bxint-bxi
        byint=byint+byi
        bzint=bzint+bzi
      endif

      if (ixsym.ne.0) then
        bxint=0.0d0
        byint=2.0d0*byint
        bzint=2.0d0*bzint
      endif !ixsym

      return
      end
