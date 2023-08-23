*CMZ :  2.03/00 22/08/2023  09.03.52  by  Michael Scheer
*CMZ :  2.02/00 30/01/2021  17.58.42  by  Michael Scheer
*CMZ :  1.25/03 03/04/2018  08.28.50  by  Michael Scheer
*CMZ :  1.23/02 16/09/2017  10.37.45  by  Michael Scheer
*-- Author :    Michael Scheer   16/09/2017
      subroutine undumag_bintinf_sym(xint,yint,zint,vxint,vyint,vzint,
     &  bxint,byint,bzint,ifail)

      use undumagf90m

      implicit none

      double precision xint,yint,zint,
     &  vxint,vyint,vzint,
     &  bxint,byint,bzint,bxi,byi,bzi

      integer ifail,kfail

      bxint=0.0d0
      byint=0.0d0
      bzint=0.0d0

      kfail=0

      call  undumag_bintinf(xint,yint,zint,
     &  vxint,vyint,vzint,
     &  bxi,byi,bzi,ifail)

      if (ifail.ne.0) kfail=kfail+ifail

      bxint=bxint+bxi
      byint=byint+byi
      bzint=bzint+bzi

      if (iysym.ne.0) then
        call  undumag_bintinf(xint,-yint,zint,
     &    vxint,vyint,vzint,
     &    bxi,byi,bzi,ifail)
        if (ifail.ne.0) kfail=kfail+ifail
        bxint=bxint-bxi
        byint=byint+byi
        bzint=bzint-bzi
      endif

      if (izsym.ne.0) then
        call  undumag_bintinf(xint,yint,-zint,
     &    vxint,vyint,vzint,
     &    bxi,byi,bzi,ifail)
        if (ifail.ne.0) kfail=kfail+ifail
        bxint=bxint+bxi
        byint=byint+byi
        bzint=bzint-bzi
      endif

      if (iysym.ne.0.and.izsym.ne.0) then
        call  undumag_bintinf(xint,-yint,-zint,
     &    vxint,vyint,vzint,
     &    bxi,byi,bzi,ifail)
        if (ifail.ne.0) kfail=kfail+ifail
        bxint=bxint-bxi
        byint=byint+byi
        bzint=bzint+bzi
      endif

      if (ixsym.ne.0) then
        bxint=0.0d0
        byint=2.0d0*byint
        bzint=2.0d0*bzint
      endif !ixsym

      call undumag_bcoilsinf(yint,zint,byi,bzi,ifail)
      if (ifail.ne.0) kfail=kfail+ifail

      byint=byint+byi
      bzint=bzint+bzi

      ifail=kfail

      return
      end
