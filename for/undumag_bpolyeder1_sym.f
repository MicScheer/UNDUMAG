*CMZ :  2.01/03 13/06/2019  16.05.33  by  Michael Scheer
*-- Author :    Michael Scheer   02/12/2003
      subroutine undumag_bpolyeder1_sym(imag,x,y,z,bxout,byout,bzout,ifail)

*KEEP,bpolyederf90u.
      include 'bpolyederf90u.cmn'
*KEND.
      use undumagf90m

      implicit none

      double precision x,y,z,bxout,byout,bzout,xcut,hx,hy,hz
      integer imag,ifail,kfail,linside,ifailin

      ifailin=ifail
      ifail=0

      bxout=0.0d0
      byout=0.0d0
      bzout=0.0d0

      call undumag_bpolyeder1(imag,x,y,z,bxout,byout,bzout,ifail)

      if (ixsym.eq.0) then

        if (iysym.ne.0) then
          kfail=ifailin
          call undumag_bpolyeder1(imag,x,-y,z,hx,hy,hz,kfail)
          ifail=ifail+kfail
          if (kinside.gt.0) then
            linside=kinside
            kinside=0
          endif
          bxout=bxout-hx
          byout=byout+hy
          bzout=bzout-hz
        endif

        if (izsym.ne.0) then
          kfail=ifailin
          call undumag_bpolyeder1(imag,x,y,-z,hx,hy,hz,kfail)
          ifail=ifail+kfail
          if (kinside.gt.0) then
            linside=kinside
            kinside=0
          endif
          bxout=bxout+hx
          byout=byout+hy
          bzout=bzout-hz
        endif

        if (iysym.ne.0.and.izsym.ne.0) then
          kfail=ifailin
          call undumag_bpolyeder1(imag,x,-y,-z,hx,hy,hz,kfail)
          if (kinside.gt.0) then
            linside=kinside
            kinside=0
          endif
          ifail=ifail+kfail
          bxout=bxout-hx
          byout=byout+hy
          bzout=bzout+hz
        endif

      else !ixsym

        xcut=2.0d0*xsym-x

        !x2y1z1
        kfail=ifailin
        call undumag_bpolyeder1(imag,xcut,y,z,hx,hy,hz,kfail)
        ifail=ifail+kfail
        if (kinside.gt.0) then
          linside=kinside
          kinside=0
        endif
        bxout=bxout-hx
        byout=byout+hy
        bzout=bzout+hz

        if (iysym.ne.0) then
          !x1y2z1
          kfail=ifailin
          call undumag_bpolyeder1(imag,x,-y,z,hx,hy,hz,kfail)
          ifail=ifail+kfail
          if (kinside.gt.0) then
            linside=kinside
            kinside=0
          endif
          bxout=bxout-hx
          byout=byout+hy
          bzout=bzout-hz
          !x2y1z1
          kfail=ifailin
          call undumag_bpolyeder1(imag,xcut,-y,z,hx,hy,hz,kfail)
          ifail=ifail+kfail
          bxout=bxout+hx
          byout=byout+hy
          bzout=bzout-hz
        endif

        if (izsym.ne.0) then
          !x1y1z2
          kfail=ifailin
          call undumag_bpolyeder1(imag,x,y,-z,hx,hy,hz,kfail)
          if (kinside.gt.0) then
            linside=kinside
            kinside=0
          endif
          ifail=ifail+kfail
          bxout=bxout+hx
          byout=byout+hy
          bzout=bzout-hz
          !x2y1z2
          kfail=ifailin
          call undumag_bpolyeder1(imag,xcut,y,-z,hx,hy,hz,kfail)
          ifail=ifail+kfail
          if (kinside.gt.0) then
            linside=kinside
            kinside=0
          endif
          bxout=bxout-hx
          byout=byout+hy
          bzout=bzout-hz
        endif

        if (iysym.ne.0.and.izsym.ne.0) then
          !x2y2z2
          kfail=ifailin
          call undumag_bpolyeder1(imag,xcut,-y,-z,hx,hy,hz,kfail)
          ifail=ifail+kfail
          if (kinside.gt.0) then
            linside=kinside
            kinside=0
          endif
          bxout=bxout+hx
          byout=byout+hy
          bzout=bzout+hz
          !x1y2z2
          kfail=ifailin
          call undumag_bpolyeder1(imag,x,-y,-z,hx,hy,hz,kfail)
          ifail=ifail+kfail
          if (kinside.gt.0) then
            linside=kinside
            kinside=0
          endif
          bxout=bxout-hx
          byout=byout+hy
          bzout=bzout+hz
        endif

      endif !ixsym

      return
      end
