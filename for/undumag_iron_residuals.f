*CMZ :  2.03/00 22/08/2023  09.03.52  by  Michael Scheer
*CMZ :  2.02/00 21/10/2020  09.46.44  by  Michael Scheer
*CMZ :  2.00/01 12/04/2018  12.32.47  by  Michael Scheer
*CMZ :  1.25/00 30/01/2018  15.11.36  by  Michael Scheer
*CMZ :  1.23/03 25/09/2017  17.24.52  by  Michael Scheer
*-- Author :    Michael Scheer   21/09/2017
      subroutine undumag_residuals_iron

      use bpolyederf90m
      use undumagf90m

      use commandlinef90m

      implicit none

*KEEP,seqdebug.
      include 'seqdebug.cmn'
*KEND.

      double precision bn,h3(3),h3n,femag
      integer ifail,mat,mapmode,nhz,imag,iwarn

      data iwarn/0/

      save iwarn

      hresidiron=0.0d0
      if (niron.eq.0) return

      do imag=nrec+1,nrec+niron

        bn=sqrt(bpebc(4,imag)**2+bpebc(5,imag)**2+bpebc(6,imag)**2)

        mat=nint(bpebc(9,imag))
        mapmode=matmaps(3,mat)

        nhz=matmaps(4,mat)

        call undumag_bpolyeder_matrix(imag,h3(1),h3(2),h3(3),ifail)

        if (ifail.ne.0) then
          write(lun6,*)
          write(lun6,*)"*** Warning in undumag_residuals_iron: Bad return from undumag_bpolyeder_matrix during calculations of residiuals ***"
          write(lun6,*)
        endif

        h3n=sqrt(h3(1)**2+h3(2)**2+h3(3)**2)

        if (mapmode.gt.0) then

          call util_interpol_linear(nhz,feh1,fem1,h3n,femag,ifail)
          if (ifail.ne.0) then
            write(lun6,*)
            write(lun6,*)"*** Warning in undumag_residuals_iron: Bad return from util_interpol_linear during calculations of residuals ***"
            write(lun6,*)
          endif
        else
          cycle
c          femag=bcmat(2,1,mat)
        endif

        hresidiron=hresidiron+(femag-bn)**2

      enddo

      hresidiron=sqrt(hresidiron/niron)

      return
      end
