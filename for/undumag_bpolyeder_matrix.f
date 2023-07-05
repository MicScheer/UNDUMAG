*CMZ :  2.02/01 11/02/2022  09.44.24  by  Michael Scheer
*CMZ :  2.02/00 21/10/2020  09.46.44  by  Michael Scheer
*CMZ :  2.00/03 24/04/2018  13.03.29  by  Michael Scheer
*CMZ :  1.25/00 08/03/2018  19.38.15  by  Michael Scheer
*CMZ :  1.23/02 18/09/2017  14.58.38  by  Michael Scheer
*CMZ :  1.15/11 20/04/2017  21.13.56  by  Michael Scheer
*CMZ :  1.15/10 13/04/2017  09.09.26  by  Michael Scheer
*CMZ :  1.15/07 05/04/2017  15.01.35  by  Michael Scheer
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
      subroutine undumag_bpolyeder_matrix(kimag,bxout,byout,bzout,ifail)

      use bpolyederf90m
      use undumagf90m
      use omp_lib
      use commandlinef90m

      implicit none

      double precision bxout,byout,bzout,xx,yy,zz,hi(3)
      double precision bci(3),bo(3),db(3)
      integer imag,ifail,kmag,idx,ical,kimag

      data ical/0/

      kmag=iabs(kimag)

      ifail=0

      bo=0.0d0
      bxout=0.0d0
      byout=0.0d0
      bzout=0.0d0


      do imag=1,nrec
        if (imag.eq.-kimag) cycle
        bci=bpebc(4:6,imag)
        if (bci(1).ne.bci(1).or.bci(2).ne.bci(2).or.bci(3).ne.bci(3)) then
          write(lun6,*)"*** Warning in undumag_bpolyeder_matrix: Magnetization is not a number (NaN) ***"
          write(lun6,*)"imag:",imag
          write(lun6,*)"Magnet is ignored!"
          cycle
        endif
        db=bci(1)*wwmatrix4(1:3,1,imag,kmag)
        bo=bo+db
        db=bci(2)*wwmatrix4(1:3,2,imag,kmag)
        bo=bo+db
        db=bci(3)*wwmatrix4(1:3,3,imag,kmag)
        bo=bo+db
      enddo !imag=1,nmag

      if (iterirontot.gt.0.or.kpreset.ne.0) then
        do imag=nrec+1,nmag
          if (imag.eq.-kimag) cycle
          bci=bpebc(4:6,imag)
          if (bci(1).ne.bci(1).or.bci(2).ne.bci(2).or.bci(3).ne.bci(3)) then
            write(lun6,*)"*** Warning in undumag_bpolyeder_matrix: Magnetization is not a number (NaN) ***"
            write(lun6,*)"imag:",imag
            write(lun6,*)"Magnet is ignored!"
            cycle
          endif
          db=bci(1)*wwmatrix4(1:3,1,imag,kmag)
          bo=bo+db
          db=bci(2)*wwmatrix4(1:3,2,imag,kmag)
          bo=bo+db
          db=bci(3)*wwmatrix4(1:3,3,imag,kmag)
          bo=bo+db
        enddo !imag=1,nmag
      endif

      bxout=bxout+bo(1)
      byout=byout+bo(2)
      bzout=bzout+bo(3)

      if (kbextern.ne.0) then
        bxout=bxout+bxex
        byout=byout+byex
        bzout=bzout+bzex
      endif

      if (ncwires.gt.0) then
        bxout=bxout+bpebc(18,kmag)
        byout=byout+bpebc(19,kmag)
        bzout=bzout+bpebc(20,kmag)
      endif

      return
      end
