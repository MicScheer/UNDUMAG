*CMZ :  4.01/00 13/03/2023  14.52.16  by  Michael Scheer
*CMZ :  3.05/00 25/04/2018  14.07.07  by  Michael Scheer
*CMZ :  3.01/02 18/09/2013  12.47.10  by  Michael Scheer
*CMZ :  2.62/00 16/04/2007  15.34.27  by  Michael Scheer
*CMZ :  2.52/05 16/08/2004  13.37.08  by  Michael Scheer
*CMZ :  2.47/23 01/03/2004  16.45.31  by  Michael Scheer
*CMZ :  0.00/07 16/01/2004  11.05.50  by  Michael Scheer
*CMZ :  0.00/06 07/01/2004  13.45.01  by  Michael Scheer
*CMZ :  0.00/03 15/12/2003  16.07.00  by  Michael Scheer
*CMZ :  0.00/01 08/12/2003  09.42.07  by  Michael Scheer
*-- Author :    Michael Scheer   02/12/2003
      module bwpolyederf90m

      integer lunbpe,nmag,iaxint,nstepint

      DOUBLE PRECISION BSCALEPM,WINPM,RANGPM

      DOUBLE PRECISION tiny,x0int,y0int,z0int,ranginti,ranginte
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: bpexpos
      DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: bpebc,bflange
      DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE :: bpemat
      DOUBLE PRECISION, DIMENSION(:,:,:,:), ALLOCATABLE :: bpemag,bpetm,bperot

      double precision bfcenxmm,bfcenymm,bfcenzmm,
     &  bflenxmm,bflenymm,bflenzmm,
     &  torqcenxmm,torqcenymm,torqcenzmm,
     &  shiftll,shiftlr,shiftul,shiftur,gappm

      real forcol,forxpl(2),forypl(2),forzpl(2)

      INTEGER, DIMENSION(:), ALLOCATABLE :: ibpeplan
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: ibpecorn

      integer iforcol
      integer lunpm,ibpnowarn

      character(128) filebpe,filepm

      NAMELIST/POLYMAGN/
     &  BSCALEPM,WINPM,RANGPM,LUNPM,FILEPM,
     &  shiftll,shiftlr,shiftul,shiftur,gappm,ibpnowarn

      end module
