*CMZ :  4.01/00 13/03/2023  14.03.06  by  Michael Scheer
*CMZ :  3.02/00 01/09/2014  12.55.26  by  Michael Scheer
*CMZ :  2.68/05 28/09/2012  09.09.02  by  Michael Scheer
*CMZ :  2.64/05 26/08/2009  11.38.26  by  Michael Scheer
*CMZ :  2.16/08 24/10/2000  12.13.06  by  Michael Scheer
*CMZ :  2.16/00 26/05/2000  14.57.11  by  Michael Scheer
*CMZ :  2.15/00 28/04/2000  10.33.28  by  Michael Scheer
*CMZ : 00.01/06 01/02/95  16.15.20  by  Michael Scheer
*CMZ : 00.01/04 28/11/94  15.24.13  by  Michael Scheer
*CMZ : 00.00/00 28/04/94  15.12.29  by  Michael Scheer
*-- Author : Michael Scheer
      MODULE OBSERVF90

      DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: OBSV, OBSVRPHI
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: OBSVZ,OBSVY,
     &  OBSVR,OBSVPHI,TBUFF
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: FPHIR,XC,YC

      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: DOBUFF,DOBUFF1,DOBUFF2
      double precision, DIMENSION(:), ALLOCATABLE ::             FILL
      INTEGER, DIMENSION(:), ALLOCATABLE ::          IOBUFF


      INTEGER IOBSV_A,IOBSVZ_A,IOBSVY_A,IOBSVRPHI_A,IOBSVR_A,IOBSVPHI_A

      END MODULE
