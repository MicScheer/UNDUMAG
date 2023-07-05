*CMZ :  4.01/00 13/03/2023  14.03.06  by  Michael Scheer
*CMZ :  4.00/11 11/06/2021  12.56.08  by  Michael Scheer
*CMZ :  3.07/01 21/03/2019  10.12.48  by  Michael Scheer
*CMZ :  2.64/00 14/08/2009  14.03.15  by  Michael Scheer
*CMZ :  2.37/01 14/11/2001  10.34.04  by  Michael Scheer
*CMZ :  2.31/01 25/04/2001  15.50.01  by  Michael Scheer
*CMZ :  2.20/07 16/03/2001  13.44.06  by  Michael Scheer
*CMZ :  2.20/01 24/11/2000  21.33.27  by  Michael Scheer
*CMZ :  2.16/08 25/10/2000  16.23.23  by  Michael Scheer
*CMZ :  2.15/00 28/04/2000  10.33.28  by  Michael Scheer
*CMZ :  2.12/03 05/08/99  17.04.20  by  Michael Scheer
*CMZ :  2.12/02 15/06/99  12.33.23  by  Michael Scheer
*CMZ :  2.12/00 27/05/99  10.10.08  by  Michael Scheer
*CMZ :  2.11/01 12/05/99  14.29.15  by  Michael Scheer
*CMZ :  2.11/00 11/05/99  12.43.20  by  Michael Scheer
*CMZ :  2.10/01 03/03/99  11.43.41  by  Michael Scheer
*CMZ :  2.02/00 05/02/99  16.15.47  by  Michael Scheer
*CMZ :  1.04/03 11/12/98  16.04.10  by  Michael Scheer
*CMZ :  1.04/02 11/12/98  11.34.54  by  Michael Scheer
*CMZ : 00.01/04 28/11/94  15.21.30  by  Michael Scheer
*CMZ : 00.00/00 28/04/94  15.12.46  by  Michael Scheer

*-- Author : Michael Scheer

      MODULE SOURCEF90

c +PATCH,//WAVE/MOD
c +DECK,SOURCEF90M,T=F77.

      DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: SOURCET,ECSOUR

      COMPLEX*16, DIMENSION(:,:), ALLOCATABLE :: DARGEXPO

      DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE ::
     &  SOURCEG,SOURCEA,SOURCEE,SOURCEN,schwingercen

      DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE :: SOURCEAO,SOURCEEO,WSOU

      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: ECMAX,SPCSMSUM,WTRA2IS
      REAL, DIMENSION(:), ALLOCATABLE :: SPCSMRAT

      INTEGER, DIMENSION(:), ALLOCATABLE :: IPOISOU,ISOURCEN,IZTOT
      INTEGER, DIMENSION(:), ALLOCATABLE :: IPOIROI
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: IWARNROI,ISOURAE

      END MODULE
