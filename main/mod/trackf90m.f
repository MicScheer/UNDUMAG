*CMZ :  4.01/00 13/03/2023  14.33.24  by  Michael Scheer
*CMZ :  3.07/01 21/03/2019  08.57.44  by  Michael Scheer
*CMZ :  3.03/02 12/02/2016  17.48.08  by  Michael Scheer
*CMZ :  3.02/00 01/09/2014  12.55.26  by  Michael Scheer
*CMZ :  3.01/00 06/05/2013  13.19.15  by  Michael Scheer
*CMZ :  3.00/01 28/03/2013  15.54.35  by  Michael Scheer
*CMZ :  2.70/05 02/01/2013  12.16.47  by  Michael Scheer
*CMZ :  2.69/00 25/10/2012  15.10.20  by  Michael Scheer
*CMZ :  2.63/05 12/08/2009  08.57.47  by  Michael Scheer
*CMZ :  2.50/00 16/04/2004  08.42.45  by  Michael Scheer
*CMZ :  2.48/04 12/03/2004  14.29.11  by  Michael Scheer
*CMZ :  2.37/02 14/11/2001  11.17.27  by  Michael Scheer
*CMZ :  2.31/01 25/04/2001  14.51.19  by  Michael Scheer
*CMZ :  2.30/01 12/04/2001  13.55.29  by  Michael Scheer
*CMZ :  2.20/10 10/04/2001  11.14.28  by  Michael Scheer
*CMZ :  2.20/09 03/04/2001  11.14.07  by  Michael Scheer
*CMZ :  2.20/08 18/03/2001  19.14.11  by  Michael Scheer
*CMZ :  2.20/03 22/02/2001  14.58.43  by  Michael Scheer
*CMZ :  2.16/08 29/10/2000  15.40.56  by  Michael Scheer
*CMZ :  2.15/00 28/04/2000  10.33.28  by  Michael Scheer
*CMZ :  2.13/11 20/03/2000  11.10.25  by  Michael Scheer
*CMZ :  2.12/01 08/06/99  14.59.11  by  Michael Scheer
*CMZ :  2.12/00 28/05/99  15.42.01  by  Michael Scheer
*CMZ :  2.11/00 12/05/99  12.07.17  by  Michael Scheer
*CMZ :  2.10/01 16/02/99  17.15.05  by  Michael Scheer
*CMZ :  1.00/00 11/07/97  14.49.14  by  Michael Scheer
*CMZ : 00.01/04 28/11/94  17.26.44  by  Michael Scheer
*CMZ : 00.00/00 28/04/94  15.12.57  by  Michael Scheer
*-- Author :  Michael Scheer
      MODULE TRACKF90

c +PATCH,//WAVE/CMNN
c +DECK,TRACKF90M,T=F77.

      DOUBLE PRECISION DTMCO
      DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE :: WTRA
      DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE :: DWTRA
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: DWT,DWX,DWB,DWY,DWZ
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: DWX2P,DWB2P,DWY2P,DWZ2P

      real (kind=16), DIMENSION(:,:,:), ALLOCATABLE :: DWTRA16
      real (kind=16), DIMENSION(:), ALLOCATABLE :: DWT16,DWX16,DWB16,DWY16,DWZ16
      real (kind=16), DIMENSION(:), ALLOCATABLE :: DWX2P16,DWB2P16,DWY2P16,DWZ2P16

      DOUBLE PRECISION, DIMENSION(:,:),   ALLOCATABLE :: WSXYZ,WVXYZ,WBXYZ,WAXYZ
     &  ,WVPXYZ
      DOUBLE PRECISION, DIMENSION(:,:),   ALLOCATABLE :: WDIS,WGAMMA
      DOUBLE PRECISION, DIMENSION(:),     ALLOCATABLE :: WTIM0,HTRA2,WTRA2
      DOUBLE PRECISION, DIMENSION(:),     ALLOCATABLE :: XAMAG,BXAMAG,BYAMAG
      DOUBLE PRECISION, DIMENSION(:),     ALLOCATABLE :: BZAMAG,BX2A,BY2A,BZ2A
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: TRAGAM

      INTEGER IXAMAG_I

      double precision
     &  yslopetr,yoffstr,zslopetr,zoffstr,
     &  ypslopetr,ypoffstr,zpslopetr,zpoffstr,
     &  yslopeetr,yoffsetr,zslopeetr,zoffsetr,
     &  ypslopeetr,ypoffsetr,zpslopeetr,zpoffsetr

      END MODULE
