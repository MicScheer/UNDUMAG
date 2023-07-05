*CMZ :  4.01/00 13/03/2023  14.33.24  by  Michael Scheer
*CMZ :  3.02/00 01/09/2014  12.55.26  by  Michael Scheer
*CMZ :  2.70/00 11/12/2012  09.42.53  by  Michael Scheer
*CMZ :  2.20/01 24/11/2000  20.41.29  by  Michael Scheer
*CMZ :  2.16/08 24/10/2000  11.02.35  by  Michael Scheer
*-- Author :    Michael Scheer   24/10/2000
      MODULE PHASEWSF90

      DOUBLE PRECISION, DIMENSION(:),   ALLOCATABLE :: WSUM

      COMPLEX*16, DIMENSION(:),   ALLOCATABLE :: PHSHIFT,EXPOM,DEXPOM
      COMPLEX*16, DIMENSION(:,:,:,:), ALLOCATABLE :: AMPLI
      DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: phspec,phspecf

      END MODULE
