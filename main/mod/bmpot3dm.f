*CMZ :  4.01/00 13/03/2023  14.33.24  by  Michael Scheer
*CMZ :  3.02/00 01/09/2014  12.55.26  by  Michael Scheer
*CMZ :  2.44/01 05/12/2002  18.08.02  by  Michael Scheer
*-- Author :    Michael Scheer   05/12/2002
      MODULE BMPOT3DM

      DOUBLE PRECISION, DIMENSION (:,:,:), ALLOCATABLE:: FSTAK
      DOUBLE PRECISION, DIMENSION (:), ALLOCATABLE:: XPOW,YPOW,ZPOW
      DOUBLE PRECISION, DIMENSION (:,:), ALLOCATABLE:: A,WA
      DOUBLE PRECISION, DIMENSION (:), ALLOCATABLE:: VC,B,WS,WB
      INTEGER, DIMENSION(:,:), ALLOCATABLE:: INDEX
      INTEGER, DIMENSION(:,:,:), ALLOCATABLE:: ICG

      ENDMODULE
