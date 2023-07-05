*CMZ :  4.01/00 13/03/2023  13.10.22  by  Michael Scheer
*CMZ :  4.00/11 17/05/2021  12.15.24  by  Michael Scheer
*CMZ :  3.04/00 09/01/2018  15.21.46  by  Michael Scheer
*CMZ :  2.15/00 28/04/2000  10.33.28  by  Michael Scheer
*CMZ : 00.01/04 28/11/94  17.29.26  by  Michael Scheer
*CMZ : 00.01/02 24/11/94  15.47.38  by  Michael Scheer
*CMZ : 00.00/00 28/04/94  15.11.57  by  Michael Scheer
*-- Author : Michael Scheer
      module fbtabzymod

      integer :: nfourzy,ifourzy0,klinearfbt,
     &  nxbyfbt=0,nxbzfbt=0,inifbt=1,iallofbt=0,nfbtabc=0,nfbtabcz=0

      double precision fourzsh,fourysh
      double precision, dimension(:), allocatable :: xbyfbt,byfbt,xbzfbt,bzfbt

      namelist/fbtabzyn/
     &  fourzsh,fourysh,nfourzy,ifourzy0,klinearfbt

      end module
