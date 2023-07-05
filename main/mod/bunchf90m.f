*CMZ :  4.01/00 13/03/2023  14.50.51  by  Michael Scheer
*CMZ :  4.00/04 02/08/2019  15.01.19  by  Michael Scheer
*CMZ :  3.07/01 29/03/2019  14.25.03  by  Michael Scheer
*CMZ :  3.02/06 12/06/2015  13.33.59  by  Michael Scheer
*CMZ :  2.68/02 31/10/2012  09.56.28  by  Michael Scheer
*CMZ :  2.66/13 24/06/2010  12.50.20  by  Michael Scheer
*CMZ :  2.66/12 20/05/2010  08.56.21  by  Michael Scheer
*CMZ :  2.66/10 04/05/2010  10.00.32  by  Michael Scheer
*CMZ :  2.66/07 10/03/2010  09.23.19  by  Michael Scheer
*CMZ :  2.66/06 24/11/2009  08.26.46  by  Michael Scheer
*CMZ :  2.66/04 16/11/2009  14.01.34  by  Michael Scheer
*CMZ :  2.66/03 11/11/2009  13.04.10  by  Michael Scheer
*-- Author :    Michael Scheer   03/11/2009
      module bunchmod

c +PATCH,//WAVE/MOD
c +KEEP,bunchf90m.

      complex*16, dimension (:), allocatable :: unphexp

      DOUBLE PRECISION , dimension (:), allocatable :: phaserphi
      DOUBLE COMPLEX , dimension (:), allocatable :: expom1rphi

      double complex , dimension (:,:), allocatable :: affe,
     &  afferphi,unphrphi
      double precision, dimension (:), allocatable :: ampz,azcos,azsin
      double complex, dimension (:), allocatable :: phexp

      double complex dexpbunch
      double precision bunchlen,xelec,yelec,zelec,ypelec,zpelec,egamma,phelec,
     &  vyelec,vzelec,vxelec
     &  ,bunchp0,bunchr56,bunchcharge
     &  ,bunnor,bunchx,xlintra,sourceaclu(3,4),sourceeclu(3,4)

      real , dimension (:), allocatable :: unphrnrn
      real, dimension (:), allocatable :: phrnrn

      integer nphsp,neinbunch,nbunch,ibunphase,ibunnor,ibun,isub,ielec,iobunch,
     &  ihbunch,nidbunch,iubunch,nbunchharm,iwbunch,ilintra,nbuncho,neinbuncho

      namelist/bunchn/neinbunch,bunchlen,nbunch,
     &  iubunch,bunchp0,bunchr56,bunchcharge,
     &  ibunphase,ibunnor,iobunch,nbunchharm,
     &  ihbunch,iwbunch,ilintra,xlintra

      save ampz,azcos,azsin,phexp,unphexp,affe
c!$OMP THREADPRIVATE(ampz,azcos,azsin,phexp,unphexp,affe)
      end module
