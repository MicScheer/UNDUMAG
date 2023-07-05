*CMZ :  2.04/05 14/03/2023  19.31.25  by  Michael Scheer
*CMZ :  2.01/08 07/08/2020  11.38.10  by  Michael Scheer
*CMZ :  2.01/04 18/07/2019  11.40.47  by  Michael Scheer
*CMZ :  2.01/03 15/07/2019  12.12.57  by  Michael Scheer
*CMZ :  2.01/02 26/04/2018  08.48.43  by  Michael Scheer
*CMZ :  1.24/01 13/10/2017  13.03.30  by  Michael Scheer
*CMZ :  1.11/04 25/01/2017  10.48.46  by  Michael Scheer
*CMZ :  1.11/01 05/01/2017  14.36.19  by  Michael Scheer
*CMZ :  1.11/00 04/01/2017  15.18.44  by  Michael Scheer
*CMZ :  1.07/01 25/09/2016  11.39.02  by  Michael Scheer
*CMZ :  1.07/00 24/09/2016  14.49.55  by  Michael Scheer
*CMZ :  0.00/06 12/08/2016  12.46.19  by  Michael Scheer
*CMZ :  0.00/02 27/04/2016  09.26.35  by  Michael Scheer
*CMZ :  0.00/01 26/04/2016  09.06.57  by  Michael Scheer
*CMZ :  1.17/08 19/04/2016  15.50.00  by  Michael Scheer
*CMZ :  1.17/07 03/04/2016  14.12.47  by  Michael Scheer
*CMZ :  1.17/06 31/03/2016  09.18.32  by  Michael Scheer
*CMZ :  1.17/05 27/03/2016  08.19.23  by  Michael Scheer
*CMZ :  1.17/04 24/03/2016  16.12.47  by  Michael Scheer
*CMZ :  1.17/03 15/03/2016  17.56.43  by  Michael Scheer
*CMZ :  1.17/02 08/03/2016  15.58.13  by  Michael Scheer
*CMZ :  1.16/04 17/04/2014  11.09.51  by  Michael Scheer
*CMZ :  1.16/00 22/01/2009  10.27.16  by  Michael Scheer
*CMZ :  1.15/05 30/05/2008  14.45.09  by  Michael Scheer
*CMZ :  1.15/04 29/05/2008  16.21.09  by  Michael Scheer
*CMZ :  1.15/02 09/05/2008  11.10.54  by  Michael Scheer
*CMZ :  1.15/01 24/04/2008  14.45.02  by  Michael Scheer
*CMZ :  1.14/00 24/10/2007  15.18.47  by  Michael Scheer
*CMZ :  1.12/17 12/06/2007  09.30.20  by  Michael Scheer
*CMZ :  1.12/16 05/06/2007  16.11.14  by  Michael Scheer
*CMZ :  1.12/14 16/05/2007  13.23.50  by  Michael Scheer
*CMZ :  1.12/13 07/05/2007  14.21.12  by  Michael Scheer
*CMZ :  1.12/12 19/04/2007  12.09.41  by  Michael Scheer
*CMZ :  1.12/11 13/04/2007  16.09.58  by  Michael Scheer
*CMZ :  1.12/09 03/04/2007  15.41.11  by  Michael Scheer
*CMZ :  1.12/08 03/08/2006  14.58.38  by  Michael Scheer
*CMZ :  1.12/05 06/01/2006  09.34.34  by  Michael Scheer
*CMZ :  1.10/02 19/08/2004  14.09.53  by  Michael Scheer
*CMZ :  1.03/00 16/08/2004  12.37.12  by  Michael Scheer
*CMZ :  1.02/04 13/08/2004  14.34.19  by  Michael Scheer
*CMZ :  1.02/00 28/07/2004  12.49.29  by  Michael Scheer
*CMZ :  0.00/07 16/01/2004  11.05.50  by  Michael Scheer
*CMZ :  0.00/06 07/01/2004  13.45.01  by  Michael Scheer
*CMZ :  0.00/03 15/12/2003  16.07.00  by  Michael Scheer
*CMZ :  0.00/01 08/12/2003  09.42.07  by  Michael Scheer
*-- Author :    Michael Scheer   02/12/2003
      module bpolyederf90m

c +PATCH,//UNDUMAG/SEQ
c +DECK,BPOLYEDERf90m,T=F77.

      DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: bpebc,bflange,bpefe,xyzsmear
      DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE :: bpemat,xyzpoi
      DOUBLE PRECISION, DIMENSION(:,:,:,:), ALLOCATABLE :: bpemag,bpetm,bperot

      integer lunbpe,nmag,iaxint,nstepint,nxyzlines,nxyzlinep,iundumag,
     &  inside,nthreads,kundurun,jcomment,jrunnum,jdate,kdebug,lunwarn,
     &  ndivfby

      integer, save :: kinside=0, kkfail=0

      parameter(nxyzlinep=1000)

      character(128) filebpe

      DOUBLE PRECISION tiny,x0int,y0int,z0int,ranginti,ranginte

      DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: wires
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: dxshim

      double precision bfcenxmm,bfcenymm,bfcenzmm,bflenxmm,bflenymm,bflenzmm,
     &  torqcenxmm,torqcenymm,torqcenzmm,
     &  bfcenx,bfceny,bfcenz,bflenx,bfleny,bflenz,
     &  torqcenx,torqceny,torqcenz,rmu0,twopi,recgap,shlr,shll,shur,shul,
     &  outbox(2,3),xyzline(3,2,nxyzlinep),xyzave(nxyzlinep),buservar(100)

      double precision energy,gamma,brho,
     &  xstart,ystart,zstart,
     &  xend,yend,zend,
     &  vxstart,vystart,vzstart,
     &  vxend,vyend,vzend,
     &  shbprec,shconv,
     &  rmyinum,window,shdamp,shbrmax,
     &  scalbxt(3),scalbxtsh(3),scbyresp,scbzresp,
     &  xell,yell,hbeta,vbeta

      INTEGER, DIMENSION(:), ALLOCATABLE :: ibpeplan,ifeshim,isfeshim,
     &  ibpeplano,ishpassed,ibpecol

      INTEGER, DIMENSION(:,:), ALLOCATABLE :: ibpecorn
     &  ,idshim

      real forcol,forxpl(2),forypl(2),forzpl(2)
      real rausch,offrausch

      integer iseedsizep,iseedsize
      parameter (iseedsizep=12)
      integer nbforcx,nbforcy,nbforcz,jplforce,iforcol,nphi,iphase,
     &  lxyzpoi(nxyzlinep)

      integer icharge,iseed(iseedsizep),iallowin,ihaveshim,magmag,isetshim,
     &  nfeshim,ishfield,ibextern,ishiter,ishwrite,mfeshim,modesh,ibellnam
c     &  ,nmagorig,nrecorig,nironorig,iuysym

      integer nwiremax,iwire,nwire,imatrix,nmagmax,iumatrix,ifsimpson

      character(256) usercom

      double precision
     &  dyshimmy,ringi,ringe,zoffwire,phii,phie,
     &  scbxout0,scbyout0,scbzout0

      integer iring,ishstep,ishsoft,iempty,margin,isplderiv,i_no_bshimjb0

      namelist/polymagn/dyshimmy,
     &  iring,ringi,ringe,zoffwire,iphase,nphi,phii,phie,
     &  xell,yell,hbeta,vbeta,ishstep,ishsoft,iempty,outbox,
     &  nxyzlines,xyzline,lxyzpoi,xyzave,margin,isplderiv,i_no_bshimjb0,
     &  scbxout0,scbyout0,scbzout0,energy

!$OMP  THREADPRIVATE(kinside)
      end module
