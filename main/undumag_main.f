*CMZ :  2.04/13 31/08/2023  12.59.53  by  Michael Scheer
*CMZ :  2.04/05 22/08/2023  09.04.09  by  Michael Scheer
*CMZ :  2.03/00 25/07/2022  22.23.37  by  Michael Scheer
*CMZ :  2.02/02 02/03/2022  12.40.00  by  Michael Scheer
*CMZ :  2.02/01 30/01/2022  10.32.11  by  Michael Scheer
*CMZ :  2.02/00 27/02/2021  13.25.00  by  Michael Scheer
*CMZ :  2.01/03 17/07/2018  11.18.30  by  Michael Scheer
*CMZ :  1.24/01 12/10/2017  16.29.21  by  Michael Scheer
*CMZ :  1.23/07 10/10/2017  14.10.57  by  Michael Scheer
*CMZ :  1.18/01 09/06/2017  08.53.53  by  Michael Scheer
*CMZ :  1.11/04 25/01/2017  16.56.02  by  Michael Scheer
*CMZ :  1.10/02 24/11/2016  10.23.14  by  Michael Scheer
*CMZ :  1.06/00 20/09/2016  17.47.09  by  Michael Scheer
*CMZ :  1.04/00 13/09/2016  15.21.39  by  Michael Scheer
*CMZ :  1.00/00 19/08/2016  14.54.42  by  Michael Scheer
*CMZ :  1.17/02 11/03/2016  15.08.19  by  Michael Scheer
*-- Author :    Michael Scheer   02/12/2003

*KEEP,GPLHINT.
!******************************************************************************
!
!      Copyright 2013 Helmholtz-Zentrum Berlin (HZB)
!      Hahn-Meitner-Platz 1
!      D-14109 Berlin
!      Germany
!
!      Author Michael Scheer, Michael.Scheer@Helmholtz-Berlin.de
!
! -----------------------------------------------------------------------
!
!    This program is free software: you can redistribute it and/or modify
!    it under the terms of the GNU General Public License as published by
!    the Free Software Foundation, either version 3 of the License, or
!    (at your option) any later version.
!
!    This program is distributed in the hope that it will be useful,
!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!    GNU General Public License for more details.
!
!    You should have received a copy (wave_gpl.txt) of the GNU General Public
!    License along with this program.
!    If not, see <http://www.gnu.org/licenses/>.
!
!    Dieses Programm ist Freie Software: Sie koennen es unter den Bedingungen
!    der GNU General Public License, wie von der Free Software Foundation,
!    Version 3 der Lizenz oder (nach Ihrer Option) jeder spaeteren
!    veroeffentlichten Version, weiterverbreiten und/oder modifizieren.
!
!    Dieses Programm wird in der Hoffnung, dass es nuetzlich sein wird, aber
!    OHNE JEDE GEWAEHRLEISTUNG, bereitgestellt; sogar ohne die implizite
!    Gewaehrleistung der MARKTFAEHIGKEIT oder EIGNUNG FueR EINEN BESTIMMTEN ZWECK.
!    Siehe die GNU General Public License fuer weitere Details.
!
!    Sie sollten eine Kopie der GNU General Public License
!    zusammen mit diesem Programm erhalten haben. Wenn nicht,
!    siehe <http://www.gnu.org/licenses/>.
!
!******************************************************************************
*KEND.

!-----------------------------------------------------------------------
c+seq,phyconmod.


*KEEP,MAGNET_STRUCT.
      module magnets_structure

      double precision ::
     &  xcwmin=1.0d30,xcwmax=-1.0d30,
     &  ycwmin=1.0d30,ycwmax=-1.0d30,
     &  zcwmin=1.0d30,zcwmax=-1.0d30,
     &  xmin_t=1.0d30,xmax_t=-1.0d30,
     &  ymin_t=1.0d30,ymax_t=-1.0d30,
     &  zmin_t=1.0d30,zmax_t=-1.0d30,
     &  xsymmm_t

      integer nclcbuff,nclcspec,nclcmag,nclccoil,nclcvar,nclcmod,nclcmat,
     &  kunduplot_mode

      integer :: nvar_t=0,nmag_t=0,nvox_t=0,niron_t=0,kvox=0,kfirstiron_t=0,
     &  nspecmag_t=0,kfirstiron_spec_t=0,ncoils_t=0,ncwires_t=0,nmagtot_t=0,
     &  ncornmax_t=8,nplanmax_t=16, nmodule_t=0, nmagcopy_t=0,nvoxcopy_t=0,
     &  nclccop_t=0,nmoth_t=0,nmothtot_t=0,nmagsym_t=0,ninhom_t=0,nmaginhom_t=0

      integer, dimension (:), allocatable :: ksort_t,kmaglist_t,maginhom_t

      character(512), dimension (:), allocatable :: clcbuff,clcmag,clccoil,
     &  clcvar,clcmod,clcmat,clctrarot,clccop,clcspec,clcinhom

      type T_variable
        character(128) cname
        double precision val
      end type T_variable

      type(T_variable), dimension(:), allocatable :: t_variables

      type T_Voxel

      double precision, dimension (:), allocatable :: xhull,yhull,zhull
      double precision, dimension (:,:), allocatable :: plan, vnorm

      integer, dimension (:,:), allocatable :: kedge
      integer, dimension (:), allocatable :: kface,khull

      double precision xyz(3),size(3),trans(3),rot(3,3),Br(3),
     &  gcen(3),volume,xmin,xmax,ymin,ymax,zmin,zmax

      integer nhull,ixdiv,iydiv,izdiv,nedge,nface,kfacelast,
     &  IsPole,IsBlock,mxdiv,mydiv,mzdiv

      end type T_Voxel

      type T_Voxel_Copy
        integer kmagnet,kmodule,kcopy,kvoxel,kproto,ispole
        double precision Br(3),gcen(3)
      end type T_Voxel_Copy

      type T_Magnet_Copy
        integer kproto,kmodule,kcopy,IsPole,IsSpecial
        double precision gcen(3),Br(3),BrN,xmin,xmax,ymin,ymax,zmin,zmax,
     &    size(3),volume
        character(32) ctype,cnam,cmoth
      end type T_Magnet_Copy

      type T_Magnet_Sym
        integer kproto,kmodule,kcopy,IsPole,IsSpecial
        double precision gcen(3),Br(3),BrN,xmin,xmax,ymin,ymax,zmin,zmax
        character(32) ctype,cnam,cmoth
      end type T_Magnet_Sym

      type T_Magnet

        double precision, dimension (:,:), allocatable :: plan
        double precision, dimension (:), allocatable :: xhull0,yhull0,zhull0,
     &    xhull,yhull,zhull,ydivs,zdivs

        integer, dimension (:,:,:), allocatable :: kvoxels
        integer, dimension (:,:), allocatable :: kedge
        integer, dimension (:), allocatable :: kface,khull,kcopy

        character(512), dimension (:), allocatable :: cinhom

        double precision xyz(3),size(3),trans(3),rot(3,3),Br(3),yfracdiv,
     &    xmin,ymin,zmin,xmax,ymax,zmax,volume,gcen(3),dxdiv,dydiv,dzdiv,yfrac,
     &    zfracdiv,zfrac,uschamf,dschamf,cylphi,BrN,xyzinh(4),xvolume,yvolume,
     &    zvolume

        integer :: nface,nhull,icol,imat,nxdiv,nydiv,nzdiv,matindex,mattype,
     &    kfacelast,nedge,nvoxels,IsPole,IsSpecial,IsBlock,nhull0,IsPart,
     &    ncopy=0,kmodule=0,IsInhom=0,mxdiv,mydiv,mzdiv

        character(32) ctype,cnam,cmoth
        character(1024) cfile

        type(T_Voxel), dimension(:), allocatable ::  t_voxels, t_xcuts
        type(T_Voxel), dimension(:,:), allocatable :: t_xycuts
        type(T_Voxel), dimension(:,:,:), allocatable :: t_xyzcuts

      end type T_Magnet

      type T_Mother

        double precision, dimension (:), allocatable :: xhull,yhull,zhull

        integer, dimension (:,:), allocatable :: kedge
        integer, dimension (:), allocatable :: kface,khull,magnets

        double precision xyz(3),xmin,ymin,zmin,xmax,ymax,zmax,gcen(3)

        integer :: nmagnets=0,nhull,kfacelast

        character(32) :: cmoth=''

      end type T_Mother

      type T_Filament
        double precision curr,x1,y1,z1,x2,y2,z2
        integer icolor
      end type T_Filament

      type T_Coil
        integer ncwireI,ncwireE,iibuff,iebuff
        character(128) ctype,cnam
        character(1024) cparams
        double precision params(100)
      endtype T_Coil

      type(T_Magnet), dimension(:), allocatable ::  t_magnets, t_magnets_copy
      type(T_Coil), dimension(:), allocatable ::  t_coils

      type T_Module
        integer ncopy
        double precision offset(3),phi,vspace(3), scalmag(3),rotmat(3,3)
      end type T_Module

      character(32), dimension(:), allocatable :: chmutts

      integer, dimension (:), allocatable :: magmodule
      integer, parameter :: ntransrotcop_p=1000, nmat_p=1000

      integer :: ntransrotcop=0, nmat_t=0, t_matrec(3,nmat_p), nowarnugv=0
      double precision transrotcop(8,ntransrotcop_p)
      character(128) ctransrotcop(ntransrotcop_p)

      type(T_Mother), dimension(:), allocatable ::  t_mothers
      type(T_Module), dimension(:), allocatable ::  t_modules
      type(T_Magnet_Copy), dimension(:), allocatable ::  t_magcopy
      type(T_Magnet_Sym), dimension(:), allocatable ::  t_magsym
      type(T_Voxel_Copy), dimension(:), allocatable ::  t_voxcopy

      double precision, dimension (:), allocatable ::
     &  xpuffer1,ypuffer1,zpuffer1,
     &  xpuffer2,ypuffer2,zpuffer2,
     &  xpuffer3,ypuffer3,zpuffer3

      end module magnets_structure
*KEEP,BPOLYEDERF90M.
      module bpolyederf90m

c +PATCH,//UNDUMAG/SEQ
c +KEEP,BPOLYEDERF90M.

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
*KEEP,UNDUMAGF90M.
      module undumagf90m

c +PATCH,//UNDUMAG/SEQ
c +KEEP,UNDUMAGF90M.

      implicit none

      integer nthreadp,nwitems
      parameter (nthreadp=1000,nwitems=11)

!      double precision, dimension (:,:,:,:), allocatable :: wwmatrix

      double precision, dimension (:,:,:), allocatable :: bcmat
      double precision, dimension (:,:), allocatable :: bc0,bc00,bpm,
     &  bpebc0,wire,race,wind,arc,carc,crace,rectbar,thickwire,brnmat

      double precision, dimension (:), allocatable ::
     &  feh1,feh2,feh3,fem1,fem2,fem3,ufespl1,
     &  fespl1,fewspl1,fewspl2,fewspl3,fewspl4,
     &  bxconvw,byconvw,bzconvw

      double precision, dimension (:,:), allocatable :: dipoles
      double precision, dimension (:,:,:), allocatable :: cornpoles

      real*4, dimension (:,:,:,:), allocatable :: wwmatrix4,convmat
c      real*4, dimension (:,:,:), allocatable :: wwmatrix

      integer, dimension (:), allocatable :: kspecmag,idamp8,ncornpoles
      integer, dimension(:,:), allocatable :: magcyl

      double precision
     &  unduplot_phi,unduplot_theta,traxyz_theta,traxyz_phi,permu,parmu,perksi,parksi,
     &  xmapmin,xmapmax,ymapmin,ymapmax,zmapmin,zmapmax,dxmap,
     &  xminpl,xmaxpl,yminpl,ymaxpl,zminpl,zmaxpl,
     &  xconvmin,xconvmax,dxconv,xconv(100),yconv,zconv,hconv,hconva,uwindow,xsym,
     &  xbeff,xbeffy,xbeffz,perlen,
     &  ebeam,zslit,xcenter,xmaxinf,xcentershift,xelec,yelec,zelec,
     &  vxelec,vyelec,vzelec,
     &  ds,xf,yf,zf,efx,efy,efz,xobsv,yobsv,zobsv,phelow,phehig,dampiron,dampi,
     &  ubfcenx,ubfceny,ubfcenz,ubflenx,ubfleny,ubflenz,
     &  utorqcenx,utorqceny,utorqcenz,
     &  fxdip,fydip,fzdip,
     &  txdip,tydip,tzdip,
     &  uservar(100),corrtiny,cuttiny,hulltiny,randos,randox,randoy,randoz
     &  ,dampfac,hconvbase,hconvexp,chicut,damp8,rcvthron,angmaprotx,
     &  cenmaprotxy,cenmaprotxz,zminprof,zmaxprof,
     &  bxex,byex,bzex,hresidiron,resiron,halt,dedgefb,coating

      real randoxa,randoya,randoza,randox10,randoy10,randoz10,
     &  randomx,randomy,randomz

      integer
     &  maxiter,maxiterrec,maxiteriron,nchiiron,ibulk,nchimax,kesti,
     &  ktetmesh,iunduplot_mode,
     &  kiter,iterrectot,iterirontot,
     &  iunduplot,kcomment,krunnum,kdate,kplsym,nrec,niron,nxmap,intmaglis,noduplis,nymap,nzmap
     &  ,mode,ncornmax,nplanmax,ncornadd,nplanadd,niterrec,niteriron,niter,
     &  kconvrec,kconviron,kconv,lunconv,kdumpconv,nmatfiles,
     &  matmaps(4,1000),kpreset,matrix,
     &  magmatrix,nuthreads,
     &  ixsym,iysym,izsym,nspecmag,nxbeff,kbeffmode,
     &  ixsymo,iysymo,izsymo,
     &  ieneloss,ivelofield,nphener,nstep,kxcenter,kudebug,kcalcvars,isimpson,
     &  nxconv,kdisplace,kechocalc,knomagmap,knopolmap,isplinefm,
     &  iforce,ivrml,iundugeo,idipoles,iforcedip,iplforce,itorque,mbforcex,mbforcey,mbforcez,mfcolor,
     &  kmapmode,kmapnohead,knointmap,konv,iwarnsum,maxpoints,nmagpols,kallodip,
     &  nwarnbound, iwarnin,iwarn2pi,iwarnbound,ndipoles,killbadmag,irecrepl,kwarncom,irandmag,kshuffle,
     &  kforcemag,kurad,kbextern,kresiron,kprint,ndivfboxy,
     &  ncwires,nrace,nwind,ncrace,ncfila,narc,ncarc,nwcarc,nwrace,nwwind,
     &  nwcrace,nwarc,ncoil,newclc,
     &  nrbar,nwrbar,nthwir,nwthwir,nmagcyl,kwave,iforcegrid,modegui,nbrnmat

      character(256) unducomment
      character(512) cundutit
      character(16) chuvers

      character, dimension (:,:), allocatable :: chmags, chmoths,
     &  chmagsm,chmothsm,chmagsi,chmothsi,chmagpols,chmothso

      character(32) chmag,chmoth,chforcemag

      namelist/undumagn/
     &  maxiter,maxiterrec,maxiteriron,nchiiron,ibulk,iunduplot,kcomment,
     &  krunnum,kdate,ktetmesh,kesti,
     &  kplsym,nxmap,intmaglis,noduplis,nymap,nzmap
     &  ,mode,kdumpconv,dxmap,iunduplot_mode,
     &  unduplot_phi,unduplot_theta,traxyz_theta,traxyz_phi,
     &  xmapmin,xmapmax,ymapmin,ymapmax,zmapmin,zmapmax,
     &  xconvmin,xconvmax,dxconv,nxconv,yconv,zconv,hconv,kpreset,uwindow,matrix,nuthreads,
     &  xminpl,xmaxpl,yminpl,ymaxpl,zminpl,zmaxpl,
     &  ixsym,iysym,izsym,xsym,
     &  xbeff,xbeffy,xbeffz,perlen,nxbeff,kbeffmode,kurad,ebeam,zslit,xcenter,
     &  xelec,yelec,zelec,vxelec,vyelec,vzelec,
     &  ds,xf,yf,zf,efx,efy,efz,xobsv,yobsv,zobsv,phelow,phehig,
     &  ieneloss,ivelofield,nphener,nstep,kxcenter,kcalcvars,
     &  dampiron,isimpson,isplinefm,kdisplace,kechocalc,knomagmap,knopolmap,
     &  kmapmode,kmapnohead,knointmap,
     &  iforce,ivrml,iundugeo,idipoles,iforcedip,iplforce,itorque,mbforcex,mbforcey,mbforcez,mfcolor,
     &  ubfcenx,ubfceny,ubfcenz,ubflenx,ubfleny,ubflenz,
     &  utorqcenx,utorqceny,utorqcenz,chforcemag,nwarnbound, iwarnin,iwarn2pi,
     &  killbadmag,corrtiny,cuttiny,hulltiny,randos,randox,randoy,randoz,irecrepl
     &  ,dampfac,hconvbase,hconvexp,chicut,irandmag,kshuffle,
     &  randomx,randomy,randomz,rcvthron,angmaprotx,cenmaprotxy,cenmaprotxz,
     &  bxex,byex,bzex,kbextern,kresiron,resiron,kprint,ndivfboxy,dedgefb,
     &  iforcegrid,kudebug,newclc

      end module undumagf90m
*KEEP,COMMANDLINEF90M.
      module commandlinef90m

      integer narg_cl, lencomline_cl, lencprog_cl, ierr_cl, iarg_cl,lun6
      character(2048) comline_cl, cprog_cl, carg_cl(32), Fclc, Fnam

      end module
*KEND.


      program undumag_main

      use bpolyederf90m
      use undumagf90m
      use commandlinef90m

      implicit none

*KEEP,PHYCONPARAM.
c-----------------------------------------------------------------------
c     phyconparam.cmn
c-----------------------------------------------------------------------

      complex*16, parameter :: zone1=(1.0d0,0.0d0), zi1=(0.0d0,1.0d0)

      complex*16, dimension(4,3), parameter ::
     &  vstokes=reshape([
     &  ( 0.0000000000000000d0,  0.0000000000000000d0),
     &  ( 0.0000000000000000d0,  0.0000000000000000d0),
     &  ( 0.0000000000000000d0,  0.0000000000000000d0),
     &  ( 0.0000000000000000d0,  0.0000000000000000d0),
     &  ( 0.0000000000000000d0,  0.0000000000000000d0),
     &  ( 0.0000000000000000d0, -0.70710678118654746d0),
     &  ( 0.0000000000000000d0, -0.70710678118654746d0),
     &  ( 0.70710678118654746d0, 0.0000000000000000d0),
     &  (-0.70710678118654746d0,-0.70710678118654746d0),
     &  ( 0.70710678118654746d0, 0.0000000000000000d0),
     &  (-0.70710678118654746d0, 0.0000000000000000d0),
     &  (-0.70710678118654746d0, 0.0000000000000000d0)
     &  ],[4,3])

c      vstokes(1,1)=( 0.0d0,        0.0d0)      !horizontal polarization
c      vstokes(1,2)=( 0.0d0,        0.0d0)
c      vstokes(1,3)=(-sqrt(1./2.),       -sqrt(1./2.))
c
c      vstokes(2,1)=( 0.0d0,        0.0d0)      !right handed polarization
c      vstokes(2,2)=( 0.0d0,       -sqrt(1./2.))
c      vstokes(2,3)=(+sqrt(1./2.),        0.0d0)
c
c      vstokes(3,1)=( 0.0d0,        0.0d0)      !left handed polarization
c      vstokes(3,2)=( 0.0d0,       -sqrt(1./2.))
c      vstokes(3,3)=(-sqrt(1./2.),        0.0d0)
c
c      vstokes(4,1)=( 0.0d0,        0.0d0)      !45 degree linear polarization
c      vstokes(4,2)=( sqrt(1./2.),        0.0d0)
c      vstokes(4,3)=(-sqrt(1./2.),        0.0d0)

      double precision, parameter ::
     &  HBAREV1=6.58211889D-16
     &  ,CLIGHT1=2.99792458D8
     &  ,EMASSKG1=9.10938188D-31
     &  ,EMASSE1=0.510998902D6
     &  ,EMASSG1=0.510998902D-3
     &  ,ECHARGE1=1.602176462D-19
     &  ,ERAD1=2.8179380D-15
     &  ,EPS01=8.854187817D-12
     &  ,PI1=3.141592653589793D0
     &  ,rmu04pi1=1.0D-7
     &  ,dnull1=0.0d0
     &  ,done1=1.0d0
     & ,HPLANCK1=6.626176D-34

      double precision, parameter ::
     & GRARAD1=PI1/180.0d0
     & ,RADGRA1=180.0d0/PI1
     & ,HBAR1=HBAREV1*ECHARGE1
     & ,WTOE1=CLIGHT1*HPLANCK1/ECHARGE1*1.0d9
     & ,CQ1=55.0d0/32.0d0/DSQRT(3.0D0)*HBAR1/EMASSKG1/CLIGHT1
     & ,CGAM1=4.0d0/3.0d0*PI1*ERAD1/EMASSG1**3
     & ,POL1CON1=8.0d0/5.0d0/DSQRT(3.0D0)
     & ,POL2CON1=8.0d0/5.0d0/DSQRT(3.0D0)/2.0d0/PI1/3600.0d0
     &  *EMASSKG1/HBAR1/ERAD1*EMASSG1**5
     & ,TWOPI1=2.0D0*PI1
     & ,HALFPI1=PI1/2.0D0
     & ,sqrttwopi1=sqrt(twopi1)
     & ,rmu01=4.0D0*PI1/1.0D7
     & ,alpha1=echarge1**2/(4.0d0*pi1*eps01*hbar1*clight1)
     & ,gaussn1=1.0d0/sqrt(twopi1)
     & ,cK934=ECHARGE1/(2.0d0*PI1*EMASSKG1*CLIGHT1)/100.0d0
     & ,powcon1=cgam1/2.0d0/pi1*clight1*(clight1/1.0d9)**2*emassg1
     &  ,gamma1=1.0d0/emassg1
     &  ,emom1=emasse1*dsqrt((gamma1-1.0d0)*(gamma1+1.0d0))
     &  ,rho1=emom1/clight1
     &  ,omegac1=1.5d0*gamma1**3*clight1/rho1
     &  ,ecdipev1=omegac1*hbar1/echarge1
     &  ,ecdipkev1=ecdipev1/1000.0d0

c-----------------------------------------------------------------------
c     end of phyconparam.cmn
c-----------------------------------------------------------------------
*KEND.

      integer lunst
      integer :: kseg=0

      lun6=6

      Fclc='undumag.clc'
      Fnam='undumag.nam'

*KEEP,COMMANDARGS.
      call get_command(comline_cl,lencomline_cl,ierr_cl)
      narg_cl = command_argument_count()

      call get_command_argument(0,cprog_cl,lencprog_cl,ierr_cl)

      do iarg_cl=1,narg_cl
        call get_command_argument(iarg_cl,carg_cl(iarg_cl))
      enddo
*KEND.

      do iarg_cl=1,narg_cl
        if(carg_cl(iarg_cl)(1:7).eq.'STDOUT=') then
          open(newunit=lun6,file=carg_cl(iarg_cl)(8:len_trim(carg_cl(iarg_cl))))
        else if(carg_cl(iarg_cl)(1:4).eq.'CLC=') then
          fclc=carg_cl(iarg_cl)(5:len_trim(carg_cl(iarg_cl)))
        else if(carg_cl(iarg_cl)(1:4).eq.'NAM=') then
          fnam=carg_cl(iarg_cl)(5:len_trim(carg_cl(iarg_cl)))
        else if(carg_cl(iarg_cl).eq.'SEGMENTATION') then
          Fclc='undumag.clc'
          kseg=1
        else if(carg_cl(iarg_cl).eq.'FILAMENTS') then
          Fclc='filaments.clc'
          kseg=2
        else
          print*
          print*,"Allowed argument keys and their default values:"
          print*
          print*,"STDOUT=Terminal"
          print*,"CLC=undumag.clc"
          print*,"NAM=undumag.nam"
          print*,"SEGMENTATION"
          print*
          stop
        endif
      enddo

*KEEP,SHOWARGS.
*KEND.

      call util_zeit_kommentar(lun6,"")

      if (narg_cl.ne.0) then
        write(lun6,*)
        write(lun6,*)"Arguments passed to UNDUMAG:"
        write(lun6,*)
        do iarg_cl=1,narg_cl
          write(lun6,*)iarg_cl, trim(carg_cl(iarg_cl))
        enddo
        write(lun6,*)
      endif

      iundumag=1 ! flag to indicate enviroment of undumag

      konv=-9

      newclc=0
      call undumag_check_newclc

      if (kseg.eq.1) then
        open(newunit=lunst,file="undumag.sta")
        write(lunst,*)"-9999",konv
        write(lunst,*)"Starting segmentation"
        close(lunst)
      else if (kseg.eq.2) then
        open(newunit=lunst,file="undumag.sta")
        write(lunst,*)"-9999",konv
        write(lunst,*)"Starting calculation of coil filaments"
        close(lunst)
      endif

      if (newclc.ne.0) then
        call undumag_ini(kseg)
      else
        call undumag_ini_old(kseg)
      endif

      if (kseg.eq.1) then

        call util_zeit_kommentar(lun6,"--- Program undumag finished ---")
        open(newunit=lunst,file="undumag.sta")
        write(lunst,*)"-9999",konv
        write(lunst,*)"Segmentation finished"
        close(lunst)

      else if (kseg.eq.2) then

        call util_zeit_kommentar(lun6,"--- Program undumag finished ---")
        open(newunit=lunst,file="undumag.sta")
        write(lunst,*)"-9999",konv
        write(lunst,*)"Calculation of coil filaments finished"
        close(lunst)

      else !kseg

        if (maxiter.gt.0) then
          call undumag_proc
          if (iforcedip.ne.0) call undumag_force_dipoles
          if (iforce.ne.0.or.itorque.ne.0) call undumag_force
        endif

        call undumag_end

        call util_zeit_kommentar(lun6,"--- Program undumag finished ---")

        if (lun6.ne.6) then
          close(lun6)
        endif

      endif !kseg

      end

c      include "uradfield.f"
c      include "uradrndm.f"
