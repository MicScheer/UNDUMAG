*CMZ :  2.05/01 06/10/2023  08.25.51  by  Michael Scheer
*CMZ :  2.04/27 02/10/2023  13.45.41  by  Michael Scheer
*CMZ :  2.04/17 12/09/2023  09.42.21  by  Michael Scheer
*CMZ :  2.04/16 06/09/2023  16.15.55  by  Michael Scheer
*CMZ :  2.04/14 06/09/2023  06.43.30  by  Michael Scheer
*CMZ :  2.04/13 03/09/2023  09.50.42  by  Michael Scheer
*CMZ :  2.04/10 23/08/2023  08.17.05  by  Michael Scheer
*CMZ :  2.04/05 14/03/2023  19.31.25  by  Michael Scheer
*CMZ :  2.04/03 03/03/2023  16.41.34  by  Michael Scheer
*CMZ :  2.04/02 27/02/2023  16.37.47  by  Michael Scheer
*CMZ :  2.04/01 23/01/2023  09.51.15  by  Michael Scheer
*CMZ :  2.04/00 13/01/2023  10.56.20  by  Michael Scheer
*CMZ :  2.02/02 16/02/2022  15.17.02  by  Michael Scheer
*CMZ :  2.02/01 06/02/2022  11.20.01  by  Michael Scheer
*-- Author :    Michael Scheer   25/04/2021
      module magnets_structure

      integer nconcave_t
      character(512), dimension (:), allocatable :: clcconcave

      type T_concave

c          double precision, dimension (:,:,:), allocatable :: faces

      double precision, dimension (:,:), allocatable :: verts,
     &  fcen,fnorm

      double precision, dimension (:), allocatable ::
     &  xhull0,yhull0,zhull0,
     &  xhull,yhull,zhull,xdivs,ydivs,zdivs

      integer, dimension (:,:,:), allocatable :: kvoxels
      integer, dimension (:,:), allocatable :: kedge,ifaces
      integer, dimension (:), allocatable :: lifaces,npois,
     &  kface,khull,kcopy,lface,kconcave

      character(512), dimension (:), allocatable :: cinhom

      double precision
     &  xyz(3),size(3),trans(3),rot(3,3),Br(3),
     &  yfracdiv,xmin,ymin,zmin,xmax,ymax,zmax,
     &  volume,gcen(3),dxdiv,dydiv,dzdiv,yfrac,
     &  zfracdiv,zfrac,uschamf,dschamf,cylphi,BrN,xyzinh(4),xvolume,yvolume,
     &  zvolume

      integer :: kmag,nhull,icol,imat,nverts,nface,
     &  nxdiv,nydiv,nzdiv,matindex,mattype,IsConvex,
     &  kfacelast,nedge,nvoxels,IsPole,IsSpecial,
     &  IsBlock,nhull0,IsPart,ncopy=0,kmodule=0,
     &  IsInhom=0,mxdiv,mydiv,mzdiv,IsRotated,nconcave,npoimax

      character(32) ctype,cnam,cmoth
      character(1024) cfile

      type(T_Voxel), dimension(:), allocatable ::  t_voxels, t_xcuts
      type(T_Voxel), dimension(:,:), allocatable :: t_xycuts
      type(T_Voxel), dimension(:,:,:), allocatable :: t_xyzcuts

      end type T_concave

      type(T_Concave), dimension(:), allocatable :: t_concaves

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

      integer :: iwwork,iwfct=0,iwgeo=0,iwvgeo=0,iwmag=0

      integer :: nvar_t=0,nmag_t=0,nvox_t=0,niron_t=0,kvox=0,kfirstiron_t=0,
     &  nspecmag_t=0,kfirstiron_spec_t=0,ncoils_t=0,ncwires_t=0,nmagtot_t=0,
     &  ncornmax_t=8,nplanmax_t=16, nmodule_t=0, nmagcopy_t=0,nvoxcopy_t=0,
     &  nclccop_t=0,nmoth_t=0,nmothtot_t=0,nmagsym_t=0,ninhom_t=0,nmaginhom_t=0

      integer, dimension (:,:), allocatable :: ifacets
      integer :: nfacets=0
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
      double precision, dimension (:,:), allocatable :: plan, vcen,vnorm

      integer, dimension (:,:), allocatable :: kedge
      integer, dimension (:), allocatable :: kface,khull,isfacet,lface

      double precision xyz(3),size(3),trans(3),rot(3,3),Br(3),
     &  gcen(3),volume,xmin,xmax,ymin,ymax,zmin,zmax

      integer nhull,ixdiv,iydiv,izdiv,nedge,nface,kfacelast,
     &  IsPole,IsBlock,mxdiv,mydiv,mzdiv

      end type T_Voxel

      type T_Voxel_Copy
        integer, dimension (:), allocatable :: isfacet
        integer kmagnet,kmodule,kcopy,kvoxel,kproto,ispole,nface
        double precision Br(3),Brn,gcen(3)
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

        double precision, dimension (:,:), allocatable :: fcen,fnorm
        double precision, dimension (:), allocatable :: xhull0,yhull0,zhull0,
     &    xhull,yhull,zhull,ydivs,zdivs

        integer, dimension (:,:,:), allocatable :: kvoxels
        integer, dimension (:,:), allocatable :: kedge
        integer, dimension (:), allocatable :: kface,khull,kcopy,lface

        character(512), dimension (:), allocatable :: cinhom

        double precision xyz(3),size(3),trans(3),rot(3,3),Br(3),yfracdiv,
     &    xmin,ymin,zmin,xmax,ymax,zmax,volume,gcen(3),dxdiv,dydiv,dzdiv,yfrac,
     &    zfracdiv,zfrac,uschamf,dschamf,cylphi,BrN,xyzinh(4),xvolume,yvolume,
     &    zvolume

        integer :: kmag,nface,nhull,icol,imat,nxdiv,nydiv,nzdiv,matindex,mattype,
     &    kfacelast,nedge,nvoxels,IsPole,IsSpecial,IsBlock,nhull0,IsPart,
     &    ncopy=0,kmodule=0,IsInhom=0,mxdiv,mydiv,mzdiv,IsRotated

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
