*CMZ :  4.01/00 13/03/2023  12.49.36  by  Michael Scheer
*CMZ :  4.00/15 28/04/2022  08.03.24  by  Michael Scheer
*CMZ :  4.00/14 31/12/2021  18.03.24  by  Michael Scheer
*CMZ :          07/12/2021  15.22.18  by  Michael Scheer
*-- Author :    Michael Scheer   07/12/2021
      module mhbook_mod

      integer*8 :: memsize_mh=-1, memstart_mh=1e8,
     &  memsizemax_mh=25, ! percentage of available mememory to be used
     &  memntuptot_mh=0

      integer,parameter :: lenvarp_mh=8, nvarp_mh=100, nflushp_mh=2**18,
     &  npathp_mh=10,lenpathp_mh=8,nfilep_mh=10

      integer, dimension(:), allocatable ::
     &  indhis_mh,idhis_mh,idhbuff_mh,indntup_mh,idntup_mh,idnbuff_mh

      double precision :: var_mh(nvarp_mh)=0.0d0

      character(lenvarp_mh) chvar_mh(nvarp_mh)
      character(lenpathp_mh) chpath_mh(npathp_mh),chdir_mh,chdir_old_mh

      Type Ntuple   !----------------------------------------------

      integer :: nentries=0,neve=0,nalloc=0
      integer :: id=-1,nvar=0,lun=0,ktouched=0

      double precision, dimension (:,:), allocatable :: eve
      double precision var(nvarp_mh),varm(2,nvarp_mh)

      character(lenvarp_mh) chvar(nvarp_mh)
      character(2048) title,file
      character(lenpathp_mh) chpath

      end Type Ntuple !----------------------------------------------

      Type Histogram  !----------------------------------------------

      integer :: id=-1, nx=1, ny=-1, nentries=0, kdelete=0,
     &  nunder=0,nover=0,ktouched=0,
     &  nuu=0,num=0,nuo=0,nmu=0,nmo=0,nou=0,nom=0,noo=0

      double precision ::
     &  xmin=-1.,xmax=1.,xmean=0.0d0,xsum=0.0d0,xsum2=0.0d0,
     &  xrms=0.0d0,dx=1.0d0,
     &  ymin=-1.,ymax=1.,ymean=0.0d0,ysum=0.0d0,ysum2=0.0d0,
     &  yrms=0.0d0,dy=1.0d0,
     &  hmin=1.0d30,hmax=-1.0d30,hmean=0.0d0,hrms=0.0d0,
     &  hsum=0.0d0,hsum2=0.0d0,hint=0.0d0,
     &  wnuu=0,wnum=0,wnuo=0,wnmu=0,wnmo=0,wnou=0,wnom=0,wnoo=0

      double precision, dimension (:,:,:), allocatable :: channels

      character(2048) title
      character(lenpathp_mh) chpath

      end Type Histogram !----------------------------------------------

      integer :: nhist_mh=0, nhist1_mh=0, nhist2_mh=0, nalloc_mh=-1,
     &  lastid_mh=0,lastind_mh=0,lun_index_mh=0,
     &  nhbooked_mh=0,nnbooked_mh=0,nntupmax_mh=0,nhistmax_mh=0,
     &  nntup_mh=0,lastnind_mh=0,lastnid_mh=0,
     &  npath_mh=0,kpath_mh=0,luns_mh(npathp_mh)=0,nfile_mh,kfile_mh=0,
     &  nscr7777=0,nscr30=0,nscr3700=0,nscr3601=0,nscr3600=0,nscr4600=0,
     &  nscr4700=0

      type(Histogram) hempty_mh
      type(Ntuple), dimension(:), allocatable :: tups_mh, tups_copy_mh
      type(Histogram), dimension(:), allocatable :: histos_mh, histos_copy_mh

      character(2048) :: chfiles_mh(nfilep_mh)=''

c!$OMP THREADPRIVATE(lastid_mh=0,lastind_mh=0,lastnind_mh=0,lastnid_mh=0)

      end module mhbook_mod
