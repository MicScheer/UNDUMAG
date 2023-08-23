*CMZ :  2.03/00 22/08/2023  09.03.52  by  Michael Scheer
*CMZ :  2.02/02 04/07/2022  11.26.34  by  Michael Scheer
*CMZ :  2.02/01 30/01/2022  08.43.28  by  Michael Scheer
*CMZ :  2.02/00 09/01/2021  12.08.32  by  Michael Scheer
*CMZ :  2.01/03 15/02/2019  12.47.44  by  Michael Scheer
*CMZ :  2.00/00 11/04/2018  15.45.12  by  Michael Scheer
*CMZ :  1.25/04 04/04/2018  11.40.25  by  Michael Scheer
*CMZ :  1.24/00 12/10/2017  13.27.59  by  Michael Scheer
*CMZ :  1.23/07 12/10/2017  08.25.43  by  Michael Scheer
*CMZ :  1.23/05 06/10/2017  09.00.40  by  Michael Scheer
*CMZ :  1.23/02 11/09/2017  15.35.11  by  Michael Scheer
*CMZ :  1.18/01 08/06/2017  14.20.17  by  Michael Scheer
*CMZ :  1.15/10 11/04/2017  11.14.47  by  Michael Scheer
*CMZ :  1.15/00 24/03/2017  10.28.46  by  Michael Scheer
*CMZ :  1.14/00 21/03/2017  17.33.35  by  Michael Scheer
*CMZ :  1.11/04 24/01/2017  10.15.10  by  Michael Scheer
*CMZ :  1.11/03 13/01/2017  15.10.38  by  Michael Scheer
*CMZ :  1.11/00 03/01/2017  16.16.45  by  Michael Scheer
*-- Author :    Michael Scheer   12/12/2016
      subroutine undumag_to_radia_old(krun,xsymmm,bpemag,mcornmax,mplanmax,mmag)

      use undumagf90m

! Interface to radia notebook. It writes the geometry to snipplets for RADIA
! Restriction:
! Isotropic materials must have a magnetization less then 0.01
!
! RADIA coordinate system: y is longitudinal, z is vertical
! UNDUMAG: x is longitudinal, y is vertical

      use commandlinef90m

      implicit none

*KEEP,PHYCONparam,T=F77.
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

      integer, parameter :: ncontp=1000

      double precision, dimension (:), allocatable :: x,y,z

      integer, dimension (:,:), allocatable :: kedge
      integer, dimension (:), allocatable :: khull,kface

      double precision bc,bcx,bcy,bcz,bn,
     &  vnor(3),dist,p1(3),p2(3),p3(3),
     &  x02(2),y02(2),z02(2),ydiv,ymin,ymax,space,ex,ey,ez,en,smx,smy,smz,
     &  br,bcxs,bcys,bczs,xsymmm,xx,yy,zz,
     &  w,h,xo,xi,zo,zi,ri,curr,currden,vx,vy,vz,alpha,vn
     &  ,radin,radout,height,angle,rmat(3,3),offx,offy,offz

      double precision bpemag(3,mcornmax,mplanmax,mmag)

      double precision :: tiny=1.0d-6

      real x0,y0,z0,xlen,ylen,zlen,yfrac

      integer lunrad,lunin,lunfi,kread,ipos(2,10),krun,
     &  istat,nxdiv,nydiv,nzdiv,nwords,ispecial,
     &  mi,me,nfirst,nlast,ncolor,mater,nmodules,narrays,imodule,iarray,
     &  nhull,nface,nedge,kfacelast,kfail,ipoi,npoi,luncorn,ieof,
     &  iface,kpoi,lface,npoimax,
     &  ifound,iplan,nplan,i,l,imag,
     &  nsymx,nsymy,nsymz,lsymx,lsymy,lsymz,ncont,icont,kcont,newcont,
     &  kradia,kforceradia,kolor,nz,ny,nphi,nr,ncyl,mcyl1,mcyl2,icorn,
     &  mcornmax,mplanmax,mmag,k,itranrot

      integer nUnduMag,nUnduPol,nMagPolTot,nUnduSpecMag,nUnduSpecPol

      character(2048) cline,cbuff(10)
      character(64) chmat
      character(32) c32x,c32y,c32z,c32t,chcont,chconts(ncontp),chmago

      ncyl=0

      maxpoints=max(maxpoints,ncornmax*nplanmax)

      allocate(x(maxpoints))
      allocate(y(maxpoints))
      allocate(z(maxpoints))

      allocate(khull(maxpoints))
      allocate(kedge(4,2*maxpoints-2))
      allocate(kface(5*maxpoints))

      nsymx=1
      nsymy=1
      nsymz=1
      if (ixsymo.lt.0) nsymx=nsymx*2
      if (iysymo.lt.0) nsymy=nsymy*2
      if (izsymo.lt.0) nsymz=nsymz*2
      ncont=0

      open(newunit=lunin,file='undumag.in',status='old')
      open(newunit=lunrad,file='undumag.nb')

      write(lunrad,*)' '
      write(lunrad,*)'(*-- Begin of lines generated by UNDUMAG --*)'
      write(lunrad,*)'(*-- Version ',trim(chuvers),' --*)'
      write(lunrad,*)' '
      write(lunrad,*)'<<Radia`; Off[General::"spell1"];'
      write(lunrad,*)' '
      write(cline,*)krun
      call util_string_trim(cline,nfirst,nlast)
      write(lunrad,*)'(*'
      call util_zeit_kommentar(lunrad,"Run: "//cline(nfirst:nlast))
      write(lunrad,*)'*)'

      kread=0
      br=-9999.0d0
      do while (kread.eq.0)
        read(lunin,'(a)')cline
        if (br.eq.-9999.0d0.and.cline(1:4).eq.'*Br:') then
          backspace(lunin)
          read(lunin,*)c32x,br
        endif
        if (cline(1:8).eq."*EndCalc") kread=1
      enddo

      call util_skip_comment(lunin)
      read(lunin,'(a)')cline ! comment line of undumag.in

      write(lunrad,*)"(*"
      write(lunrad,*)"Comment: "//trim(unducomment)
      write(lunrad,*)"*)"
      write(lunrad,*)' '
      write(lunrad,*)'run = ',krun,";"
      write(lunrad,*)'comment = "'//trim(unducomment)//'";'
      write(lunrad,*)' '
      write(lunrad,*)'iUnduXsym = ',ixsymo,";"
      write(lunrad,*)'iUnduYsym = ',iysymo,";"
      write(lunrad,*)'iUnduZsym = ',izsymo,";"
      write(lunrad,*)' '
      write(lunrad,*)'UnduSymX = ',xsym,";"
      write(lunrad,*)' '
      write(lunrad,*)"kDraw = ",iunduplot,";"
      write(lunrad,*)"unduplot_theta = ",unduplot_theta,";"
      write(lunrad,*)"unduplot_phi = ",unduplot_phi,";"
      write(lunrad,*)' '
      nUnduMag=0
      nUnduPol=0
      nUnduSpecMag=0
      nUnduSpecPol=0
      write(lunrad,*)"nUnduMag=0;"
      write(lunrad,*)"nUnduPol=0;"
      write(lunrad,*)"nUnduSpecMag=0;"
      write(lunrad,*)"nUnduSpecPol=0;"
      write(lunrad,*)' '
      write(c32x,*)br
      call undumag_double_to_radia(c32x)
      write(lunrad,*)"UnduBr = ",c32x," ;"
      write(lunrad,*)' '
      write(c32x,*)xcenter
      call undumag_double_to_radia(c32x)
      write(lunrad,*)"UnduXCenter = ",c32x," ;"
      write(c32x,*)xcentershift
      call undumag_double_to_radia(c32x)
      write(lunrad,*)"UnduXCenterShift = ",c32x," ;"
      write(lunrad,*)' '
      write(c32x,*)utorqcenx
      call undumag_double_to_radia(c32x)
      write(lunrad,*)"UnduTorqCenX = ",c32x," ;"
      write(c32x,*)utorqceny
      call undumag_double_to_radia(c32x)
      write(lunrad,*)"UnduTorqCenY = ",c32x," ;"
      write(c32x,*)utorqcenz
      call undumag_double_to_radia(c32x)
      write(lunrad,*)"UnduTorqCenZ = ",c32x," ;"
      write(lunrad,*)' '
      write(c32x,*)xmapmin
      call undumag_double_to_radia(c32x)
      write(lunrad,*)"UnduXMapMin = ",c32x," ;"
      write(c32x,*)xmapmax
      call undumag_double_to_radia(c32x)
      write(lunrad,*)"UnduXMapMax = ",c32x," ;"
      write(lunrad,*)"nUnduXMap = ",nxmap," ;"
      write(lunrad,*)' '
      write(c32x,*)ymapmin
      call undumag_double_to_radia(c32x)
      write(lunrad,*)"UnduYMapMin = ",c32x," ;"
      write(c32x,*)ymapmax
      call undumag_double_to_radia(c32x)
      write(lunrad,*)"UnduYMapMax = ",c32x," ;"
      write(lunrad,*)"nUnduYMap = ",nymap," ;"
      write(lunrad,*)' '
      write(c32x,*)zmapmin
      call undumag_double_to_radia(c32x)
      write(lunrad,*)"UnduZMapMin = ",c32x," ;"
      write(c32x,*)zmapmax
      call undumag_double_to_radia(c32x)
      write(lunrad,*)"UnduZMapMax = ",c32x," ;"
      write(lunrad,*)"nUnduZMap = ",nzmap," ;"
      write(lunrad,*)' '
      write(lunrad,*)' '
      write(lunrad,*)"nUnduNoPolMap = ",knopolmap," ;"
      write(lunrad,*)"nUnduNoMagMap = ",knomagmap," ;"
      write(lunrad,*)' '

      kread=0
      ispecial=0

      do while (kread.eq.0)

        call util_read_line(lunin,cbuff(1),nlast)

        itranrot=1
        do while (itranrot.eq.1)
          if (cbuff(1).eq.'Translate') then
            print*,"--- Warning in undumag_to_radia_old: Translate is not yet implemented..."
            write(lunrad,*)'(*'
            do i=1,3
              call util_read_line(lunin,cbuff(1),nlast)
            enddo
            write(lunrad,*)'*)'
            itranrot=1
          else if (cbuff(1).eq.'Rotate') then
            print*,"--- Warning in undumag_to_radia_old: Translate is not yet implemented..."
            write(lunrad,*)'(*'
            do i=1,5
              call util_read_line(lunin,cbuff(1),nlast)
            enddo
            write(lunrad,*)'*)'
            itranrot=1
          else
            itranrot=0
          endif
        enddo

        read(cbuff(1),*)x0,y0,z0

        if (x0.eq.-9999.0.and.y0.eq.-9999.0.and.z0.eq.-9999.0) then
          if (ispecial.eq.0) then

            ispecial=1

            write(lunrad,*)' '
            write(lunrad,*)'(*- End of regular magnets -*)'
            write(lunrad,*)' '

            call util_skip_comment(lunin)
            read(lunin,*)nmodules

            write(lunrad,*)" "
            write(lunrad,*)"nUnduModules = ",nmodules,";"
            write(lunrad,*)" "

            do imodule=1,nmodules

              call util_skip_comment(lunin)
              read(lunin,*)offx,offy,offz

              write(lunrad,*)"UnduOffx[",imodule,"] = ",offx,";"
              write(lunrad,*)"UnduOffy[",imodule,"] = ",offy,";"
              write(lunrad,*)"UnduOffz[",imodule,"] = ",offz,";"
              write(lunrad,*)" "

              call util_skip_comment(lunin)
              read(lunin,*)smx,smy,smz
              if (smx.ne.1.0d0.or.smy.ne.0.0d0.or.smz.ne.0.0d0) then
                write(lun6,*)
                write(lun6,*)"*** Warning in undumag_to_radia_old:  Rotation matrix not unit matrix ***"
                write(lun6,*)"module:",imodule
                write(lun6,*)"*** NOT YET IMPLEMENTED ***"
                write(lun6,*)
              endif

              call util_skip_comment(lunin)
              read(lunin,*)smx,smy,smz
              if (smx.ne.0.0d0.or.smy.ne.1.0d0.or.smz.ne.0.0d0) then
                write(lun6,*)
                write(lun6,*)"*** Warning in undumag_to_radia_old:  Rotation matrix not unit matrix ***"
                write(lun6,*)"module:",imodule
                write(lun6,*)"*** NOT YET IMPLEMENTED ***"
                write(lun6,*)
              endif

              call util_skip_comment(lunin)
              read(lunin,*)smx,smy,smz
              if (smx.ne.0.0d0.or.smy.ne.0.0d0.or.smz.ne.1.0d0) then
                write(lun6,*)
                write(lun6,*)"*** Warning in undumag_to_radia_old:  Rotation matrix not unit matrix ***"
                write(lun6,*)"module:",imodule
                write(lun6,*)"*** NOT YET IMPLEMENTED ***"
                write(lun6,*)
              endif

              call util_skip_comment(lunin)
              read(lunin,*)narrays
              write(lunrad,*)"nUnduArrays[",imodule,"] = ",narrays,";"
              write(lunrad,*)" "
              call util_skip_comment(lunin)
              read(lunin,*)space,ex,ey,ez
              en=sqrt(ex**2+ey**2+ez**2)
              ex=ex/en
              ey=ey/en
              ez=ez/en
              write(lunrad,*)"UnduArraySpace[",imodule,"] = ",space,";"
              write(lunrad,*)"UnduArrayEx[",imodule,"] = ",ex,";"
              write(lunrad,*)"UnduArrayEy[",imodule,"] = ",ey,";"
              write(lunrad,*)"UnduArrayEz[",imodule,"] = ",ez,";"
              call util_skip_comment(lunin)
              read(lunin,*)smx,smy,smz
              write(lunrad,*)"UnduArraySMx[",imodule,"] = ",smx,";"
              write(lunrad,*)"UnduArraySMy[",imodule,"] = ",smy,";"
              write(lunrad,*)"UnduArraySMz[",imodule,"] = ",smz,";"
              if (smx.ne.1.0d0.or.smy.ne.1.0d0.or.smz.ne.1.0d0) then
                write(lun6,*)
                write(lun6,*)"*** Warning in undumag_to_radia_old: Scaling of magnetic field of module array not 1. 1. 1. ***"
                write(lun6,*)"module:",imodule
                write(lun6,*)"*** NOT YET IMPLEMENTED ***"
                write(lun6,*)
              endif
            enddo !nmodules

            write(lunrad,*)" "

            cycle

          else !ispecial
            kread=1
            exit
          endif !ispecial

        endif !(x0.eq.-9999.0.and.y0.eq.-9999.0.and.z0.eq.-9999.0) then

        call util_read_line(lunin,cbuff(2),nlast)
        call util_read_line(lunin,cbuff(3),nlast)
        call util_read_line(lunin,cbuff(4),nlast)

        read(cbuff(2),*)bc,bcx,bcy,bcz,mater

        bcx=bcx*bc
        bcy=bcy*bc
        bcz=bcz*bc
        bc=abs(bc)
        bn=sqrt(bcx**2+bcy**2+bcz**2)

        bcx=bcx/bn
        bcy=bcy/bn
        bcz=bcz/bn

        kradia=0
        call util_string_split(cbuff(1),6,nwords,ipos,istat)

        if (nwords.eq.5) then
          kradia=1
          chmag=cbuff(1)(ipos(1,4):ipos(2,4))
          chcont=cbuff(1)(ipos(1,5):ipos(2,5))
          newcont=1
          do icont=1,ncont
            if (chconts(icont).eq.chcont) then
              newcont=0
              kcont=icont
              exit
            endif
          enddo
          if (newcont.eq.1) then
            ncont=ncont+1
            if (ncont.gt.ncontp) then
              write(lun6,*)"*** Error in undumag_to_radia_old: Dimension ncontp exceeded ***"
              write(lun6,*)'*** returning from undumag_to_radia ***'
              goto 9999
            endif
            kcont=ncont
            chconts(kcont)=trim(chcont)//'xyz'
            write(lunrad,*)" "
            write(lunrad,*)trim(chconts(ncont))//" =  radObjCnt[{}];"
            if (ispecial.eq.0) then
              if (abs(bc).gt.0.01) then
                nUnduMag=nUnduMag+1
                write(lunrad,*)"nUnduMag=",nUnduMag," ;"
                write(lunrad,*)"UnduMag[",nUnduMag," ] = ",
     &            trim(chconts(kcont)),";"
                write(lunrad,*)" "
                nMagPolTot=nMagPolTot+1
                write(lunrad,*)"nMagPolTot=",nMagPolTot,";"
                write(lunrad,*)"AllMagPols[",nMagPolTot," ] = ",
     &            trim(chconts(kcont)),";"
                if (chcont.eq.chforcemag) then
                  write(lunrad,*)' '
                  write(lunrad,*)"iForceTyp = 1;"
                  write(lunrad,*)"nForce = ",nUnduMag,";"
                  write(lunrad,*)' '
                endif
              else if (abs(bc).gt.0.0) then
                write(lunrad,*)" "
                nUnduPol=nUnduPol+1
                write(lunrad,*)"nUnduPol=",nUnduPol,";"
                write(lunrad,*)"UnduPol[",nUnduPol,"] = ",
     &            trim(chconts(kcont)),";"
                if (chcont.eq.chforcemag) then
                  write(lunrad,*)' '
                  write(lunrad,*)"iForceTyp = 2;"
                  write(lunrad,*)"nForce = ",nUnduPol,";"
                  write(lunrad,*)' '
                endif
              endif
            else !ispecial
              if (abs(bc).gt.0.01) then
                nUnduSpecMag=nUnduSpecMag+1
                write(lunrad,*)"nUnduSpecMag=",nUnduSpecMag,";"
                write(lunrad,*)"UnduSpecMag[",nUnduSpecMag,"] = ",
     &            trim(chconts(kcont)),";"
                write(lunrad,*)" "
                nMagPolTot=nMagPolTot+1
                write(lunrad,*)"nMagPolTot=",nMagPolTot,";"
                write(lunrad,*)"AllMagPols[",nMagPolTot,"] = ",
     &            trim(chconts(kcont)),";"
                if (chcont.eq.chforcemag) then
                  write(lunrad,*)' '
                  write(lunrad,*)"iForceTyp = 3;"
                  write(lunrad,*)"nForce = ",nUnduSpecMag,";"
                  write(lunrad,*)' '
                endif
              else if (abs(bc).gt.0.0) then
                write(lunrad,*)" "
                nUnduSpecPol=nUnduSpecPol+1
                write(lunrad,*)"nUnduSpecPol=",nUnduSpecPol,";"
                write(lunrad,*)"UnduSpecPol[",nUnduSpecPol,"] = ",
     &            trim(chconts(kcont)),";"
                if (chcont.eq.chforcemag) then
                  write(lunrad,*)' '
                  write(lunrad,*)"iForceTyp = 4;"
                  write(lunrad,*)"nForce = ",nUnduSpecPol,";"
                  write(lunrad,*)' '
                endif
              endif
            endif !ispecial
            write(lunrad,*)" "
          endif !newcont
        endif !nwords

        call util_string_split(cbuff(3),7,nwords,ipos,istat)

        if (
     &      cbuff(3)(ipos(1,1):ipos(2,1)).eq.'Block'.or.
     &      cbuff(3)(ipos(1,1):ipos(2,1)).eq.'-6'
     &      ) then

          if (kradia.eq.0) cycle

          read(cbuff(3)(ipos(1,2):ipos(2,2)),*) ncolor

          cline=cbuff(1)
          call util_string_split(cline,6,nwords,ipos,istat)

          yfrac=1.0
          read(cbuff(4),*,iostat=istat)xlen,ylen,zlen,nxdiv,nydiv,nzdiv,yfrac

          if (abs(bc).gt.0.0) then

            do lsymx=1,nsymx
              do lsymy=1,nsymy
                do lsymz=1,nsymz

                  if (abs(bc).lt.0.01) then
                    bcx=0.0d0
                    bcy=0.0d0
                    bcz=0.0d0
                  endif

                  if (lsymx.eq.1) then
                    write(c32x,*)x0
                    call util_string_append(chmag,'x',mi,me)
                  else if (lsymx.eq.2) then
                    write(c32x,*)xsymmm+(xsymmm-x0)
                    call util_string_append(chmag,'X',mi,me)
                  endif
                  call undumag_double_to_radia(c32x)

                  if (lsymy.eq.1) then
                    write(c32y,*) y0
                    call util_string_append(chmag,'y',mi,me)
                  else if (lsymy.eq.2) then
                    write(c32y,*) -y0
                    call util_string_append(chmag,'Y',mi,me)
                  endif
                  call undumag_double_to_radia(c32y)

                  if (lsymz.eq.1) then
                    write(c32z,*) z0
                    call util_string_append(chmag,'z',mi,me)
                  else if (lsymz.eq.2) then
                    write(c32z,*) -z0
                    call util_string_append(chmag,'Z',mi,me)
                  endif
                  call undumag_double_to_radia(c32z)

                  write(lunrad,*)chmag(mi:me)//" = radObjRecMag["
                  write(lunrad,*)"           {",trim(c32z),",",trim(c32x),",",trim(c32y),"},"
                  write(c32x,*)xlen
                  call undumag_double_to_radia(c32x)
                  write(c32y,*)ylen
                  call undumag_double_to_radia(c32y)
                  write(c32z,*)zlen
                  call undumag_double_to_radia(c32z)
                  write(lunrad,*)"           {",trim(c32z),",",trim(c32x),",",trim(c32y),"},"

                  bcxs=bcx
                  bcys=bcy
                  bczs=bcz
                  if (lsymx.eq.2) then
                    bcxs=-bcx
                  endif
                  if (lsymy.eq.2) then
                    bcxs=-bcx
                    bczs=-bcz
                  endif
                  if (lsymz.eq.2) then
                    bczs=-bcz
                  endif

                  call util_string_trim(chmag,mi,me)

                  write(lunrad,*)"               {",
     &              sngl(bczs),",",sngl(bcxs),",",sngl(bcys),"}"
                  write(lunrad,*)"       ];"

                  if (nzdiv*nydiv*nxdiv.gt.1) then
                    write(lunrad,*)' '
                    if (yfrac.eq.1.0) then
                      write(lunrad,*)' '
                      write(lunrad,*)
     &                  " radObjDivMag["//chmag(mi:me)//", {",
     &                  nzdiv,",",nxdiv,",",nydiv,"},kxkykz->Numb];"
                    else
                      write(lunrad,*)' '
                      write(lunrad,*)
     &                  " radObjDivMag["//chmag(mi:me)//", {",
     &                  nzdiv,",",nxdiv,",{",nydiv,",",yfrac,"}},kxkykz->Numb];"
                    endif
                    write(lunrad,*)' '
                  endif

                  write(chmat,*)mater
                  call util_string_trim(chmat,nfirst,nlast)
                  chmat="MatRec"//chmat(nfirst:nlast)

                  call util_string_trim(chmag,mi,me)

                  write(lunrad,*)" "
                  write(lunrad,*)"(***       radMatApl["//chmag(mi:me)//","//
     &              trim(chmat)//"]; ***)"
                  write(lunrad,*)" "

                  if (ncolor.eq.2) then
                    write(lunrad,*)"       radObjDrwAtr["//chmag(mi:me)//
     &                ",{1,0,0},0.0001];"
                  else if (ncolor.eq.3) then
                    write(lunrad,*)"       radObjDrwAtr["//chmag(mi:me)//
     &                ",{0,1,0},0.0001];"
                  else if (ncolor.eq.4) then
                    write(lunrad,*)"       radObjDrwAtr["//chmag(mi:me)//
     &                ",{0,0,1},0.0001];"
                  else if (ncolor.eq.5) then
                    write(lunrad,*)"       radObjDrwAtr["//chmag(mi:me)//
     &                ",{1,1,0},0.0001];"
                  else if (ncolor.eq.6) then
                    write(lunrad,*)"       radObjDrwAtr["//chmag(mi:me)//
     &                ",{1,0,1},0.0001];"
                  else if (ncolor.eq.7) then
                    write(lunrad,*)"       radObjDrwAtr["//chmag(mi:me)//
     &                ",{0,1,1},0.0001];"
                  endif

                  write(lunrad,*)" "
                  write(lunrad,*)"radObjAddToCnt["//chconts(kcont)//",{"//
     &              chmag(mi:me)//"}];"

                enddo !nsymz
              enddo !nsymz
            enddo !nsymz

          endif !(abs(bc).gt.0.0) then

        else if (kradia.ne.0.and.cbuff(3)(ipos(1,1):ipos(2,1)).eq.'Cyl') then

          if (kradia.eq.0) cycle

          ncyl=ncyl+1

          read(cbuff(3)(ipos(1,2):ipos(2,2)),*) ncolor
          read(cbuff(4),*)radin,radout,height,angle,nr,nphi,nzdiv
          call util_skip_comment(lunin)
          read(lunin,*)rmat(1,1:3)
          call util_skip_comment(lunin)
          read(lunin,*)rmat(2,1:3)
          call util_skip_comment(lunin)
          read(lunin,*)rmat(3,1:3)

          if (bc.ne.0.0) then

            mcyl1=magcyl(ncyl,1)
            mcyl2=magcyl(ncyl,2)

            chmago=chmag

            do imag=mcyl1,mcyl2

              iplan=2
              do icorn=1,4
                x(icorn)=bpemag(1,icorn,iplan,imag)
                y(icorn)=bpemag(2,icorn,iplan,imag)
                z(icorn)=bpemag(3,icorn,iplan,imag)
              enddo

              iplan=4
              do icorn=1,4
                x(4+icorn)=bpemag(1,icorn,iplan,imag)
                y(4+icorn)=bpemag(2,icorn,iplan,imag)
                z(4+icorn)=bpemag(3,icorn,iplan,imag)
              enddo

              npoi=8

              call util_convex_hull_3d(npoi,x,y,z,khull,kedge,kface,
     &          nhull,nedge,nface,kfacelast,tiny,istat)

              if (istat.ne.0) then
                write(lun6,*)"*** Error in undumag_to_radia_old: Subroutine util_convex_hull_3d failed for ",
     &            trim(chmag)
                write(lun6,*)'*** returning from undumag_to_radia ***'
                goto 9999
              endif

              bcxs=bcx
              bcys=bcy
              bczs=bcz

              write(c32x,*)imag
              call util_string_trim(c32x,nfirst,nlast)
              chmag=trim(chmag(ipos(1,1):ipos(1,2)))//c32x(nfirst:nlast)

              do lsymx=1,nsymx
                do lsymy=1,nsymy
                  do lsymz=1,nsymz

                    if (lsymx.eq.1) then
                      write(c32x,*)x0
                      call util_string_append(chmag,'x',mi,me)
                    else if (lsymx.eq.2) then
                      bcxs=-bcx
                      write(c32x,*)xsymmm+(xsymmm-x0)
                      call util_string_append(chmag,'X',mi,me)
                    endif
                    call undumag_double_to_radia(c32x)

                    if (lsymy.eq.1) then
                      write(c32y,*) y0
                      call util_string_append(chmag,'y',mi,me)
                    else if (lsymy.eq.2) then
                      bcxs=-bcx
                      bczs=-bcz
                      write(c32y,*) -y0
                      call util_string_append(chmag,'Y',mi,me)
                    endif
                    call undumag_double_to_radia(c32y)

                    if (lsymz.eq.1) then
                      write(c32z,*) z0
                      call util_string_append(chmag,'z',mi,me)
                    else if (lsymz.eq.2) then
                      bczs=-bcz
                      write(c32z,*) -z0
                      call util_string_append(chmag,'Z',mi,me)
                    endif
                    call undumag_double_to_radia(c32z)

                    ! radObjPolyhdr[
                    ! {
                    !   {x1,y1,z1}, ..., {xn,yn,zn} Corners
                    ! },
                    ! {
                    !   {f1n1,f1n2,...}, ...., {f2n1,f2n2,...} Faces
                    ! },
                    ! {mx,my,mz}:{0,0,0}]

                    write(lunrad,*)chmag(mi:me)//" = radObjPolyhdr["
                    write(lunrad,*)"        { (* List of points *)"

                    do kpoi=1,npoi
                      write(c32x,*)sngl(x0+x(kpoi))
                      call undumag_double_to_radia(c32x)
                      write(c32y,*)sngl(y0+y(kpoi))
                      call undumag_double_to_radia(c32y)
                      write(c32z,*)sngl(z0+z(kpoi))
                      call undumag_double_to_radia(c32z)
                      if (kpoi.lt.npoi) then
                        write(lunrad,*)
     &                    "           {",trim(c32z),",",trim(c32x),",",trim(c32y),"},"
                      else
                        write(lunrad,*)
     &                    "           {",trim(c32z),",",trim(c32x),",",trim(c32y),"}"
                      endif
                    enddo !kpoi

                    write(lunrad,*)"        }, (* End of list of points *)"

                    write(lunrad,*)"        { (* List of faces *)"

                    kpoi=1
                    do iface=1,nface
                      npoi=kface(kpoi)
                      cline="         {"
                      do ipoi=kpoi+1,kpoi+npoi
                        call util_string_append_num(cline,kface(ipoi),nfirst,nlast)
                        if (ipoi.lt.kpoi+npoi) then
                          call util_string_append(cline,',',nfirst,nlast)
                        else
                          call util_string_append(cline,'},',nfirst,nlast)
                        endif
                      enddo !ipoi
                      if (iface.lt.nface) then
                        write(lunrad,*)cline(1:nlast)
                        call util_string_append(cline,'},',nfirst,nlast)
                      else
                        write(lunrad,*)cline(1:nlast-1)
                      endif
                      kpoi=kpoi+npoi+1
                    enddo !nface

                    write(lunrad,*)"        }, (* End of list of faces *)"

                    write(c32x,*)bcx
                    call undumag_double_to_radia(c32x)
                    write(c32y,*)bcy
                    call undumag_double_to_radia(c32y)
                    write(c32z,*)bcz
                    call undumag_double_to_radia(c32z)

                    if (abs(bc).ge.0.01) then
                      write(lunrad,*)"         {",trim(c32z),",",trim(c32x),",",trim(c32y),"}"
                    else
                      write(lunrad,*)"         {0,0,0}"
                    endif
                    write(lunrad,*)"       ];"

                    write(chmat,*)mater
                    call util_string_trim(chmat,nfirst,nlast)
                    chmat="MatRec"//chmat(nfirst:nlast)

                    call util_string_trim(chmag,mi,me)

                    write(lunrad,*)" "
                    write(lunrad,*)"(***       radMatApl["//chmag(mi:me)//","//
     &                trim(chmat)//"]; ***)"
                    write(lunrad,*)" "

                    if (ncolor.eq.2) then
                      write(lunrad,*)"       radObjDrwAtr["//chmag(mi:me)//
     &                  ",{1,0,0},0.0001];"
                    else if (ncolor.eq.3) then
                      write(lunrad,*)"       radObjDrwAtr["//chmag(mi:me)//
     &                  ",{0,1,0},0.0001];"
                    else if (ncolor.eq.4) then
                      write(lunrad,*)"       radObjDrwAtr["//chmag(mi:me)//
     &                  ",{0,0,1},0.0001];"
                    else if (ncolor.eq.5) then
                      write(lunrad,*)"       radObjDrwAtr["//chmag(mi:me)//
     &                  ",{1,1,0},0.0001];"
                    else if (ncolor.eq.6) then
                      write(lunrad,*)"       radObjDrwAtr["//chmag(mi:me)//
     &                  ",{1,0,1},0.0001];"
                    else if (ncolor.eq.7) then
                      write(lunrad,*)"       radObjDrwAtr["//chmag(mi:me)//
     &                  ",{0,1,1},0.0001];"
                    endif

                    write(lunrad,*)" "
                    write(lunrad,*)"radObjAddToCnt["//chconts(kcont)//",{"//
     &                chmag(mi:me)//"}];"
                    write(lunrad,*)" "
                    write(lunrad,*)" "

                  enddo !izsym
                enddo !iysym
              enddo !ixsym

              chmag=chmago

            enddo !imag=mcyl1,mcyl2

          endif !bc.ne.0.0

        else !} Block

          read(cbuff(3)(ipos(1,2):ipos(2,2)),*) ncolor
          read(cbuff(3)(ipos(1,3):ipos(2,3)),*) nxdiv
          read(cbuff(3)(ipos(1,4):ipos(2,4)),*) nydiv
          read(cbuff(3)(ipos(1,5):ipos(2,5)),*) nzdiv
          yfrac=1.9
          if(nwords.gt.5) read(cbuff(3)(ipos(1,6):ipos(2,6)),*) yfrac

          if (kradia.ne.0.and.cbuff(3)(ipos(1,1):ipos(2,1)).eq.'File') then

            open(newunit=lunfi,file=trim(cbuff(4)),status='old')

            npoi=0

            do while (.true.)
              call util_read_line(lunfi,cline,nlast)
              if (nlast.gt.0) then
                npoi=npoi+1
                read(cline,*)x(npoi),y(npoi),z(npoi)
              else
                exit
              endif
            enddo

            close(lunfi)

          else if (kradia.ne.0.and.cbuff(3)(ipos(1,1):ipos(2,1)).eq.'Corners') then

            read(cbuff(4),*) npoi

            do ipoi=1,npoi
              call util_skip_comment(lunin)
              read(lunin,*)x(ipoi),y(ipoi),z(ipoi)
            enddo

          else ! polyhedron

            read(cbuff(3),*) nplan
            backspace(lunin)
            npoi=0

            do iplan=1,nplan

              call util_skip_comment(lunin)
              read(lunin,*)kpoi

              if (kpoi.lt.3) then
                write(lun6,*)"*** Error in undumag_to_radia_old: Plane has less then three points  ***"
                write(lun6,*)"Magnet and plane:", trim(chmag)," ",nplan
                write(lun6,*)'*** returning from undumag_to_radia ***'
                goto 9999
              endif

              do ipoi=1,kpoi
                npoi=npoi+1
                call util_skip_comment(lunin)
                read(lunin,*)x(npoi),y(npoi),z(npoi)
              enddo

            enddo !nplan

          endif ! File, Corners, Polyhedron

          call util_convex_hull_3d(npoi,x,y,z,khull,kedge,kface,
     &      nhull,nedge,nface,kfacelast,tiny,istat)

          if (istat.ne.0) then
            write(lun6,*)"*** Error in undumag_to_radia_old: Subroutine util_convex_hull_3d failed for ",
     &        trim(chmag)
            write(lun6,*)'*** returning from undumag_to_radia ***'
            goto 9999
          endif

          bcxs=bcx
          bcys=bcy
          bczs=bcz

          do lsymx=1,nsymx
            do lsymy=1,nsymy
              do lsymz=1,nsymz

                if (lsymx.eq.1) then
                  write(c32x,*)x0
                  call util_string_append(chmag,'x',mi,me)
                else if (lsymx.eq.2) then
                  bcxs=-bcx
                  write(c32x,*)xsymmm+(xsymmm-x0)
                  call util_string_append(chmag,'X',mi,me)
                endif
                call undumag_double_to_radia(c32x)

                if (lsymy.eq.1) then
                  write(c32y,*) y0
                  call util_string_append(chmag,'y',mi,me)
                else if (lsymy.eq.2) then
                  bcxs=-bcx
                  bczs=-bcz
                  write(c32y,*) -y0
                  call util_string_append(chmag,'Y',mi,me)
                endif
                call undumag_double_to_radia(c32y)

                if (lsymz.eq.1) then
                  write(c32z,*) z0
                  call util_string_append(chmag,'z',mi,me)
                else if (lsymz.eq.2) then
                  bczs=-bcz
                  write(c32z,*) -z0
                  call util_string_append(chmag,'Z',mi,me)
                endif
                call undumag_double_to_radia(c32z)

                ! radObjPolyhdr[
                ! {
                !   {x1,y1,z1}, ..., {xn,yn,zn} Corners
                ! },
                ! {
                !   {f1n1,f1n2,...}, ...., {f2n1,f2n2,...} Faces
                ! },
                ! {mx,my,mz}:{0,0,0}]

                write(lunrad,*)chmag(mi:me)//" = radObjPolyhdr["
                write(lunrad,*)"        { (* List of points *)"

                do kpoi=1,npoi
                  write(c32x,*)sngl(x0+x(kpoi))
                  call undumag_double_to_radia(c32x)
                  write(c32y,*)sngl(y0+y(kpoi))
                  call undumag_double_to_radia(c32y)
                  write(c32z,*)sngl(z0+z(kpoi))
                  call undumag_double_to_radia(c32z)
                  if (kpoi.lt.npoi) then
                    write(lunrad,*)
     & "           {",trim(c32z),",",trim(c32x),",",trim(c32y),"},"
                  else
                    write(lunrad,*)
     & "           {",trim(c32z),",",trim(c32x),",",trim(c32y),"}"
                  endif
                enddo !kpoi

                write(lunrad,*)"        }, (* End of list of points *)"
                write(lunrad,*)"        { (* List of faces *)"

                kpoi=1
                do iface=1,nface
                  npoi=kface(kpoi)
                  cline="         {"
                  do ipoi=kpoi+1,kpoi+npoi
                    call util_string_append_num(cline,kface(ipoi),nfirst,nlast)
                    if (ipoi.lt.kpoi+npoi) then
                      call util_string_append(cline,',',nfirst,nlast)
                    else
                      call util_string_append(cline,'},',nfirst,nlast)
                    endif
                  enddo !ipoi
                  if (iface.lt.nface) then
                    write(lunrad,*)cline(1:nlast)
                    call util_string_append(cline,'},',nfirst,nlast)
                  else
                    write(lunrad,*)cline(1:nlast-1)
                  endif
                  kpoi=kpoi+npoi+1
                enddo !nface

                write(lunrad,*)"        }, (* End of list of faces *)"

                write(c32x,*)bcx
                call undumag_double_to_radia(c32x)
                write(c32y,*)bcy
                call undumag_double_to_radia(c32y)
                write(c32z,*)bcz
                call undumag_double_to_radia(c32z)

                if (abs(bc).ge.0.01) then
                  write(lunrad,*)"         {",trim(c32z),",",trim(c32x),",",trim(c32y),"}"
                else
                  write(lunrad,*)"         {0,0,0}"
                endif
                write(lunrad,*)"       ];"

                if (nzdiv*nydiv*nxdiv.gt.1) then
                  write(lunrad,*)' '
                  if (yfrac.eq.1.0) then
                    write(lunrad,*)' '
                    write(lunrad,*)
     &                " radObjDivMag["//trim(chmag)//", {",
     &                nzdiv,",",nxdiv,",",nydiv,"},kxkykz->Numb];"
                  else
                    write(lunrad,*)' '
                    write(lunrad,*)
     &                " radObjDivMag["//trim(chmag)//", {",
     &                nzdiv,",",nxdiv,",{",nydiv,",",yfrac,"}},kxkykz->Numb];"
                  endif
                  write(lunrad,*)' '
                endif

                write(chmat,*)mater
                call util_string_trim(chmat,nfirst,nlast)
                chmat="MatRec"//chmat(nfirst:nlast)

                call util_string_trim(chmag,mi,me)

                write(lunrad,*)" "
                write(lunrad,*)"(***       radMatApl["//chmag(mi:me)//","//
     &            trim(chmat)//"]; ***)"
                write(lunrad,*)" "

                if (ncolor.eq.2) then
                  write(lunrad,*)"       radObjDrwAtr["//chmag(mi:me)//
     &              ",{1,0,0},0.0001];"
                else if (ncolor.eq.3) then
                  write(lunrad,*)"       radObjDrwAtr["//chmag(mi:me)//
     &              ",{0,1,0},0.0001];"
                else if (ncolor.eq.4) then
                  write(lunrad,*)"       radObjDrwAtr["//chmag(mi:me)//
     &              ",{0,0,1},0.0001];"
                else if (ncolor.eq.5) then
                  write(lunrad,*)"       radObjDrwAtr["//chmag(mi:me)//
     &              ",{1,1,0},0.0001];"
                else if (ncolor.eq.6) then
                  write(lunrad,*)"       radObjDrwAtr["//chmag(mi:me)//
     &              ",{1,0,1},0.0001];"
                else if (ncolor.eq.7) then
                  write(lunrad,*)"       radObjDrwAtr["//chmag(mi:me)//
     &              ",{0,1,1},0.0001];"
                endif

                write(lunrad,*)" "
                write(lunrad,*)"radObjAddToCnt["//chconts(kcont)//",{"//
     &            chmag(mi:me)//"}];"

              enddo !izsym
            enddo !iysym
          enddo !ixsym

        endif !Block, File, Corners etc.

      enddo !kread

      kforceradia=0
      if (iforce.ne.0.or.iforcedip.ne.0) then

        if (iforce.ne.9999.and.iforce.ne.0) then
          write(lun6,*)
          write(lun6,*)"*** Warning in undumag_to_radia_old: Iforce not 9999 ***"
          write(lun6,*)"Force calculations within RADIA refer to ",chforcemag
          write(lun6,*)
          write(lun6,*)"*** Be careful ***"
          write(lun6,*)
          write(lunrad,*)' '
          write(lunrad,*)'Print["*** Warning in undumag_to_radia_old: Iforce not 9999 ***"];'
          write(lunrad,*)'Print["*** Be careful ***"];'
          write(lunrad,*)' '
        endif

        write(lunrad,*)' '
        write(lunrad,*)'Print["Force calculations refer to ',
     &    chforcemag,'"];'
        write(lunrad,*)' '
        kforceradia=1
      endif

      write(lunrad,*)' '
      write(lunrad,*)'iUnduForce = ',kforceradia,";"
      write(lunrad,*)' '

      write(lunrad,*)" "
      write(lunrad,*)'UnduSetUp = radObjCnt[{}];'
      write(lunrad,*)' '
      write(lunrad,*)"Coils =  radObjCnt[{}];"
      write(lunrad,*)' '

      if (ncwires.gt.0) then

        if (nrace.eq.0.and.nrbar.eq.0) then
          write(lun6,*)'*** Warning in undumag_to_radia_old: Only racetrack coils with rectangular '
          write(lun6,*)'cross-section and rectangular bars are written to RADIA notebook ***'
        endif

        do i=1,nrace

          curr=race(1,i)
          xx=race(2,i)
          yy=race(3,i)
          zz=race(4,i)
          vx=race(5,i)
          vy=race(6,i)
          vz=race(7,i)
          vn=sqrt(vx**2+vy**2+vz**2)
          vx=vx/vn
          vy=vy/vn
          vz=vz/vn
          alpha=race(8,i)
          xo=race(9,i)/2.0d0
          zi=race(10,i)/2.0d0
          zo=race(11,i)/2.0d0
          ri=race(12,i)
          w=zo-zi
          h=race(13,i)
          currden=curr/w/h
          ny=race(14,i)
          nz=race(15,i)
          nphi=race(16,i)
          kolor=race(17,i)

          write(c32y,*)race(3,i)
          write(lunrad,*)" RectCoil[",i,"] = radObjRaceTrk["
          write(c32x,*)xx
          write(c32y,*)yy
          write(c32z,*)zz
          call undumag_double_to_radia(c32x)
          call undumag_double_to_radia(c32y)
          call undumag_double_to_radia(c32z)
          write(lunrad,*)"           {",trim(c32z),",",trim(c32x),",",trim(c32y),"},"
          write(c32x,*)ri
          write(c32y,*)ri+w
          write(lunrad,*)"           {",trim(c32x),",",trim(c32y),"},"
          write(c32x,*)(xo-w-ri)*2.0d0
          write(c32z,*)(zi-ri)*2.0d0
          write(lunrad,*)"           {",trim(c32z),",",trim(c32x),"},"
          write(c32y,*)h
          write(c32x,*)currden
          write(lunrad,*)"           ",trim(c32y),",",nphi,",",trim(c32x),","
          write(lunrad,*)' "man"];'
          write(c32x,*)xx
          write(c32y,*)yy
          write(c32z,*)zz
          call undumag_double_to_radia(c32x)
          call undumag_double_to_radia(c32y)
          call undumag_double_to_radia(c32z)
          write(lunrad,*)' '
          write(lunrad,*)'RotC = radTrfRot['
          write(lunrad,*)"           {",trim(c32z),",",trim(c32x),",",trim(c32y),"},"
          write(c32x,*)vx
          write(c32y,*)vy
          write(c32z,*)vz
          call undumag_double_to_radia(c32x)
          call undumag_double_to_radia(c32y)
          call undumag_double_to_radia(c32z)
          write(lunrad,*)"           {",trim(c32z),",",trim(c32x),",",trim(c32y),"},"
          write(c32z,*)alpha/180.0d0*pi1
          call undumag_double_to_radia(c32z)
          write(lunrad,*)"           ",trim(c32z),'];'
          write(lunrad,*)' '
          write(lunrad,*)" RectCoil[",i,"] = radTrfOrnt[RectCoil[",i,"],RotC];"
          write(lunrad,*)' '

          if (kolor.eq.2) then
            write(lunrad,*)"       radObjDrwAtr[RectCoil[",i,"]"//
     &        ",{1,0,0},0.0001];"
          else if (kolor.eq.3) then
            write(lunrad,*)"       radObjDrwAtr[RectCoil[",i,"]"//
     &        ",{0,1,0},0.0001];"
          else if (kolor.eq.4) then
            write(lunrad,*)"       radObjDrwAtr[RectCoil[",i,"]"//
     &        ",{0,0,1},0.0001];"
          else if (kolor.eq.5) then
            write(lunrad,*)"       radObjDrwAtr[RectCoil[",i,"]"//
     &        ",{1,1,0},0.0001];"
          else if (kolor.eq.6) then
            write(lunrad,*)"       radObjDrwAtr[RectCoil[",i,"]"//
     &        ",{1,0,1},0.0001];"
          else if (kolor.eq.7) then
            write(lunrad,*)"       radObjDrwAtr[RectCoil[",i,"]"//
     &        ",{0,1,1},0.0001];"
          endif

          write(lunrad,*)
          write(lunrad,*)'radObjAddToCnt[Coils,{RectCoil[',i,']}];'
          write(lunrad,*)

        enddo !nrace

        if (nrace.gt.0) then
          write(lunrad,*)'radObjAddToCnt[UnduSetUp,{Coils}];'
          write(lunrad,*)
        endif

        do i=1,nrbar
        enddo !nrbar

      endif !ncwires

      write(lunrad,*)' '
      write(lunrad,*)'(*-- End of lines generated by UNDUMAG --*)'
      write(lunrad,*)' '

9999  close(lunrad)
      close(lunin)

      deallocate(x,y,z)
      deallocate(khull,kedge,kface)

      return
      end
