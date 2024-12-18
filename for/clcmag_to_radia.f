*CMZ :  2.05/06 11/10/2024  10.28.47  by  Michael Scheer
*CMZ :  2.03/00 22/08/2023  09.03.52  by  Michael Scheer
*CMZ :  2.02/01 09/02/2022  19.36.16  by  Michael Scheer
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
      subroutine clcmag_to_radia

      use undumagf90m
      use bpolyederf90m
      use commandlinef90m
      use magnets_structure

! Interface to radia notebook. It writes the geometry to snipplets for RADIA
! Restriction:
! Isotropic materials must have a magnetization less then 0.01
!
! RADIA coordinate system: y is longitudinal, z is vertical
! UNDUMAG: x is longitudinal, y is vertical

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

      double precision, dimension (:), allocatable :: x,y,z,brrec

      integer, dimension (:), allocatable :: khull,imatrec,imatiron,
     &  imatmagpol

      double precision :: gcen(3)

      real :: hfe,bfe

      integer ::
     &  nsymx,nsymy,nsymz,npoi,nmatfe,nlast,nfirst,nface,ncyl,ncolor,mater
     &  ,lunrad,lunfe,lmat,kproto,kpoi,k,ironmode,iron,imp,ipoi,imag,imat
     &  ,ifound,i,iface,ieof,nbr,nUnduMag,nUnduPol,nMagPolTot,lenout,lunproc,
     &  istat=0

      character(2048) cline,cbuff(10),cout
      character(64) chmat
      character(32) c32,c32x,c32y,c32z

      ncyl=0

      maxpoints=max(maxpoints,ncornmax*nplanmax)

      allocate(x(maxpoints),y(maxpoints),z(maxpoints),khull(maxpoints))
      allocate(brrec(nmagtot_t),imatrec(nmagtot_t),
     &  imatiron(nmagtot_t),imatmagpol(nmagtot_t))

      nbr=0
      nmatfe=0
      brrec=0.0d0
      imatrec=0

      do imag=1,nmagtot_t
        kproto=t_magcopy(imag)%kproto
        ifound=0
        if (t_magnets(kproto)%ispole.ne.0) then
          do i=1,nmatfe
            if (t_magnets(kproto)%imat.eq.imatiron(i)) then
              ifound=1
              imatmagpol(imag)=i
              exit
            endif
          enddo
          if (ifound.eq.1) then
            cycle
          endif
          nmatfe=nmatfe+1
          imatiron(nmatfe)=t_magnets(kproto)%imat
          imatmagpol(imag)=nmatfe
        else
          do i=1,nbr
            if (brrec(i).eq.t_magcopy(imag)%brn.and.
     &          t_magnets(kproto)%imat.eq.imatrec(i)) then
              ifound=1
              imatmagpol(imag)=i
              exit
            endif
          enddo
          if (ifound.eq.1) cycle
          nbr=nbr+1
          brrec(nbr)=t_magcopy(imag)%brn
          imatrec(nbr)=t_magnets(kproto)%imat
          imatmagpol(imag)=nbr
        endif
      enddo

      nsymx=1
      nsymy=1
      nsymz=1

      if (ixsymo.lt.0) nsymx=nsymx*2
      if (iysymo.lt.0) nsymy=nsymy*2
      if (izsymo.lt.0) nsymz=nsymz*2

      open(newunit=lunrad,file='undumag.nb')

      write(lunrad,*)' '
      write(lunrad,*)'(*-- Begin of lines generated by UNDUMAG --*)'
      write(lunrad,*)'(*-- Version ',trim(chuvers),' --*)'
      write(lunrad,*)' '
      write(lunrad,*)'<<Radia`; Off[General::"spell1"];'
      write(lunrad,*)' '
      write(cline,*)kundurun
      call util_string_trim(cline,nfirst,nlast)
      write(lunrad,*)'(*'
      call util_zeit_kommentar(lunrad,"Run: "//cline(nfirst:nlast))
      write(lunrad,*)'*)'

      write(lunrad,*)"(*"
      write(lunrad,*)"Comment: "//trim(unducomment)
      write(lunrad,*)"*)"
      write(lunrad,*)' '
      write(lunrad,*)'run = ',kundurun,";"
      write(lunrad,*)'comment = "'//trim(unducomment)//'";'
      write(lunrad,*)' '
      write(lunrad,*)'(* --- Variables of undumag.nam ---*)'
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

      write(lunrad,*)' '
      write(lunrad,*)'(* --- Materials of undumag.clc ---*)'
      write(lunrad,*)' '

      do i=1,nbr
        imat=imatrec(i)
        if (imat.eq.0) cycle
        write(c32,*)i
        if (matmaps(1,imat).eq.1) then
          if (matmaps(2,imat).eq.1) then
            write(c32x,'(f6.4)')bcmat(2,1,imat) !-1.0d0
            write(c32y,'(f6.4)')bcmat(3,1,imat) !-1.0d0
            write(c32z,'(g12.5)')brrec(i)
            write(lunrad,*)
     &        "UmatREC[" // trim(adjustl(c32)) // "] = radMatLin[{" //
     &        trim(adjustl(c32x)) // "," //
     &        trim(adjustl(c32y)) //
     &        "}," // trim(adjustl(c32z)) // "];"
          else
            print*,"*** Error in clcmag_to_radia: mode",
     &        matmaps(2,imat), " not yet implemented for REC material ",imat
            print*,""
            !stop "--- UNDUMAG aborted ---"
          endif
        else
          print*,"*** Error in clcmag_to_radia: Unknown material or mode", imat,
     &      matmaps(:,imat)
          print*,""
          !stop "--- UNDUMAG aborted ---"
        endif
      enddo

      do i=1,nmatfe
        imat=imatiron(i)
        if (imat.eq.0) cycle
        write(c32,*)i
        if (matmaps(1,imat).eq.2) then
          write(lunrad,*)
     &      "UmatIron[" // trim(adjustl(c32)) // "] = radMatSatIso[{"
          do k=2,nclcmat
            read(clcmat(k),*)iron,lmat,ironmode,cbuff(1)
            if (iron.eq.imat) then
              open(newunit=lunfe,file=trim(cbuff(1)),status='old')
              ieof=0
              do while (ieof.eq.0)
                call util_skip_comment_end(lunfe,ieof)
                if (ieof.ne.0) exit
                read(lunfe,*) hfe,bfe
                write(lunrad,*)"{",hfe,",",bfe,"},"
              enddo
              backspace(lunrad)
              write(lunrad,*)"{",hfe,",",bfe,"}"
              close(lunfe)
            endif
          enddo
          write(lunrad,*)"}];"
        else
          print*,"*** Error in clcmag_to_radia: Unknown material or mode", imat,
     &      matmaps(:,imat)
          print*,""
          !stop "--- UNDUMAG aborted ---"
        endif
      enddo

      write(lunrad,*)' '
      write(lunrad,*)'(* --- Poles and magnets of undumag.clc ---*)'
      write(lunrad,*)' '

      do imp=1,nmagtot_t

        kproto=t_magcopy(imp)%kproto

        ! radObjPolyhdr[
        ! {
        !   {x1,y1,z1}, ..., {xn,yn,zn} Corners
        ! },
        ! {
        !   {f1n1,f1n2,...}, ...., {f2n1,f2n2,...} Faces
        ! },
        ! {mx,my,mz}:{0,0,0}]

        nface=t_magnets(kproto)%nface
        ncolor=t_magnets(kproto)%icol

        write(lunrad,*)"   ",trim(t_magcopy(imp)%cnam)//" = radObjPolyhdr["
        write(lunrad,*)"        { (* List of points *)"

        gcen=t_magcopy(imp)%gcen

        do kpoi=1,t_magnets(kproto)%nhull
          write(c32x,*)sngl(t_magnets(kproto)%xhull(kpoi)+gcen(1)+xcentershift)
          call undumag_double_to_radia(c32x)
          write(c32y,*)sngl(t_magnets(kproto)%yhull(kpoi)+gcen(2))
          call undumag_double_to_radia(c32y)
          write(c32z,*)sngl(t_magnets(kproto)%zhull(kpoi)+gcen(3))
          call undumag_double_to_radia(c32z)
          if (kpoi.lt.t_magnets(kproto)%nhull) then
            write(lunrad,*)
     &        "           {",trim(c32z),",",trim(c32x),",",trim(c32y),"},"
          else
            write(lunrad,*)
     &        "           {",trim(c32z),",",trim(c32x),",",trim(c32y),"}"
          endif
        enddo !kpoi

        write(lunrad,*)"        }, (* End of list of points *)"

        write(lunrad,*)"        { (* List of faces *)"

        kpoi=1
        do iface=1,nface
          npoi=t_magnets(kproto)%kface(kpoi)
          cline="         {"
          do ipoi=kpoi+1,kpoi+npoi
            call util_string_append_num(cline,t_magnets(kproto)%kface(ipoi),
     &        nfirst,nlast)
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

        if (t_magnets(kproto)%IsPole.eq.0) then
          write(c32x,*)t_magnets(kproto)%br(1)
          call undumag_double_to_radia(c32x)
          write(c32y,*)t_magnets(kproto)%br(2)
          call undumag_double_to_radia(c32y)
          write(c32z,*)t_magnets(kproto)%br(3)
          call undumag_double_to_radia(c32z)
          write(lunrad,*)"         {",trim(c32z),",",trim(c32x),",",trim(c32y),"}"
          mater = imatmagpol(imp)
          write(chmat,*)mater
          call util_string_trim(chmat,nfirst,nlast)
          chmat="UmatREC["//chmat(nfirst:nlast)//"]"
        else
          write(lunrad,*)"         {0,0,0}"
          write(lunrad,*)"       ];"
          mater = imatmagpol(imp)
          write(chmat,*)mater
          call util_string_trim(chmat,nfirst,nlast)
          chmat="UmatIron["//chmat(nfirst:nlast)//"]"
        endif !Pole/Mag

        if (t_magnets(kproto)%nzdiv*t_magnets(kproto)%nydiv*t_magnets(kproto)%nxdiv.gt.1) then
          write(lunrad,*)' '
          write(cline,*)
     &      "      radObjDivMag["//trim(adjustl(t_magcopy(imp)%cnam))//", {{",
     &      t_magnets(kproto)%nzdiv,",",
     &      sngl(t_magnets(kproto)%zfracdiv),"},",t_magnets(kproto)%nxdiv,
     &      ",{",t_magnets(kproto)%nydiv,",",
     &      sngl(t_magnets(kproto)%yfracdiv),"}},kxkykz->Numb];"
          call util_remove_double_blanks(cline,cout,lenout)
          write(lunrad,*)'       ',cout(1:lenout)
          write(lunrad,*)' '
        endif

        write(lunrad,*)" "
        write(lunrad,*)"       radMatApl["//trim(adjustl(t_magcopy(imp)%cnam))//","//
     &    trim(chmat)//"];"

        if (ncolor.eq.2) then
          write(lunrad,*)"       radObjDrwAtr["//trim(adjustl(t_magcopy(imp)%cnam))//
     &      ",{1,0,0},0.0001];"
        else if (ncolor.eq.3) then
          write(lunrad,*)"       radObjDrwAtr["//trim(adjustl(t_magcopy(imp)%cnam))//
     &      ",{0,1,0},0.0001];"
        else if (ncolor.eq.4) then
          write(lunrad,*)"       radObjDrwAtr["//trim(adjustl(t_magcopy(imp)%cnam))//
     &      ",{0,0,1},0.0001];"
        else if (ncolor.eq.5) then
          write(lunrad,*)"       radObjDrwAtr["//trim(adjustl(t_magcopy(imp)%cnam))//
     &      ",{1,1,0},0.0001];"
        else if (ncolor.eq.6) then
          write(lunrad,*)"       radObjDrwAtr["//trim(adjustl(t_magcopy(imp)%cnam))//
     &      ",{1,0,1},0.0001];"
        else if (ncolor.eq.7) then
          write(lunrad,*)"       radObjDrwAtr["//trim(adjustl(t_magcopy(imp)%cnam))//
     &      ",{0,1,1},0.0001];"
        endif

        write(lunrad,*)" "

      enddo !imp=1,nmagtot_t

      do imp=1,nmagtot_t

        kproto=t_magcopy(imp)%kproto

        if (t_magnets(kproto)%IsPole.eq.0) then

          nUnduMag=nUnduMag+1

          write(c32,*)nUnduMag
          write(lunrad,*)"       nUnduMag = " // trim(adjustl(c32)) // ";"
          write(lunrad,*)"       UnduMag[" // trim(adjustl(c32)) // "] = " //
     &      trim(adjustl(t_magcopy(imp)%cnam)) // ";"

          nMagPolTot=nMagPolTot+1

          write(c32,*)nMagPolTot
          write(lunrad,*)"       nMagPolTot = " // trim(adjustl(c32)) // ";"
          write(lunrad,*)"       AllMagPols[" // trim(adjustl(c32)) // "] = " //
     &      trim(adjustl(t_magcopy(imp)%cnam)) // ";"
          write(lunrad,*)" "

          if (t_magcopy(imp)%cnam.eq.chforcemag) then
            write(lunrad,*)' '
            write(lunrad,*)"       iForceTyp = 1;"
            write(c32,*)nUnduMag
            write(lunrad,*)"      nForce = " // trim(adjustl(c32)) //";"
            write(lunrad,*)' '
            write(lunrad,*)" "
          endif

        else

          nUnduPol=nUnduPol+1
          write(c32,*)nUnduPol
          write(lunrad,*)"       nUnduPol = " // trim(adjustl(c32)) // ";"
          write(lunrad,*)"       UnduPol[" // trim(adjustl(c32)) // "] = " //
     &      trim(adjustl(t_magcopy(imp)%cnam)) // ";"

          nMagPolTot=nMagPolTot+1

          write(c32,*)nMagPolTot
          write(lunrad,*)"       nMagPolTot = " // trim(adjustl(c32)) // ";"
          write(lunrad,*)"       AllMagPols[" // trim(adjustl(c32)) // "] = " //
     &      trim(adjustl(t_magcopy(imp)%cnam)) // ";"
          write(lunrad,*)" "

          if (t_magcopy(imp)%cnam.eq.chforcemag) then
            write(lunrad,*)' '
            write(lunrad,*)"       iForceTyp = 1;"
            write(c32,*)nUnduPol
            write(lunrad,*)"      nForce = " // trim(adjustl(c32)) //";"
            write(lunrad,*)' '
            write(lunrad,*)" "
          endif

        endif !Pole/Mag

      enddo !imp=1,nmagtot_t

      deallocate(brrec)

      write(lunrad,*)' '
      write(lunrad,*)'(*-- End of lines generated by UNDUMAG --*)'
      write(lunrad,*)' '

      write(lunrad,*)' '
      write(lunrad,*)'(*-- Reading undumag_proc.nb and appending to undumag.nb --*)'
      write(lunrad,*)' '

      open(newunit=lunproc,file='undumag_proc.nb',status='old', iostat=istat)

      if (istat.ne.0) then
        write(lun6,*)"*** Error in clcmag_to_radia: File undumag_proc.nb not found ***"
      else

        ieof=0
        cline=''

        do while (ieof.eq.0)
          read(lunproc,'(a)',iostat=ieof) cline
          if (ieof.eq.0) write(lunrad,'(a)') trim(cline)
        enddo
        close(lunproc)

      endif

      flush(lunrad)
      close(lunrad)

      return
      end
