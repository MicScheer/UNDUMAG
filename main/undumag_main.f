*CMZ :          11/10/2024  13.17.12  by  Michael Scheer
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

