*CMZ :  2.02/00 22/08/2023  09.03.52  by  Michael Scheer
*CMZ :  1.25/00 29/01/2018  11.03.33  by  Michael Scheer
*CMZ :  1.24/01 13/10/2017  08.16.08  by  Michael Scheer
*CMZ :  1.24/00 12/10/2017  14.26.18  by  Michael Scheer
*CMZ :  1.23/05 06/10/2017  08.44.11  by  Michael Scheer
*CMZ :  1.23/04 05/10/2017  09.27.56  by  Michael Scheer
*CMZ :  1.18/02 13/06/2017  12.25.04  by  Michael Scheer
*CMZ :  1.13/03 10/03/2017  12.16.46  by  Michael Scheer
*CMZ :  1.11/04 25/01/2017  16.55.27  by  Michael Scheer
*-- Author :    Michael Scheer   24/01/2017
      subroutine undumag_force_dipoles

      use undumagf90m
      use bpolyederf90m

      use commandlinef90m

      use commandlinef90m

      implicit none

      double precision :: eps=0.0001 !Meter
      double precision x,y,z,xm,xp,ym,yp,zm,zp,bxm,bym,bzm,bxp,byp,bzp,p(3),pn,
     &  dbxdx,dbxdy,dbxdz,
     &  dbydx,dbydy,dbydz,
     &  dbzdx,dbzdy,dbzdz

      integer imag,ifail,kmag,luno,kfail,moth,kmoth

      call util_zeit_kommentar(lun6,"Starting force calculations for dipole approximation")

      if (kallodip.ne.1) then
        x=0.0d0
        y=0.0d0
        z=0.0d0
        call undumag_dipoles_field(x,y,z,bxm,bym,bzm,ifail)
      endif

      fxdip=0.0d0
      fydip=0.0d0
      fzdip=0.0d0

      txdip=0.0d0
      tydip=0.0d0
      tzdip=0.0d0

      if (chforcemag.eq.'') goto 9999

      if (kforcemag.eq.0) then
        write(lun6,*)
        write(lun6,*)"*** Warning in undumag_force_dipoles: Magnet ",
     &    trim(chforcemag),
     &    " not found ***"
        write(lun6,*)"*** Check undumag_magnets.lis ***"
        write(lun6,*)
        goto 9999
      endif

      kmoth=bpebc(15,kforcemag)
      do kmag=1,ndipoles
        moth=nint(dipoles(9,kmag))
        if (moth.eq.kmoth) then
          call undumag_force_dipoles_nos(kmag,kfail)
        endif
      enddo

 9999 continue

      write(lun6,*)
      write(lun6,*)'* FxDip [N], FyDip [N], FzDip [N]:'
      write(lun6,'(3f15.4)')fxdip,fydip,fzdip
      write(lun6,*)
      write(lun6,*)'* TxDip, TyDip, TzDip [Nmm]:'
      write(lun6,'(3f15.4)')txdip*1000.0d0,tydip*1000.0d0,tzdip*1000.0d0
      write(lun6,*)

      open(newunit=luno,file='undumag.dfr')
      write(luno,*)'* FxDip, FyDip, FzDip [N]:'
      write(luno,'(3f15.4)')fxdip,fydip,fzdip
      write(luno,*)'* TxDip, TyDip, TzDip [Nmm]:'
      write(luno,'(3f15.4)')txdip*1000.0d0,tydip*1000.0d0,tzdip*1000.0d0
      close(luno)

      call util_zeit_kommentar(lun6,"Force calculations for dipole approximation finished")

      return
      end
