*CMZ :  2.02/01 22/08/2023  09.03.52  by  Michael Scheer
*-- Author :    Michael Scheer   22/10/2021
      subroutine undumag_to_vrml

      use bpolyederf90m
      use undumagf90m
      use commandlinef90m
      use magnets_structure

      implicit none

      real xp1,yp1,zp1,xp2,yp2,zp2
      integer i,lunvrml,idatetime(8)

      character(2048) cline
      character(10) dtday,dttime,dtzone

      if (ivrml.eq.0) return

      if (coating.ne.0) then
        write(lun6,*)
        write(lun6,*)'*** Warning in undumag_to_vrml: Non-zero coating of magnets, items are bigger in reallity ***'
        write(lun6,*)
      endif

      write(lun6,*)
      write(lun6,*)'Writing geometry to CAD file undumag.wrl'
      write(lun6,*)

      call date_and_time(dtday,dttime,dtzone,idatetime)

      open(newunit=lunvrml,file="undumag.wrl")

      write(lunvrml,'(a)')"#VRML V2.0 utf8"
      write(lunvrml,*)
      cline=''
      write(cline,*) kundurun
      write(cline,'(a)')"# UNDUMAG: " // trim(cline) // ' '
     &  // trim(unducomment) // ' '
     &  // dttime(1:2) // ':' // dttime(3:4) // ':' // dttime(5:6) // ' '
     &  // dtday(7:8) // '.' // dtday(5:6) // '.' // dtday(3:4)
      write(lunvrml,*)trim(cline)
      write(lunvrml,*)" "

      if (ixsym.ne.0.and.iysym.ne.0.and.izsym.ne.0) then
        call clcmag_to_vrml(0,0,0,lunvrml)
        call clcmag_to_vrml(0,0,1,lunvrml)
        call clcmag_to_vrml(0,1,0,lunvrml)
        call clcmag_to_vrml(1,0,0,lunvrml)
        call clcmag_to_vrml(0,1,1,lunvrml)
        call clcmag_to_vrml(1,0,1,lunvrml)
        call clcmag_to_vrml(1,1,0,lunvrml)
        call clcmag_to_vrml(1,1,1,lunvrml)
      else if (ixsym.ne.0.and.iysym.ne.0) then
        call clcmag_to_vrml(0,0,0,lunvrml)
        call clcmag_to_vrml(0,1,0,lunvrml)
        call clcmag_to_vrml(1,0,0,lunvrml)
        call clcmag_to_vrml(1,1,0,lunvrml)
      else if (ixsym.ne.0.and.izsym.ne.0) then
        call clcmag_to_vrml(0,0,0,lunvrml)
        call clcmag_to_vrml(0,0,1,lunvrml)
        call clcmag_to_vrml(1,0,0,lunvrml)
        call clcmag_to_vrml(1,0,1,lunvrml)
      else if (iysym.ne.0.and.izsym.ne.0) then
        call clcmag_to_vrml(0,0,0,lunvrml)
        call clcmag_to_vrml(0,0,1,lunvrml)
        call clcmag_to_vrml(0,1,0,lunvrml)
        call clcmag_to_vrml(0,1,1,lunvrml)
      else if (ixsym.ne.0) then
        call clcmag_to_vrml(0,0,0,lunvrml)
        call clcmag_to_vrml(1,0,0,lunvrml)
      else if (iysym.ne.0) then
        call clcmag_to_vrml(0,0,0,lunvrml)
        call clcmag_to_vrml(0,1,0,lunvrml)
      else if (izsym.ne.0) then
        call clcmag_to_vrml(0,0,0,lunvrml)
        call clcmag_to_vrml(0,0,1,lunvrml)
      else
        call clcmag_to_vrml(0,0,0,lunvrml)
      endif

      if (ncwires.gt.0) then

        write(lunvrml,*)
        write(lunvrml,*)"# Coils"
        write(lunvrml,*)

        do i=1,ncwires
          xp1=sngl(wire(3,i))
          yp1=sngl(wire(4,i))
          zp1=sngl(wire(5,i))
          xp2=sngl(wire(6,i))
          yp2=sngl(wire(7,i))
          zp2=sngl(wire(8,i))
          if (((xp2-xp1)**2+(yp2-yp1)**2+(zp2-zp1)**2)*abs(wire(2,i)).lt.1.0d-12)
     &      cycle
          write(lunvrml,*)"Shape{"
          write(lunvrml,*)" "
          write(lunvrml,*)"  geometry IndexedLineSet {"
          write(lunvrml,*)
          write(lunvrml,*)"      coord Coordinate{"
          write(lunvrml,*)"         point ["
          write(lunvrml,*)"          ",xp1,yp1,zp1,","
          write(lunvrml,*)"          ",xp2,yp2,zp2,","
          write(lunvrml,*)"         ] # End of point"
          write(lunvrml,*)"      } # End of Coordinate"
          write(lunvrml,*)
          write(lunvrml,*)
          write(lunvrml,*)"       coordIndex ["
          write(lunvrml,*)"                 0,"
          write(lunvrml,*)"                 1, -1,"
          write(lunvrml,*)"       ] # End of coordIndex"

          write(lunvrml,*)"  } # End of Geometry"
          write(lunvrml,*)"} # End of Shape"
          write(lunvrml,*)" "

        enddo !ncwires

        write(lunvrml,*)
        write(lunvrml,*)"# End of Coils"
        write(lunvrml,*)

      endif !ncwires

      flush(lunvrml)
      close(lunvrml)

      write(lun6,*)
      write(lun6,*)'Done'
      write(lun6,*)

      return
      end
