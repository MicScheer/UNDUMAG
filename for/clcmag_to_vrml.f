*CMZ :  2.04/00 16/01/2023  15.35.00  by  Michael Scheer
*CMZ :  2.02/01 03/02/2022  12.01.28  by  Michael Scheer
*-- Author :    Michael Scheer   22/10/2021
      subroutine clcmag_to_vrml(kxsym,kysym,kzsym,lunvrml)

      use bpolyederf90m
      use undumagf90m
      use commandlinef90m
      use magnets_structure

      implicit none

      Type(T_Magnet) tmag
      Type(T_Magnet_Copy) tmc
      Type(T_Voxel) tv
      Type(T_Voxel_Copy) tvc

      double precision gcen(3),x,y,z,scalx,scaly,scalz
      real color(3)

      integer nhull,korn,ipoi,ncorn,ncol,moth,lunvrml,kproto,iplan,
     &  icorn,ic,i,kxsym,kysym,kzsym,imag,iv

      character(4) chscal


      if (kxsym.ne.0.and.kysym.ne.0.and.kzsym.ne.0) then
        scalx=-1.0d0
        scaly=-1.0d0
        scalz=-1.0d0
        chscal="_111"
      else if (kxsym.ne.0.and.kysym.ne.0) then
        scalx=-1.0d0
        scaly=-1.0d0
        scalz=1.0d0
        chscal="_110"
      else if (kxsym.ne.0.and.kzsym.ne.0) then
        scalx=-1.0d0
        scaly=1.0d0
        scalz=-1.0d0
        chscal="_101"
      else if (kysym.ne.0.and.kzsym.ne.0) then
        scalx=1.0d0
        scaly=-1.0d0
        scalz=-1.0d0
        chscal="_011"
      else if (kxsym.ne.0) then
        scalx=-1.0d0
        scaly=1.0d0
        scalz=1.0d0
        chscal="_100"
      else if (kysym.ne.0.) then
        scalx=1.0d0
        scaly=-1.0d0
        scalz=1.0d0
        chscal="_010"
      else if (kzsym.ne.0.) then
        scalx=1.0d0
        scaly=1.0d0
        scalz=-1.0d0
        chscal="_001"
      else
        scalx=1.0d0
        scaly=1.0d0
        scalz=1.0d0
        chscal=""
      endif !syms

      if (ivrml.eq.1.or.ivrml.eq.2) then

        do moth=1,nmoth_t

          do ic=1,nmagtot_t

            tmc=t_magcopy(ic)
            if (tmc%cmoth.ne.tmc%cmoth) cycle

            write(lunvrml,*)" "
            write(lunvrml,*)"# --- ",trim(chmutts(moth)) // ":" //
     &        trim(tmc%cnam) // trim(chscal)
            write(lunvrml,*)" "

            kproto=tmc%kproto
            tmag=t_magnets(kproto)
            ncol=tmag%icol
            nhull=tmag%nhull
            gcen=tmc%gcen

            ! Magnet is represented by faces

            color(1)=1
            color(2)=1
            color(3)=1

            if (ncol.eq.1) then !black
              color(1)=0
              color(2)=0
              color(3)=0
            else if (ncol.eq.2) then !red
              color(1)=1
              color(2)=0
              color(3)=0
            else if (ncol.eq.3) then !bright green
              color(1)=0
              color(2)=1
              color(3)=0
            else if (ncol.eq.4) then !blue
              color(1)=0
              color(2)=0
              color(3)=1
            else if (ncol.eq.5) then !gelb
              color(1)=1.
              color(2)=1.
              color(3)=0
            else if (ncol.eq.6) then !magenta
              color(1)=1
              color(2)=0
              color(3)=1
            else if (ncol.eq.7) then !light blau
              color(1)=0
              color(2)=1
              color(3)=1
            else if (ncol.eq.8) then ! green
              color(1)=0.35
              color(2)=0.83
              color(3)=0.33
            else if (ncol.eq.9) then !dark blue
              color(1)=0.35
              color(2)=0.33
              color(3)=0.85
            endif

            write(lunvrml,*)"Shape{"
            write(lunvrml,*)" "
            write(lunvrml,*)"  appearance Appearance{"
            write(lunvrml,*)"    material Material{"

            write(lunvrml,*)"      diffuseColor", color

c            if (ivrml.eq.2)
c     &        write(lunvrml,*)"      transparency 0.8"

            write(lunvrml,*)"    } # End of Material"
            write(lunvrml,*)"  } # End of Appearance"
            write(lunvrml,*)" "

            write(lunvrml,*)"  geometry IndexedFaceSet {"
            write(lunvrml,*)
            write(lunvrml,*)"      coord Coordinate{"

            write(lunvrml,*)"         point ["

            do korn=1,nhull
              x=tmag%xhull(korn)+gcen(1)
              y=tmag%yhull(korn)+gcen(2)
              z=tmag%zhull(korn)+gcen(3)
              write(lunvrml,*)"          ",
     &          sngl(x*scalx),sngl(y*scaly),sngl(z*scalz),","
            enddo !nhull

            write(lunvrml,*)"         ] # End of point"

            write(lunvrml,*)"      } # End of Coordinate"
            write(lunvrml,*)

            write(lunvrml,*)"       coordIndex ["

            korn=1
            do iplan=1,tmag%nface
              ncorn=tmag%kface(korn)
              do icorn=1,ncorn
                korn=korn+1
                ipoi=tmag%kface(korn)-1
                if (icorn.eq.1) i=ipoi
                write(lunvrml,*)"              ",ipoi,","
              enddo
              write(lunvrml,*)"                 ",i,"-1,"
              korn=korn+1
            enddo !nplan

            write(lunvrml,*)"       ] # End of coordIndex"
            write(lunvrml,*)"  } # End of Geometry"
            write(lunvrml,*)"} # End of Shape"
            write(lunvrml,*)" "

          enddo !imag
        enddo !nmoth

      endif !ivrml.eq.1

! Magnets is represented by lines to visualize the segmentation

      if (ivrml.eq.2) then

        do iv=1,nvoxcopy_t

          tvc=t_voxcopy(iv)
          imag=tvc%kproto
          tmag=t_magnets(imag)
          ncol=tmag%icol
          tv=tmag%t_voxels(tvc%kvoxel)

          nhull=tv%nhull
          gcen=tvc%gcen

          if (
     &      tv%ixdiv.ne.1.and.tv%ixdiv.ne.tmag%nxdiv.and.
     &      tv%iydiv.ne.1.and.tv%iydiv.ne.tmag%nydiv.and.
     &      tv%izdiv.ne.1.and.tv%izdiv.ne.tmag%nzdiv) cycle

          write(lunvrml,*)"Shape{"
          write(lunvrml,*)" "

          write(lunvrml,*)"  appearance Appearance{"
          write(lunvrml,*)"    material Material{"
          write(lunvrml,*)"      diffuseColor 1 1 1 "
          write(lunvrml,*)"    } # End of Material"
          write(lunvrml,*)"  } # End of Appearance"
          write(lunvrml,*)" "

          write(lunvrml,*)"  geometry IndexedLineSet {"
          write(lunvrml,*)
          write(lunvrml,*)"      coord Coordinate{"

          write(lunvrml,*)"         point ["

          do korn=1,nhull
            x=tv%xhull(korn)+gcen(1)
            y=tv%yhull(korn)+gcen(2)
            z=tv%zhull(korn)+gcen(3)
            write(lunvrml,*)"          ",
     &        sngl(x*scalx),sngl(y*scaly),sngl(z*scalz),","
          enddo !nhull

          write(lunvrml,*)"         ] # End of point"

          write(lunvrml,*)"      } # End of Coordinate"
          write(lunvrml,*)

          write(lunvrml,*)
          write(lunvrml,*)"       coordIndex ["

          korn=1
          do iplan=1,tv%nface
            ncorn=tv%kface(korn)
            do icorn=1,ncorn
              korn=korn+1
              ipoi=tv%kface(korn)-1
              if (icorn.eq.1) i=ipoi
              write(lunvrml,*)"              ",ipoi,","
            enddo
            write(lunvrml,*)"                 ",i,"-1,"
            korn=korn+1
          enddo !nplan

          write(lunvrml,*)"       ] # End of coordIndex"

          write(lunvrml,*)"  } # End of Geometry"
          write(lunvrml,*)"} # End of Shape"
          write(lunvrml,*)" "

        enddo !nvoxels

      endif !vrml.eq.2


      return
      end
