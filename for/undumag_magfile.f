*CMZ :  2.04/19 18/09/2023  09.28.11  by  Michael Scheer
*CMZ :  2.02/01 22/08/2023  09.03.52  by  Michael Scheer
*-- Author :    Michael Scheer   20/10/2021
      subroutine undumag_magfile

      use commandlinef90m
      use bpolyederf90m
      use undumagf90m
      use magnets_structure

      implicit none

      integer, parameter :: ntupp=12

      double precision vmaglab(3),htup(ntupp),dx,dy,dz,bc
      integer luno,iplan,icorn,imag,kmag,i,ivox

      character(2048) cline

      type (T_Magnet_Copy) tmc
      type (T_Voxel_Copy) tvc

      open(newunit=luno,file='undumag.mag',form='formatted',status='unknown')

      write(luno,'(a)')trim(cundutit)

      do ivox=1,nvoxcopy_t

        tvc=t_voxcopy(ivox)
        kmag=tvc%kproto
        imag=tvc%kmagnet
        kvox=tvc%kvoxel
        tmc=t_magcopy(imag)

        do iplan=1,ibpeplan(ivox)
          do icorn=1,ibpecorn(iplan,ivox)

            htup(1)=bpebc(15,ivox)
            htup(2)=ivox
            htup(3)=ibpecol(ivox)
            htup(4)=iplan
            htup(5)=icorn*sign(1,ibpecorn(iplan,ivox))
            htup(6)=bpemag(1,icorn,iplan,ivox)
            htup(7)=bpemag(2,icorn,iplan,ivox)
            htup(8)=bpemag(3,icorn,iplan,ivox)
            htup(9)=bpebc(4,ivox)
            htup(10)=bpebc(5,ivox)
            htup(11)=bpebc(6,ivox)
            htup(12)=bpebc(9,ivox)

            vmaglab(1)=bpebc(4,ivox)
            vmaglab(2)=bpebc(5,ivox)
            vmaglab(3)=bpebc(6,ivox)

            bc=sqrt(vmaglab(1)**2+vmaglab(2)**2+vmaglab(3)**2)
            bpebc(7,ivox)=bc

            if (bc.ne.0.0d0.or.t_magnets(kmag)%IsPole.ne.0) then
              do i=5,12
                if (abs(htup(i)).lt.1.0d-15) htup(i)=1.0d-15
              enddo
              write(cline,'(5f9.0,6e15.5e3,f7.0)')htup
              cline=trim(cline) // " " // trim(tmc%cnam) // " " //
     &          trim(tmc%cmoth)
              if (t_magnets(kmag)%IsPole.ne.0) then
                write(luno,'(a)') trim(cline) // " 1"
              else
                write(luno,'(a)') trim(cline) // " 0"
              endif
            endif !BC

          enddo !ncorn
        enddo !nplan
      enddo !nvoxcopy_t

      if (kplsym.eq.0) then
        if (ixsymo.ne.0) then
          dx=xmax_t-xmin_t
          xmax_t=xmax_t-dx/2.
        endif
        if (iysymo.ne.0) then
          dy=ymax_t-ymin_t
          ymax_t=ymax_t-dy/2.
        endif
        if (izsymo.ne.0) then
          dz=zmax_t-zmin_t
          zmax_t=zmax_t-dz/2.
        endif
        goto 19
      endif

      if (ixsym.eq.0) then
        if (iysym.ne.0.and.izsym.eq.0) then
          do ivox=1,nvoxcopy_t

            tvc=t_voxcopy(ivox)
            kvox=tvc%kproto
            kmag=tvc%kproto
            imag=tvc%kmagnet
            tmc=t_magcopy(imag)

            do iplan=1,ibpeplan(ivox)
              do icorn=1,ibpecorn(iplan,ivox)

                htup(1)=bpebc(15,ivox)
                htup(2)=ivox+nvoxcopy_t
                htup(3)=ibpecol(ivox)
                htup(4)=iplan
                htup(5)=icorn*sign(1,ibpecorn(iplan,ivox))
                htup(6)=bpemag(1,icorn,iplan,ivox)
                htup(7)=-bpemag(2,icorn,iplan,ivox)
                htup(8)=bpemag(3,icorn,iplan,ivox)
                htup(9)=-bpebc(4,ivox)
                htup(10)=bpebc(5,ivox)
                htup(11)=-bpebc(6,ivox)
                htup(12)=bpebc(9,ivox)

                vmaglab(1)=bpebc(4,ivox)
                vmaglab(2)=bpebc(5,ivox)
                vmaglab(3)=bpebc(6,ivox)

                bc=sqrt(vmaglab(1)**2+vmaglab(2)**2+vmaglab(3)**2)
                bpebc(7,ivox)=bc

                if (bc.ne.0.0d0.or.t_magnets(kmag)%IsPole.ne.0) then
                  do i=5,12
                    if (abs(htup(i)).lt.1.0d-15) htup(i)=1.0d-15
                  enddo
                  write(cline,'(5f9.0,6e15.5e3,f7.0)')htup
                  cline=trim(cline) // " " // trim(tmc%cnam) // " " //
     &              trim(tmc%cmoth)
                  if (t_magnets(kmag)%IsPole.ne.0) then
                    write(luno,'(a)') trim(cline) // " 1"
                  else
                    write(luno,'(a)') trim(cline) // " 0"
                  endif
                endif !BC

              enddo !ncorn
            enddo !nplan
          enddo !nvoxcopy_t

        else if (izsym.ne.0.and.iysym.eq.0) then

          do ivox=1,nvoxcopy_t
            tvc=t_voxcopy(ivox)
            kvox=tvc%kproto
            kmag=tvc%kproto
            imag=tvc%kmagnet
            tmc=t_magcopy(imag)
            do iplan=1,ibpeplan(ivox)
              do icorn=1,ibpecorn(iplan,ivox)

                htup(1)=bpebc(15,ivox)
                htup(2)=ivox+nvoxcopy_t
                htup(3)=ibpecol(ivox)
                htup(4)=iplan
                htup(5)=icorn*sign(1,ibpecorn(iplan,ivox))
                htup(6)=bpemag(1,icorn,iplan,ivox)
                htup(7)=bpemag(2,icorn,iplan,ivox)
                htup(8)=-bpemag(3,icorn,iplan,ivox)
                htup(9)=bpebc(4,ivox)
                htup(10)=bpebc(5,ivox)
                htup(11)=-bpebc(6,ivox)
                htup(12)=bpebc(9,ivox)

                vmaglab(1)=bpebc(4,ivox)
                vmaglab(2)=bpebc(5,ivox)
                vmaglab(3)=bpebc(6,ivox)

                bc=sqrt(vmaglab(1)**2+vmaglab(2)**2+vmaglab(3)**2)
                bpebc(7,ivox)=bc

                if (bc.ne.0.0d0.or.t_magnets(kmag)%IsPole.ne.0) then
                  do i=5,12
                    if (abs(htup(i)).lt.1.0d-15) htup(i)=1.0d-15
                  enddo
                  write(cline,'(5f9.0,6e15.5e3,f7.0)')htup
                  cline=trim(cline) // " " // trim(tmc%cnam) // " " //
     &              trim(tmc%cmoth)
                  if (t_magnets(kmag)%IsPole.ne.0) then
                    write(luno,'(a)') trim(cline) // " 1"
                  else
                    write(luno,'(a)') trim(cline) // " 0"
                  endif
                endif !BC

              enddo !ncorn
            enddo !nplan
          enddo !nvoxcopy_t

        else if (izsym.ne.0.and.iysym.ne.0) then

          do ivox=1,nvoxcopy_t !izsym
            tvc=t_voxcopy(ivox)
            kvox=tvc%kproto
            kmag=tvc%kproto
            imag=tvc%kmagnet
            tmc=t_magcopy(imag)
            do iplan=1,ibpeplan(ivox)
              do icorn=1,ibpecorn(iplan,ivox)

                htup(1)=bpebc(15,ivox)
                htup(2)=ivox+nvoxcopy_t
                htup(3)=ibpecol(ivox)
                htup(4)=iplan
                htup(5)=icorn*sign(1,ibpecorn(iplan,ivox))
                htup(6)=bpemag(1,icorn,iplan,ivox)
                htup(7)=bpemag(2,icorn,iplan,ivox)
                htup(8)=-bpemag(3,icorn,iplan,ivox)
                htup(9)=bpebc(4,ivox)
                htup(10)=bpebc(5,ivox)
                htup(11)=-bpebc(6,ivox)
                htup(12)=bpebc(9,ivox)

                vmaglab(1)=bpebc(4,ivox)
                vmaglab(2)=bpebc(5,ivox)
                vmaglab(3)=bpebc(6,ivox)

                bc=sqrt(vmaglab(1)**2+vmaglab(2)**2+vmaglab(3)**2)
                bpebc(7,ivox)=bc

                if (bc.ne.0.0d0.or.t_magnets(kmag)%IsPole.ne.0) then
                  do i=5,12
                    if (abs(htup(i)).lt.1.0d-15) htup(i)=1.0d-15
                  enddo
                  write(cline,'(5f9.0,6e15.5e3,f7.0)')htup
                  cline=trim(cline) // " " // trim(tmc%cnam) // " " //
     &              trim(tmc%cmoth)
                  if (t_magnets(kmag)%IsPole.ne.0) then
                    write(luno,'(a)') trim(cline) // " 1"
                  else
                    write(luno,'(a)') trim(cline) // " 0"
                  endif
                endif !BC

              enddo !ncorn
            enddo !nplan
          enddo !nvoxcopy_t

          do ivox=1,nvoxcopy_t !iysym
            tvc=t_voxcopy(ivox)
            kvox=tvc%kproto
            kmag=tvc%kproto
            imag=tvc%kmagnet
            tmc=t_magcopy(imag)
            do iplan=1,ibpeplan(ivox)
              do icorn=1,ibpecorn(iplan,ivox)

                htup(1)=bpebc(15,ivox)
                htup(2)=ivox+2*nvoxcopy_t
                htup(3)=ibpecol(ivox)
                htup(4)=iplan
                htup(5)=icorn*sign(1,ibpecorn(iplan,ivox))
                htup(6)=bpemag(1,icorn,iplan,ivox)
                htup(7)=-bpemag(2,icorn,iplan,ivox)
                htup(8)=bpemag(3,icorn,iplan,ivox)
                htup(9)=-bpebc(4,ivox)
                htup(10)=bpebc(5,ivox)
                htup(11)=-bpebc(6,ivox)
                htup(12)=bpebc(9,ivox)

                vmaglab(1)=bpebc(4,ivox)
                vmaglab(2)=bpebc(5,ivox)
                vmaglab(3)=bpebc(6,ivox)

                bc=sqrt(vmaglab(1)**2+vmaglab(2)**2+vmaglab(3)**2)
                bpebc(7,ivox)=bc

                if (bc.ne.0.0d0.or.t_magnets(kmag)%IsPole.ne.0) then
                  do i=5,12
                    if (abs(htup(i)).lt.1.0d-15) htup(i)=1.0d-15
                  enddo
                  write(cline,'(5f9.0,6e15.5e3,f7.0)')htup
                  cline=trim(cline) // " " // trim(tmc%cnam) // " " //
     &              trim(tmc%cmoth)
                  if (t_magnets(kmag)%IsPole.ne.0) then
                    write(luno,'(a)') trim(cline) // " 1"
                  else
                    write(luno,'(a)') trim(cline) // " 0"
                  endif
                endif !BC

              enddo !ncorn
            enddo !nplan
          enddo !nvoxcopy_t

          do ivox=1,nvoxcopy_t !iysym and izsym
            tvc=t_voxcopy(ivox)
            kvox=tvc%kproto
            kmag=tvc%kproto
            imag=tvc%kmagnet
            tmc=t_magcopy(imag)
            do iplan=1,ibpeplan(ivox)
              do icorn=1,ibpecorn(iplan,ivox)

                htup(1)=bpebc(15,ivox)
                htup(2)=ivox+3*nvoxcopy_t
                htup(3)=ibpecol(ivox)
                htup(4)=iplan
                htup(5)=icorn*sign(1,ibpecorn(iplan,ivox))
                htup(6)=bpemag(1,icorn,iplan,ivox)
                htup(7)=-bpemag(2,icorn,iplan,ivox)
                htup(8)=-bpemag(3,icorn,iplan,ivox)
                htup(9)=-bpebc(4,ivox)
                htup(10)=bpebc(5,ivox)
                htup(11)=bpebc(6,ivox)
                htup(12)=bpebc(9,ivox)

                vmaglab(1)=bpebc(4,ivox)
                vmaglab(2)=bpebc(5,ivox)
                vmaglab(3)=bpebc(6,ivox)

                bc=sqrt(vmaglab(1)**2+vmaglab(2)**2+vmaglab(3)**2)
                bpebc(7,ivox)=bc

                if (bc.ne.0.0d0.or.t_magnets(kmag)%IsPole.ne.0) then
                  do i=5,12
                    if (abs(htup(i)).lt.1.0d-15) htup(i)=1.0d-15
                  enddo
                  write(cline,'(5f9.0,6e15.5e3,f7.0)')htup
                  cline=trim(cline) // " " // trim(tmc%cnam) // " " //
     &              trim(tmc%cmoth)
                  if (t_magnets(kmag)%IsPole.ne.0) then
                    write(luno,'(a)') trim(cline) // " 1"
                  else
                    write(luno,'(a)') trim(cline) // " 0"
                  endif
                endif !BC

              enddo !ncorn
            enddo !nplan
          enddo !nvoxcopy_t

        endif !(izsym.ne.) then

      else !:ixsym.eq.0

        if (iysym.eq.0.and.izsym.eq.0) then

          do ivox=1,nvoxcopy_t
            tvc=t_voxcopy(ivox)
            kvox=tvc%kproto
            kmag=tvc%kproto
            imag=tvc%kmagnet
            tmc=t_magcopy(imag)
            do iplan=1,ibpeplan(ivox)
              do icorn=1,ibpecorn(iplan,ivox)

                htup(1)=bpebc(15,ivox)
                htup(2)=ivox+nvoxcopy_t
                htup(3)=ibpecol(ivox)
                htup(4)=iplan
                htup(5)=icorn*sign(1,ibpecorn(iplan,ivox))
                htup(6)=xsymmm_t+(xsymmm_t-bpemag(1,icorn,iplan,ivox))
                htup(7)=bpemag(2,icorn,iplan,ivox)
                htup(8)=bpemag(3,icorn,iplan,ivox)
                htup(9)=-bpebc(4,ivox)
                htup(10)=bpebc(5,ivox)
                htup(11)=bpebc(6,ivox)
                htup(12)=bpebc(9,ivox)

                vmaglab(1)=bpebc(4,ivox)
                vmaglab(2)=bpebc(5,ivox)
                vmaglab(3)=bpebc(6,ivox)

                bc=sqrt(vmaglab(1)**2+vmaglab(2)**2+vmaglab(3)**2)
                bpebc(7,ivox)=bc

                if (bc.ne.0.0d0.or.t_magnets(kmag)%IsPole.ne.0) then
                  do i=5,12
                    if (abs(htup(i)).lt.1.0d-15) htup(i)=1.0d-15
                  enddo
                  write(cline,'(5f9.0,6e15.5e3,f7.0)')htup
                  cline=trim(cline) // " " // trim(tmc%cnam) // " " //
     &              trim(tmc%cmoth)
                  if (t_magnets(kmag)%IsPole.ne.0) then
                    write(luno,'(a)') trim(cline) // " 1"
                  else
                    write(luno,'(a)') trim(cline) // " 0"
                  endif
                endif !BC

              enddo !ncorn
            enddo !nplan
          enddo !nvoxcopy_t

        else if (iysym.ne.0.and.izsym.eq.0) then

          do ivox=1,nvoxcopy_t
            tvc=t_voxcopy(ivox)
            kvox=tvc%kproto
            kmag=tvc%kproto
            imag=tvc%kmagnet
            tmc=t_magcopy(imag)
            do iplan=1,ibpeplan(ivox)
              do icorn=1,ibpecorn(iplan,ivox)

                htup(1)=bpebc(15,ivox)
                htup(2)=ivox+nvoxcopy_t
                htup(3)=ibpecol(ivox)
                htup(4)=iplan
                htup(5)=icorn*sign(1,ibpecorn(iplan,ivox))
                htup(6)=2.0d0*xsymmm_t-bpemag(1,icorn,iplan,ivox)
                htup(7)=-bpemag(2,icorn,iplan,ivox)
                htup(8)=bpemag(3,icorn,iplan,ivox)
                htup(9)=bpebc(4,ivox)
                htup(10)=bpebc(5,ivox)
                htup(11)=-bpebc(6,ivox)
                htup(12)=bpebc(9,ivox)

                vmaglab(1)=bpebc(4,ivox)
                vmaglab(2)=bpebc(5,ivox)
                vmaglab(3)=bpebc(6,ivox)

                bc=sqrt(vmaglab(1)**2+vmaglab(2)**2+vmaglab(3)**2)
                bpebc(7,ivox)=bc

                if (bc.ne.0.0d0.or.t_magnets(kmag)%IsPole.ne.0) then
                  do i=5,12
                    if (abs(htup(i)).lt.1.0d-15) htup(i)=1.0d-15
                  enddo
                  write(cline,'(5f9.0,6e15.5e3,f7.0)')htup
                  cline=trim(cline) // " " // trim(tmc%cnam) // " " //
     &              trim(tmc%cmoth)
                  if (t_magnets(kmag)%IsPole.ne.0) then
                    write(luno,'(a)') trim(cline) // " 1"
                  else
                    write(luno,'(a)') trim(cline) // " 0"
                  endif
                endif !BC

              enddo !ncorn
            enddo !nplan
          enddo !nvoxcopy_t

          do ivox=1,nvoxcopy_t
            tvc=t_voxcopy(ivox)
            kvox=tvc%kproto
            kmag=tvc%kproto
            imag=tvc%kmagnet
            tmc=t_magcopy(imag)
            do iplan=1,ibpeplan(ivox)
              do icorn=1,ibpecorn(iplan,ivox)

                htup(1)=bpebc(15,ivox)
                htup(2)=ivox+2*nvoxcopy_t
                htup(3)=ibpecol(ivox)
                htup(4)=iplan
                htup(5)=icorn*sign(1,ibpecorn(iplan,ivox))
                htup(6)=bpemag(1,icorn,iplan,ivox)
                htup(7)=-bpemag(2,icorn,iplan,ivox)
                htup(8)=bpemag(3,icorn,iplan,ivox)
                htup(9)=-bpebc(4,ivox)
                htup(10)=bpebc(5,ivox)
                htup(11)=-bpebc(6,ivox)
                htup(12)=bpebc(9,ivox)

                vmaglab(1)=bpebc(4,ivox)
                vmaglab(2)=bpebc(5,ivox)
                vmaglab(3)=bpebc(6,ivox)

                bc=sqrt(vmaglab(1)**2+vmaglab(2)**2+vmaglab(3)**2)
                bpebc(7,ivox)=bc

                if (bc.ne.0.0d0.or.t_magnets(kmag)%IsPole.ne.0) then
                  do i=5,12
                    if (abs(htup(i)).lt.1.0d-15) htup(i)=1.0d-15
                  enddo
                  write(cline,'(5f9.0,6e15.5e3,f7.0)')htup
                  cline=trim(cline) // " " // trim(tmc%cnam) // " " //
     &              trim(tmc%cmoth)
                  if (t_magnets(kmag)%IsPole.ne.0) then
                    write(luno,'(a)') trim(cline) // " 1"
                  else
                    write(luno,'(a)') trim(cline) // " 0"
                  endif
                endif !BC

              enddo !ncorn
            enddo !nplan
          enddo !nvoxcopy_t

          do ivox=1,nvoxcopy_t
            tvc=t_voxcopy(ivox)
            kvox=tvc%kproto
            kmag=tvc%kproto
            imag=tvc%kmagnet
            tmc=t_magcopy(imag)
            do iplan=1,ibpeplan(ivox)
              do icorn=1,ibpecorn(iplan,ivox)

                htup(1)=bpebc(15,ivox)
                htup(2)=ivox+3*nvoxcopy_t
                htup(3)=ibpecol(ivox)
                htup(4)=iplan
                htup(5)=icorn*sign(1,ibpecorn(iplan,ivox))
                htup(6)=2.0d0*xsymmm_t-bpemag(1,icorn,iplan,ivox)
                htup(7)=bpemag(2,icorn,iplan,ivox)
                htup(8)=bpemag(3,icorn,iplan,ivox)
                htup(9)=-bpebc(4,ivox)
                htup(10)=bpebc(5,ivox)
                htup(11)=bpebc(6,ivox)
                htup(12)=bpebc(9,ivox)

                vmaglab(1)=bpebc(4,ivox)
                vmaglab(2)=bpebc(5,ivox)
                vmaglab(3)=bpebc(6,ivox)

                bc=sqrt(vmaglab(1)**2+vmaglab(2)**2+vmaglab(3)**2)
                bpebc(7,ivox)=bc

                if (bc.ne.0.0d0.or.t_magnets(kmag)%IsPole.ne.0) then
                  do i=5,12
                    if (abs(htup(i)).lt.1.0d-15) htup(i)=1.0d-15
                  enddo
                  write(cline,'(5f9.0,6e15.5e3,f7.0)')htup
                  cline=trim(cline) // " " // trim(tmc%cnam) // " " //
     &              trim(tmc%cmoth)
                  if (t_magnets(kmag)%IsPole.ne.0) then
                    write(luno,'(a)') trim(cline) // " 1"
                  else
                    write(luno,'(a)') trim(cline) // " 0"
                  endif
                endif !BC

              enddo !ncorn
            enddo !nplan
          enddo !nvoxcopy_t

        else if (izsym.ne.0.and.iysym.eq.0) then

          do ivox=1,nvoxcopy_t
            tvc=t_voxcopy(ivox)
            kvox=tvc%kproto
            kmag=tvc%kproto
            imag=tvc%kmagnet
            tmc=t_magcopy(imag)
            do iplan=1,ibpeplan(ivox)
              do icorn=1,ibpecorn(iplan,ivox)

                !x1y1z2
                htup(1)=bpebc(15,ivox)
                htup(2)=ivox+nvoxcopy_t
                htup(3)=ibpecol(ivox)
                htup(4)=iplan
                htup(5)=icorn*sign(1,ibpecorn(iplan,ivox))
                htup(6)=bpemag(1,icorn,iplan,ivox)
                htup(7)=bpemag(2,icorn,iplan,ivox)
                htup(8)=-bpemag(3,icorn,iplan,ivox)
                htup(9)=bpebc(4,ivox)
                htup(10)=bpebc(5,ivox)
                htup(11)=-bpebc(6,ivox)
                htup(12)=bpebc(9,ivox)

                vmaglab(1)=bpebc(4,ivox)
                vmaglab(2)=bpebc(5,ivox)
                vmaglab(3)=bpebc(6,ivox)

                bc=sqrt(vmaglab(1)**2+vmaglab(2)**2+vmaglab(3)**2)
                bpebc(7,ivox)=bc

                if (bc.ne.0.0d0.or.t_magnets(kmag)%IsPole.ne.0) then
                  do i=5,12
                    if (abs(htup(i)).lt.1.0d-15) htup(i)=1.0d-15
                  enddo
                  write(cline,'(5f9.0,6e15.5e3,f7.0)')htup
                  cline=trim(cline) // " " // trim(tmc%cnam) // " " //
     &              trim(tmc%cmoth)
                  if (t_magnets(kmag)%IsPole.ne.0) then
                    write(luno,'(a)') trim(cline) // " 1"
                  else
                    write(luno,'(a)') trim(cline) // " 0"
                  endif
                endif !BC

              enddo !ncorn
            enddo !nplan
          enddo !nvoxcopy_t

          do ivox=1,nvoxcopy_t
            tvc=t_voxcopy(ivox)
            kvox=tvc%kproto
            kmag=tvc%kproto
            imag=tvc%kmagnet
            tmc=t_magcopy(imag)
            do iplan=1,ibpeplan(ivox)
              do icorn=1,ibpecorn(iplan,ivox)

                !x2y1z1
                htup(1)=bpebc(15,ivox)
                htup(2)=ivox+2*nvoxcopy_t
                htup(3)=ibpecol(ivox)
                htup(4)=iplan
                htup(5)=icorn*sign(1,ibpecorn(iplan,ivox))
                htup(6)=xsymmm_t+(xsymmm_t-bpemag(1,icorn,iplan,ivox))
                htup(7)=bpemag(2,icorn,iplan,ivox)
                htup(8)=bpemag(3,icorn,iplan,ivox)
                htup(9)=-bpebc(4,ivox)
                htup(10)=bpebc(5,ivox)
                htup(11)=bpebc(6,ivox)
                htup(12)=bpebc(9,ivox)

                vmaglab(1)=bpebc(4,ivox)
                vmaglab(2)=bpebc(5,ivox)
                vmaglab(3)=bpebc(6,ivox)

                bc=sqrt(vmaglab(1)**2+vmaglab(2)**2+vmaglab(3)**2)
                bpebc(7,ivox)=bc

                if (bc.ne.0.0d0.or.t_magnets(kmag)%IsPole.ne.0) then
                  do i=5,12
                    if (abs(htup(i)).lt.1.0d-15) htup(i)=1.0d-15
                  enddo
                  write(cline,'(5f9.0,6e15.5e3,f7.0)')htup
                  cline=trim(cline) // " " // trim(tmc%cnam) // " " //
     &              trim(tmc%cmoth)
                  if (t_magnets(kmag)%IsPole.ne.0) then
                    write(luno,'(a)') trim(cline) // " 1"
                  else
                    write(luno,'(a)') trim(cline) // " 0"
                  endif
                endif !BC

              enddo !ncorn
            enddo !nplan
          enddo !nvoxcopy_t

          do ivox=1,nvoxcopy_t
            tvc=t_voxcopy(ivox)
            kvox=tvc%kproto
            kmag=tvc%kproto
            imag=tvc%kmagnet
            tmc=t_magcopy(imag)
            do iplan=1,ibpeplan(ivox)
              do icorn=1,ibpecorn(iplan,ivox)

                !x2y1z2
                htup(1)=bpebc(15,ivox)
                htup(2)=ivox+3*nvoxcopy_t
                htup(3)=ibpecol(ivox)
                htup(4)=iplan
                htup(5)=icorn*sign(1,ibpecorn(iplan,ivox))
                htup(6)=xsymmm_t+(xsymmm_t-bpemag(1,icorn,iplan,ivox))
                htup(7)=bpemag(2,icorn,iplan,ivox)
                htup(8)=-bpemag(3,icorn,iplan,ivox)
                htup(9)=-bpebc(4,ivox)
                htup(10)=bpebc(5,ivox)
                htup(11)=-bpebc(6,ivox)
                htup(12)=bpebc(9,ivox)

                vmaglab(1)=bpebc(4,ivox)
                vmaglab(2)=bpebc(5,ivox)
                vmaglab(3)=bpebc(6,ivox)

                bc=sqrt(vmaglab(1)**2+vmaglab(2)**2+vmaglab(3)**2)
                bpebc(7,ivox)=bc

                if (bc.ne.0.0d0.or.t_magnets(kmag)%IsPole.ne.0) then
                  do i=5,12
                    if (abs(htup(i)).lt.1.0d-15) htup(i)=1.0d-15
                  enddo
                  write(cline,'(5f9.0,6e15.5e3,f7.0)')htup
                  cline=trim(cline) // " " // trim(tmc%cnam) // " " //
     &              trim(tmc%cmoth)
                  if (t_magnets(kmag)%IsPole.ne.0) then
                    write(luno,'(a)') trim(cline) // " 1"
                  else
                    write(luno,'(a)') trim(cline) // " 0"
                  endif
                endif !BC

              enddo !ncorn
            enddo !nplan
          enddo !nvoxcopy_t

        else if (izsym.ne.0.and.iysym.ne.0) then

          do ivox=1,nvoxcopy_t !izsym
            tvc=t_voxcopy(ivox)
            kvox=tvc%kproto
            kmag=tvc%kproto
            imag=tvc%kmagnet
            tmc=t_magcopy(imag)
            do iplan=1,ibpeplan(ivox)
              do icorn=1,ibpecorn(iplan,ivox)

                !x1y1z2
                htup(1)=bpebc(15,ivox)
                htup(2)=ivox+nvoxcopy_t
                htup(3)=ibpecol(ivox)
                htup(4)=iplan
                htup(5)=icorn*sign(1,ibpecorn(iplan,ivox))
                htup(6)=bpemag(1,icorn,iplan,ivox)
                htup(7)=bpemag(2,icorn,iplan,ivox)
                htup(8)=-bpemag(3,icorn,iplan,ivox)
                htup(9)=bpebc(4,ivox)
                htup(10)=bpebc(5,ivox)
                htup(11)=-bpebc(6,ivox)
                htup(12)=bpebc(9,ivox)

                vmaglab(1)=bpebc(4,ivox)
                vmaglab(2)=bpebc(5,ivox)
                vmaglab(3)=bpebc(6,ivox)

                bc=sqrt(vmaglab(1)**2+vmaglab(2)**2+vmaglab(3)**2)
                bpebc(7,ivox)=bc

                if (bc.ne.0.0d0.or.t_magnets(kmag)%IsPole.ne.0) then
                  do i=5,12
                    if (abs(htup(i)).lt.1.0d-15) htup(i)=1.0d-15
                  enddo
                  write(cline,'(5f9.0,6e15.5e3,f7.0)')htup
                  cline=trim(cline) // " " // trim(tmc%cnam) // " " //
     &              trim(tmc%cmoth)
                  if (t_magnets(kmag)%IsPole.ne.0) then
                    write(luno,'(a)') trim(cline) // " 1"
                  else
                    write(luno,'(a)') trim(cline) // " 0"
                  endif
                endif !BC

              enddo !ncorn
            enddo !nplan
          enddo !nvoxcopy_t

          do ivox=1,nvoxcopy_t !iysym
            tvc=t_voxcopy(ivox)
            kvox=tvc%kproto
            kmag=tvc%kproto
            imag=tvc%kmagnet
            tmc=t_magcopy(imag)
            do iplan=1,ibpeplan(ivox)
              do icorn=1,ibpecorn(iplan,ivox)

                !x1y2z2
                htup(1)=bpebc(15,ivox)
                htup(2)=ivox+2*nvoxcopy_t
                htup(3)=ibpecol(ivox)
                htup(4)=iplan
                htup(5)=icorn*sign(1,ibpecorn(iplan,ivox))
                htup(6)=bpemag(1,icorn,iplan,ivox)
                htup(7)=-bpemag(2,icorn,iplan,ivox)
                htup(8)=bpemag(3,icorn,iplan,ivox)
                htup(9)=-bpebc(4,ivox)
                htup(10)=bpebc(5,ivox)
                htup(11)=-bpebc(6,ivox)
                htup(12)=bpebc(9,ivox)

                vmaglab(1)=bpebc(4,ivox)
                vmaglab(2)=bpebc(5,ivox)
                vmaglab(3)=bpebc(6,ivox)

                bc=sqrt(vmaglab(1)**2+vmaglab(2)**2+vmaglab(3)**2)
                bpebc(7,ivox)=bc

                if (bc.ne.0.0d0.or.t_magnets(kmag)%IsPole.ne.0) then
                  do i=5,12
                    if (abs(htup(i)).lt.1.0d-15) htup(i)=1.0d-15
                  enddo
                  write(cline,'(5f9.0,6e15.5e3,f7.0)')htup
                  cline=trim(cline) // " " // trim(tmc%cnam) // " " //
     &              trim(tmc%cmoth)
                  if (t_magnets(kmag)%IsPole.ne.0) then
                    write(luno,'(a)') trim(cline) // " 1"
                  else
                    write(luno,'(a)') trim(cline) // " 0"
                  endif
                endif !BC

              enddo !ncorn
            enddo !nplan
          enddo !nvoxcopy_t

          do ivox=1,nvoxcopy_t !iysym and izsym
            tvc=t_voxcopy(ivox)
            kvox=tvc%kproto
            kmag=tvc%kproto
            imag=tvc%kmagnet
            tmc=t_magcopy(imag)
            do iplan=1,ibpeplan(ivox)
              do icorn=1,ibpecorn(iplan,ivox)

                !x1y2z2
                htup(1)=bpebc(15,ivox)
                htup(2)=ivox+3*nvoxcopy_t
                htup(3)=ibpecol(ivox)
                htup(4)=iplan
                htup(5)=icorn*sign(1,ibpecorn(iplan,ivox))
                htup(6)=bpemag(1,icorn,iplan,ivox)
                htup(7)=-bpemag(2,icorn,iplan,ivox)
                htup(8)=-bpemag(3,icorn,iplan,ivox)
                htup(9)=-bpebc(4,ivox)
                htup(10)=bpebc(5,ivox)
                htup(11)=bpebc(6,ivox)
                htup(12)=bpebc(9,ivox)

                vmaglab(1)=bpebc(4,ivox)
                vmaglab(2)=bpebc(5,ivox)
                vmaglab(3)=bpebc(6,ivox)

                bc=sqrt(vmaglab(1)**2+vmaglab(2)**2+vmaglab(3)**2)
                bpebc(7,ivox)=bc

                if (bc.ne.0.0d0.or.t_magnets(kmag)%IsPole.ne.0) then
                  do i=5,12
                    if (abs(htup(i)).lt.1.0d-15) htup(i)=1.0d-15
                  enddo
                  write(cline,'(5f9.0,6e15.5e3,f7.0)')htup
                  cline=trim(cline) // " " // trim(tmc%cnam) // " " //
     &              trim(tmc%cmoth)
                  if (t_magnets(kmag)%IsPole.ne.0) then
                    write(luno,'(a)') trim(cline) // " 1"
                  else
                    write(luno,'(a)') trim(cline) // " 0"
                  endif
                endif !BC

              enddo !ncorn
            enddo !nplan
          enddo !nvoxcopy_t

          do ivox=1,nvoxcopy_t !izsym
            tvc=t_voxcopy(ivox)
            kvox=tvc%kproto
            kmag=tvc%kproto
            imag=tvc%kmagnet
            tmc=t_magcopy(imag)
            do iplan=1,ibpeplan(ivox)
              do icorn=1,ibpecorn(iplan,ivox)

                !x2y1z1
                htup(1)=bpebc(15,ivox)
                htup(2)=ivox+4*nvoxcopy_t
                htup(3)=ibpecol(ivox)
                htup(4)=iplan
                htup(5)=icorn*sign(1,ibpecorn(iplan,ivox))
                htup(6)=2.0d0*xsymmm_t-bpemag(1,icorn,iplan,ivox)
                htup(7)=bpemag(2,icorn,iplan,ivox)
                htup(8)=bpemag(3,icorn,iplan,ivox)
                htup(9)=-bpebc(4,ivox)
                htup(10)=bpebc(5,ivox)
                htup(11)=bpebc(6,ivox)
                htup(12)=bpebc(9,ivox)

                vmaglab(1)=bpebc(4,ivox)
                vmaglab(2)=bpebc(5,ivox)
                vmaglab(3)=bpebc(6,ivox)

                bc=sqrt(vmaglab(1)**2+vmaglab(2)**2+vmaglab(3)**2)
                bpebc(7,ivox)=bc

                if (bc.ne.0.0d0.or.t_magnets(kmag)%IsPole.ne.0) then
                  do i=5,12
                    if (abs(htup(i)).lt.1.0d-15) htup(i)=1.0d-15
                  enddo
                  write(cline,'(5f9.0,6e15.5e3,f7.0)')htup
                  cline=trim(cline) // " " // trim(tmc%cnam) // " " //
     &              trim(tmc%cmoth)
                  if (t_magnets(kmag)%IsPole.ne.0) then
                    write(luno,'(a)') trim(cline) // " 1"
                  else
                    write(luno,'(a)') trim(cline) // " 0"
                  endif
                endif !BC

              enddo !ncorn
            enddo !nplan
          enddo !nvoxcopy_t

          do ivox=1,nvoxcopy_t !iysym
            tvc=t_voxcopy(ivox)
            kvox=tvc%kproto
            kmag=tvc%kproto
            imag=tvc%kmagnet
            tmc=t_magcopy(imag)
            do iplan=1,ibpeplan(ivox)
              do icorn=1,ibpecorn(iplan,ivox)

                htup(1)=bpebc(15,ivox)
                htup(2)=ivox+5*nvoxcopy_t
                htup(3)=ibpecol(ivox)
                htup(4)=iplan
                htup(5)=icorn*sign(1,ibpecorn(iplan,ivox))
                htup(6)=2.0d0*xsymmm_t-bpemag(1,icorn,iplan,ivox)
                htup(7)=-bpemag(2,icorn,iplan,ivox)
                htup(8)=bpemag(3,icorn,iplan,ivox)
                htup(9)=bpebc(4,ivox)
                htup(10)=bpebc(5,ivox)
                htup(11)=-bpebc(6,ivox)
                htup(12)=bpebc(9,ivox)

                vmaglab(1)=bpebc(4,ivox)
                vmaglab(2)=bpebc(5,ivox)
                vmaglab(3)=bpebc(6,ivox)

                bc=sqrt(vmaglab(1)**2+vmaglab(2)**2+vmaglab(3)**2)
                bpebc(7,ivox)=bc

                if (bc.ne.0.0d0.or.t_magnets(kmag)%IsPole.ne.0) then
                  do i=5,12
                    if (abs(htup(i)).lt.1.0d-15) htup(i)=1.0d-15
                  enddo
                  write(cline,'(5f9.0,6e15.5e3,f7.0)')htup
                  cline=trim(cline) // " " // trim(tmc%cnam) // " " //
     &              trim(tmc%cmoth)
                  if (t_magnets(kmag)%IsPole.ne.0) then
                    write(luno,'(a)') trim(cline) // " 1"
                  else
                    write(luno,'(a)') trim(cline) // " 0"
                  endif
                endif !BC

              enddo !ncorn
            enddo !nplan
          enddo !nvoxcopy_t

          do ivox=1,nvoxcopy_t !iysym and izsym
            tvc=t_voxcopy(ivox)
            kvox=tvc%kproto
            kmag=tvc%kproto
            imag=tvc%kmagnet
            tmc=t_magcopy(imag)
            do iplan=1,ibpeplan(ivox)
              do icorn=1,ibpecorn(iplan,ivox)

                htup(1)=bpebc(15,ivox)
                htup(2)=ivox+6*nvoxcopy_t
                htup(3)=ibpecol(ivox)
                htup(4)=iplan
                htup(5)=icorn*sign(1,ibpecorn(iplan,ivox))
                htup(6)=2.0d0*xsymmm_t-bpemag(1,icorn,iplan,ivox)
                htup(7)=-bpemag(2,icorn,iplan,ivox)
                htup(8)=-bpemag(3,icorn,iplan,ivox)
                htup(9)=bpebc(4,ivox)
                htup(10)=bpebc(5,ivox)
                htup(11)=bpebc(6,ivox)
                htup(12)=bpebc(9,ivox)

                vmaglab(1)=bpebc(4,ivox)
                vmaglab(2)=bpebc(5,ivox)
                vmaglab(3)=bpebc(6,ivox)

                bc=sqrt(vmaglab(1)**2+vmaglab(2)**2+vmaglab(3)**2)
                bpebc(7,ivox)=bc

                if (bc.ne.0.0d0.or.t_magnets(kmag)%IsPole.ne.0) then
                  do i=5,12
                    if (abs(htup(i)).lt.1.0d-15) htup(i)=1.0d-15
                  enddo
                  write(cline,'(5f9.0,6e15.5e3,f7.0)')htup
                  cline=trim(cline) // " " // trim(tmc%cnam) // " " //
     &              trim(tmc%cmoth)
                  if (t_magnets(kmag)%IsPole.ne.0) then
                    write(luno,'(a)') trim(cline) // " 1"
                  else
                    write(luno,'(a)') trim(cline) // " 0"
                  endif
                endif !BC

              enddo !ncorn
            enddo !nplan
          enddo !nvoxcopy_t

          do ivox=1,nvoxcopy_t !izsym
            tvc=t_voxcopy(ivox)
            kvox=tvc%kproto
            kmag=tvc%kproto
            imag=tvc%kmagnet
            tmc=t_magcopy(imag)
            do iplan=1,ibpeplan(ivox)
              do icorn=1,ibpecorn(iplan,ivox)

                htup(1)=bpebc(15,ivox)
                htup(2)=ivox+7*nvoxcopy_t
                htup(3)=ibpecol(ivox)
                htup(4)=iplan
                htup(5)=icorn*sign(1,ibpecorn(iplan,ivox))
                htup(6)=2.0d0*xsymmm_t-bpemag(1,icorn,iplan,ivox)
                htup(7)=bpemag(2,icorn,iplan,ivox)
                htup(8)=-bpemag(3,icorn,iplan,ivox)
                htup(9)=-bpebc(4,ivox)
                htup(10)=bpebc(5,ivox)
                htup(11)=-bpebc(6,ivox)
                htup(12)=bpebc(9,ivox)

                vmaglab(1)=bpebc(4,ivox)
                vmaglab(2)=bpebc(5,ivox)
                vmaglab(3)=bpebc(6,ivox)

                bc=sqrt(vmaglab(1)**2+vmaglab(2)**2+vmaglab(3)**2)
                bpebc(7,ivox)=bc

                if (bc.ne.0.0d0.or.t_magnets(kmag)%IsPole.ne.0) then
                  do i=5,12
                    if (abs(htup(i)).lt.1.0d-15) htup(i)=1.0d-15
                  enddo
                  write(cline,'(5f9.0,6e15.5e3,f7.0)')htup
                  cline=trim(cline) // " " // trim(tmc%cnam) // " " //
     &              trim(tmc%cmoth)
                  if (t_magnets(kmag)%IsPole.ne.0) then
                    write(luno,'(a)') trim(cline) // " 1"
                  else
                    write(luno,'(a)') trim(cline) // " 0"
                  endif
                endif !BC

              enddo !ncorn
            enddo !nplan
          enddo !nvoxcopy_t

        endif !(izsym.ne.) then

      endif !(ixsym.eq.0) then

19    close(luno)

      return
      end
