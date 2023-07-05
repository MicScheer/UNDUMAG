*CMZ :  2.04/05 14/03/2023  20.06.46  by  Michael Scheer
*CMZ :  2.04/03 04/03/2023  19.30.13  by  Michael Scheer
*CMZ :  2.04/02 27/02/2023  20.28.35  by  Michael Scheer
*CMZ :  2.04/00 09/12/2022  11.08.58  by  Michael Scheer
*CMZ :  2.02/01 29/01/2022  10.13.35  by  Michael Scheer
*-- Author :    Michael Scheer   01/10/2021
      subroutine clcmag_voxels

      use commandlinef90m
      use bpolyederf90m
      use undumagf90m
      use magnets_structure
      use displacement


      implicit none

      double precision br(3),volmag,gcen(3)
      integer imag,ix,iy,iz,nvox,k,luno,npoi,l,ipoi,iface
      integer :: idebug=0
      character(32) ctype

      open(newunit=luno,file='undumag_voxels.geo')

      do imag=1,nmag_t+nspecmag_t

        volmag=0.0d0

        ctype=t_magnets(imag)%ctype

          allocate(t_magnets(imag)%t_voxels(t_magnets(imag)%nvoxels))

          nvox=0
          if (idebug.eq.1) then
            print*,"ix, iy, iz, ixdiv, iydiv, izdiv, kvox, volume"
          endif
          do iz=1,t_magnets(imag)%nzdiv
            do iy=1,t_magnets(imag)%nydiv
              do ix=1,t_magnets(imag)%nxdiv
                if (idebug.eq.1) then
                  print*,ix,iy,iz,
     &              t_magnets(imag)%t_xyzcuts(ix,iy,iz)%ixdiv,
     &              t_magnets(imag)%t_xyzcuts(ix,iy,iz)%iydiv,
     &              t_magnets(imag)%t_xyzcuts(ix,iy,iz)%izdiv,
     &              t_magnets(imag)%t_xyzcuts(ix,iy,iz)%ixdiv,
     &              t_magnets(imag)%kvoxels(ix,iy,iz),
     &              t_magnets(imag)%t_xyzcuts(ix,iy,iz)%volume
                endif
                if (t_magnets(imag)%t_xyzcuts(ix,iy,iz)%ixdiv.eq.0.or.
     &            t_magnets(imag)%t_xyzcuts(ix,iy,iz)%iydiv.eq.0.or.
     &              t_magnets(imag)%t_xyzcuts(ix,iy,iz)%izdiv.eq.0) then
                  if (t_magnets(imag)%t_xyzcuts(ix,iy,iz)%volume.ne.0d0) then
                    print*,imag,ix,iy,iz
                    stop "Error 1 in clcmag_voxels"
                  endif
                  cycle
                endif
                kvox=t_magnets(imag)%kvoxels(ix,iy,iz)
                if (kvox.eq.0) then
                  if (t_magnets(imag)%t_xyzcuts(ix,iy,iz)%volume.ne.0d0) then
                    print*,imag,ix,iy,iz
                    stop "Error 2 in clcmag_voxels"
                  endif
                  cycle
                endif
                nvox=nvox+1
                !print*,ix,iy,iz,kvox,nvox
                t_magnets(imag)%t_voxels(nvox)=
     &            t_magnets(imag)%t_xyzcuts(ix,iy,iz)
                t_magnets(imag)%t_voxels(nvox)%ixdiv=ix
                t_magnets(imag)%t_voxels(nvox)%iydiv=iy
                t_magnets(imag)%t_voxels(nvox)%izdiv=iz
                call clcmag_voxel_volume(imag,nvox)
                l=0
                do iface=1,t_magnets(imag)%t_voxels(nvox)%nface
                  l=l+1
                  npoi=t_magnets(imag)%t_voxels(nvox)%kface(l)
                  gcen=t_magnets(imag)%t_voxels(nvox)%gcen
                  do ipoi=1,npoi
                    l=l+1
                    k=t_magnets(imag)%t_voxels(nvox)%kface(l)
                    write(luno,*)imag,nvox,ix,iy,iz,iface,ipoi,
     &                t_magnets(imag)%t_voxels(nvox)%xhull(k)+gcen(1),
     &                t_magnets(imag)%t_voxels(nvox)%yhull(k)+gcen(2),
     &                t_magnets(imag)%t_voxels(nvox)%zhull(k)+gcen(3),
     &                gcen,
     &                t_magnets(imag)%t_voxels(nvox)%volume
                  enddo
                enddo
                volmag=volmag+t_magnets(imag)%t_voxels(nvox)%volume
                call clcmag_br_inhom(imag,nvox,br)
                t_magnets(imag)%t_voxels(nvox)%Br=br
                t_magnets(imag)%t_voxels(nvox)%IsPole=t_magnets(imag)%IsPole
              enddo
            enddo
          enddo

          t_magnets(imag)%nvoxels=nvox

          deallocate(t_magnets(imag)%t_xcuts,t_magnets(imag)%t_xycuts,
     &      t_magnets(imag)%t_xyzcuts)


        if (abs((volmag-t_magnets(imag)%volume)/volmag).gt.1.0d-10) then
          write(lun6,*)'*** Warning in clcmag_voxels: Sum of volumes of voxels does not match the one of the magnet ',t_magnets(imag)%cnam
          write(lun6,*)"Rel. error :",(volmag-t_magnets(imag)%volume)/volmag
        endif

      enddo !imag

      flush(luno)
      close(luno)

      return
      end
