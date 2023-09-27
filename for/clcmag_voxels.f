*CMZ :  2.04/23 27/09/2023  07.36.57  by  Michael Scheer
*CMZ :  2.04/20 20/09/2023  15.31.58  by  Michael Scheer
*CMZ :  2.04/17 12/09/2023  14.13.06  by  Michael Scheer
*CMZ :  2.04/16 06/09/2023  16.43.43  by  Michael Scheer
*CMZ :  2.04/14 04/09/2023  16.36.15  by  Michael Scheer
*CMZ :  2.04/13 04/09/2023  11.19.16  by  Michael Scheer
*CMZ :  2.04/09 22/08/2023  09.03.52  by  Michael Scheer
*CMZ :  2.04/08 11/08/2023  12.58.25  by  Michael Scheer
*CMZ :  2.04/07 09/08/2023  12.43.30  by  Michael Scheer
*CMZ :  2.04/06 04/08/2023  11.32.02  by  Michael Scheer
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

c      Type(T_Magnet) tmag
      Type(T_Voxel) tv

      double precision br(3),volmag,gcen(3),p(3,3),wnorm(3),wcen(3),
     &  radin,radout,height,phi,dr2,dh2,dphi2,cylcen(3),v(3)
      integer imag,ix,iy,iz,nx,ny,nz,nvox,k1,k,luno,npoi,l,ipoi,ivface,nf,
     &  ison,icol,iface

      integer :: idebug=0

c+self,if=voxcyl.
      integer iv
c+self.
      character(32) ctype


      call util_zeit_kommentar(lun6,"Entered clcmag_voxels")

      if (iwvgeo.ne.0) then
        open(newunit=luno,file='undumag_voxels.geo')
      endif

      do imag=1,nmag_t+nspecmag_t

        ctype=t_magnets(imag)%ctype

        volmag=0.0d0

        if (ctype.ne.'Cylinder') then

          allocate(t_magnets(imag)%t_voxels(t_magnets(imag)%nvoxels))

          nvox=0
          if (idebug.eq.1) then
            print*,"imag, ix, iy, iz, ixdiv, iydiv, izdiv, kvox, volume"
          else if (idebug.ge.2) then
            print*,"imag, nvoxels:",imag,t_magnets(imag)%nvoxels
          endif

          do iz=1,t_magnets(imag)%nzdiv
            do iy=1,t_magnets(imag)%nydiv
              do ix=1,t_magnets(imag)%nxdiv

                if (idebug.eq.1) then
                  print*,imag,ix,iy,iz,
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

                icol=t_magnets(imag)%icol

                t_magnets(imag)%t_voxels(nvox)%ixdiv=0
                t_magnets(imag)%t_voxels(nvox)%iydiv=0
                t_magnets(imag)%t_voxels(nvox)%izdiv=0

                t_magnets(imag)%t_voxels(nvox)=
     &            t_magnets(imag)%t_xyzcuts(ix,iy,iz)

                allocate(t_magnets(imag)%t_voxels(nvox)%lface(
     &            t_magnets(imag)%t_voxels(nvox)%nface))

                k=0
                do iface=1,
     &              t_magnets(imag)%t_voxels(nvox)%nface
                  k=k+1
                  t_magnets(imag)%t_voxels(nvox)%lface(iface)=k
                  k=k+t_magnets(imag)%t_voxels(nvox)%kface(k)
                enddo

                t_magnets(imag)%t_voxels(nvox)%ixdiv=ix
                t_magnets(imag)%t_voxels(nvox)%iydiv=iy
                t_magnets(imag)%t_voxels(nvox)%izdiv=iz
                t_magnets(imag)%t_voxels(nvox)%isBlock=
     &            t_magnets(imag)%isBlock

                allocate(t_magnets(imag)%t_voxels(nvox)%isfacet(
     &            t_magnets(imag)%t_voxels(nvox)%nface))
                call clcmag_voxel_volume(imag,nvox)
                l=0
                if (iwvgeo.ne.0) then
                  do iface=1,t_magnets(imag)%t_voxels(nvox)%nface
                    l=l+1
                    npoi=t_magnets(imag)%t_voxels(nvox)%kface(l)
                    gcen=t_magnets(imag)%t_voxels(nvox)%gcen
                    do ipoi=1,npoi
                      l=l+1
                      k=t_magnets(imag)%t_voxels(nvox)%kface(l)
                      write(luno,*)imag,nvox,ix,iy,iz,iface,ipoi,
     &                  t_magnets(imag)%t_voxels(nvox)%xhull(k)+gcen(1),
     &                  t_magnets(imag)%t_voxels(nvox)%yhull(k)+gcen(2),
     &                  t_magnets(imag)%t_voxels(nvox)%zhull(k)+gcen(3),
     &                  gcen,
     &                  t_magnets(imag)%t_voxels(nvox)%volume,icol
                    enddo
                  enddo
                endif
                volmag=volmag+t_magnets(imag)%t_voxels(nvox)%volume
                if (t_magnets(imag)%IsInhom.ne.0) then
                  call clcmag_br_inhom(imag,nvox,br)
                  t_magnets(imag)%t_voxels(nvox)%Br=br
                else
                  t_magnets(imag)%t_voxels(nvox)%Br=t_magnets(imag)%Br
                endif
                t_magnets(imag)%t_voxels(nvox)%IsPole=t_magnets(imag)%IsPole
              enddo
            enddo
          enddo

          t_magnets(imag)%nvoxels=nvox

          deallocate(t_magnets(imag)%t_xcuts,t_magnets(imag)%t_xycuts,
     &      t_magnets(imag)%t_xyzcuts)

        else !(ctype.ne.'Cylinder') then

          nvox=
     &      t_magnets(imag)%nxdiv *
     &      t_magnets(imag)%nydiv *
     &      t_magnets(imag)%nzdiv

          do iv=1,nvox

            call clcmag_voxel_volume(imag,iv)
            volmag=volmag+t_magnets(imag)%t_voxels(iv)%volume

            if (t_magnets(imag)%IsInhom.ne.0) then
              call clcmag_br_inhom(imag,iv,br)
              t_magnets(imag)%t_voxels(iv)%Br=br
            else
              t_magnets(imag)%t_voxels(iv)%Br=t_magnets(imag)%Br
            endif

            t_magnets(imag)%t_voxels(iv)%IsPole=t_magnets(imag)%IsPole

            l=0

            if (iwvgeo.ne.0) then
              do iface=1,t_magnets(imag)%t_voxels(iv)%nface
                l=l+1
                npoi=t_magnets(imag)%t_voxels(iv)%kface(l)
                gcen=t_magnets(imag)%t_voxels(iv)%gcen
                do ipoi=1,npoi
                  l=l+1
                  k=t_magnets(imag)%t_voxels(iv)%kface(l)
                  write(luno,*)imag,iv,ix,iy,iz,iface,ipoi,
     &              t_magnets(imag)%t_voxels(iv)%xhull(k)+gcen(1),
     &              t_magnets(imag)%t_voxels(iv)%yhull(k)+gcen(2),
     &              t_magnets(imag)%t_voxels(iv)%zhull(k)+gcen(3),
     &              gcen,
     &              t_magnets(imag)%t_voxels(iv)%volume
                enddo
              enddo
            endif

            allocate(t_magnets(imag)%t_voxels(iv)%isfacet(8))

          enddo !nvox

        endif !(ctype.ne.'Cylinder') then

        t_magnets(imag)%nvoxels=nvox
        nvoxcopy_t=nvoxcopy_t+nvox

        !call clcmag_check_orient(imag)

        if (abs((volmag-t_magnets(imag)%volume)/volmag).gt.1.0d-10.and.
     &      t_magnets(imag)%ctype.ne.'Cylinder') then
          write(lun6,*)'*** Warning in clcmag_voxels: Sum of volumes of voxels does not match the one of the magnet ',t_magnets(imag)%cnam
          write(lun6,*)"Rel. error :",(volmag-t_magnets(imag)%volume)/volmag
          write(lun6,*)'*** Consider to set MODSIMPHULL=1  in undumag.nam'
        endif
      enddo !imag

      if (iundugeo.ne.0) then
        flush(luno)
        close(luno)
      endif

      call util_zeit_kommentar(lun6,"Calculating facets")

c      allocate(ifacets(5,nfacets))
c      ifacets=0
      nfacets=0

      do imag=1,nmag_t+nspecmag_t

!        tmag=t_magnets(imag)

        ctype=t_magnets(imag)%ctype

        !call util_break
        if (ctype.eq.'Cylinder') then
          radin=t_magnets(imag)%size(1)
          radout=t_magnets(imag)%size(2)
          dr2=(radout-radin)/dble(max(1,nx-1))/2.0d0
          height=t_magnets(imag)%size(3)
          dh2=height/dble(max(1,ny-1))/2.0d0
          phi=t_magnets(imag)%cylphi
          dphi2=phi/dble(max(1,nz-1))/2.0d0
          cylcen=t_magnets(imag)%xyz
        endif

        nx=t_magnets(imag)%nxdiv
        ny=t_magnets(imag)%nydiv
        nz=t_magnets(imag)%nzdiv

        do iv=1,t_magnets(imag)%nvoxels

          tv=t_magnets(imag)%t_voxels(iv)

          gcen=tv%gcen
          ix=tv%ixdiv
          iy=tv%iydiv
          iz=tv%izdiv
          nf=tv%nface

          allocate(
     &      t_magnets(imag)%t_voxels(iv)%vcen(3,nf),
     &      t_magnets(imag)%t_voxels(iv)%vnorm(3,nf)
     &      )

          k=0

          do ivface=1,nf

            k=k+1
            npoi=tv%kface(k)
            wcen=0.0d0

            k1=k
            do l=1,npoi
              k=k+1
              ipoi=tv%kface(k)
              if (l.le.3) then
                p(1:3,l)=[tv%xhull(ipoi),tv%yhull(ipoi),tv%zhull(ipoi)]
              endif
              wcen=wcen+[tv%xhull(ipoi),tv%yhull(ipoi),tv%zhull(ipoi)]
            enddo

            call util_vcross(p(:,2)-p(:,1),p(:,3)-p(:,2),wnorm)
            wnorm=wnorm/norm2(wnorm)
            t_magnets(imag)%t_voxels(iv)%vnorm(:,ivface)=wnorm
            wcen=wcen/dble(npoi)
            t_magnets(imag)%t_voxels(iv)%vcen(:,ivface)=wcen
            t_magnets(imag)%t_voxels(iv)%isfacet(ivface)=0

            if (
     &        ix.gt.1.and.ix.lt.nx.and.
     &        iy.gt.1.and.iy.lt.ny.and.
     &        iz.gt.1.and.iz.lt.nz
     &        ) cycle

            if (ctype.ne.'Cylinder') then
              call clcmag_magnet_facet(t_magnets(imag),iv,ivface,ison,
     &          hulltiny)
            else

              v=[wcen(1)-cylcen(1),wcen(2)-cylcen(2),0.0d0]
              v=v/norm2(v)

              if (dot_product(v,wnorm).lt.hulltiny.or.
     &            abs(wcen(2)-radin).lt.dr2.or.
     &            abs(wcen(2)-radout).lt.dr2.or.
     &            abs(wcen(3)).lt.dh2.or.
     &            abs(wcen(3)-height).lt.dh2
     &            ) then
                ison=1
              endif

            endif !Cylinder

            if (ison.ne.1) cycle
            t_magnets(imag)%t_voxels(iv)%isfacet(ivface)=npoi

          enddo !ivface

        enddo !iv
      enddo !imag

      call util_zeit_kommentar(lun6,"Facets calculated")

      return
      end
