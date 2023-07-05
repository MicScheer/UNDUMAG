*CMZ :          07/08/2018  09.08.25  by  Michael Scheer
*CMZ :  1.03/00 07/10/2014  10.55.15  by  Michael Scheer
*CMZ :  1.02/01 05/10/2014  14.25.34  by  Michael Scheer
*CMZ :  1.02/00 03/10/2014  12.39.05  by  Michael Scheer
*-- Author :    Michael Scheer   30/09/2014
      subroutine mshplt_surf(nx,xmin,xmax,ny,ymin,ymax,z,chopt)

      implicit none

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.

      real xmin,xmax,ymin,ymax,xp(4),yp(4),zp(4),dx,dy,z(*),
     &  dz10,zmin,zmax

      integer nx,ny,ix,iy,ioutlined,lenc,i,iz,icoltile,
     &  kco,kro,kbo,kgo,kc,imcol,mco,mro,mgo,mbo,isurf

      character(*) chopt
      character c1
      character(4) c4
      character(400) copt
      integer ib
      equivalence (ib,c1)

      call mshplt_fill_buff('% begin of mshplt_surf')

      ioutlined=0
      icoltile=0
      imcol=0
      isurf=0

      lenc=min(400,len_trim(chopt))

      if (lenc.lt.4) then
        copt='linesurf'
        lenc=8
      else
        copt=chopt(1:min(lenc,400))
      endif

      ib=0
      do i=1,lenc
        c1=copt(i:i)
        if (ib.lt.97) then
          ib=ib+32
          copt(i:i)=c1
        endif
      enddo

      do i=1,lenc
        c4=copt(i:i+4)
        if (c4.eq.'line') ioutlined=1
        if (c4.eq.'tile') then
          icoltile=1
          isurf=1
        endif
        if (c4.eq.'mark') imcol=1
        if (c4.eq.'surf') isurf=1
      enddo

      if (imcol+icoltile+isurf.eq.0) then
        isurf=1
        ioutlined=1
      endif

      call mshplt_get_fill_color(kco,kro,kgo,kbo)
      call mshplt_get_marker_color(mco,mro,mgo,mbo)

c      if (imcol.ne.0) then
c        call mshplt_set_fill_color(-1,1,1,1)
c      endif

      zmin=1.0e30
      zmax=1.0e-30
      iz=0
      do iy=1,ny
        do ix=1,nx
          iz=iz+1
          if (z(iz).lt.zmin) zmin=z(iz)
          if (z(iz).gt.zmax) zmax=z(iz)
        enddo
      enddo

      if (log10z_ps.eq.0) then
        dz10=(zmax-zmin)*1.001/7.
      else
        dz10=(alog10(zmax)-alog10(zmin))*1.001/7.
      endif
      if (dz10.eq.0.0) dz10=1.

      if (nx.lt.1.or.ny.lt.1) then
        return
      else if (nx.eq.1.and.ny.eq.1) then

        xp(1)=xmin
        yp(1)=ymin
        zp(1)=z(1)
        call mshplt_marker_3d(1,xp,yp,zp)

      else if (nx.gt.1.and.ny.eq.1) then
        dx=(xmax-xmin)/(nx-1)
        do ix=1,nx-1
          xp(1)=xmin+(ix-1)*dx
          xp(2)=xp(1)+dx
          yp(1)=ymin
          yp(2)=ymin
          zp(1)=z(ix)
          zp(2)=z(ix+1)
          call mshplt_pline_3d(2,xp,yp,zp)
        enddo

      else if (ny.gt.1.and.nx.eq.1) then
        dy=(ymax-ymin)/(ny-1)
        do iy=1,ny-1
          yp(1)=ymin+(iy-1)*dy
          yp(2)=yp(1)+dy
          xp(1)=xmin
          xp(2)=xmin
          zp(1)=z(iy)
          zp(2)=z(iy+1)
          call mshplt_pline_3d(2,xp,yp,zp)
        enddo

      else

        dx=(xmax-xmin)/(nx-1)
        dy=(ymax-ymin)/(ny-1)

        if (theta_ps.le.90.) then

          if (phi_ps.le.90.) then

            do iy=ny-1,1,-1
              do ix=nx-1,1,-1

                xp(1)=xmin+(ix-1)*dx
                xp(2)=xp(1)+dx
                xp(3)=xp(2)
                xp(4)=xp(1)

                yp(1)=ymin+(iy-1)*dy
                yp(2)=yp(1)
                yp(3)=yp(2)+dy
                yp(4)=yp(3)

                zp(1)=z(nx*(iy-1)+ix)
                zp(2)=z(nx*(iy-1)+ix+1)
                zp(3)=z(nx*iy+ix+1)
                zp(4)=z(nx*iy+ix)

                if (icoltile.ne.0) then
                  if(log10z_ps.eq.0) then
                    kc=mod(int(((zp(1)+zp(2)+zp(3)+zp(4))/4.-zmin)/dz10)+1,8)
                  else
                    kc=mod(
     &                int(
     &                (alog10((zp(1)+zp(2)+zp(3)+zp(4))/4.)-
     &                alog10(zmin))/dz10)+1,8)
                  endif
                  call mshplt_set_fill_color(kc,0,0,0)
                endif

                if (isurf.ne.0) call mshplt_filled_mesh_3d(4,xp,yp,zp,ioutlined)

                if (imcol.ne.0) then
                  do i=1,4
                    if(log10z_ps.eq.0) then
                      kc=mod(int((zp(i)-zmin)/dz10)+1,8)
                    else
                      kc=mod(int((alog10(zp(i))-alog10(zmin))/dz10)+1,8)
                    endif
                    call mshplt_set_marker_color(kc,0,0,0)
                    call mshplt_marker_3d(1,xp(i),yp(i),zp(i))
                  enddo
                endif
              enddo
            enddo

          else if (phi_ps.le.180.) then

            do ix=nx-1,1,-1
              do iy=1,ny-1

                xp(1)=xmin+(ix-1)*dx
                xp(2)=xp(1)+dx
                xp(3)=xp(2)
                xp(4)=xp(1)

                yp(1)=ymin+(iy-1)*dy
                yp(2)=yp(1)
                yp(3)=yp(2)+dy
                yp(4)=yp(3)

                zp(1)=z(nx*(iy-1)+ix)
                zp(2)=z(nx*(iy-1)+ix+1)
                zp(3)=z(nx*iy+ix+1)
                zp(4)=z(nx*iy+ix)

                if (icoltile.ne.0) then
                  kc=mod(int(((zp(1)+zp(2)+zp(3)+zp(4))/4.-zmin)/dz10)+1,8)
                  call mshplt_set_fill_color(kc,0,0,0)
                endif

                if (isurf.ne.0) call mshplt_filled_mesh_3d(4,xp,yp,zp,ioutlined)

                if (imcol.ne.0) then
                  do i=1,4
                    kc=mod(int((zp(i)-zmin)/dz10)+1,8)
                    call mshplt_set_marker_color(kc,0,0,0)
                    call mshplt_marker_3d(1,xp(i),yp(i),zp(i))
                  enddo
                endif
              enddo
            enddo

          else if (phi_ps.le.270.) then

            do iy=1,ny-1
              do ix=1,nx-1

                xp(1)=xmin+(ix-1)*dx
                xp(2)=xp(1)+dx
                xp(3)=xp(2)
                xp(4)=xp(1)

                yp(1)=ymin+(iy-1)*dy
                yp(2)=yp(1)
                yp(3)=yp(2)+dy
                yp(4)=yp(3)

                zp(1)=z(nx*(iy-1)+ix)
                zp(2)=z(nx*(iy-1)+ix+1)
                zp(3)=z(nx*iy+ix+1)
                zp(4)=z(nx*iy+ix)

                if (icoltile.ne.0) then
                  kc=mod(int(((zp(1)+zp(2)+zp(3)+zp(4))/4.-zmin)/dz10)+1,8)
                  call mshplt_set_fill_color(kc,0,0,0)
                endif

                if (isurf.ne.0) call mshplt_filled_mesh_3d(4,xp,yp,zp,ioutlined)

                if (imcol.ne.0) then
                  do i=1,4
                    kc=mod(int((zp(i)-zmin)/dz10)+1,8)
                    call mshplt_set_marker_color(kc,0,0,0)
                    call mshplt_marker_3d(1,xp(i),yp(i),zp(i))
                  enddo
                endif
              enddo
            enddo

          else if (phi_ps.le.360.) then
            do ix=1,nx-1
              do iy=ny-1,1,-1

                xp(1)=xmin+(ix-1)*dx
                xp(2)=xp(1)+dx
                xp(3)=xp(2)
                xp(4)=xp(1)

                yp(1)=ymin+(iy-1)*dy
                yp(2)=yp(1)
                yp(3)=yp(2)+dy
                yp(4)=yp(3)

                zp(1)=z(nx*(iy-1)+ix)
                zp(2)=z(nx*(iy-1)+ix+1)
                zp(3)=z(nx*iy+ix+1)
                zp(4)=z(nx*iy+ix)

                if (icoltile.ne.0) then
                  kc=mod(int(((zp(1)+zp(2)+zp(3)+zp(4))/4.-zmin)/dz10)+1,8)
                  call mshplt_set_fill_color(kc,0,0,0)
                endif

                if (isurf.ne.0) call mshplt_filled_mesh_3d(4,xp,yp,zp,ioutlined)

                if (imcol.ne.0) then
                  do i=1,4
                    kc=mod(int((zp(i)-zmin)/dz10)+1,8)
                    call mshplt_set_marker_color(kc,0,0,0)
                    call mshplt_marker_3d(1,xp(i),yp(i),zp(i))
                  enddo
                endif
              enddo
            enddo

          endif !phi.le.90.

        else if (theta_ps.le.180.) then

          if (phi_ps.le.90.) then

            do iy=1,ny-1
              do ix=1,nx-1

                xp(1)=xmin+(ix-1)*dx
                xp(2)=xp(1)+dx
                xp(3)=xp(2)
                xp(4)=xp(1)

                yp(1)=ymin+(iy-1)*dy
                yp(2)=yp(1)
                yp(3)=yp(2)+dy
                yp(4)=yp(3)

                zp(1)=z(nx*(iy-1)+ix)
                zp(2)=z(nx*(iy-1)+ix+1)
                zp(3)=z(nx*iy+ix+1)
                zp(4)=z(nx*iy+ix)

                if (icoltile.ne.0) then
                  kc=mod(int(((zp(1)+zp(2)+zp(3)+zp(4))/4.-zmin)/dz10)+1,8)
                  call mshplt_set_fill_color(kc,0,0,0)
                endif

                if (isurf.ne.0) call mshplt_filled_mesh_3d(4,xp,yp,zp,ioutlined)

                if (imcol.ne.0) then
                  do i=1,4
                    kc=mod(int((zp(i)-zmin)/dz10)+1,8)
                    call mshplt_set_marker_color(kc,0,0,0)
                    call mshplt_marker_3d(1,xp(i),yp(i),zp(i))
                  enddo
                endif
              enddo
            enddo

          else if (phi_ps.le.180.) then

            do ix=1,nx-1
              do iy=ny-1,1,-1

                xp(1)=xmin+(ix-1)*dx
                xp(2)=xp(1)+dx
                xp(3)=xp(2)
                xp(4)=xp(1)

                yp(1)=ymin+(iy-1)*dy
                yp(2)=yp(1)
                yp(3)=yp(2)+dy
                yp(4)=yp(3)

                zp(1)=z(nx*(iy-1)+ix)
                zp(2)=z(nx*(iy-1)+ix+1)
                zp(3)=z(nx*iy+ix+1)
                zp(4)=z(nx*iy+ix)

                if (icoltile.ne.0) then
                  kc=mod(int(((zp(1)+zp(2)+zp(3)+zp(4))/4.-zmin)/dz10)+1,8)
                  call mshplt_set_fill_color(kc,0,0,0)
                endif

                if (isurf.ne.0) call mshplt_filled_mesh_3d(4,xp,yp,zp,ioutlined)

                if (imcol.ne.0) then
                  do i=1,4
                    kc=mod(int((zp(i)-zmin)/dz10)+1,8)
                    call mshplt_set_marker_color(kc,0,0,0)
                    call mshplt_marker_3d(1,xp(i),yp(i),zp(i))
                  enddo
                endif
              enddo
            enddo

          else if (phi_ps.le.270.) then

            do iy=ny-1,1,-1
              do ix=nx-1,1,-1

                xp(1)=xmin+(ix-1)*dx
                xp(2)=xp(1)+dx
                xp(3)=xp(2)
                xp(4)=xp(1)

                yp(1)=ymin+(iy-1)*dy
                yp(2)=yp(1)
                yp(3)=yp(2)+dy
                yp(4)=yp(3)

                zp(1)=z(nx*(iy-1)+ix)
                zp(2)=z(nx*(iy-1)+ix+1)
                zp(3)=z(nx*iy+ix+1)
                zp(4)=z(nx*iy+ix)

                if (icoltile.ne.0) then
                  kc=mod(int(((zp(1)+zp(2)+zp(3)+zp(4))/4.-zmin)/dz10)+1,8)
                  call mshplt_set_fill_color(kc,0,0,0)
                endif

                if (isurf.ne.0) call mshplt_filled_mesh_3d(4,xp,yp,zp,ioutlined)

                if (imcol.ne.0) then
                  do i=1,4
                    kc=mod(int((zp(i)-zmin)/dz10)+1,8)
                    call mshplt_set_marker_color(kc,0,0,0)
                    call mshplt_marker_3d(1,xp(i),yp(i),zp(i))
                  enddo
                endif
              enddo
            enddo

          else if (phi_ps.le.360.) then

            do ix=nx-1,1,-1
              do iy=1,ny-1

                xp(1)=xmin+(ix-1)*dx
                xp(2)=xp(1)+dx
                xp(3)=xp(2)
                xp(4)=xp(1)

                yp(1)=ymin+(iy-1)*dy
                yp(2)=yp(1)
                yp(3)=yp(2)+dy
                yp(4)=yp(3)

                zp(1)=z(nx*(iy-1)+ix)
                zp(2)=z(nx*(iy-1)+ix+1)
                zp(3)=z(nx*iy+ix+1)
                zp(4)=z(nx*iy+ix)

                if (icoltile.ne.0) then
                  kc=mod(int(((zp(1)+zp(2)+zp(3)+zp(4))/4.-zmin)/dz10)+1,8)
                  call mshplt_set_fill_color(kc,0,0,0)
                endif

                if (isurf.ne.0) call mshplt_filled_mesh_3d(4,xp,yp,zp,ioutlined)

                if (imcol.ne.0) then
                  do i=1,4
                    kc=mod(int((zp(i)-zmin)/dz10)+1,8)
                    call mshplt_set_marker_color(kc,0,0,0)
                    call mshplt_marker_3d(1,xp(i),yp(i),zp(i))
                  enddo
                endif
              enddo
            enddo

          endif !phi.le.90.

        endif !theta

      endif

      if (ifbox_ps.ne.0) call mshplt_draw_forground_box

      call mshplt_set_marker_color(mco,mro,mgo,mbo)
      call mshplt_set_fill_color(kco,kro,kgo,kbo)

      call mshplt_fill_buff('% end of mshplt_surf')

      return
      end
