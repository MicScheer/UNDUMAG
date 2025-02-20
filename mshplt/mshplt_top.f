*CMZ :  1.04/00 12/02/2025  13.27.29  by  Michael Scheer
*CMZ :  1.03/03 04/02/2025  10.25.27  by  Michael Scheer
*CMZ :  1.03/00 07/10/2014  10.55.15  by  Michael Scheer
*CMZ :  1.02/01 05/10/2014  14.25.34  by  Michael Scheer
*CMZ :  1.02/00 03/10/2014  12.39.05  by  Michael Scheer
*-- Author :    Michael Scheer   30/09/2014
      subroutine mshplt_top(nxin,xmin,xmax,nyin,ymin,ymax,z,chopt,icolor)

      use cmapmod

! Options: surf, tile, mark, spline

      implicit none

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.

      real xmin,xmax,ymin,ymax,xp(4),yp(4),zp(4),dx,dy,z(nxin*nyin),zminmax,
     &  dz10,zmin,zmax,zs(nxspline*nyspline),red,green,blue,zpmax(4)

      integer nxin,nyin,nx,ny,ix,iy,ioutlined,i,icoltile,icolor,icolmap,
     &  kco,kro,kbo,kgo,kc,imcol,mco,mro,mgo,mbo,isurf,ianf,iend,istatus

      integer :: ncol=7

      character(*) chopt
      character(32) copt

      copt=trim(chopt)
      call util_lower_case(copt)

      nx=nxin
      ny=nyin
      ksplinecmap=0

      call util_string_substring(copt,'spline',ianf,iend,istatus)
      if (istatus.eq.0) then
        copt(ianf:iend)=''
        call mshplt_z_spline(nx,xmin,xmax,ny,ymin,ymax,z,nxspline,nyspline,zs)
        nx=nxspline
        ny=nyspline
        ksplinecmap=1
      else
        zs(1:nx*ny)=z(1:nx*ny)
      endif

      if (log10z_ps.ne.0) then
        zs=alog10(zs)
      endif

      call mshplt_fill_buff('% begin of mshplt_surf')

      ioutlined=0
      icoltile=0
      imcol=0
      isurf=0

      call util_string_substring(copt,'line',ianf,iend,istatus)
      if (istatus.eq.0) ioutlined=1
      call util_string_substring(copt,'tile',ianf,iend,istatus)
      if (istatus.eq.0) then
          icoltile=1
          isurf=1
      endif
      call util_string_substring(copt,'mark',ianf,iend,istatus)
      if (istatus.eq.0) imcol=1
      call util_string_substring(copt,'surf',ianf,iend,istatus)
      if (istatus.eq.0) isurf=1

      if (imcol+icoltile+isurf.eq.0) then
        isurf=1
        ioutlined=1
      endif

      if (icolor.eq.0) then
        ncol=256
      endif

      call mshplt_get_fill_color(kco,kro,kgo,kbo)
      call mshplt_get_marker_color(mco,mro,mgo,mbo)

      zmin=minval(zs)
      zmax=maxval(zs)
      zminmax=zmax-zmin
      zpmax=zmax

c      if (log10z_ps.eq.0) then
        zmincmap=zmin
        zmaxcmap=zmax
        dz10=(zmax-zmin)*1.001/ncol
c      else
c        zmincmap=alog10(zmin)
c        zmaxcmap=alog10(zmax)
c        dz10=(alog10(zmax)-alog10(zmin))*1.001/ncol
c      endif

      if (dz10.eq.0.0) dz10=1.

      if (nx.lt.1.or.ny.lt.1) then
        return
      else if (nx.eq.1.and.ny.eq.1) then

        xp(1)=xmin
        yp(1)=ymin
        zp(1)=zs(1)
        call mshplt_cmap_inter(zs(1),icolmap,red,green,blue)
        call mshplt_marker_3d(1,xp,yp,zpmax)

      else if (nx.gt.1.and.ny.eq.1) then

        dx=(xmax-xmin)/(nx-1)
        do ix=1,nx-1
          xp(1)=xmin+(ix-1)*dx
          xp(2)=xp(1)+dx
          yp(1)=ymin
          yp(2)=ymin
          zp(1)=zs(ix)
          zp(2)=zs(ix+1)
          call mshplt_pline_3d(2,xp,yp,zpmax)
        enddo

      else if (ny.gt.1.and.nx.eq.1) then

        dy=(ymax-ymin)/(ny-1)
        do iy=1,ny-1
          yp(1)=ymin+(iy-1)*dy
          yp(2)=yp(1)+dy
          xp(1)=xmin
          xp(2)=xmin
          zp(1)=zs(iy)
          zp(2)=zs(iy+1)
          call mshplt_pline_3d(2,xp,yp,zpmax)
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

                zp(1)=zs(nx*(iy-1)+ix)
                zp(2)=zs(nx*(iy-1)+ix+1)
                zp(3)=zs(nx*iy+ix+1)
                zp(4)=zs(nx*iy+ix)

                if (icoltile.ne.0) then
c                  if(log10z_ps.eq.0) then
                  if (ksplinecmap.eq.0) then
                    kc=mod(int((sum(zp)/size(zp)-zmin)/dz10)+1,(ncol+1))
                    call mshplt_set_fill_color(kc,0,0,0)
                  else
                    zcmap=(sum(zp)/size(zp)-zmin)/zminmax
                    call mshplt_cmap_inter(zcmap,icolmap,red,green,blue)
                    call mshplt_set_fill_color(9999,
     &                int(red*100000),
     &                int(green*100000),
     &                int(blue*100000))
                  endif
c                  else
c                    kc=mod(
c     &                int(
c     &                (alog10((zp(1)+zp(2)+zp(3)+zp(4))/4.)-
c     &                alog10(zmin))/dz10)+1,(ncol+1))
c                  endif
                endif

                if (isurf.ne.0) call mshplt_filled_mesh_3d(4,xp,yp,zpmax,ioutlined)

                if (imcol.ne.0) then
                  do i=1,4
c                    if(log10z_ps.eq.0) then
                    if (ksplinecmap.eq.0) then
                      kc=mod(int((zp(i)-zmin)/dz10)+1,(ncol+1))
                      call mshplt_set_marker_color(kc,0,0,0)
                    else
                      zcmap=(sum(zp)/size(zp)-zmin)/zminmax
                      call mshplt_cmap_inter(zcmap,icolmap,red,green,blue)
                      call mshplt_set_marker_color(9999,
     &                  int(red*100000),
     &                  int(green*100000),
     &                  int(blue*100000))
                    endif
                    call mshplt_marker_3d(1,xp(i),yp(i),zpmax(i))
c                    else
c                      kc=mod(int((alog10(zp(i))-alog10(zmin))/dz10)+1,(ncol+1))
c                    endif
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

                zp(1)=zs(nx*(iy-1)+ix)
                zp(2)=zs(nx*(iy-1)+ix+1)
                zp(3)=zs(nx*iy+ix+1)
                zp(4)=zs(nx*iy+ix)

                if (icoltile.ne.0) then
                  if (ksplinecmap.eq.0) then
                    kc=mod(int((sum(zp)/size(zp)-zmin)/dz10)+1,(ncol+1))
                    call mshplt_set_fill_color(kc,0,0,0)
                  else
                    zcmap=(sum(zp)/size(zp)-zmin)/zminmax
                    call mshplt_cmap_inter(zcmap,icolmap,red,green,blue)
                    call mshplt_set_fill_color(9999,
     &                int(red*100000),
     &                int(green*100000),
     &                int(blue*100000))
                  endif
                endif

                if (isurf.ne.0) call mshplt_filled_mesh_3d(4,xp,yp,zpmax,ioutlined)

                if (imcol.ne.0) then
                  do i=1,4
                    if (ksplinecmap.eq.0) then
                      kc=mod(int((zp(i)-zmin)/dz10)+1,(ncol+1))
                      call mshplt_set_marker_color(kc,0,0,0)
                    else
                      zcmap=(zp(i)-zmin)/zminmax
                      call mshplt_cmap_inter(zcmap,icolmap,red,green,blue)
                      call mshplt_set_marker_color(9999,
     &                  int(red*100000),
     &                  int(green*100000),
     &                  int(blue*100000))
                    endif
                    call mshplt_marker_3d(1,xp(i),yp(i),zpmax(i))
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

                zp(1)=zs(nx*(iy-1)+ix)
                zp(2)=zs(nx*(iy-1)+ix+1)
                zp(3)=zs(nx*iy+ix+1)
                zp(4)=zs(nx*iy+ix)

                if (icoltile.ne.0) then
                  if (ksplinecmap.eq.0) then
                    kc=mod(int((sum(zp)/size(zp)-zmin)/dz10)+1,(ncol+1))
                    call mshplt_set_fill_color(kc,0,0,0)
                  else
                    zcmap=(sum(zp)/size(zp)-zmin)/zminmax
                    call mshplt_cmap_inter(zcmap,icolmap,red,green,blue)
                    call mshplt_set_fill_color(9999,
     &                int(red*100000),
     &                int(green*100000),
     &                int(blue*100000))
                  endif
                endif

                if (isurf.ne.0) call mshplt_filled_mesh_3d(4,xp,yp,zpmax,ioutlined)

                if (imcol.ne.0) then
                  do i=1,4
                    if (ksplinecmap.eq.0) then
                      kc=mod(int((zp(i)-zmin)/dz10)+1,(ncol+1))
                      call mshplt_set_marker_color(kc,0,0,0)
                    else
                      zcmap=(sum(zp)/size(zp)-zmin)/zminmax
                      call mshplt_cmap_inter(zcmap,icolmap,red,green,blue)
                      call mshplt_set_marker_color(9999,
     &                  int(red*100000),
     &                  int(green*100000),
     &                  int(blue*100000))
                    endif
                    call mshplt_marker_3d(1,xp(i),yp(i),zpmax(i))
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

                zp(1)=zs(nx*(iy-1)+ix)
                zp(2)=zs(nx*(iy-1)+ix+1)
                zp(3)=zs(nx*iy+ix+1)
                zp(4)=zs(nx*iy+ix)

                if (icoltile.ne.0) then
                  if (ksplinecmap.eq.0) then
                    kc=mod(int((sum(zp)/size(zp)-zmin)/dz10)+1,(ncol+1))
                    call mshplt_set_fill_color(kc,0,0,0)
                  else
                    zcmap=(sum(zp)/size(zp)-zmin)/zminmax
                    call mshplt_cmap_inter(zcmap,icolmap,red,green,blue)
                    call mshplt_set_fill_color(9999,
     &                int(red*100000),
     &                int(green*100000),
     &                int(blue*100000))
                  endif
                endif

                if (isurf.ne.0) call mshplt_filled_mesh_3d(4,xp,yp,zpmax,ioutlined)

                if (imcol.ne.0) then
                  do i=1,4
                    if (ksplinecmap.eq.0) then
                      kc=mod(int((zp(i)-zmin)/dz10)+1,(ncol+1))
                      call mshplt_set_marker_color(kc,0,0,0)
                    else
                      zcmap=(sum(zp)/size(zp)-zmin)/zminmax
                      call mshplt_cmap_inter(zcmap,icolmap,red,green,blue)
                      call mshplt_set_marker_color(9999,
     &                  int(red*100000),
     &                  int(green*100000),
     &                  int(blue*100000))
                    endif
                    call mshplt_marker_3d(1,xp(i),yp(i),zpmax(i))
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

                zp(1)=zs(nx*(iy-1)+ix)
                zp(2)=zs(nx*(iy-1)+ix+1)
                zp(3)=zs(nx*iy+ix+1)
                zp(4)=zs(nx*iy+ix)

                if (icoltile.ne.0) then
                  if (ksplinecmap.eq.0) then
                    kc=mod(int((sum(zp)/size(zp)-zmin)/dz10)+1,(ncol+1))
                    call mshplt_set_fill_color(kc,0,0,0)
                  else
                    zcmap=(sum(zp)/size(zp)-zmin)/zminmax
                    call mshplt_cmap_inter(zcmap,icolmap,red,green,blue)
                    call mshplt_set_fill_color(9999,
     &                int(red*100000),
     &                int(green*100000),
     &                int(blue*100000))
                  endif

                endif

                if (isurf.ne.0) call mshplt_filled_mesh_3d(4,xp,yp,zpmax,ioutlined)

                if (imcol.ne.0) then
                  do i=1,4
                    if (ksplinecmap.eq.0) then
                      kc=mod(int((zp(i)-zmin)/dz10)+1,(ncol+1))
                      call mshplt_set_marker_color(kc,0,0,0)
                    else
                      zcmap=(sum(zp)/size(zp)-zmin)/zminmax
                      call mshplt_cmap_inter(zcmap,icolmap,red,green,blue)
                      call mshplt_set_marker_color(9999,
     &                  int(red*100000),
     &                  int(green*100000),
     &                  int(blue*100000))
                    endif
                    call mshplt_marker_3d(1,xp(i),yp(i),zpmax(i))
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

                zp(1)=zs(nx*(iy-1)+ix)
                zp(2)=zs(nx*(iy-1)+ix+1)
                zp(3)=zs(nx*iy+ix+1)
                zp(4)=zs(nx*iy+ix)

                if (icoltile.ne.0) then
                  if (ksplinecmap.eq.0) then
                    kc=mod(int((sum(zp)/size(zp)-zmin)/dz10)+1,(ncol+1))
                    call mshplt_set_fill_color(kc,0,0,0)
                  else
                    zcmap=(sum(zp)/size(zp)-zmin)/zminmax
                    call mshplt_cmap_inter(zcmap,icolmap,red,green,blue)
                    call mshplt_set_fill_color(9999,
     &                int(red*100000),
     &                int(green*100000),
     &                int(blue*100000))
                  endif
                endif

                if (isurf.ne.0) call mshplt_filled_mesh_3d(4,xp,yp,zpmax,ioutlined)

                if (imcol.ne.0) then
                  do i=1,4
                    if (ksplinecmap.eq.0) then
                      kc=mod(int((zp(i)-zmin)/dz10)+1,(ncol+1))
                      call mshplt_set_marker_color(kc,0,0,0)
                    else
                      zcmap=(sum(zp)/size(zp)-zmin)/zminmax
                      call mshplt_cmap_inter(zcmap,icolmap,red,green,blue)
                      call mshplt_set_marker_color(9999,
     &                  int(red*100000),
     &                  int(green*100000),
     &                  int(blue*100000))
                    endif
                    call mshplt_marker_3d(1,xp(i),yp(i),zpmax(i))
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

                zp(1)=zs(nx*(iy-1)+ix)
                zp(2)=zs(nx*(iy-1)+ix+1)
                zp(3)=zs(nx*iy+ix+1)
                zp(4)=zs(nx*iy+ix)

                if (icoltile.ne.0) then
                  if (ksplinecmap.eq.0) then
                    kc=mod(int((sum(zp)/size(zp)-zmin)/dz10)+1,(ncol+1))
                    call mshplt_set_fill_color(kc,0,0,0)
                  else
                    zcmap=(sum(zp)/size(zp)-zmin)/zminmax
                    call mshplt_cmap_inter(zcmap,icolmap,red,green,blue)
                    call mshplt_set_fill_color(9999,
     &                int(red*100000),
     &                int(green*100000),
     &                int(blue*100000))
                  endif
                endif

                if (isurf.ne.0) call mshplt_filled_mesh_3d(4,xp,yp,zpmax,ioutlined)

                if (imcol.ne.0) then
                  do i=1,4
                    if (ksplinecmap.eq.0) then
                      kc=mod(int((zp(i)-zmin)/dz10)+1,(ncol+1))
                      call mshplt_set_marker_color(kc,0,0,0)
                    else
                      zcmap=(sum(zp)/size(zp)-zmin)/zminmax
                      call mshplt_cmap_inter(zcmap,icolmap,red,green,blue)
                      call mshplt_set_marker_color(9999,
     &                  int(red*100000),
     &                  int(green*100000),
     &                  int(blue*100000))
                    endif
                    call mshplt_marker_3d(1,xp(i),yp(i),zpmax(i))
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

                zp(1)=zs(nx*(iy-1)+ix)
                zp(2)=zs(nx*(iy-1)+ix+1)
                zp(3)=zs(nx*iy+ix+1)
                zp(4)=zs(nx*iy+ix)

                if (icoltile.ne.0) then
                  if (ksplinecmap.eq.0) then
                    kc=mod(int((sum(zp)/size(zp)-zmin)/dz10)+1,(ncol+1))
                    call mshplt_set_fill_color(kc,0,0,0)
                  else
                    zcmap=(sum(zp)/size(zp)-zmin)/zminmax
                    call mshplt_cmap_inter(zcmap,icolmap,red,green,blue)
                    call mshplt_set_fill_color(9999,
     &                int(red*100000),
     &                int(green*100000),
     &                int(blue*100000))
                  endif
                endif

                if (isurf.ne.0) call mshplt_filled_mesh_3d(4,xp,yp,zpmax,ioutlined)

                if (imcol.ne.0) then
                  do i=1,4
                    if (ksplinecmap.eq.0) then
                      kc=mod(int((zp(i)-zmin)/dz10)+1,(ncol+1))
                      call mshplt_set_marker_color(kc,0,0,0)
                    else
                      zcmap=(sum(zp)/size(zp)-zmin)/zminmax
                      call mshplt_cmap_inter(zcmap,icolmap,red,green,blue)
                      call mshplt_set_marker_color(9999,
     &                  int(red*100000),
     &                  int(green*100000),
     &                  int(blue*100000))
                    endif
                  enddo
                  call mshplt_marker_3d(1,xp(i),yp(i),zpmax(i))
                endif
              enddo
            enddo

          endif !phi.le.90.

        endif !theta

      endif

      if (ifbox_ps.ne.0) call mshplt_draw_forground_box

      if (theta_ps.eq.90.and.phi_ps.eq.0.0) then
        call mshplt_color_bar(256,zmin,zmax)
      endif

      call mshplt_set_marker_color(mco,mro,mgo,mbo)
      call mshplt_set_fill_color(kco,kro,kgo,kbo)

      call mshplt_fill_buff('% end of mshplt_surf')

      ksplinecmap=0

      return
      end
