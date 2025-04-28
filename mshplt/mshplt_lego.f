*CMZ :  1.04/00 11/02/2025  16.35.11  by  Michael Scheer
*CMZ :  1.03/03 02/02/2025  09.09.45  by  Michael Scheer
*CMZ :  1.03/01 07/10/2014  14.12.42  by  Michael Scheer
*CMZ :  1.03/00 07/10/2014  10.36.01  by  Michael Scheer
*CMZ :  1.02/01 05/10/2014  15.49.53  by  Michael Scheer
*CMZ :  1.02/00 03/10/2014  12.39.05  by  Michael Scheer
*-- Author :    Michael Scheer   30/09/2014
      subroutine mshplt_lego(nx,xminin,xmaxin,ny,yminin,ymaxin,z,ilego)

      implicit none

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.

      real dx,dy,xmin,xmax,ymin,ymax,z(nx*ny),dz10,zmin,zmax,zc,zl,zbase,x,y,zmean,
     &  xminin,xmaxin,yminin,ymaxin

      integer nx,ny,ix,iy,ioutlined,i,iz,
     &  kco,kro,kbo,kgo,ilego,levels,nseg,kdcol,
     &  ifilcol,ifr,ifg,ifb

      integer kcol(3),kred(3),kgreen(3),kblue(3)

      data ioutlined/1/

      call mshplt_get_fill_color(ifilcol,ifr,ifg,ifb)

      xmin=xminin
      xmax=xmaxin
      ymin=yminin
      ymax=ymaxin

      if (xminin.eq.xmaxin) then
        x=(xmaxin+xminin)/2.
        xmin=x-0.5
        xmax=x+0.5
      endif

      if (yminin.eq.ymaxin) then
        y=(ymaxin+yminin)/2.
        ymin=y-0.5
        ymax=y+0.5
      endif

      dx=(xmax-xmin)/max(1,(nx-1))
      dy=(ymax-ymin)/max(1,(ny-1))

      zmin=minval(z)
      zmax=maxval(z)

      xmin=xmin-dx/2.0
      ymin=ymin-dy/2.0

      if (
     &    xmin3d_ps.eq.xmax3d_ps.or.
     &    ymin3d_ps.eq.ymax3d_ps.or.
     &    zmin3d_ps.eq.zmax3d_ps
     &    ) then
        print*,''
        print*,"*** Warning in mshplt_lego: Bad 3d-frame ***"
        print*,"*** Will try default 3d-frame ***"
        print*,''
        if (zmin3d_ps.eq.zmax3d_ps) then
          zmean=(zmin+zmax)/2.
          zmin3d_ps=zmean-0.5
          zmax3d_ps=zmean+0.5
        endif
        call mshplt_frame3d(xmin-dx/2.,xmax+dx/2.,ymin-dy/2.,ymax+dy/2.,zmin,zmax,'x','y','z','')
      endif

      call mshplt_fill_buff('% begin of mshplt_lego')

      call mshplt_get_fill_color(kco,kro,kgo,kbo)

      kcol=kco
      kred=kro
      kgreen=kgreen
      kblue=kbo

      levels=1
      if (ilego.gt.1) levels=ilego

      if (log10z_ps.eq.0) then
        zbase=0.0
        dz10=max(abs(zmax),abs(zmin),zmax-zmin)*1.001/max(1,levels)
      else
        zbase=10**int(zmin3d_ps)
        dz10=(zmax/zbase)**(1./max(1,levels))
      endif

      if (theta_ps.le.90.) then

        if (phi_ps.le.90.) then

          do iy=ny,1,-1
            do ix=nx,1,-1

              iz=nx*(iy-1)+ix
              if (levels.gt.1) then
                if (log10z_ps.eq.0) then
                  nseg=int(abs(z(iz))/dz10)+1
                else
                  nseg=0
                  do i=1,levels
                    if (zbase*dz10**i.ge.z(iz)) goto 122
                  enddo
 122              nseg=min(levels,i)
                endif
              else
                nseg=1
              endif
              do i=1,nseg
                if (levels.gt.1) then
                  kdcol=256/levels
                  kred=0
                  kgreen=0
                  kblue=0
                  zl=dz10
                  if (log10z_ps.eq.0) then
                    if (z(iz).ge.zbase) then
                      if (zbase+i*dz10.gt.z(iz)) then
                        zl=z(iz)-(zbase+dz10*(i-1))
                      endif
                      zc=zbase+dz10*(i-1)+zl/2.
                    else
                      if (z(iz)+i*dz10.gt.zbase) zl=zbase-(z(iz)+dz10*(i-1))
                      zc=z(iz)+dz10*(i-1)+zl/2.
                    endif
                    kcol=int((zc-zmin)/dz10)*kdcol+1
                  else
                    zl=zbase*(levels**i-levels**(i-1))
                    zc=zbase*levels**(i-1)+zl/2.
                    zl=zbase*(dz10**i-dz10**(i-1))
                    zc=zbase*dz10**(i-1)+zl/2.
                    if (zc+zl/2.0.gt.z(iz)) then
                      zl=z(iz)-zbase*dz10**(i-1)
                      zc=z(iz)-zl/2.
                    endif
c obsolete                    kcol=mod(int(alog10(zc+zl/2.)-0.001)+1,8)
                    kcol=mod(i+1,8)
                  endif

                  call mshplt_box_3d(
     &              xmin+(ix-0.5)*dx,ymin+(iy-0.5)*dy,zc,
     &              dx,dy,zl,kcol,kred,kgreen,kblue,0)
                  if (i.eq.nseg.and.ioutlined.ne.0) then
                    zc=(z(iz)+zbase)/2.
                    zl=abs(z(iz)-zbase)
                    call mshplt_box_3d(
     &                xmin+(ix-0.5)*dx,ymin+(iy-0.5)*dy,zc,
     &                dx,dy,zl,kcol,kred,kgreen,kblue,-1)
                  endif
                else
                  zc=(z(iz)+zbase)/2.
                  zl=abs(z(iz)-zbase)
                  call mshplt_box_3d(
     &              xmin+(ix-0.5)*dx,ymin+(iy-0.5)*dy,zc,
     &              dx,dy,zl,kcol,kred,kgreen,kblue,1)
                endif
              enddo
c              call mshplt_stop
            enddo
          enddo

        else if (phi_ps.le.180.) then

            do ix=nx,1,-1
              do iy=1,ny

              iz=nx*(iy-1)+ix
              if (levels.gt.1) then
                if (log10z_ps.eq.0) then
                  nseg=int(abs(z(iz))/dz10)+1
                else
                  nseg=0
                  do i=1,levels
                    if (zbase*dz10**i.ge.z(iz)) goto 124
                  enddo
 124              nseg=min(levels,i)
                endif
              else
                nseg=1
              endif
              do i=1,nseg
                if (levels.gt.1) then
                  kred=0
                  kgreen=0
                  kblue=0
                  zl=dz10
                  if (log10z_ps.eq.0) then
                    if (z(iz).ge.zbase) then
                      if (zbase+i*dz10.gt.z(iz)) then
                        zl=z(iz)-(zbase+dz10*(i-1))
                      endif
                      zc=zbase+dz10*(i-1)+zl/2.
                    else
                      if (z(iz)+i*dz10.gt.zbase) zl=zbase-(z(iz)+dz10*(i-1))
                      zc=z(iz)+dz10*(i-1)+zl/2.
                    endif
                    kcol=mod(int((zc-zmin)/dz10)+1,8)
                  else
                    zl=zbase*(levels**i-levels**(i-1))
                    zc=zbase*levels**(i-1)+zl/2.
                    zl=zbase*(dz10**i-dz10**(i-1))
                    zc=zbase*dz10**(i-1)+zl/2.
                    if (zc+zl/2.0.gt.z(iz)) then
                      zl=z(iz)-zbase*dz10**(i-1)
                      zc=z(iz)-zl/2.
                    endif
c obsolete                    kcol=mod(int(alog10(zc+zl/2.)-0.001)+1,8)
                    kcol=mod(i+1,8)
                  endif

                  call mshplt_box_3d(
     &              xmin+(ix-0.5)*dx,ymin+(iy-0.5)*dy,zc,
     &              dx,dy,zl,kcol,kred,kgreen,kblue,0)
                  if (i.eq.nseg.and.ioutlined.ne.0) then
                    zc=(z(iz)+zbase)/2.
                    zl=abs(z(iz)-zbase)
                    call mshplt_box_3d(
     &                xmin+(ix-0.5)*dx,ymin+(iy-0.5)*dy,zc,
     &                dx,dy,zl,kcol,kred,kgreen,kblue,-1)
                  endif
                else
                  zc=(z(iz)+zbase)/2.
                  zl=abs(z(iz)-zbase)
                  call mshplt_box_3d(
     &              xmin+(ix-0.5)*dx,ymin+(iy-0.5)*dy,zc,
     &              dx,dy,zl,kcol,kred,kgreen,kblue,1)
                endif
              enddo
c              call mshplt_stop
              enddo
            enddo

          else if (phi_ps.le.270.) then

            do iy=1,ny
              do ix=1,nx

              iz=nx*(iy-1)+ix
              if (levels.gt.1) then
                if (log10z_ps.eq.0) then
                  nseg=int(abs(z(iz))/dz10)+1
                else
                  nseg=0
                  do i=1,levels
                    if (zbase*dz10**i.ge.z(iz)) goto 125
                  enddo
 125              nseg=min(levels,i)
                endif
              else
                nseg=1
              endif
              do i=1,nseg
                if (levels.gt.1) then
                  kred=0
                  kgreen=0
                  kblue=0
                  zl=dz10
                  if (log10z_ps.eq.0) then
                    if (z(iz).ge.zbase) then
                      if (zbase+i*dz10.gt.z(iz)) then
                        zl=z(iz)-(zbase+dz10*(i-1))
                      endif
                      zc=zbase+dz10*(i-1)+zl/2.
                    else
                      if (z(iz)+i*dz10.gt.zbase) zl=zbase-(z(iz)+dz10*(i-1))
                      zc=z(iz)+dz10*(i-1)+zl/2.
                    endif
                    kcol=mod(int((zc-zmin)/dz10)+1,8)
                  else
                    zl=zbase*(levels**i-levels**(i-1))
                    zc=zbase*levels**(i-1)+zl/2.
                    zl=zbase*(dz10**i-dz10**(i-1))
                    zc=zbase*dz10**(i-1)+zl/2.
                    if (zc+zl/2.0.gt.z(iz)) then
                      zl=z(iz)-zbase*dz10**(i-1)
                      zc=z(iz)-zl/2.
                    endif
c obsolete                    kcol=mod(int(alog10(zc+zl/2.)-0.001)+1,8)
                    kcol=mod(i+1,8)
                  endif

                  call mshplt_box_3d(
     &              xmin+(ix-0.5)*dx,ymin+(iy-0.5)*dy,zc,
     &              dx,dy,zl,kcol,kred,kgreen,kblue,0)
                  if (i.eq.nseg.and.ioutlined.ne.0) then
                    zc=(z(iz)+zbase)/2.
                    zl=abs(z(iz)-zbase)
                    call mshplt_box_3d(
     &                xmin+(ix-0.5)*dx,ymin+(iy-0.5)*dy,zc,
     &                dx,dy,zl,kcol,kred,kgreen,kblue,-1)
                  endif
                else
                  zc=(z(iz)+zbase)/2.
                  zl=abs(z(iz)-zbase)
                  call mshplt_box_3d(
     &              xmin+(ix-0.5)*dx,ymin+(iy-0.5)*dy,zc,
     &              dx,dy,zl,kcol,kred,kgreen,kblue,1)
                endif
              enddo
c              call mshplt_stop
              enddo
            enddo

          else if (phi_ps.le.360.) then
            do ix=1,nx
              do iy=ny,1,-1

              iz=nx*(iy-1)+ix
              if (levels.gt.1) then
                if (log10z_ps.eq.0) then
                  nseg=int(abs(z(iz))/dz10)+1
                else
                  nseg=0
                  do i=1,levels
                    if (zbase*dz10**i.ge.z(iz)) goto 127
                  enddo
 127              nseg=min(levels,i)
                endif
              else
                nseg=1
              endif
              do i=1,nseg
                if (levels.gt.1) then
                  kred=0
                  kgreen=0
                  kblue=0
                  zl=dz10
                  if (log10z_ps.eq.0) then
                    if (z(iz).ge.zbase) then
                      if (zbase+i*dz10.gt.z(iz)) then
                        zl=z(iz)-(zbase+dz10*(i-1))
                      endif
                      zc=zbase+dz10*(i-1)+zl/2.
                    else
                      if (z(iz)+i*dz10.gt.zbase) zl=zbase-(z(iz)+dz10*(i-1))
                      zc=z(iz)+dz10*(i-1)+zl/2.
                    endif
                    kcol=mod(int((zc-zmin)/dz10)+1,8)
                  else
                    zl=zbase*(levels**i-levels**(i-1))
                    zc=zbase*levels**(i-1)+zl/2.
                    zl=zbase*(dz10**i-dz10**(i-1))
                    zc=zbase*dz10**(i-1)+zl/2.
                    if (zc+zl/2.0.gt.z(iz)) then
                      zl=z(iz)-zbase*dz10**(i-1)
                      zc=z(iz)-zl/2.
                    endif
c obsolete                    kcol=mod(int(alog10(zc+zl/2.)-0.001)+1,8)
                    kcol=mod(i+1,8)
                  endif

                  call mshplt_box_3d(
     &              xmin+(ix-0.5)*dx,ymin+(iy-0.5)*dy,zc,
     &              dx,dy,zl,kcol,kred,kgreen,kblue,0)
                  if (i.eq.nseg.and.ioutlined.ne.0) then
                    zc=(z(iz)+zbase)/2.
                    zl=abs(z(iz)-zbase)
                    call mshplt_box_3d(
     &                xmin+(ix-0.5)*dx,ymin+(iy-0.5)*dy,zc,
     &                dx,dy,zl,kcol,kred,kgreen,kblue,-1)
                  endif
                else
                  zc=(z(iz)+zbase)/2.
                  zl=abs(z(iz)-zbase)
                  call mshplt_box_3d(
     &              xmin+(ix-0.5)*dx,ymin+(iy-0.5)*dy,zc,
     &              dx,dy,zl,kcol,kred,kgreen,kblue,1)
                endif
              enddo
c              call mshplt_stop
              enddo
            enddo

          endif !phi.le.90.

        else if (theta_ps.le.180.) then

          if (phi_ps.le.90.) then

            do iy=1,ny
              do ix=1,nx

              iz=nx*(iy-1)+ix
              if (levels.gt.1) then
                if (log10z_ps.eq.0) then
                  nseg=int(abs(z(iz))/dz10)+1
                else
                  nseg=0
                  do i=1,levels
                    if (zbase*dz10**i.ge.z(iz)) goto 150
                  enddo
 150              nseg=min(levels,i)
                endif
              else
                nseg=1
              endif
              do i=nseg,1,-1
                if (levels.gt.1) then
                  kred=0
                  kgreen=0
                  kblue=0
                  zl=dz10
                  if (log10z_ps.eq.0) then
                    if (z(iz).ge.zbase) then
                      if (zbase+i*dz10.gt.z(iz)) then
                        zl=z(iz)-(zbase+dz10*(i-1))
                      endif
                      zc=zbase+dz10*(i-1)+zl/2.
                    else
                      if (z(iz)+i*dz10.gt.zbase) zl=zbase-(z(iz)+dz10*(i-1))
                      zc=z(iz)+dz10*(i-1)+zl/2.
                    endif
                    kcol=mod(int((zc-zmin)/dz10)+1,8)
                  else
                    zl=zbase*(levels**i-levels**(i-1))
                    zc=zbase*levels**(i-1)+zl/2.
                    zl=zbase*(dz10**i-dz10**(i-1))
                    zc=zbase*dz10**(i-1)+zl/2.
                    if (zc+zl/2.0.gt.z(iz)) then
                      zl=z(iz)-zbase*dz10**(i-1)
                      zc=z(iz)-zl/2.
                    endif
c obsolete                    kcol=mod(int(alog10(zc+zl/2.)-0.001)+1,8)
                    kcol=mod(i+1,8)
                  endif

                  call mshplt_box_3d(
     &              xmin+(ix-0.5)*dx,ymin+(iy-0.5)*dy,zc,
     &              dx,dy,zl,kcol,kred,kgreen,kblue,0)
                  if (i.eq.1.and.ioutlined.ne.0) then
                    zc=(z(iz)+zbase)/2.
                    zl=abs(z(iz)-zbase)
                    call mshplt_box_3d(
     &                xmin+(ix-0.5)*dx,ymin+(iy-0.5)*dy,zc,
     &                dx,dy,zl,kcol,kred,kgreen,kblue,-1)
                  endif
                else
                  zc=(z(iz)+zbase)/2.
                  zl=abs(z(iz)-zbase)
                  call mshplt_box_3d(
     &              xmin+(ix-0.5)*dx,ymin+(iy-0.5)*dy,zc,
     &              dx,dy,zl,kcol,kred,kgreen,kblue,1)
                endif
              enddo
c              call mshplt_stop
              enddo
            enddo

          else if (phi_ps.le.180.) then

            do ix=1,nx
              do iy=ny,1,-1

              iz=nx*(iy-1)+ix
              if (levels.gt.1) then
                if (log10z_ps.eq.0) then
                  nseg=int(abs(z(iz))/dz10)+1
                else
                  nseg=0
                  do i=1,levels
                    if (zbase*dz10**i.ge.z(iz)) goto 128
                  enddo
 128              nseg=min(levels,i)
                endif
              else
                nseg=1
              endif
              do i=nseg,1,-1
                if (levels.gt.1) then
                  kred=0
                  kgreen=0
                  kblue=0
                  zl=dz10
                  if (log10z_ps.eq.0) then
                    if (z(iz).ge.zbase) then
                      if (zbase+i*dz10.gt.z(iz)) then
                        zl=z(iz)-(zbase+dz10*(i-1))
                      endif
                      zc=zbase+dz10*(i-1)+zl/2.
                    else
                      if (z(iz)+i*dz10.gt.zbase) zl=zbase-(z(iz)+dz10*(i-1))
                      zc=z(iz)+dz10*(i-1)+zl/2.
                    endif
                    kcol=mod(int((zc-zmin)/dz10)+1,8)
                  else
                    zl=zbase*(levels**i-levels**(i-1))
                    zc=zbase*levels**(i-1)+zl/2.
                    zl=zbase*(dz10**i-dz10**(i-1))
                    zc=zbase*dz10**(i-1)+zl/2.
                    if (zc+zl/2.0.gt.z(iz)) then
                      zl=z(iz)-zbase*dz10**(i-1)
                      zc=z(iz)-zl/2.
                    endif
c obsolete                    kcol=mod(int(alog10(zc+zl/2.)-0.001)+1,8)
                    kcol=mod(i+1,8)
                  endif

                  call mshplt_box_3d(
     &              xmin+(ix-0.5)*dx,ymin+(iy-0.5)*dy,zc,
     &              dx,dy,zl,kcol,kred,kgreen,kblue,0)
                  if (i.eq.1.and.ioutlined.ne.0) then
                    zc=(z(iz)+zbase)/2.
                    zl=abs(z(iz)-zbase)
                    call mshplt_box_3d(
     &                xmin+(ix-0.5)*dx,ymin+(iy-0.5)*dy,zc,
     &                dx,dy,zl,kcol,kred,kgreen,kblue,-1)
                  endif
                else
                  zc=(z(iz)+zbase)/2.
                  zl=abs(z(iz)-zbase)
                  call mshplt_box_3d(
     &              xmin+(ix-0.5)*dx,ymin+(iy-0.5)*dy,zc,
     &              dx,dy,zl,kcol,kred,kgreen,kblue,1)
                endif
              enddo
            enddo
          enddo

        else if (phi_ps.le.270.) then

          do iy=ny,1,-1
            do ix=nx,1,-1

              iz=nx*(iy-1)+ix
              if (levels.gt.1) then
                if (log10z_ps.eq.0) then
                  nseg=int(abs(z(iz))/dz10)+1
                else
                  nseg=0
                  do i=1,levels
                    if (zbase*dz10**i.ge.z(iz)) goto 129
                  enddo
 129              nseg=min(levels,i)
                endif
              else
                nseg=1
              endif
              do i=nseg,1,-1
                if (levels.gt.1) then
                  kred=0
                  kgreen=0
                  kblue=0
                  zl=dz10
                  if (log10z_ps.eq.0) then
                    if (z(iz).ge.zbase) then
                      if (zbase+i*dz10.gt.z(iz)) then
                        zl=z(iz)-(zbase+dz10*(i-1))
                      endif
                      zc=zbase+dz10*(i-1)+zl/2.
                    else
                      if (z(iz)+i*dz10.gt.zbase) zl=zbase-(z(iz)+dz10*(i-1))
                      zc=z(iz)+dz10*(i-1)+zl/2.
                    endif
                    kcol=mod(int((zc-zmin)/dz10)+1,8)
                  else
                    zl=zbase*(levels**i-levels**(i-1))
                    zc=zbase*levels**(i-1)+zl/2.
                    zl=zbase*(dz10**i-dz10**(i-1))
                    zc=zbase*dz10**(i-1)+zl/2.
                    if (zc+zl/2.0.gt.z(iz)) then
                      zl=z(iz)-zbase*dz10**(i-1)
                      zc=z(iz)-zl/2.
                    endif
c obsolete                    kcol=mod(int(alog10(zc+zl/2.)-0.001)+1,8)
                    kcol=mod(i+1,8)
                  endif

                  call mshplt_box_3d(
     &              xmin+(ix-0.5)*dx,ymin+(iy-0.5)*dy,zc,
     &              dx,dy,zl,kcol,kred,kgreen,kblue,0)
                  if (i.eq.1.and.ioutlined.ne.0) then
                    zc=(z(iz)+zbase)/2.
                    zl=abs(z(iz)-zbase)
                    call mshplt_box_3d(
     &                xmin+(ix-0.5)*dx,ymin+(iy-0.5)*dy,zc,
     &                dx,dy,zl,kcol,kred,kgreen,kblue,-1)
                  endif
                else
                  zc=(z(iz)+zbase)/2.
                  zl=abs(z(iz)-zbase)
                  call mshplt_box_3d(
     &              xmin+(ix-0.5)*dx,ymin+(iy-0.5)*dy,zc,
     &              dx,dy,zl,kcol,kred,kgreen,kblue,1)
                endif
              enddo
            enddo
          enddo

        else if (phi_ps.le.360.) then

          do ix=nx,1,-1
            do iy=1,ny

              iz=nx*(iy-1)+ix
              if (levels.gt.1) then
                if (log10z_ps.eq.0) then
                  nseg=int(abs(z(iz))/dz10)+1
                else
                  nseg=0
                  do i=1,levels
                    if (zbase*dz10**i.ge.z(iz)) goto 130
                  enddo
 130              nseg=min(levels,i)
                endif
              else
                nseg=1
              endif
              do i=nseg,1,-1
                if (levels.gt.1) then
                  kred=0
                  kgreen=0
                  kblue=0
                  zl=dz10
                  if (log10z_ps.eq.0) then
                    if (z(iz).ge.zbase) then
                      if (zbase+i*dz10.gt.z(iz)) then
                        zl=z(iz)-(zbase+dz10*(i-1))
                      endif
                      zc=zbase+dz10*(i-1)+zl/2.
                    else
                      if (z(iz)+i*dz10.gt.zbase) zl=zbase-(z(iz)+dz10*(i-1))
                      zc=z(iz)+dz10*(i-1)+zl/2.
                    endif
                    kcol=mod(int((zc-zmin)/dz10)+1,8)
                  else
                    zl=zbase*(levels**i-levels**(i-1))
                    zc=zbase*levels**(i-1)+zl/2.
                    zl=zbase*(dz10**i-dz10**(i-1))
                    zc=zbase*dz10**(i-1)+zl/2.
                    if (zc+zl/2.0.gt.z(iz)) then
                      zl=z(iz)-zbase*dz10**(i-1)
                      zc=z(iz)-zl/2.
                    endif
c obsolete                    kcol=mod(int(alog10(zc+zl/2.)-0.001)+1,8)
                    kcol=mod(i+1,8)
                  endif

                  call mshplt_box_3d(
     &              xmin+(ix-0.5)*dx,ymin+(iy-0.5)*dy,zc,
     &              dx,dy,zl,kcol,kred,kgreen,kblue,0)
                  if (i.eq.1.and.ioutlined.ne.0) then
                    zc=(z(iz)+zbase)/2.
                    zl=abs(z(iz)-zbase)
                    call mshplt_box_3d(
     &                xmin+(ix-0.5)*dx,ymin+(iy-0.5)*dy,zc,
     &                dx,dy,zl,kcol,kred,kgreen,kblue,-1)
                  endif
                else
                  zc=(z(iz)+zbase)/2.
                  zl=abs(z(iz)-zbase)
                  call mshplt_box_3d(
     &              xmin+(ix-0.5)*dx,ymin+(iy-0.5)*dy,zc,
     &              dx,dy,zl,kcol,kred,kgreen,kblue,1)
                endif
              enddo
            enddo
          enddo

        endif !phi.le.90.

      endif !theta

      if (ifbox_ps.ne.0) call mshplt_draw_forground_box

      call mshplt_set_fill_color(kco,kro,kgo,kbo)

      call mshplt_fill_buff('% end of mshplt_lego')

      return
      end
