*CMZ :  2.02/02 03/03/2022  14.18.59  by  Michael Scheer
*CMZ :  2.02/01 10/11/2021  10.13.19  by  Michael Scheer
*CMZ :  2.02/00 21/10/2020  09.46.44  by  Michael Scheer
*CMZ :  2.00/01 16/04/2018  13.59.39  by  Michael Scheer
*CMZ :  1.25/04 03/04/2018  10.39.49  by  Michael Scheer
*CMZ :  1.25/02 22/03/2018  15.08.36  by  Michael Scheer
*CMZ :  1.25/01 16/03/2018  16.51.35  by  Michael Scheer
*CMZ :  1.25/00 15/03/2018  21.53.23  by  Michael Scheer
*-- Author :    Michael Scheer   08/03/2018
      subroutine undumag_crace_to_fila(k,icoil)

      use undumagf90m

      use commandlinef90m

      implicit none

*KEEP,phyconparam.
      include 'phyconparam.cmn'
*KEND.

      double precision, dimension (:,:,:), allocatable :: zy

      double precision hdum,w,r,barl,curr,rwire,cw,vx,vy,vz,phi,alpha,cosa,sina,
     &  cosphi,sinphi,cosphi1,sinphi2,cosphi2,sinphi1,costhe,sinthe,
     &  xc,yc,zc,x0,y0,z0,dphi,dr,dy,ro,ri,xr1,zr1,xr2,zr2,vn,
     &  xi,xo,zi,zo,wx,wy,wz,ux,uy,uz,rotmat(3,3),vnor(3),rmat(3,3),
     &  delta,ddelta,rr,xx,yy,zz,zcw

      integer k,icoil,iw,ir,nr,nphi,iphi,iy,iz,kolor,ical,istat,i,j,id,
     &  ndelta,iarc1,iarc2,jarc1,jarc2,ihalf1,ihalf2,jhalf1,jhalf2

      data ical/0/

      save ical,iw

      !rotate (vx,vy,vz) to y-axis

      if (ical.eq.0) then
        ical=1
      endif

      iw=ncwires

      curr=crace(1,k)

      x0=crace(2,k)
      y0=crace(3,k)
      z0=crace(4,k)

      vx=crace(5,k)
      vy=crace(6,k)
      vz=crace(7,k)

      vn=sqrt(vx**2+vy**2+vz**2)
      if (vn.eq.0.0d0) then
        write(lun6,*)
        write(lun6,*)"*** Error in undumag_crace_to_fila: Zero normal vector for coil",k
        stop
      endif
      vx=vx/vn
      vy=vy/vn
      vz=vz/vn

      alpha=crace(8,k)
      xo=crace(9,k)/2.0d0
      zi=crace(10,k)/2.0d0
      zo=crace(11,k)/2.0d0
      rwire=(zo-zi)/2.0d0
      ri=crace(12,k)
      hdum=crace(13,k)
      nr=crace(14,k)
      ndelta=crace(15,k)
      nphi=crace(16,k)
      kolor=crace(17,k)

      w=zo-zi
      xi=xo-w
      ro=ri+w

      yc=0.0d0

      if (ri.gt.zi) then
        write(lun6,*)
        write(lun6,*)"*** Error in undumag_crace_to_fila: Inner radius of coils is greater than the inner width ***"
        stop
      endif

      dr=rwire/nr
      dphi=pi1/2.0d0/nphi
      ddelta=twopi1/ndelta

      cw=curr/(nr*ndelta)

      allocate(zy(2,ndelta,nr))

      ! first x bar
      do ir=1,nr
        rr=(ir-0.5d0)*dr
        do id=1,ndelta
          delta=(id-1)*ddelta
          cosa=cos(delta)
          sina=sin(delta)
          iw=iw+1
          wire(1,iw)=3 ! cracetrack flag
          wire(2,iw)=cw
          wire(3,iw)=+xi-ri
          wire(4,iw)=rr*sina
          wire(5,iw)=-(zi+rwire+rr*cosa)
          zy(1,id,ir)=rr*cosa
          zy(2,id,ir)=rr*sina
          wire(6,iw)=-xi+ri
          wire(7,iw)=wire(4,iw)
          wire(8,iw)=wire(5,iw)
          wire(9,iw)=kolor
          wire(10,iw)=k ! cracetrack number
          wire(11,iw)=icoil ! coil number
        enddo
      enddo

      iarc1=iw+1

      ! first arc
      xc=-xi+ri
      zc=-zi+ri
      zcw=-zi-rwire
      sinphi1=0.0d0
      cosphi1=1.0d0
      do iphi=1,nphi
        sinphi2=sin(iphi*dphi)
        cosphi2=cos(iphi*dphi)
        do ir=1,nr
          do id=1,ndelta
            iw=iw+1
            yy=zy(2,id,ir)
            r=abs(zcw-zy(1,id,ir)-zc)
            xx=xc-r*sinphi1
            zz=zc-r*cosphi1
            wire(1,iw)=2
            wire(2,iw)=cw
            wire(3,iw)=xx
            wire(4,iw)=yy
            wire(5,iw)=zz
            xx=xc-r*sinphi2
            zz=zc-r*cosphi2
            wire(6,iw)=xx
            wire(7,iw)=yy
            wire(8,iw)=zz
            wire(9,iw)=kolor
            wire(10,iw)=k ! cracetrack number
            wire(11,iw)=icoil ! coil number
          enddo
        enddo
        sinphi1=sinphi2
        cosphi1=cosphi2
      enddo

      iarc2=iw

      ! first z bar
      do ir=1,nr
        do id=1,ndelta
          iw=iw+1
          wire(1,iw)=2 ! cracetrack flag
          wire(2,iw)=cw
          wire(3:5,iw)=wire(6:8,iw-nr*ndelta)
          wire(6:7,iw)=wire(6:7,iw-nr*ndelta)
          wire(8,iw)=wire(8,iw-nr*ndelta)+2.0d0*(zi-ri)
          wire(9,iw)=kolor
          wire(10,iw)=k ! cracetrack number
          wire(11,iw)=icoil ! coil number
        enddo
      enddo

      ! second arc

      jarc1=iw+1
      jarc2=iw+iarc2-iarc1+1

      wire(1:11,jarc1:jarc2)=wire(1:11,iarc1:iarc2)

      xc=-xi+ri
      zc=-zi+ri

      do i=jarc1,jarc2
        xx=wire(3,i)-xc
        zz=wire(5,i)-zc
        wire(3,i)=zz+xc
        wire(5,i)=-xx-zc
        xx=wire(6,i)-xc
        zz=wire(8,i)-zc
        wire(6,i)=zz+xc
        wire(8,i)=-xx-zc
      enddo

      iw=jarc2

      ihalf1=ncwires+1
      ihalf2=iw
      jhalf1=iw+1
      jhalf2=iw+ihalf2-ihalf1+1

      wire(1:11,jhalf1:jhalf2)=wire(1:11,ihalf1:ihalf2)

      do i=jhalf1,jhalf2
        iw=iw+1
        xx=wire(3,i)
        zz=wire(5,i)
        wire(3,i)=-xx
        wire(5,i)=-zz
        xx=wire(6,i)
        zz=wire(8,i)
        wire(6,i)=-xx
        wire(8,i)=-zz
      enddo

      !rotate and translate coil

      vnor(1)=vx
      vnor(2)=vy
      vnor(3)=vz
      call util_rotate_vector_to_y_axis(vnor,rotmat,istat)

      do i=1,3
        do j=1,3
          rmat(i,j)=rotmat(j,i)
        enddo
      enddo

      cosa=cos(alpha/180.0d0*pi1)
      sina=sin(alpha/180.0d0*pi1)

      do i=ncwires+1,iw
        wx=cosa*wire(3,i)+sina*wire(5,i)
        wy=wire(4,i)
        wz=-sina*wire(3,i)+cosa*wire(5,i)
        wire(3:5,i)=rmat(1:3,1)*wx+rmat(1:3,2)*wy+rmat(1:3,3)*wz
        wx=wire(6,i)
        wy=wire(7,i)
        wz=wire(8,i)
        wx=cosa*wire(6,i)+sina*wire(8,i)
        wy=wire(7,i)
        wz=-sina*wire(6,i)+cosa*wire(8,i)
        wire(6:8,i)=rmat(1:3,1)*wx+rmat(1:3,2)*wy+rmat(1:3,3)*wz
        wire(3,i)=wire(3,i)+x0
        wire(4,i)=wire(4,i)+y0
        wire(5,i)=wire(5,i)+z0
        wire(6,i)=wire(6,i)+x0
        wire(7,i)=wire(7,i)+y0
        wire(8,i)=wire(8,i)+z0
      enddo

      ncwires=iw

      deallocate(zy)

      return
      end
