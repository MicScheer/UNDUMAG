*CMZ :  2.02/02 03/03/2022  11.56.11  by  Michael Scheer
*CMZ :  2.02/01 10/11/2021  10.13.19  by  Michael Scheer
*CMZ :  2.02/00 21/10/2020  09.46.44  by  Michael Scheer
*CMZ :  2.00/01 16/04/2018  14.01.51  by  Michael Scheer
*CMZ :  1.25/02 20/03/2018  18.02.01  by  Michael Scheer
*CMZ :  1.25/01 20/03/2018  15.53.53  by  Michael Scheer
*CMZ :  1.25/00 15/03/2018  21.53.23  by  Michael Scheer
*-- Author :    Michael Scheer   08/03/2018
      subroutine undumag_circ_arc_to_fila(k,icoil)

      use undumagf90m

      use commandlinef90m

      implicit none

*KEEP,phyconparam.
      include 'phyconparam.cmn'
*KEND.

      double precision xx,yy,zz,curr,xc,yc,zc,rc,r,rmat(3,3),phi,
     &  phi0,dphi,dr,wx,wy,wz,sinphi1,cosphi1,sinphi2,cosphi2,cw,
     &  alpha,dalpha,x0,z0,sina,cosa,rwire

      integer k,icoil,iw,ir,ia,iphi,nr,nalpha,nphi,i,kolor

c The arc is created in the x-z-plane, than rotated and shifted

      iw=ncwires

      curr=carc(1,k)

      xc=carc(2,k)
      yc=carc(3,k)
      zc=carc(4,k)

      rc=carc(5,k)
      rwire=carc(6,k)
      if (rc.le.rwire) then
        write(lun6,*)"*** Error in undumag_circ_arc_to_fila: Radius of arc",k,
     &    " lower than radius of wire ***"
        stop
      endif

      phi=carc(7,k)*pi1/180.0d0

      nr=carc(8,k)
      nalpha=carc(9,k)
      nphi=carc(10,k)

      kolor=carc(11,k)

      rmat(1,1:3)=carc(12:14,k)
      rmat(2,1:3)=carc(15:17,k)
      rmat(3,1:3)=carc(18:20,k)

      dphi=phi/nphi
      dalpha=twopi1/nalpha
      dr=rwire/nr

      cw=curr/(nalpha*nr)

      phi0=-phi/2.0d0
      sinphi1=sin(phi0)
      cosphi1=cos(phi0)
      do iphi=1,nphi
        sinphi2=sin(phi0+dphi)
        cosphi2=cos(phi0+dphi)
        do ir=1,nr
          r=(ir-0.5d0)*dr
          do ia=1,nalpha
            alpha=(ia-1)*dalpha
            sina=sin(alpha)
            cosa=cos(alpha)
            iw=iw+1
            wire(1,iw)=5 !type circ-arc
            xx=rc+r*cosa
            yy=r*sina
            zz=0.0d0
            wire(2,iw)=cw
            wire(3,iw)=xx*cosphi1
            wire(4,iw)=yy
            wire(5,iw)=xx*sinphi1
            wire(6,iw)=xx*cosphi2
            wire(7,iw)=yy
            wire(8,iw)=xx*sinphi2
            wire(9,iw)=kolor
            wire(10,iw)=k ! arctrack number
            wire(11,iw)=icoil ! coil number
          enddo
        enddo
        phi0=phi0+dphi
        sinphi1=sinphi2
        cosphi1=cosphi2
      enddo

      !rotate and translate arc

      do i=ncwires+1,iw
        wx=wire(3,i)
        wy=wire(4,i)
        wz=wire(5,i)
        wire(3,i)=rmat(1,1)*wx+rmat(1,2)*wy+rmat(1,3)*wz+xc
        wire(4,i)=rmat(2,1)*wx+rmat(2,2)*wy+rmat(2,3)*wz+yc
        wire(5,i)=rmat(3,1)*wx+rmat(3,2)*wy+rmat(3,3)*wz+zc
        wx=wire(6,i)
        wy=wire(7,i)
        wz=wire(8,i)
        wire(6,i)=rmat(1,1)*wx+rmat(1,2)*wy+rmat(1,3)*wz+xc
        wire(7,i)=rmat(2,1)*wx+rmat(2,2)*wy+rmat(2,3)*wz+yc
        wire(8,i)=rmat(3,1)*wx+rmat(3,2)*wy+rmat(3,3)*wz+zc
      enddo

      ncwires=iw

      return
      end
