*CMZ :  2.02/02 03/03/2022  11.56.11  by  Michael Scheer
*CMZ :  2.02/01 10/11/2021  10.13.19  by  Michael Scheer
*CMZ :  2.00/03 20/04/2018  12.36.16  by  Michael Scheer
*CMZ :  2.00/01 16/04/2018  14.02.48  by  Michael Scheer
*CMZ :  1.25/03 23/03/2018  16.36.44  by  Michael Scheer
*CMZ :  1.25/02 22/03/2018  14.48.44  by  Michael Scheer
*CMZ :  1.25/01 16/03/2018  16.51.35  by  Michael Scheer
*CMZ :  1.25/00 15/03/2018  21.53.23  by  Michael Scheer
*-- Author :    Michael Scheer   08/03/2018
      subroutine undumag_bar_to_fila(k,icoil)

      use undumagf90m

      implicit none

*KEEP,phyconparam.
      include 'phyconparam.cmn'
*KEND.

      double precision w,h,xc,yc,zc,xx,yy,zz,rmat(3,3),x0,y0,z0,dz,dy,dl,
     &  curr,cw

      integer k,icoil,iw,nz,ny,kolor,iz,iy

      iw=ncwires

      curr=rectbar(1,k)

      x0=rectbar(2,k)
      y0=rectbar(3,k)
      z0=rectbar(4,k)

      dl=rectbar(5,k)
      w=rectbar(6,k)
      h=rectbar(7,k)
      nz=rectbar(8,k)
      ny=rectbar(9,k)
      kolor=rectbar(10,k)

      rmat(1,1:3)=rectbar(11:13,k)
      rmat(2,1:3)=rectbar(14:16,k)
      rmat(3,1:3)=rectbar(17:19,k)

      dy=h/ny
      dz=w/nz

      cw=curr/(ny*nz)

      xx=dl/2.0d0
      do iy=1,ny
        yy=-h/2.0d0+(iy-0.5d0)*dy
        do iz=1,nz
          iw=iw+1
          zz=-w/2.0d0+(iz-0.5d0)*dz
          wire(1,iw)=6 ! type bar
          wire(2,iw)=cw
          wire(3,iw)=-rmat(1,1)*xx+rmat(1,2)*yy+rmat(1,3)*zz+x0
          wire(4,iw)=-rmat(2,1)*xx+rmat(2,2)*yy+rmat(2,3)*zz+y0
          wire(5,iw)=-rmat(3,1)*xx+rmat(3,2)*yy+rmat(3,3)*zz+z0
          wire(6,iw)= rmat(1,1)*xx+rmat(1,2)*yy+rmat(1,3)*zz+x0
          wire(7,iw)= rmat(2,1)*xx+rmat(2,2)*yy+rmat(2,3)*zz+y0
          wire(8,iw)= rmat(3,1)*xx+rmat(3,2)*yy+rmat(3,3)*zz+z0
          wire(9,iw)=kolor
          wire(10,iw)=k ! number
          wire(11,iw)=icoil ! coil number
        enddo
      enddo

      ncwires=iw

      return
      end
