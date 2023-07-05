*CMZ :  1.17/04 24/03/2016  18.25.57  by  Michael Scheer
*CMZ :  1.17/02 11/03/2016  13.32.32  by  Michael Scheer
*CMZ :  0.00/06 14/01/2004  15.36.48  by  Michael Scheer
*CMZ :  0.00/05 23/12/2003  14.17.24  by  Michael Scheer
*CMZ :  0.00/04 23/12/2003  10.12.22  by  Michael Scheer
*CMZ :  0.00/03 15/12/2003  14.19.52  by  Michael Scheer
*CMZ :  0.00/02 12/12/2003  10.32.59  by  Michael Scheer
*CMZ :  0.00/01 08/12/2003  11.19.50  by  Michael Scheer
*-- Author :    Michael Scheer   04/12/2003
      subroutine undumag_rotate(vec,t,t1)

! Calculates rotation matrix t such, that (t,vec) points in z-direction
! t1 rotates back

      double precision vec(3),vnz(3),t(3,3),t1(3,3),vn
      double precision eps,vz1,vxyz1,vyyz1

      integer i,j
      integer ifail

      data eps/1.0d-10/

      vn=sqrt(vec(1)**2+vec(2)**2+vec(3)**2)
      vnz=vec/vn

      if (abs(vnz(3)+1.0d0).lt.eps) then

        t(1,1)=-1.d0
        t(1,2)=0.d0
        t(1,3)=0.d0

        t(2,1)=0.d0
        t(2,2)=1.d0
        t(2,3)=0.d0

        t(3,1)=0.d0
        t(3,2)=0.d0
        t(3,3)=-1.d0

      else

        vz1=vnz(3)+1.0d0
        vxyz1=-vnz(1)*vnz(2)/vz1
        vyyz1=vnz(2)*vnz(2)/vz1

        t(1,1)=vnz(3)+vyyz1
        t(1,2)=vxyz1
        t(1,3)=-vnz(1)

        t(2,1)=vxyz1
        t(2,2)=1.d0-vyyz1
        t(2,3)=-vnz(2)

        t(3,1)=vnz(1)
        t(3,2)=vnz(2)
        t(3,3)=vnz(3)

      endif

      do i=1,3
        do j=1,3
          t1(i,j)=t(j,i)
        enddo
      enddo

      return
      end
