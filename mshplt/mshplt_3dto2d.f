*CMZ :  1.02/00 30/09/2014  21.25.28  by  Michael Scheer
*CMZ :  1.01/02 25/09/2014  16.02.25  by  Michael Scheer
*CMZ :  0.01/02 05/09/2014  15.41.43  by  Michael Scheer
*CMZ :  0.00/06 20/08/2014  12.06.59  by  Michael Scheer
*CMZ :  0.00/03 04/08/2014  13.33.24  by  Michael Scheer
*CMZ :  0.00/02 09/07/2014  16.03.13  by  Michael Scheer
*-- Author :    Michael Scheer   07/07/2014
      subroutine mshplt_3dto2d(n,x,y,z,xp,yp)

      implicit none

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.

      real x(*),y(*),z(*),xp(*),yp(*)

      real t(3,3),tx(3,3),tz(3,3),
     &  cosphi,sinphi,costhe,sinthe,phio,theo,phi,the
      integer i,j,k,m,n

      data phio/-9999./
      data theo/-9999./

      save

      if (phi_ps.ne.phio.or.theta_ps.ne.theo) then

        phi=phi_ps/radtodeg_ps
        the=theta_ps/radtodeg_ps

        sinphi=sin(phi)
        cosphi=cos(phi)
c        sinthe=sin(the)
c        costhe=cos(the)
        if (theta_ps.gt.90.) then
          sinthe=cos(the) ! we use hplot-convention here
          costhe=-sin(the)
        else
          sinthe=cos(the) ! we use hplot-convention here
          costhe=sin(the)
        endif

        !rotate around z-axis

        tz(1,1)=cosphi
        tz(1,2)=-sinphi
        tz(1,3)=0.0

        tz(2,1)=sinphi
        tz(2,2)=cosphi
        tz(2,3)=0.0

        tz(3,1)=0.0
        tz(3,2)=0.0
        tz(3,3)=1.0

        !rotate around x-axis

        tx(1,1)=1.0
        tx(1,2)=0.0
        tx(1,3)=0.0

        tx(2,1)=0.0
        tx(2,2)=costhe
        tx(2,3)=sinthe

        tx(3,1)=0.0
        tx(3,2)=-sinthe
        tx(3,3)=costhe

        t=0.0
        do i=1,3
          do j=1,3
            do k=1,3
              t(i,j)=t(i,j)+tx(i,k)*tz(k,j)
            enddo
          enddo
        enddo

        phio=phi_ps
        theo=theta_ps

      endif

      do m=1,n
        xp(m)=t(1,1)*x(m)+t(1,2)*y(m)+t(1,3)*z(m)
        yp(m)=t(2,1)*x(m)+t(2,2)*y(m)+t(2,3)*z(m)
      enddo

      return
      end
