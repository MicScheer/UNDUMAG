*CMZ :  2.03/00 01/08/2022  10.14.43  by  Michael Scheer
*CMZ :  2.02/00 21/10/2020  09.28.08  by  Michael Scheer
*CMZ :  0.00/00 20/04/2016  12.43.58  by  Michael Scheer
*CMZ :  0.00/06 14/01/2004  15.36.48  by  Michael Scheer
*CMZ :  0.00/05 23/12/2003  14.17.24  by  Michael Scheer
*CMZ :  0.00/04 23/12/2003  10.12.22  by  Michael Scheer
*CMZ :  0.00/03 15/12/2003  14.19.52  by  Michael Scheer
*CMZ :  0.00/02 12/12/2003  10.32.59  by  Michael Scheer
*CMZ :  0.00/01 08/12/2003  11.19.50  by  Michael Scheer
*-- Author :    Michael Scheer   04/12/2003
      subroutine undumag_bpet(vnz,t,t1)

      double precision vnz(3),t(3,3),t1(3,3)
      double precision det,eps,vz1,vxyz1,vyyz1

      integer i,j
      integer ifail

      data eps/1.0d-10/

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

        vz1=vnz(3)+1.d0
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

      call util_determinante(3,t,det,ifail)

      if (ifail.ne.0) then
        write(lun6,*)'*** Error in undumag_bpet: Bad determinante of matrix'
        stop
      endif

      if (abs(det-1.0d0).gt.1.0d-8) then
        write(lun6,*)'*** Error in undumag_bpet: Bad determinante of matrix'
        write(lun6,*)
        write(lun6,*)'Matrix t'
        do i=1,3
          write(lun6,*)t(i,1),t(i,2),t(i,3)
        enddo
        write(lun6,*)
        write(lun6,*)'det,abs(det)-1: ',det,abs(det)-1.0d0
        write(lun6,*)
        stop
      endif


      return
      end
