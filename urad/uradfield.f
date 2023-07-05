*CMZ :  2.01/03 17/07/2018  11.17.29  by  Michael Scheer
*CMZ :  1.25/00 19/10/2017  12.21.37  by  Michael Scheer
*CMZ :  0.00/01 26/04/2016  08.56.34  by  Michael Scheer
*-- Author :    Michael Scheer   26/04/2016
      subroutine uradfield(x,y,z,bx,by,bz,ex,ey,ez,gamma,istatus)

! +PATCH,//UNDUMAG/FOR
! +DECK,undumag_uradfield.

      implicit none

      double precision x,y,z,bx,by,bz,ex,ey,ez,tolarence,gamma

      integer istatus,lun,interpol

      save

      data lun/20/
      data interpol/0/
      data tolarence/0.001d0/

c      call uradbmap(lun,x,y,z,bx,by,bz,tolarence,interpol,istatus)
      call undumag_field(x,y,z,bx,by,bz,istatus)

      ex=0.0d0
      ey=0.0d0
      ez=0.0d0

      return
      end
