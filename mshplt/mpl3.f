*CMZ :  0.00/06 19/08/2014  18.59.39  by  Michael Scheer
*CMZ :  1.17/00 17/04/2014  18.23.45  by  Michael Scheer
*CMZ :  1.16/04 16/04/2014  14.56.04  by  Michael Scheer
*-- Author :    Michael Scheer   16/04/2014
      subroutine mpl3(n,x,y,z)

      implicit none

      integer n
     &  ,icolor,ired,igreen,iblue
      real x(n),y(n),z(n)



      call mshplt_get_line_color(
     &  icolor,ired,igreen,iblue
     &  )
      call mshplt_set_line_color(
     &  icolor,ired,igreen,iblue
     &  )

      call mshplt_pline3d(n,x,y,z)

      call mshplt_get_color(
     &  icolor,ired,igreen,iblue
     &  )
      call mshplt_set_color(
     &  icolor,ired,igreen,iblue
     &  )

      return
      end
