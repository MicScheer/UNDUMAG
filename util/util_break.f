*CMZ :  2.04/16 11/09/2023  09.16.02  by  Michael Scheer
*CMZ :  2.04/03 01/03/2023  12.23.09  by  Michael Scheer
*CMZ :  2.04/00 04/10/2022  08.13.33  by  Michael Scheer
*CMZ :  4.00/11 27/05/2021  09.41.25  by  Michael Scheer
*CMZ :  3.02/04 03/12/2014  15.11.16  by  Michael Scheer
*-- Author :    Michael Scheer   03/12/2014
      subroutine util_break
*KEEP,debugutil,T=F77.
      double precision x_debug,y_debug,z_debug,a_debug(100)
      integer i_debug,k_debug

      common/c_debug/x_debug,y_debug,z_debug,a_debug,i_debug,k_debug
*KEND.
      print*,"i_debug:",i_debug
      return
      end
