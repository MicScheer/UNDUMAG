*CMZ :  2.02/01 28/12/2021  10.22.37  by  Michael Scheer
*CMZ : 00.00/02 21/07/2004  15.43.47  by  Michael Scheer
*-- Author :    Michael Scheer   21/07/2004
      subroutine util_mat_mul_vec_3x3(a,vin,vout)

      implicit none

      double precision a(3,3),vin(3),vout(3),v(3)

      v=vin

      vout(1)=a(1,1)*v(1)+a(1,2)*v(2)+a(1,3)*v(3)
      vout(2)=a(2,1)*v(1)+a(2,2)*v(2)+a(2,3)*v(3)
      vout(3)=a(3,1)*v(1)+a(3,2)*v(2)+a(3,3)*v(3)

      return
      end
