*CMZ :  1.20/00 21/06/2017  16.25.37  by  Michael Scheer
*-- Author :    Michael Scheer   21/06/2017
*CMZ :  0.99/00 27/01/2004  16.39.01  by  Michael Scheer
*CMZ :  0.00/08 21/01/2004  17.45.18  by  Michael Scheer
*CMZ :  0.00/06 14/01/2004  16.32.20  by  Michael Scheer
*CMZ :  0.00/05 23/12/2003  16.08.27  by  Michael Scheer
*CMZ :  0.00/04 19/12/2003  18.32.08  by  Michael Scheer
*-- Author :    Michael Scheer   19/12/2003
      subroutine areim(x,a,b,z,are,aim)

      implicit none

      double precision x,a,b,z,are,aim,arg1,x2,z2,a2,b2,y,az2,y2,b3

      x2=x*x
      z2=z*z
      a2=a*a
      b2=b*b
      b3=b2*b
      az2=a2*z2
      y=a*x+b
      y2=y*y

      arg1=sqrt(y2+x2+z2)

      are=2.0d0*z2*(
     &  arg1*((a*x-b)*(az2+b2))
     &  +(((a2+1.d0)*x2-b2+z2)*a2-b2)*z2
     &  +(( a2-1.d0)*x2-b2)*b2)

      aim=2.0d0*z*(
     &  arg1*((az2+b*y)*a*z2+b3*x)
     &  + ((y*a2+2.0d0*b)*z2 + b*(y2+2.d0*x2))*a*z2
     &  +y*b3*x)

      return
      end
