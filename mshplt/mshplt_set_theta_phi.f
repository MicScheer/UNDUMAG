*CMZ :  1.02/00 01/10/2014  08.23.56  by  Michael Scheer
*CMZ :  0.01/02 05/09/2014  15.41.43  by  Michael Scheer
*CMZ :  0.00/03 11/07/2014  12.27.06  by  Michael Scheer
*CMZ :  0.00/02 07/07/2014  12.22.09  by  Michael Scheer
*-- Author :    Michael Scheer   07/07/2014
      subroutine mshplt_set_theta_phi(theta,phi)

      implicit none

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.

      real theta,phi,p,t

      t=mod(theta,180.)
      if (t.lt.0.0) t=t+180.

      p=mod(phi,360.)
      if (p.lt.0.0) p=p+360.

      phi_ps=p
      theta_ps=t

      return
      end
