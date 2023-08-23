*CMZ :  1.17/06 22/08/2023  09.03.52  by  Michael Scheer
*CMZ :  1.13/03 10/03/2017  10.58.57  by  Michael Scheer
*CMZ :  1.11/04 25/01/2017  09.36.52  by  Michael Scheer
*-- Author :    Michael Scheer   23/01/2017
      subroutine undumag_dipole_field(idip,xin,yin,zin,bxout,byout,bzout,kfail)

c Calculates the field of the dipole idip at xin,yin,zin

      use undumagf90m
      use bpolyederf90m

      implicit none

      double precision gcen(3),ri,rmin,rmax,riv(3),x,y,z,bn(3),b,
     &  bx,by,bz,bxd,byd,bzd,p(3),q,pr,rob,dlen,
     &  xin,yin,zin,bxout,byout,bzout,rob2,rob3

      integer idip,iplan,icorn,npoi,ifail,kfail,ical

      bxout=0.0d0
      byout=0.0d0
      bzout=0.0d0

      x=xin*1.0d3-dipoles(1,idip)
      y=yin*1.0d3-dipoles(2,idip)
      z=zin*1.0d3-dipoles(3,idip)

      p=dipoles(5:7,idip)*dipoles(4,idip)
      pr=p(1)*x+p(2)*y+p(3)*z
      rob2=x**2+y**2+z**2
      rob=sqrt(rob2)
      rob3=rob2*rob

      if (rob.gt.1.0d-6) then
        bxout=(3.0d0*pr*x/rob2-p(1))/rob3
        byout=(3.0d0*pr*y/rob2-p(2))/rob3
        bzout=(3.0d0*pr*z/rob2-p(3))/rob3
      else
        kfail=1
      endif

      return
      end
