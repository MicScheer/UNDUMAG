*CMZ :          11/09/2023  14.12.49  by  Michael Scheer
*CMZ : 00.00/19 07/06/2016  12.17.28  by  Michael Scheer
*-- Author :    Michael Scheer   07/06/2016
      subroutine util_plane_tiny(p1,p2,p3,p,vnor,dist,tiny,iover,istat)

      implicit none

      double precision p1(3),p2(3),p3(3),p(3),vnor(3),dist,tiny,
     &  v21(3),v31(3),vp1(3),vin(3),v32(3),vcross(3),vp2(3),vp3(3)
      integer iover,istat

c 5.6.2019: Now, iover=1 if point over bounderaries or inside

      istat=0

      v21=p2-p1
      v31=p3-p1
      v32=p3-p2

      vnor(1)=v21(2)*v31(3)-v21(3)*v31(2)
      vnor(2)=v21(3)*v31(1)-v21(1)*v31(3)
      vnor(3)=v21(1)*v31(2)-v21(2)*v31(1)

      dist=norm2(vnor)

      if (dist.le.tiny) then
        istat=-1
        goto 9999
      endif

      vnor=vnor/dist
      vp1=p-p1
      vp2=p-p2
      vp3=p-p3

      dist=vp1(1)*vnor(1)+vp1(2)*vnor(2)+vp1(3)*vnor(3)
      vin=vp1-dist*vnor

      iover=1

      vcross(1)=v21(2)*vin(3)-v21(3)*vin(2)
      vcross(2)=v21(3)*vin(1)-v21(1)*vin(3)
      vcross(3)=v21(1)*vin(2)-v21(2)*vin(1)

c5.6.2019      if (vcross(1)*vnor(1)+vcross(2)*vnor(2)+vcross(3)*vnor(3).le.0.0d0) then
      if (vcross(1)*vnor(1)+vcross(2)*vnor(2)+vcross(3)*vnor(3).lt.0.0d0) then
        iover=0
        goto 9999
      endif

      vin=vp2-dist*vnor

      vcross(1)=v32(2)*vin(3)-v32(3)*vin(2)
      vcross(2)=v32(3)*vin(1)-v32(1)*vin(3)
      vcross(3)=v32(1)*vin(2)-v32(2)*vin(1)

c5.6.2019      if (vcross(1)*vnor(1)+vcross(2)*vnor(2)+vcross(3)*vnor(3).le.0.0d0) then
      if (vcross(1)*vnor(1)+vcross(2)*vnor(2)+vcross(3)*vnor(3).lt.0.0d0) then
        iover=0
        goto 9999
      endif

      vin=vp3-dist*vnor

      vcross(1)=v31(2)*vin(3)-v31(3)*vin(2)
      vcross(2)=v31(3)*vin(1)-v31(1)*vin(3)
      vcross(3)=v31(1)*vin(2)-v31(2)*vin(1)

c5.6.2019      if (vcross(1)*vnor(1)+vcross(2)*vnor(2)+vcross(3)*vnor(3).ge.0.0d0) then
      if (vcross(1)*vnor(1)+vcross(2)*vnor(2)+vcross(3)*vnor(3).gt.0.0d0) then
        iover=0
        goto 9999
      endif

9999  continue

      return
      end
