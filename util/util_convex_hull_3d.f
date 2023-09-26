*CMZ :  2.04/22 26/09/2023  21.29.40  by  Michael Scheer
*CMZ :  2.04/09 16/08/2023  07.59.23  by  Michael Scheer
*CMZ :  2.04/04 06/03/2023  09.45.40  by  Michael Scheer
*CMZ :  2.04/03 03/03/2023  11.50.12  by  Michael Scheer
*CMZ :  2.03/00 30/08/2022  21.37.01  by  Michael Scheer
*CMZ :  2.02/02 05/07/2022  11.16.26  by  Michael Scheer
*CMZ :  2.02/01 03/11/2021  13.16.39  by  Michael Scheer
*CMZ :  2.02/00 17/09/2020  13.46.21  by  Michael Scheer
*CMZ :  2.01/03 28/05/2019  15.59.50  by  Michael Scheer
*CMZ :  2.00/03 24/04/2018  09.24.03  by  Michael Scheer
*CMZ :  1.23/04 02/10/2017  11.25.14  by  Michael Scheer
*CMZ : 00.00/21 13/03/2017  11.19.41  by  Michael Scheer
*CMZ : 00.00/20 12/03/2017  13.22.16  by  Michael Scheer
*CMZ :  1.11/05 20/02/2017  22.15.13  by  Michael Scheer
*CMZ :  1.11/04 21/01/2017  17.14.54  by  Michael Scheer
*-- Author :    Michael Scheer   22/11/2016
      subroutine util_convex_hull_3d(nin,xin,yin,zin,khull,kedge,kface,
     &  nhull,nedge,nface,kfacelast,tinyin,
     &  kfail)

      ! Calculates the convex hull for the points (x,y,z)

      ! Input:

      ! nin: Number of points
      ! x(1:nin),y(1:nin),z(1:nin): coordinates of the points

      ! Dimensions: The arrays must have at least the size of
      ! Euler: Nvertices + Nfaces - Nedges = 2


      ! khull(nin)
      ! kedge(4,nin*nin-2)

      ! kface((nin+1)*nin)

      ! Output:

      ! khull(1:nhull) is the array of indices of the points belonging to the
      ! convex hull

      ! kedge(1:4,1:nedge) contains the indices of the edges of the hull
      ! kedge(1,iedge) is the first point of the edge iedge
      ! kedge(2,iedge) is the second point of the edge iedge
      ! kedge(3,iedge) is the index of the first face having this edge
      ! kedge(4,iedge) is the index of the second face having this edge

      ! kface contains a list of indices describing the faces of the hull
      ! kface(1) is the number of points of the first face
      ! kface(1+1:1+kface(1)) are the indices of these points
      ! Let lpoi(iface) = kface(k), then kface(k+1:k+lpoi) are the lpoi
      ! indices of the points belonging to this face, and kface(k+nin+1) is
      ! then nin(iface+1).
      ! kfacelast is the used length of kface, i.e. the index of last entry

      ! Example:
      ! iface=1, kface(1)=4, kface(2:5) are the indices of the 4 points of
      ! the first face, and kface(6) are the number of points of the second
      ! face, and so on.

      implicit none

      double precision xin(*),yin(*),zin(*),
     &  zmin,zmax,ymin,ymax,xmin,xmax,xsc,ysc,zsc,zmin1,zmin2,
     &  cosphi,sinphi,qn,cosmax,cosqpv,ex(3),ey(3),ez(3),q(3),
     &  p1(3),p2(3),p3(3),pt1(3),pt2(3),pt3(3),
     &  p21(3),vnor(3),dist,pt1r(3),pn,
     &  gcen(3),gceno(3),tinyin,tiny,tiny2,rotmat(3,3),rotg(3,3),rotm(3,3),
     &  rotws(3,3),gcenin(3),pcen(3)

      double precision, dimension (:), allocatable ::  x2,y2,z2,x,y,z

      integer, dimension (:,:), allocatable ::  kedgebuff
      integer, dimension (:,:), allocatable ::  iplans
      integer, dimension (:), allocatable ::  ibuff,ibuffp,ibuffm,kbuff,
     &  kedgeheap,kfaceheap,istore,iveto,kveto

      integer khull(*),kedge(4,*),kface(*),ifail,i,kzmin,nhull,nedge,nface,
     &  mface,lface,nin,npoi,ifound,iedge,ifirst,isecond,ithird,ipoi,kpoi,
     &  ledge,medge,jpoi,iover,istat,nh2d,nedgeheap,nbuff,iface,nfpoi,kfail,
     &  kfacelast,k,ib1,ib2

      integer :: idimfail=0,ical=0

      character(128) cerror

      data ex/1.0d0,0.0d0,0.0d0/
      data ey/0.0d0,1.0d0,0.0d0/
      data ez/0.0d0,0.0d0,1.0d0/

      ical=ical+1
c+self,if=debug3d.
c      print*,"hull_3d, ical:",ical
c      if (ical.eq.195) then
c        !call util_break
c      endif
c+self.
      idimfail=0
      ifirst=0
      isecond=0
      ithird=0
      nface=0
c 1.3.2023      tiny2=tiny**2
      tiny=tinyin
      if (tiny.le.0.0d0) tiny=1.0d-12
      tiny2=tiny

      npoi=nin

      allocate(kfaceheap(npoi*npoi))
      allocate(kedgeheap(npoi*npoi-2))
      allocate(ibuff(npoi),ibuffp(npoi),ibuffm(npoi),kbuff((npoi+1)*npoi))
      allocate(iplans(npoi,npoi),istore(npoi),iveto(npoi),kveto(npoi))
      allocate(x(npoi),y(npoi),z(npoi),x2(npoi),y2(npoi),z2(npoi))

      ibuff=0
      kbuff=0
      ibuffm=0
      ibuffp=0
      iplans=0
      iveto=0
      kveto=0

      x(1:npoi)=xin(1:npoi)
      y(1:npoi)=yin(1:npoi)
      z(1:npoi)=zin(1:npoi)


      call util_weed_points(npoi,x,y,z,tiny) !essential...


      if (npoi.eq.0) then
        kfail=100
        cerror="*** Error in util_convex_hull_3d: Number of points is zero ***"
        nhull=0
        nedge=0
        nface=0
        goto 9999
      else if (npoi.eq.1) then
        cerror="*** Error in util_convex_hull_3d: Number of points is one ***"
        kfail=100
        nhull=1
        nedge=0
        nface=0
        goto 9999
      else if (npoi.eq.2) then
        cerror="*** Error in util_convex_hull_3d: Number of points is two ***"
        nhull=2
        nedge=1
        nface=0
        kfail=100
        goto 9999
      else if (npoi.eq.3) then
        p1(1)=x(1)
        p1(2)=y(1)
        p1(3)=z(1)
        p2(1)=x(2)
        p2(2)=y(2)
        p2(3)=z(2)
        p21=p2-p1
        p2(1)=x(3)
        p2(2)=y(3)
        p2(3)=z(3)
        p3=p2-p1
        q(1)=p21(2)*p3(3)-p21(3)*p3(2)
        q(2)=p21(3)*p3(1)-p21(1)*p3(3)
        q(3)=p21(1)*p3(2)-p21(2)*p3(1)
        qn=sqrt(q(1)**2+q(2)**2+q(3)**2)
        if (abs(qn).lt.tiny) then
          kfail=100
          cerror="*** Error in util_convex_hull_3d: Flat polyeder ***"
          goto 9999
        endif
      endif

      xmin=1.0d30
      xmax=-1.0d30
      ymin=1.0d30
      ymax=-1.0d30
      zmin=1.0d30
      zmax=-1.0d30

      gcen=0.0d0
      do i=1,npoi
        if (z(i).lt.zmin) zmin=z(i)
        if (z(i).gt.zmax) zmax=z(i)
        if (y(i).lt.ymin) ymin=y(i)
        if (y(i).gt.ymax) ymax=y(i)
        if (x(i).lt.xmin) xmin=x(i)
        if (x(i).gt.xmax) xmax=x(i)
        gcen(1)=gcen(1)+x(i)
        gcen(2)=gcen(2)+y(i)
        gcen(3)=gcen(3)+z(i)
        ibuff(i)=i
      enddo

      gcen=gcen/npoi
      gcenin=gcen

      ! All points in a plane?{

      p1(1)=x(1)
      p1(2)=y(1)
      p1(3)=z(1)

      p2(1)=x(2)
      p2(2)=y(2)
      p2(3)=z(2)

      kfail=1

      do ipoi=3,npoi

        q(1)=x(ipoi)
        q(2)=y(ipoi)
        q(3)=z(ipoi)

        call util_plane(p1,p2,gcen,q,vnor,dist,iover,istat)

        if (istat.eq.0.and.abs(dist).gt.tiny) then
          kfail=0
          exit
        endif

      enddo

      if (kfail.ne.0) then

        if (abs(zmax-zmin).gt.tiny) then
          call util_convex_hull_2d(npoi,x,y,nh2d,kbuff,tiny2,ifail)
        else if (abs(xmax-xmin).gt.tiny) then
          call util_convex_hull_2d(npoi,z,y,nh2d,kbuff,tiny2,ifail)
        else if (abs(ymax-ymin).gt.tiny) then
          call util_convex_hull_2d(npoi,x,z,nh2d,kbuff,tiny2,ifail)
        endif

        if (ifail.ne.0) then
          kfail=-11
          cerror="*** Error in util_convex_hull_3d: Bad return from util_convex_hull_2d ***"
          goto 9999
        endif

        nh2d=nh2d-1

        kface(1)=nh2d
        nhull=nh2d
        nface=1

        if (nh2d.gt.2) then
          nedge=nh2d
        else
          nedge=1
        endif

        do ipoi=1,nh2d
          kpoi=ibuff(kbuff(ipoi))
          jpoi=ibuff(kbuff(ipoi+1))
          khull(ipoi)=kpoi
          kface(1+ipoi)=kpoi
          kedge(1,ipoi)=kpoi
          kedge(2,ipoi)=jpoi
          kedge(3:4,ipoi)=1
        enddo
        kfail=0
        goto 9999
      endif

      ! All points in a plane?}

      if (xmin.eq.xmax) then
        kfail=1
        cerror="*** Error in util_convex_hull_3d: Xmin = Xmax ***"
        goto 9999
      endif
      if (ymin.eq.ymax) then
        kfail=1
        cerror="*** Error in util_convex_hull_3d: Ymin = Ymax ***"
        goto 9999
      endif
      if (zmin.eq.zmax) then
        kfail=1
        cerror="*** Error in util_convex_hull_3d: Zmin = Zmax ***"
        goto 9999
      endif

      ! due to weeding
      istore=0
      do k=1,npoi
        do i=1,nin
          if (
     &        xin(i).eq.x(k).and.yin(i).eq.y(k).and.zin(i).eq.z(k)
     &        ) then
            istore(k)=i
            exit
          endif
        enddo
      enddo

      nedge=0
      nface=0
      nhull=0
      kedge(1:4,1:2*npoi-2)=0
      khull(1:npoi)=0
      kface(1:(npoi+1)*npoi)=0

      xsc=1.0d0/(xmax-xmin)
      ysc=1.0d0/(ymax-ymin)
      zsc=1.0d0/(zmax-zmin)

      do i=1,npoi
        x(i)=(x(i)-xmin)*xsc
        y(i)=(y(i)-ymin)*ysc
        z(i)=(z(i)-zmin)*zsc
      enddo

      xmax=(xmax-xmin)*xsc
      xmin=0.0d0
      ymax=(ymax-ymin)*ysc
      ymin=0.0d0
      zmax=(zmax-zmin)*zsc
      zmin=0.0d0

      gcen=0.0d0
      kzmin=0
      do i=1,npoi
        gcen(1)=gcen(1)+x(i)
        gcen(2)=gcen(2)+y(i)
        gcen(3)=gcen(3)+z(i)
        if (abs(z(i)-zmin).lt.tiny) then
          kzmin=kzmin+1
          ibuffm(kzmin)=i
          if (kzmin.eq.1) then
            ifirst=i
          else if (kzmin.eq.2) then
            isecond=i
          endif
          ibuffm(kzmin)=i
        endif
      enddo

      if (kzmin.gt.2) then

        do ipoi=1,kzmin
          kpoi=ibuffm(ipoi)
          x2(ipoi)=x(kpoi)
          y2(ipoi)=y(kpoi)
          z2(ipoi)=z(kpoi)
        enddo

        call util_convex_hull_2d(kzmin,x2,y2,nh2d,khull,tiny,kfail)

        if (nh2d.eq.3) then
          ibuff=ibuffm
          ! all points on a line
          kveto(ibuffm(1:3))=1
          ibuff(1:2)=ibuffm(khull(1:2))
          kveto(ibuff(1:2))=0
          ibuffm=0
          ibuffm(1:2)=ibuff(1:2)
          ifirst=ibuffm(1)
          isecond=ibuffm(2)
          kzmin=2
        endif

      endif

      gcen=gcen/npoi
      gceno=gcen

      zmin1=1.0d30
      zmin2=1.0d30

      p1(1)=x(ifirst)
      p1(2)=y(ifirst)
      p1(3)=z(ifirst)

      if (kzmin.eq.1) then

        do i=1,npoi
          if (i.eq.ifirst) cycle
          p21(1)=x(i)-p1(1)
          p21(2)=y(i)-p1(2)
          p21(3)=z(i)-p1(3)
c18Feb2020          pn=sqrt(p21(1)**2+p21(2)**2+p21(3)**2)
          pn=sqrt(p21(1)**2+p21(3)**2)
          if (pn.lt.tiny) then
            cycle
          endif
          p21=p21/pn
          if (p21(3).lt.zmin1) then
            isecond=i
            zmin1=p21(3)
            ibuffm(2)=i
          endif
        enddo

      endif

      p21(1)=x(isecond)-x(ifirst)
      p21(2)=y(isecond)-y(ifirst)
      p21(3)=z(isecond)-z(ifirst)

      ithird=0
      do i=1,npoi

        if (i.eq.ifirst.or.i.eq.isecond.or.kveto(i).ne.0) cycle

c16.8.2023        p3(1)=x(i)-p21(1)
c16.8.2023        p3(2)=y(i)-p21(2)
c16.8.2023        p3(3)=z(i)-p21(3)
        p3(1)=x(i)-p1(1)
        p3(2)=y(i)-p1(2)
        p3(3)=z(i)-p1(3)
        pn=sqrt(p3(1)**2+p3(3)**2)
c18Feb2020        pn=sqrt(p3(1)**2+p3(2)**2+p3(3)**2)
        if (pn.lt.tiny) then
          cycle
        endif

        p3=p3/pn

        if (p3(3).le.zmin1) then
c 2.3.2023          q(1)=p21(2)*p3(3)-p21(3)*p3(2)
c 2.3.2023          q(2)=p21(3)*p3(1)-p21(1)*p3(3)
c 2.3.2023          q(3)=p21(1)*p3(2)-p21(2)*p3(1)
c 2.3.2023          qn=sqrt(q(1)**2+q(2)**2+q(3)**2)
c 2.3.2023          if (abs(qn).gt.tiny) then
c 2.3.2023   points on the line are eleminated later
            ithird=i
            ibuffm(3)=i
            zmin1=p3(3)
c 2.3.2023          endif
        endif
      enddo

      if (ithird.eq.0) then
        zmin2=1.0d30
        do i=1,npoi
          if (i.eq.ifirst.or.i.eq.isecond) cycle
          p21(1)=x(i)-p1(1)
          p21(2)=y(i)-p1(2)
          p21(3)=z(i)-p1(3)
c15Feb2020          pn=sqrt(p21(1)**2+p21(2)**2+p21(3)**2)
          pn=sqrt(p21(1)**2+p21(3)**2)
          if (pn.lt.tiny) cycle
          p21=p21/pn
          if (p21(3).gt.zmin1.and.p21(3).lt.zmin2) then
            ithird=i
            zmin2=p21(3)
            ibuffm(3)=i
          endif
        enddo
      endif

      !Find all points of first plane

      ifirst=ibuffm(1)
      isecond=ibuffm(2)
      ithird=ibuffm(3)

      ifirst=ibuffm(1)
      isecond=ibuffm(2)
      ithird=ibuffm(3)

      ibuff(1:3)=ibuffm(1:3)
      iplans(1:3,1)=ibuff(1:3)

      nbuff=3

      ipoi=ithird

      gcen=gceno

      p1(1)=x(ifirst)
      p1(2)=y(ifirst)
      p1(3)=z(ifirst)

      p2(1)=x(isecond)
      p2(2)=y(isecond)
      p2(3)=z(isecond)

      p3(1)=x(ipoi)
      p3(2)=y(ipoi)
      p3(3)=z(ipoi)

      do jpoi=1,npoi

        if (jpoi.eq.ipoi.or.jpoi.eq.ifirst.or.jpoi.eq.isecond) cycle

        q(1)=x(jpoi)
        q(2)=y(jpoi)
        q(3)=z(jpoi)

        call util_plane(p1,p2,p3,q,vnor,dist,iover,istat)

        if (istat.ne.0) then
          kfail=-8
          cerror="*** Error in util_convex_hull_3d: Bad return from util_plane ***"
          goto 9999
        endif

        if (abs(dist).lt.tiny) then
          nbuff=nbuff+1
          iplans(nbuff,1)=jpoi
          ibuff(nbuff)=jpoi
        endif

      enddo !jpoi

      do ipoi=1,nbuff
        kpoi=ibuff(ipoi)
        x2(ipoi)=x(kpoi)
        y2(ipoi)=y(kpoi)
        z2(ipoi)=z(kpoi)
      enddo
      q=gcen-p1

      call util_plane(p1,p2,p3,q,vnor,dist,iover,istat)
      if (istat.ne.0) then
        kfail=-8
        cerror="*** Error in util_convex_hull_3d: Bad return from util_plane ***"
        goto 9999
      endif

      if (
     &    abs(vnor(1)).ge.abs(vnor(2)).and.abs(vnor(1)).ge.abs(vnor(3))) then
        call util_convex_hull_2d(nbuff,y2,z2,nh2d,kbuff,tiny2,ifail)
      else if (
     &    abs(vnor(2)).ge.abs(vnor(1)).and.abs(vnor(2)).ge.abs(vnor(3))) then
        call util_convex_hull_2d(nbuff,x2,z2,nh2d,kbuff,tiny2,ifail)
      else
        call util_convex_hull_2d(nbuff,x2,y2,nh2d,kbuff,tiny2,ifail)
      endif

      kfail=kfail+iabs(ifail)

      do ipoi=1,nbuff
        ifound=0
        do i=1,nh2d
          if (ipoi.eq.kbuff(i)) then
            ifound=1
            exit
          endif
        enddo
        if (ifound.eq.0) then
          kveto(ibuff(ipoi))=1
        endif
      enddo

      ibuff=0
      ibuff(1:nh2d)=iplans(kbuff(1:nh2d),1) ! must be a closed polygon

      k=ibuff(1)
      p1(1)=x(k)
      p1(2)=y(k)
      p1(3)=z(k)

      k=ibuff(2)
      p2(1)=x(k)
      p2(2)=y(k)
      p2(3)=z(k)

      k=ibuff(3)
      p3(1)=x(k)
      p3(2)=y(k)
      p3(3)=z(k)

      ! get normal vector vnor
      call util_plane(p1,p2,p3,p3,vnor,dist,iover,istat)
      if (istat.ne.0) then
        kfail=-8
        cerror="*** Error in util_convex_hull_3d: Bad return from util_plane ***"
        goto 9999
      endif

      q=gcen-p1

      ! check orientation of plane

      if (q(1)*vnor(1)+q(2)*vnor(2)+q(3)*vnor(3).gt.0.0d0) then

        k=nh2d-1

        ibuffp(1:k)=ibuff(1:k)

        if (nh2d.gt.(npoi+1)*npoi) then
          cerror="*** Error in util_convex_hull_3d: Boundary for kbuff exceeded ***"
          kfail=9
          goto 9999
        endif

        do ipoi=1,k
          kpoi=ibuffp(k-ipoi+1)
          ibuff(ipoi)=kpoi
        enddo

        ibuff(nh2d)=ibuff(1) ! must be a closed polygon

        vnor=-vnor

      endif

      nh2d=nh2d-1


      ! First plane found
      nface=1
      nedge=nh2d
      nedgeheap=nedge
      lface=1
      kfaceheap(nface)=lface

      do iedge=1,nedge
        ib1=ibuff(iedge)
        ib2=ibuff(iedge+1)
        if (ib1.lt.ib2) then
          kedge(1,iedge)=ib1
          kedge(2,iedge)=ib2
        else
          kedge(1,iedge)=ib2
          kedge(2,iedge)=ib1
        endif
        kedge(3,iedge)=nface
        kedgeheap(iedge)=iedge
        k=ibuff(iedge)
        kface(lface+iedge)=k
      enddo

      kface(lface)=nh2d

      do while (nedgeheap.gt.0)

        medge=kedgeheap(nedgeheap)
        mface=kedge(3,medge)

        if (mface.le.0.or.kedge(4,medge).ne.0) then
          cerror="*** Bad pointer of face or bad edge in util_convex_hull_3d ***"
          kfail=20
          !!call util_break
          goto 9999
        endif

        lface=kfaceheap(mface)

        !Three pt1, pt2, and pt3 define current plane
        ifirst=kedge(1,medge)

        k=ifirst
        pt1(1)=x(k)
        pt1(2)=y(k)
        pt1(3)=z(k)

        isecond=kedge(2,medge)

        k=isecond
        pt2(1)=x(k)
        pt2(2)=y(k)
        pt2(3)=z(k)

        iveto=kveto
        do i=1,npoi
          k=iplans(i,mface)
          if (k.eq.0) exit
          iveto(k)=1
        enddo

        ithird=0
        do i=1,kface(lface)
          k=kface(lface+i)
          if (k.eq.ifirst.or.k.eq.isecond) cycle
          pt3(1)=x(k)
          pt3(2)=y(k)
          pt3(3)=z(k)
          exit
        enddo

c        print*,"mface,medge,ifirst,isecond,ithird:",mface,medge,ifirst,isecond,k

        p1=pt2-pt1
        p2=pt3-pt1

        vnor(1)=p1(2)*p2(3)-p1(3)*p2(2)
        vnor(2)=p1(3)*p2(1)-p1(1)*p2(3)
        vnor(3)=p1(1)*p2(2)-p1(2)*p2(1)

c        p3=gcen-pt1

c        if (vnor(1)*p3(1)+vnor(2)*p3(2)+vnor(3)*p3(3).lt.0.0d0) then
c          vnor=-vnor
c        endif

        !Rotate normal vector to z-axis

        call util_matrix_to_rot_vec_to_z(vnor,rotmat,ifail)

        q(1)=rotmat(1,1)*p1(1)+rotmat(1,2)*p1(2)+rotmat(1,3)*p1(3)
        q(2)=rotmat(2,1)*p1(1)+rotmat(2,2)*p1(2)+rotmat(2,3)*p1(3)
        q(3)=rotmat(3,1)*p1(1)+rotmat(3,2)*p1(2)+rotmat(3,3)*p1(3)

        qn=sqrt(q(1)**2+q(2)**2) !normalize x-y component

        if (qn.ne.qn.or.qn.lt.tiny) then
          kfail=77
          cerror="*** Error in util_convex_hull_3d: Problems with rotation to z-axis ***"
          goto 9999
        endif
        q=q/qn

        !Rotate edge to x-axis

        cosphi=q(1)
        sinphi=q(2)

        rotg(1,1)=cosphi
        rotg(1,2)=sinphi
        rotg(1,3)=0.0d0
        rotg(2,1)=-sinphi
        rotg(2,2)=cosphi
        rotg(2,3)=0.0d0
        rotg(3,1)=0.0d0
        rotg(3,2)=0.0d0
        rotg(3,3)=1.0d0

        call util_matrix_multiplication(3,3,3,rotg,rotmat,rotm,rotws)

c        q(1)=rotm(1,1)*p1(1)+rotm(1,2)*p1(2)+rotm(1,3)*p1(3)
c        q(2)=rotm(2,1)*p1(1)+rotm(2,2)*p1(2)+rotm(2,3)*p1(3)
c        q(3)=rotm(3,1)*p1(1)+rotm(3,2)*p1(2)+rotm(3,3)*p1(3)
c        print*,q

        cosmax=-2.0d0

        ! Look for new plane, starting from current edge

        pt1r(1)=rotm(1,1)*pt1(1)+rotm(1,2)*pt1(2)+rotm(1,3)*pt1(3)
        pt1r(2)=rotm(2,1)*pt1(1)+rotm(2,2)*pt1(2)+rotm(2,3)*pt1(3)
        pt1r(3)=rotm(3,1)*pt1(1)+rotm(3,2)*pt1(2)+rotm(3,3)*pt1(3)

        do ipoi=1,npoi

          if (iveto(ipoi).ne.0.or.kveto(ipoi).ne.0) cycle

          p3(1)=x(ipoi)
          p3(2)=y(ipoi)
          p3(3)=z(ipoi)

          q(1)=rotm(1,1)*p3(1)+rotm(1,2)*p3(2)+rotm(1,3)*p3(3)
          q(2)=rotm(2,1)*p3(1)+rotm(2,2)*p3(2)+rotm(2,3)*p3(3)
          q(3)=rotm(3,1)*p3(1)+rotm(3,2)*p3(2)+rotm(3,3)*p3(3)

          q=q-pt1r

          qn=sqrt(q(2)**2+q(3)**2)
          q=q/qn

          cosqpv=-q(2)

          if (cosqpv.gt.cosmax) then
            cosmax=cosqpv
            ibuff(3)=ipoi
          endif

        enddo

        ibuff(1)=ifirst
        ibuff(2)=isecond
        nbuff=3

        !p1, p2, and p3 define candidate for a new plane
        p1(1)=x(ibuff(1))
        p1(2)=y(ibuff(1))
        p1(3)=z(ibuff(1))

        p2(1)=x(ibuff(2))
        p2(2)=y(ibuff(2))
        p2(3)=z(ibuff(2))

        p3(1)=x(ibuff(3))
        p3(2)=y(ibuff(3))
        p3(3)=z(ibuff(3))

        !Collect all points belonging to the candidate plane

        iveto=kveto
        iveto(ibuff(1:3))=1

        do ipoi=1,npoi
          if (iveto(ipoi).ne.0.or.kveto(ipoi).ne.0.or.
     &      ipoi.eq.ibuff(1).or.ipoi.eq.ibuff(2).or.ipoi.eq.ibuff(3)) cycle
          q(1)=x(ipoi)
          q(2)=y(ipoi)
          q(3)=z(ipoi)
          call util_plane(p1,p2,p3,q,vnor,dist,iover,istat)
          if (istat.eq.0.and.abs(dist).lt.tiny) then
            nbuff=nbuff+1
            if (nbuff.gt.npoi) then
              kfail=-4
              cerror="*** Error in util_convex_hull_3d: Buffer exceeded ***"
              goto 9999
            endif
            ibuff(nbuff)=ipoi
          endif
        enddo

        do ipoi=1,nbuff
          kpoi=ibuff(ipoi)
          x2(ipoi)=x(kpoi)
          y2(ipoi)=y(kpoi)
          z2(ipoi)=z(kpoi)
          iplans(ipoi,nface+1)=kpoi !nface is incremented later
        enddo
        if (
     &      abs(vnor(1)).ge.abs(vnor(2)).and.abs(vnor(1)).ge.abs(vnor(3))) then
          call util_convex_hull_2d(nbuff,y2,z2,nh2d,kbuff,tiny2,ifail)
        else if (
     &      abs(vnor(2)).ge.abs(vnor(1)).and.abs(vnor(2)).ge.abs(vnor(3))) then
          call util_convex_hull_2d(nbuff,x2,z2,nh2d,kbuff,tiny2,ifail)
        else
          call util_convex_hull_2d(nbuff,x2,y2,nh2d,kbuff,tiny2,ifail)
        endif

        kfail=kfail+iabs(ifail)

        do ipoi=1,nbuff
          ifound=0
          do i=1,nh2d
            if (ipoi.eq.kbuff(i)) then
              ifound=1
              exit
            endif
          enddo
          if (ifound.eq.0) then
            kveto(ibuff(ipoi))=1
          endif
        enddo

        k=ibuff(kbuff(1))
        p1(1)=x(k)
        p1(2)=y(k)
        p1(3)=z(k)

        k=ibuff(kbuff(2))
        p2(1)=x(k)
        p2(2)=y(k)
        p2(3)=z(k)

        k=ibuff(kbuff(3))
        p3(1)=x(k)
        p3(2)=y(k)
        p3(3)=z(k)

        call util_plane(p1,p2,p3,p3,vnor,dist,iover,istat)
        if (istat.ne.0) then
          kfail=-8
          cerror="*** Error in util_convex_hull_3d: Bad return from util_plane ***"
          goto 9999
        endif

        q=gcen-p1

        ! check orientation of plane

        if (q(1)*vnor(1)+q(2)*vnor(2)+q(3)*vnor(3).gt.0.0d0) then

          ibuffp(1:nh2d)=kbuff(1:nh2d)

          if (nh2d.gt.(npoi+1)*npoi) then
            cerror="*** Error in util_convex_hull_3d: Boundary for kbuff exceeded ***"
            kfail=9
            goto 9999
          endif

          do ipoi=1,nh2d
            kbuff(ipoi)=ibuffp(nh2d-ipoi+1)
          enddo

        endif

        nh2d=nh2d-1
        nfpoi=nh2d

        !New face found!
        lface=kfaceheap(nface)
        lface=lface+kface(lface)+1

        nface=nface+1
        kfaceheap(nface)=lface

        kface(lface)=nfpoi
        do ipoi=1,nfpoi
          k=ibuff(kbuff(ipoi))
          kface(lface+ipoi)=k
        enddo

        kedge(4,medge)=nface

        !Check for new edges

        ifound=0

        iedge=nedge
        do ipoi=1,nfpoi
          ib1=ibuff(kbuff(ipoi))
          ib2=ibuff(kbuff(ipoi+1))
          if (ib1.gt.ib2) then
            ib2=ibuff(kbuff(ipoi))
            ib1=ibuff(kbuff(ipoi+1))
          endif
          ifound=0
          do ledge=1,iedge
            if (
     &          ib1.eq.kedge(1,ledge).and.
     &          ib2.eq.kedge(2,ledge)
     &          ) then
              ifound=1
              if (kedge(3,ledge).eq.0) then
                cerror="*** Error in util_convex_hull_3d: Bad edge ***"
                !!call util_break
                kfail=kfail+1
              endif
              if (kedge(4,ledge).eq.0) then
                kedge(4,ledge)=nface
              endif
              exit
            endif
          enddo

          if (ifound.eq.0) then
            nedge=nedge+1
            if (nedge.gt.2*npoi-2) then
              kfail=81
              goto 9999
            endif
            kedge(1,nedge)=ib1
            kedge(2,nedge)=ib2
            kedge(3,nedge)=nface
          endif
        enddo !iedge=1,nfpoi

        nedgeheap=0
        do iedge=1,nedge
          if (kedge(4,iedge).eq.0) then
            nedgeheap=nedgeheap+1
            if (nedgeheap.gt.2*npoi-2) then
              kfail=82
              goto 9999
            endif
            kedgeheap(nedgeheap)=iedge
          endif
        enddo

      enddo !while (nedgeheap.gt.0)

89    continue

      nhull=0

      do iedge=1,nedge
        kpoi=kedge(1,iedge)
        if (kpoi.le.0) then
          write(6,*)
          write(6,*)"*** Error in util_convex_hull_3d: Bad edges ***"
          write(6,*)
          !!call util_break
          kfail=86
        endif
        ifound=0
        do ipoi=1,nhull
          if (kpoi.eq.khull(ipoi)) then
            ifound=1
            exit
          endif
        enddo
        if (ifound.eq.0) then
          nhull=nhull+1
          if (nhull.gt.npoi) then
            kfail=83
            goto 91
          endif
          khull(nhull)=kpoi
        endif
        kpoi=kedge(2,iedge)
        ifound=0
        do ipoi=1,nhull
          if (kpoi.eq.khull(ipoi)) then
            ifound=1
            exit
          endif
        enddo
        if (ifound.eq.0) then
          nhull=nhull+1
          if (nhull.gt.npoi) then
            kfail=84
            goto 9999
          endif
          khull(nhull)=kpoi
        endif
      enddo !nedge

91    if (kfail.eq.0) then

        allocate(kedgebuff(2,nedge))

        kbuff(1:nhull)=khull(1:nhull)
        do i=1,nhull
          khull(i)=istore(kbuff(i))
        enddo

        kedgebuff(1:2,1:nedge)=kedge(1:2,1:nedge)
        do i=1,nedge
          kedge(1,i)=istore(kedgebuff(1,i))
          kedge(2,i)=istore(kedgebuff(2,i))
        enddo

        do kfacelast=1,(npoi+1)*npoi
          if (kface(kfacelast).eq.0) then
            exit
          endif
          if (kfacelast.gt.(npoi+1)*npoi) then
            cerror="*** Error in util_convex_hull_3d: Boundary for kbuff exceeded ***"
            kfail=9
            deallocate(kedgebuff)
            goto 9999
          endif
          kbuff(kfacelast)=kface(kfacelast)
        enddo
        kfacelast=kfacelast-1

        lface=1
        do iface=1,nface
          nfpoi=kbuff(lface)
          do i=1,nfpoi
            if (lface+i.gt.(npoi+1)*npoi) then
              cerror="*** Error in util_convex_hull_3d: Boundary for kbuff exceeded ***"
              kfail=9
              deallocate(kedgebuff)
              goto 9999
            endif
            kface(lface+i)=istore(kbuff(lface+i))
          enddo
          lface=lface+nfpoi+1
        enddo

        if (lface-1.ne.kfacelast) then
          kfail=kfail+1
          cerror="*** Error in util_convex_hull_3d: Bad kfacelast ***"
        endif

        deallocate(kedgebuff)

      endif !kfail


9999  continue

c      if (ical.eq.195) then
c        !call util_break
c      endif

      deallocate(kfaceheap,kedgeheap,ibuff,ibuffp,ibuffm,kbuff,iplans,istore,
     &  iveto,kveto,x,y,z,x2,y2,z2)

c      if (ical.eq.195) then
c        !call util_break
c      endif

      if (idimfail.ne.0) then
        kfail=idimfail
      endif

      if (kfail.ne.0.and.kfail.ne.100) then
        write(6,*)trim(cerror)
        write(6,*)"*** Error in util_convex_hull3d: kfail.ne.0 .and. kfail.ne.100, ical:",ical
      endif

      if (kfail.eq.0.and.nface+nhull-nedge.ne.2) then
        write(6,*)"Error in util_convex_hull3d: Euler's formula not fullfilled"
        kfail=-99
      endif

      return
      end
