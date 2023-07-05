*CMZ :  2.02/01 03/11/2021  12.49.04  by  Michael Scheer
*CMZ :  2.00/03 19/04/2018  15.03.14  by  Michael Scheer
*CMZ :  1.23/04 01/10/2017  14.44.56  by  Michael Scheer
*CMZ : 00.00/21 13/03/2017  11.19.41  by  Michael Scheer
*CMZ : 00.00/20 12/03/2017  13.22.16  by  Michael Scheer
*CMZ :  1.11/05 20/02/2017  22.15.13  by  Michael Scheer
*CMZ :  1.11/04 21/01/2017  17.14.54  by  Michael Scheer
*-- Author :    Michael Scheer   22/11/2016
      subroutine util_convex_hull_3d_old(npoi,x,y,z,khull,kedge,kface,
     &  nhull,nedge,nface,kfacelast,tiny,
     &  kfail)

      ! Calculates the convex hull for the points (x,y,z)

      ! Input:

      ! npoi: Number of points
      ! x(1:npoi),y(1:npoi),z(1:npoi): coordinates of the points

      ! Dimensions: The arrays must have at least the size of

      ! khull(npoi)
      ! kedge(4,2*npoi)
      ! kface((npoi+1)*npoi)

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
      ! indices of the points belonging to this face, and kface(k+npoi+1) is
      ! then npoi(iface+1).
      ! kfacelast is the used length of kface, i.e. the ndex of last entry

      ! Example:
      ! iface=1, kface(1)=4, kface(2:5) are the indices of the 4 points of
      ! the first face, and kface(6) are the number of points of the second
      ! face, and so on.

      implicit none

      double precision x(*),y(*),z(*),
     &  zmin,zmax,ymin,ymax,xmin,xmax,xsc,ysc,zsc,
     &  cosmax,cosqpv,
     &  ex(3),ey(3),ez(3),vn(3),q(3),qp(3),qpn(3),
     &  p1(3),p2(3),p3(3),pt1(3),pt2(3),pt3(3),
     &  p21(3),p31(3),vnor(3),dist,
     &  p21n(3),p32(3),
     &  gcen(3),tiny,tiny2
c      double precision pvv

      double precision, dimension (:), allocatable ::  x2,y2,z2,xb,yb,zb

      integer, dimension (:,:), allocatable ::  kedgebuff
      integer, dimension (:), allocatable ::  ibuff,ibuffp,ibuffm,kbuff,
     &  kedgeheap,kfaceheap,istore

      integer khull(*),kedge(4,*),kface(*),ifail,i,kzmin,
     &  nhull,nedge,nface,mface,lface,npoi,ifound,iedge,
     &  ifirst,isecond,ipoi,kpoi,ledge,medge,
     &  jpoi,iover,istat,isame,iside,kside,
     &  nh2d,nedgeheap,
     &  nbuff,iface,nfpoi,kfail,kfacelast,nb,k,
     &  ib1,ib2

      integer :: ical=0

      data ex/1.0d0,0.0d0,0.0d0/
      data ey/0.0d0,1.0d0,0.0d0/
      data ez/0.0d0,0.0d0,1.0d0/

      ical=ical+1
      tiny2=tiny**2

      allocate(kfaceheap(2*npoi))
      allocate(kedgeheap(2*npoi))
      allocate(ibuff(npoi))
      allocate(ibuffp(npoi))
      allocate(ibuffm(npoi))
      allocate(kbuff((npoi+1)*npoi))
      allocate(x2(npoi))
      allocate(y2(npoi))
      allocate(z2(npoi))
      allocate(istore(npoi),xb(npoi),yb(npoi),zb(npoi))

      xb(1:npoi)=x(1:npoi)
      yb(1:npoi)=y(1:npoi)
      zb(1:npoi)=z(1:npoi)

      nb=npoi

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      xmin=1.0d30
      xmax=-1.0d30
      ymin=1.0d30
      ymax=-1.0d30
      zmin=1.0d30
      zmax=-1.0d30

      do i=1,npoi
        if (z(i).lt.zmin) zmin=z(i)
        if (z(i).gt.zmax) zmax=z(i)
        if (y(i).lt.ymin) ymin=y(i)
        if (y(i).gt.ymax) ymax=y(i)
        if (x(i).lt.xmin) xmin=x(i)
        if (x(i).gt.xmax) xmax=x(i)
      enddo

      if (xmin.eq.xmax) then
        kfail=1
        return
      endif
      if (ymin.eq.ymax) then
        kfail=1
        return
      endif
      if (xmin.eq.xmax) then
        kfail=1
        return
      endif

      call util_weed_points(npoi,x,y,z,tiny)

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      istore=0
      do k=1,npoi
        do i=1,nb
          if (
     &        xb(i).eq.x(k).and.yb(i).eq.y(k).and.zb(i).eq.z(k)
     &        ) then
            istore(k)=i
            exit
          endif
        enddo
      enddo

      nedge=0
      nface=0
      nhull=0
      kedge(4,1:2*npoi)=0
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
        endif
      enddo
      gcen=gcen/npoi

      kfail=0
      nface=0
      lface=0
      nedge=0
      nhull=0

      if (kzmin.eq.npoi) then
        goto 9999
      endif

      ifirst=ibuffm(1)
      isecond=0

      p1(1)=x(ifirst)
      p1(2)=y(ifirst)
      p1(3)=z(ifirst)

      if (kzmin.gt.3) then

        do ipoi=1,kzmin
          kpoi=ibuffm(ipoi)
          x2(ipoi)=x(kpoi)
          y2(ipoi)=y(kpoi)
          z2(ipoi)=z(kpoi)
        enddo

        call util_convex_hull_2d(kzmin,x2,y2,nh2d,kbuff,tiny2,ifail)
        kfail=kfail+iabs(ifail)

        ifirst=ibuffm(kbuff(1))
        isecond=ibuffm(kbuff(2))

      else if (kzmin.gt.1) then
        isecond=ibuffm(2)
      endif

c      if (kzmin.gt.1) isecond=ibuffm(2)

      if (isecond.eq.0) then

        ! Search second point of the convex hull

        vn=ey

        do while (isecond.eq.0)

          cosmax=-1.0d30

          do ipoi=1,npoi

            p2(1)=x(ipoi)
            p2(2)=y(ipoi)
            p2(3)=z(ipoi)
            p21=p2-p1

            ! is q = p ?
            if (abs(p21(1)).lt.tiny.and.abs(p21(2)).lt.tiny.and.
     &          abs(p21(3)).lt.tiny) then
              cycle
            endif

            qp=p21
            qpn=qp/sqrt(qp(1)**2+qp(2)**2+qp(3)**2)
            cosqpv=qpn(1)*vn(1)+qpn(2)*vn(2)+qpn(3)*vn(3)

            if (cosqpv.gt.cosmax) then
              cosmax=cosqpv
              isecond=ipoi
            endif

          enddo

        enddo

      endif !isecond.eq.0

      p1(1)=x(ifirst)
      p1(2)=y(ifirst)
      p1(3)=z(ifirst)

      p2(1)=x(isecond)
      p2(2)=y(isecond)
      p2(3)=z(isecond)

      p21=p2-p1
      p21n=p21/sqrt(p21(1)**2+p21(2)**2+p21(3)**2)

      ibuff(1)=ifirst
      ibuff(2)=isecond
c      ibuffm(1)=ifirst
c      ibuffm(2)=isecond
c      ibuffp(1:2)=ibuffm(1:2)
c      nbuffp=2
c      nbuffm=2

      do ipoi=1,npoi

        if (ipoi.eq.ifirst.or.ipoi.eq.isecond) cycle

        !p1, p2, and p3 define a test plane

        p3(1)=x(ipoi)
        p3(2)=y(ipoi)
        p3(3)=z(ipoi)

        p31=p3-p1
        p32=p3-p2

        call util_plane(p1,p2,p3,p1,vnor,dist,iover,istat)
        if (istat.ne.0) then
          cycle
        endif

        if (
     &      abs(p31(1)).lt.tiny.and.abs(p31(2)).lt.tiny.and.
     &      abs(p31(3)).lt.tiny
     &      .or.
     &      abs(p32(1)).lt.tiny.and.abs(p32(2)).lt.tiny.and.
     &      abs(p32(3)).lt.tiny
     &      ) then
c error?          exit
          cycle
        endif

        isame=-1

        ! look for points not belonging to the test plane
        do jpoi=1,npoi

          if (jpoi.eq.ipoi.or.jpoi.eq.ifirst.or.jpoi.eq.isecond) cycle

          q(1)=x(jpoi)
          q(2)=y(jpoi)
          q(3)=z(jpoi)

          call util_plane(p1,p2,p3,q,vnor,dist,iover,istat)

          if (istat.ne.0) then
            kfail=-8
            return
          endif

          if (dist.gt.tiny) then
            iside=1
          else if (dist.lt.-tiny) then
            iside=-1
          else
            iside=0
c            nbuffm=nbuffm+1
c            ibuffm(nbuffm)=jpoi
          endif

          if (iside.eq.0) cycle

          if (isame.eq.-1) then
            isame=1
            kside=iside
          endif

          if (iside.ne.kside) then
            isame=0
            exit
          endif

        enddo !jpoi

c        if (nbuffm.gt.2) then
c          exit
c        endif

        if (isame.eq.1) then
          ibuff(3)=ipoi
          nbuff=3
          exit
        endif

      enddo !ipoi

c      if (nbuffm.ge.3) then
c        nbuff=nbuffm
c        ibuff(1:nbuffm)=ibuffm(1:nbuffm)
c      else if (nbuffp.ge.3) then
c        nbuff=nbuffp
c        ibuff(1:nbuffp)=ibuffp(1:nbuffp)
c      else
c        print*, "*** Error 1 in util_convex_hull_3d_old: No face found!"
c        goto 9999
c      endif

      if (nbuff.ne.3) then
        print*, "*** Error 1 in util_convex_hull_3d_old: No face found!"
        goto 9999
      endif

      !Find all points of first plane

      ifirst=ibuff(1)
      isecond=ibuff(2)
      ipoi=ibuff(3)

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
          return
        endif

        if (abs(dist).lt.tiny) then
          nbuff=nbuff+1
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
        return
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
        return
      endif

      q=gcen-p1
      if (q(1)*vnor(1)+q(2)*vnor(2)+q(3)*vnor(3).gt.0.0d0) then
        ibuffp(1:nh2d)=kbuff(1:nh2d)
        do ipoi=1,nh2d
          kbuff(ipoi)=ibuffp(nh2d-ipoi+1)
        enddo
        vnor=-vnor
      endif

      nh2d=nh2d-1

      ! First plane found
      nface=1
      nedge=nh2d
      nedgeheap=nedge
      lface=1
      kfaceheap(nface)=lface
c      vnpl(1:3,nface)=vnor(1:3)

      do iedge=1,nedge
        ib1=ibuff(kbuff(iedge))
        ib2=ibuff(kbuff(iedge+1))
        if (ib1.lt.ib2) then
          kedge(1,iedge)=ib1
          kedge(2,iedge)=ib2
        else
          kedge(1,iedge)=ib2
          kedge(2,iedge)=ib1
        endif
        kedge(3,iedge)=nface
        kedgeheap(iedge)=iedge
        k=ibuff(kbuff(iedge))
        kface(lface+iedge)=k
      enddo
      kface(lface)=nh2d

c      lface=lface+nh2d

      do while (nedgeheap.gt.0)

        medge=kedgeheap(nedgeheap)
        mface=kedge(3,medge)

        if (mface.le.0.or.kedge(4,medge).ne.0) then
          print*,"*** Error 2 in util_convex_hull_3d_old ***"
          goto 9999
        endif

        lface=kfaceheap(mface)

        !Three pt1, pt2, and pt3 define current plane
        k=kface(lface+1)
        pt1(1)=x(k)
        pt1(2)=y(k)
        pt1(3)=z(k)

        k=kface(lface+2)
        pt2(1)=x(k)
        pt2(2)=y(k)
        pt2(3)=z(k)

        k=kface(lface+3)
        pt3(1)=x(k)
        pt3(2)=y(k)
        pt3(3)=z(k)

        ifirst=kedge(1,medge)
        isecond=kedge(2,medge)

        p1(1)=x(ifirst)
        p1(2)=y(ifirst)
        p1(3)=z(ifirst)

        p2(1)=x(isecond)
        p2(2)=y(isecond)
        p2(3)=z(isecond)

        ! Look for new plane, starting from current edge
        ibuff(1)=ifirst
        ibuff(2)=isecond
        nbuff=2

        do ipoi=1,npoi

          p3(1)=x(ipoi)
          p3(2)=y(ipoi)
          p3(3)=z(ipoi)

          ifound=0
c          do jpoi=1,npoi
c            if (jpoi.eq.ifirst.or.jpoi.eq.isecond) cycle
            call util_plane(pt1,pt2,pt3,p3,vnor,dist,iover,istat)
            if (istat.ne.0) then
              kfail=-8
              return
            endif
            if (abs(dist).lt.tiny) then
              ifound=1
c              exit
            endif
c          enddo

          if (ifound.ne.0.or.ipoi.eq.ifirst.or.ipoi.eq.isecond) cycle

          p31=p3-p1
          p32=p3-p2

          if (
     &        abs(p31(1)).lt.tiny.and.abs(p31(2)).lt.tiny.and.
     &        abs(p31(3)).lt.tiny
     &        .or.
     &        abs(p32(1)).lt.tiny.and.abs(p32(2)).lt.tiny.and.
     &        abs(p32(3)).lt.tiny
     &        ) then
            exit
          endif

          isame=-1

          do jpoi=1,npoi

            if (jpoi.eq.ipoi.or.jpoi.eq.ifirst.or.jpoi.eq.isecond) cycle

            q(1)=x(jpoi)
            q(2)=y(jpoi)
            q(3)=z(jpoi)

            call util_plane(p1,p2,p3,q,vnor,dist,iover,istat)

            if (istat.ne.0) then
              iside=0
              cycle
            endif

            if (dist.gt.tiny) then
              iside=1
            else if (dist.lt.-tiny) then
              iside=-1
            else
              iside=0
            endif

            if (iside.eq.0) cycle

            if (isame.eq.-1) then
              isame=1
              kside=iside
            endif

            if (iside.ne.kside) then
              isame=0
              exit
            endif

          enddo !jpoi

          if (isame.eq.1) then
            ! New point of new plane found
            nbuff=nbuff+1
            if (nbuff.gt.npoi) then
              kfail=-3
              goto 9999
            endif
            ibuff(nbuff)=ipoi
            exit
          endif !same

        enddo !ipoi

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

        nbuff=3

        !Collect all points belonging to the candidate plane
        do ipoi=1,npoi
          if (ipoi.eq.ibuff(1).or.ipoi.eq.ibuff(2).or.ipoi.eq.ibuff(3)) cycle
          q(1)=x(ipoi)
          q(2)=y(ipoi)
          q(3)=z(ipoi)
          call util_plane(p1,p2,p3,q,vnor,dist,iover,istat)
          if (istat.eq.0.and.abs(dist).lt.tiny) then
            nbuff=nbuff+1
            if (nbuff.gt.npoi) then
              kfail=-4
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
          return
        endif

        q=gcen-p1
        if (q(1)*vnor(1)+q(2)*vnor(2)+q(3)*vnor(3).gt.0.0d0) then
          ibuffp(1:nh2d)=kbuff(1:nh2d)
          do ipoi=1,nh2d
            kbuff(ipoi)=ibuffp(nh2d-ipoi+1)
          enddo
          vnor=-vnor
        endif

        nh2d=nh2d-1

c        ! Is the candidate already known?
        ifound=0
c
c        do iface=1,nface
c
cc          if (iface.eq.mface) cycle
c
c          pvv=vnor(1)*vnpl(1,iface)+vnor(2)*vnpl(2,iface)+vnor(3)*vnpl(3,iface)
c          if (pvv.gt.0.99999d0) then
c            ifound=iface
c            exit
c          endif
c
c        enddo !nface

        nfpoi=nh2d

        if (ifound.eq.0) then

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
c          vnpl(1:3,nface)=vnor(1:3)

        else

          kedge(4,medge)=ifound

        endif

c        nedgeheap=nedgeheap-1

        !Check for new edges

        ifound=0

        iedge=nedge
        do ipoi=1,nfpoi
          ifound=0
          do ledge=1,iedge
            ib1=ibuff(kbuff(ipoi))
            ib2=ibuff(kbuff(ipoi+1))
            if (ib1.gt.ib2) then
              ib2=ibuff(kbuff(ipoi))
              ib1=ibuff(kbuff(ipoi+1))
            endif
            if (
     &          ib1.eq.kedge(1,ledge).and.
     &          ib2.eq.kedge(2,ledge)
     &          ) then
              ifound=1
              if (kedge(3,ledge).eq.0) then
                print*,"*** Error in util_convex_hull_3d_old: Bad edge ***"
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
            kedge(1,nedge)=ib1
            kedge(2,nedge)=ib2
            kedge(3,nedge)=nface
          endif
        enddo !iedge=1,nfpoi

        nedgeheap=0
        do iedge=1,nedge
          if (kedge(4,iedge).eq.0) then
            nedgeheap=nedgeheap+1
            kedgeheap(nedgeheap)=iedge
          endif
        enddo

      enddo !while (nedgeheap.gt.0)

      nhull=0
      do iedge=1,nedge
        kpoi=kedge(1,iedge)
        ifound=0
        do ipoi=1,nhull
          if (kpoi.eq.khull(ipoi)) then
            ifound=1
            exit
          endif
        enddo
        if (ifound.eq.0) then
          nhull=nhull+1
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
          khull(nhull)=kpoi
        endif
      enddo

      if (kfail.eq.0) then

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
          kbuff(kfacelast)=kface(kfacelast)
        enddo
        kfacelast=kfacelast-1

        lface=1
        do iface=1,nface
          nfpoi=kbuff(lface)
          do i=1,nfpoi
            kface(lface+i)=istore(kbuff(lface+i))
          enddo
          lface=lface+nfpoi+1
        enddo

        if (lface-1.ne.kfacelast) then
          kfail=kfail+1
          print*,"*** Error in util_convex_hull_3d_old: Bad kfacelast ***"
        endif

        deallocate(kedgebuff)

      endif !kfail

9999  continue

      npoi=nb
      x(1:npoi)=xb(1:npoi)
      y(1:npoi)=yb(1:npoi)
      z(1:npoi)=zb(1:npoi)

c      deallocate(vnpl)
      deallocate(kfaceheap)
      deallocate(kedgeheap)
      deallocate (ibuff)
      deallocate (ibuffp)
      deallocate (ibuffm)
      deallocate (kbuff)
      deallocate(x2)
      deallocate(y2)
      deallocate(z2)
      deallocate(istore,xb,yb,zb)

      return
      end
