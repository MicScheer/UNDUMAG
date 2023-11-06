*CMZ :  2.05/03 06/11/2023  14.07.42  by  Michael Scheer
*CMZ :  2.05/02 04/11/2023  13.09.13  by  Michael Scheer
*CMZ :  2.05/01 11/10/2023  17.11.14  by  Michael Scheer
*-- Author :    Michael Scheer   02/10/2023
      subroutine util_hull_check_convex(nverts,verts,
     &  khull,kedge,kface,nhull,nedge,nface,kfacelast,
     &  nconcave,lconcave,hulltiny,kfail)

      implicit none

*KEEP,hulldim.
      include 'hulldim.cmn'
*KEND.

      double precision, dimension (:), allocatable :: xp,yp,zp

      double precision :: verts(3,nverts),hulltiny,p1(3),p2(3),p3(3),q(3),dist,
     &  vnor(3),hit(3)

      integer :: i,nconcave,nverts,nface,nedge,nhull,lconcave(nverts),kfail,
     &  kfacelast,khull(nverts),kedge(4,lenedge),kface(lenface),l,iover,
     &  idebug=0,ical=0,ked1,ked2,ied,nbuff

      integer, dimension(:), allocatable :: ibuff

      integer :: kloc(3),iface,k,ipoi,npoi

      nconcave=0

      allocate(xp(lenhull),yp(lenhull),zp(lenhull),ibuff(lenhull))

      xp(1:nverts)=verts(1,1:nverts)
      yp(1:nverts)=verts(2,1:nverts)
      zp(1:nverts)=verts(3,1:nverts)

      if (idebug.ne.0) then
        ical=ical+1
        do i=1,nverts
          write(44,*) xp(i),yp(i),zp(i),i,ical,' 1 '
        enddo
        flush(44)
      endif

      call util_convex_hull_3d(
     &  nverts,xp,yp,zp,khull,kedge,kface,
     &  nhull,nedge,nface,kfacelast,hulltiny,kfail)

      if (idebug.ne.0) then
        do i=1,nhull
          write(44,*) xp(khull(i)),yp(khull(i)),zp(khull(i)),khull(i),ical,' 2 '
        enddo
        flush(44)
      endif

      if (nhull.eq.nverts) return

      do i=1,nverts
        kloc=findloc(khull,i,1)
        if (kloc(1).eq.0) then
          nconcave=nconcave+1
          lconcave(nconcave)=i
        endif
      enddo

      if (idebug.ne.0) call util_break

      do l=1,nconcave

        nbuff=0
        k=0
        do iface=1,nface

          k=k+1
          if (k.gt.kfacelast) exit

          npoi=kface(k)
          do i=1,npoi
            k=k+1
            ipoi=kface(k)
            if (i.eq.1) p1=[xp(ipoi),yp(ipoi),zp(ipoi)]
            if (i.eq.2) p2=[xp(ipoi),yp(ipoi),zp(ipoi)]
            if (i.eq.3) p3=[xp(ipoi),yp(ipoi),zp(ipoi)]
          enddo

          ipoi=lconcave(l)
          if (ipoi.lt.0) cycle

          q=[xp(ipoi),yp(ipoi),zp(ipoi)]

          call util_plane_tiny(p1,p2,p3,q,vnor,dist,hulltiny,iover,kfail)

          if(abs(dist).le.hulltiny) then

            nbuff=nbuff+1
            ibuff(nbuff)=iface

            do ied=1,nedge
              ked1=kedge(1,ied)
              ked2=kedge(2,ied)
              if (kedge(3,ied).ne.iface.and.kedge(4,ied).ne.iface) cycle
              call util_dist_to_line(
     &          [xp(ked1),yp(ked1),zp(ked1)],
     &          [xp(ked2),yp(ked2),zp(ked2)],
     &          q,hit,dist,kfail)
              if (dist.lt.hulltiny) then
                ibuff(nbuff)=-ibuff(nbuff)
                exit
              endif
            enddo !nedge

          endif

        enddo !nface

        if (maxval(ibuff(1:nbuff)).lt.0) then
          lconcave(l)=-lconcave(l)
        endif

      enddo !nconcave

      deallocate(xp,yp,zp,ibuff)

      return
      end
