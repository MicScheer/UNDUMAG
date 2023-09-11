*CMZ :          11/09/2023  15.31.12  by  Michael Scheer
*CMZ :  2.04/16 10/09/2023  20.05.58  by  Michael Scheer
*-- Author :    Michael Scheer   08/09/2023
      subroutine util_convex_hull_3d_simp(n,x,y,z,tiny,nhull,khull,nsimp,ksimp,
     &  nface,kface,lface,kfacelast,nedge,kedge,vn,kfail)

      implicit none

      double precision, dimension(:), allocatable :: xh,yh,zh,xb,yb,zb
      double precision, dimension(:,:), allocatable :: fn
      integer, dimension(:), allocatable :: nveto,kveto,ihull,ibuff
      integer, dimension(:,:), allocatable :: ksim

      double precision  x(*),y(*),z(*),vn(3,*),tiny,vnor(3),gcen(3),dist,p(3)

      integer n,ksimp(*),kface(*),lface(*),kedge(4,*),khull(*),
     &  nface,kfail,nn,kfacelast,nsim,nedge,nh,nhull,
     &  mpoi,npoi,lpoi,l,kl,k,ipoi,isim,i,nsimp,n3,
     &  iface,mnpoi,jface,iover,ll,i1,i2,j1,j2,ied,ked,ifound

      n3=n*(n+1)**2
      nsimp=0
      nface=0

      gcen=[sum(x(1:n)),sum(y(1:n)),sum(z(1:n))]/dble(n)
      nhull=0

      call util_convex_hull_3d_simplices(n,x,y,z,tiny,nsimp,ksimp,vn,kfail)
      if (kfail.ne.0) return

      allocate(nveto(n),kveto(4*n3),ksim(nsimp,nsimp),xh(n),yh(n),zh(n),
     &  ihull(n),fn(3,nsimp),xb(n),yb(n),zb(n))
      allocate(ibuff(n))

      ksim=0

      nface=0
      kveto=0
      do i=1,nsimp
        if (kveto(i).ne.0) cycle
        nface=nface+1
        vnor=vn(:,i)
        nsim=1
        ksim(1,nface)=1
        ksim(2,nface)=i
        kveto(nface)=1
        fn(:,nface)=vnor
        do k=i+1,nsimp
          if (dot_product(vnor,vn(:,k)).gt.1.0d0-tiny.and.kveto(k).eq.0) then
            nsim=nsim+1
            ksim(nsim+1,nface)=k
            ksim(1,nface)=nsim
            kveto(k)=1
          endif
        enddo
      enddo

      kfacelast=0

      do isim=1,nface
        npoi=0
        kfacelast=kfacelast+1
        kl=kfacelast
        nn=ksim(1,isim)
        kveto(1:3*n)=0
        do i=1,nn
          l=ksim(i+1,isim)
          do ipoi=1,3
            lpoi=ksimp(4*(l-1)+1+ipoi)
            if (kveto(lpoi).ne.0) cycle
            npoi=npoi+1
            ibuff(npoi)=lpoi
            xh(npoi)=x(lpoi)
            yh(npoi)=y(lpoi)
            zh(npoi)=z(lpoi)
            kveto(lpoi)=1
          enddo
        enddo
        if (abs(fn(1,isim)).gt.0.5d0) then
          call util_convex_hull_2d(npoi,yh,zh,nh,ihull,tiny,kfail)
        else if (abs(fn(2,isim)).gt.0.5d0) then
          call util_convex_hull_2d(npoi,xh,zh,nh,ihull,tiny,kfail)
        else
          call util_convex_hull_2d(npoi,xh,yh,nh,ihull,tiny,kfail)
        endif
        if (kfail.ne.0) then
          print*,"*** Bad return from util_convex_hull_2d in util_convex_hull_3d_simp ***"
          stop
        endif

        nh=nh-1

        p=[xh(1),yh(1),zh(1)]
        call util_plane(p,[xh(2),yh(2),zh(2)],[xh(3),yh(3),zh(3)],p,
     &    vnor,dist,iover,kfail)

        if (dot_product(p-gcen,vnor).lt.0.0d0) then
          xb(1:nh)=xh(1:nh)
          yb(1:nh)=yh(1:nh)
          zb(1:nh)=zh(1:nh)
          do i=1,nh
            xh(i)=xb(nh-i+1)
            yh(i)=yb(nh-i+1)
            zh(i)=zb(nh-i+1)
          enddo
        endif

        do ipoi=1,nh
          mpoi=ibuff(ihull(ipoi))
          kfacelast=kfacelast+1
          kface(kfacelast)=mpoi
        enddo

        kface(kl)=nh
        lface(isim)=kl

      enddo

      nedge=0
      kedge(1:4,1:n)=0
      nveto=0

      do iface=1,nface

        l=lface(iface)
        npoi=kface(l)

        do ipoi=1,npoi

          if (ipoi.lt.npoi) then
            i1=kface(l+ipoi)
            i2=kface(l+ipoi+1)
          else
            i1=kface(l+ipoi)
            i2=kface(l+1)
          endif

          if (nveto(i1).eq.0) then
            nhull=nhull+1
            khull(nhull)=i1
            nveto(i1)=1
          endif

          ifound=0
          do ied=1,nedge
            j1=kedge(1,ied)
            j2=kedge(2,ied)
            if (j1.eq.i1.and.j2.eq.i2.or.j1.eq.i2.and.j2.eq.i1) then
              ifound=1
              ked=ied
              exit
            endif
          enddo
          if (ifound.eq.0) then
            nedge=nedge+1
            kedge(1,nedge)=i1
            kedge(2,nedge)=i2
            kedge(3,nedge)=iface
            ked=nedge
          endif
          if (kedge(4,ked).ne.0) cycle
          ifound=0
          do jface=iface+1,nface
            ll=lface(jface)
            mnpoi=kface(ll)
            do mpoi=1,mnpoi
              if (mpoi.lt.npoi) then
                j1=kface(ll+mpoi)
                j2=kface(ll+mpoi+1)
              else
                j1=kface(ll+mpoi)
                j2=kface(ll+1)
              endif
              if (j1.eq.i1.and.j2.eq.i2.or.j1.eq.i2.and.j2.eq.i1) then
                kedge(4,ked)=jface
                ifound=1
                exit
              endif
            enddo
            if (ifound.eq.1) exit
          enddo !jface
        enddo !iedg
      enddo !iface

      if (nhull+nface-nedge.ne.2) then
        print*,"*** Error in util_convex_hull_3d_simp: Euler's rule not fullfilled ***"
        kfail=-1
      endif

      deallocate(nveto,kveto,ksim,xh,yh,zh,ihull,ibuff,fn,xb,yb,zb)

      return
      end
