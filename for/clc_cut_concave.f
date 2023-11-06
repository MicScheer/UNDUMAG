*CMZ :  2.05/02 29/10/2023  11.14.03  by  Michael Scheer
*CMZ :  2.05/01 11/10/2023  17.22.07  by  Michael Scheer
*-- Author :    Michael Scheer   02/10/2023
      subroutine clc_cut_concave(tc1,tc2,kcut)

      use bpolyederf90m
      use undumagf90m
      use magnets_structure
      use utilmod

      implicit none

      double precision, dimension(:,:), allocatable :: verts,verts1,verts2,
     &  vwork,verts2d
      double precision, dimension(:), allocatable :: xb,yb,zb

      integer, dimension(:,:), allocatable :: ifaces1,ifaces2,ifaces
      integer, dimension(:), allocatable :: npois1,npois2,npois,
     &  lifaces,lifaces1,lifaces2,khull,ivera2d

      double precision ymin,ymax,
     &  pcut(3),p1(3),p2(3),p3(3),vnor(3),dot,pcen(3),rotmat(3,3),pcenrot(3),
     &  rotinv(3,3),x1,x2,y1,y2,z1,z2,xc(2),zc(2),cut,dy,p(3),q(3)
c      double precision u33(3,3),w33(3,3)

      integer :: kpoicut,kcut,iplan,ipoi,npoi,kplancut,isig,isigo,
     &  i,istatus,nallo=-32,npoimax,npoimax1,npoimax2,nface,nface1,nface2,n1,n2,
     &  nverts1,nverts2,nverts,ic,lcut(2),icut(2),ifound,mfaces,k,
     &  ntri,itri,idebug=0,iscutplan

*KEEP,hulldim.
      include 'hulldim.cmn'
*KEND.

      Type(T_Concave) tc1,tc2

      save

      if (nallo.lt.0) then
        nallo=-nallo
        allocate(xb(nallo),yb(nallo),zb(nallo),
     &    vwork(3,nallo),verts(3,nallo),verts2d(2,nallo),
     &    ifaces(nallo,nallo),ifaces1(nallo,nallo),ifaces2(nallo,nallo),
     &    lifaces(nallo),lifaces1(nallo),lifaces2(nallo),
     &    npois(nallo),npois1(nallo),npois2(nallo),khull(nallo),ivera2d(nallo))
      endif

      if (nallo.lt.max(npoimax,nface)**2) then
        deallocate(xb,yb,zb,verts,ifaces1,ifaces2,npois1,npois2,khull,
     &    verts2d,ivera2d,vwork,ifaces,lifaces,lifaces1,lifaces2,npois)
        nallo=2*max(npoimax,nface)**2
        allocate(xb(nallo),yb(nallo),zb(nallo),verts2d(2,nallo),
     &    vwork(3,nallo),verts(3,nallo),
     &    ifaces(nallo,nallo),ifaces1(nallo,nallo),ifaces2(nallo,nallo),
     &    lifaces(nallo),lifaces1(nallo),lifaces2(nallo),
     &    npois(nallo),npois1(nallo),npois2(nallo),khull(nallo),ivera2d(nallo))
      endif

      npoimax=tc1%npoimax
      nface=tc1%tmag%nface
      ifaces(1:npoimax,1:nface)=tc1%ifaces(1:npoimax,1:nface)
      npois(1:nface)=tc1%npois(1:nface)
      lifaces(1:nface)=tc1%lifaces(1:nface)
      nverts=tc1%nverts
      verts=tc1%verts

      kcut=0
      if (tc1%isconvex.ne.0.or.tc1%nconcave.eq.0) return

      !all util_break

      ! Triangulation

      mfaces=nface
      do iplan=1,mfaces

        npoi=npois(iplan)
        khull(1:npoi)=ifaces(1:npoi,iplan)
        xb(1:npoi)=verts(1,ifaces(1:npoi,iplan))
        yb(1:npoi)=verts(2,ifaces(1:npoi,iplan))
        zb(1:npoi)=verts(3,ifaces(1:npoi,iplan))

        p=[xb(2)-xb(1),yb(2)-yb(1),zb(2)-zb(1)]
        q=[xb(3)-xb(2),yb(3)-yb(2),zb(3)-zb(2)]

        call util_vcross(p,q,vnor)
        vnor=abs(vnor)/norm2(vnor)

        if (vnor(1).gt.0.1d0) then
          verts2d(1,1:npoi)=yb(1:npoi)
          verts2d(2,1:npoi)=zb(1:npoi)
        else if (vnor(2).gt.0.1d0) then
          verts2d(1,1:npoi)=xb(1:npoi)
          verts2d(2,1:npoi)=zb(1:npoi)
        else
          verts2d(1,1:npoi)=xb(1:npoi)
          verts2d(2,1:npoi)=yb(1:npoi)
        endif

        call  util_triangu_2d(npoi,verts2d,ivera2d,ntri,tiny,istatus)

        ifaces(1:3,iplan)=khull(ivera2d(1:3))
        npois(iplan)=3

        do itri=2,ntri
          nface=nface+1
          ifaces(1:3,nface)=khull(ivera2d((itri-1)*3+1:(itri-1)*3+3))
          npois(nface)=3
        enddo

      enddo !nface

      kpoicut=tc1%kconcave(1)
      pcut=verts(:,kpoicut)

      kplancut=0
      do iplan=1,nface
        npoi=npois(iplan)
        do ipoi=1,npoi
          if(ifaces(ipoi,iplan).eq.kpoicut) then
            kplancut=iplan
            exit
          endif
        enddo
        if (kplancut.gt.0) then
          isigo=0
          p1=verts(:,ifaces(1,kplancut))
          p2=verts(:,ifaces(2,kplancut))
          p3=verts(:,ifaces(3,kplancut))
          pcen=(p1+p2+p3)/3.0d0
          call util_vcross(p2-p1,p3-p2,vnor)
          vnor=vnor/norm2(vnor)
          do i=1,nverts
            dot=dot_product(verts(:,i)-pcen,vnor)
            if (dot.gt.hulltiny) then
              isig=1
            else if (dot.lt.-hulltiny) then
              isig=-1
            else
              isig=0
            endif
            if (isigo.eq.0.and.isig.ne.0) isigo=isig
            if (isig.ne.0.and.isig.ne.isigo) then
              kplancut=-kplancut
              exit
            endif
          enddo
        endif
        if (kplancut.lt.0) then
          exit
        else
          kplancut=0
        endif
      enddo

      if (kplancut.gt.0) then
        kcut=-1
        return
      endif

      kplancut=-kplancut
      kcut=1

      call util_rotate_vector_to_y_axis(vnor,rotmat,istatus)
      rotinv=transpose(rotmat)

      call util_mat_mul_vec_3x3(rotmat,pcen,pcenrot)
      do i=1,nverts
        call util_mat_mul_vec_3x3(rotmat,verts(:,i),vwork(:,i))
        if (idebug.ne.0) then
          write(88,*)sngl(verts(:,i)),sngl(vwork(:,i)),i
          flush(88)
        endif
      enddo
      verts=vwork

      if (idebug.ne.0) then
        write(80,*)nverts
        do i=1,nverts
          write(80,*)verts(:,i)
        enddo
        write(80,*)nface
        do iplan=1,nface
          npoi=npois(iplan)
          write(80,*)npoi
          write(80,*)ifaces(1:npoi,iplan)
        enddo
        flush(80)
      endif

      cut=pcenrot(2)

      nface1=0
      nface2=0
      npoimax1=0
      npoimax2=0
      nverts1=nverts
      nverts2=nverts

      verts1=verts
      verts2=verts

      !if (idebug.ne.0) !all util_break
      !print*,cut
      do iplan=1,nface

        ic=0
        icut=0
        npoi=npois(iplan)

        do ipoi=1,npoi
          xb(ipoi)=verts(1,ifaces(ipoi,iplan))
          yb(ipoi)=verts(2,ifaces(ipoi,iplan))
          zb(ipoi)=verts(3,ifaces(ipoi,iplan))
          if (idebug.ne.0) then
            write(89,*)xb(ipoi),yb(ipoi),zb(ipoi),ipoi,iplan
            flush(89)
          endif
        enddo

        xb(npoi+1)=xb(1)
        yb(npoi+1)=yb(1)
        zb(npoi+1)=zb(1)

        ymin=minval(yb(1:npoi))
        ymax=maxval(yb(1:npoi))

c        print*,iplan,ymin,ymax

        if (abs(ymax-ymin).lt.hulltiny.and.abs(ymax-cut).lt.hulltiny) then
          iscutplan=1
        else
          iscutplan=0
        endif

        if(iscutplan.eq.0.and.ymax.le.cut+hulltiny) then
          nface1=nface1+1
          ifaces1(1:npoi,nface1)=ifaces(1:npoi,iplan)
          npois1(nface1)=npoi
        else if(ymin.ge.cut-hulltiny) then
          nface2=nface2+1
          ifaces2(1:npoi,nface2)=ifaces(1:npoi,iplan)
          npois2(nface2)=npoi
        else
          do ipoi=1,npoi
            x1=xb(ipoi)
            x2=xb(ipoi+1)
            y1=yb(ipoi)
            y2=yb(ipoi+1)
            z1=zb(ipoi)
            z2=zb(ipoi+1)
            if (y1.lt.cut-hulltiny.and.y2.gt.cut+hulltiny
     &          .or.
     &          y1.gt.cut+hulltiny.and.y2.lt.cut-hulltiny) then
c              print*,iplan,cut
c              print*,x1,y1,z1
c              print*,x2,y2,z2
              dy=(cut-y1)/(y2-y1)
c              print*,dy
              ic=ic+1
              if (ic.gt.2) then
                stop "*** Schneidefehler!"
              endif
              icut(ic)=ipoi
              xc(ic)=x1+(x2-x1)*dy
              zc(ic)=z1+(z2-z1)*dy
c              print*,xc(ic),zc(ic)
            endif !cut it
          enddo
        endif

        if (ic.ne.0) then

c          !all util_break

          do i=1,ic
            ifound=0
            p1=[xc(i),cut,zc(i)]
            do ipoi=1,nverts
              if(norm2(p1-verts(:,ipoi)).lt.hulltiny) then
                ifound=1
                exit
              endif
            enddo
            if (ifound.eq.0) then
              nverts=nverts+1
              lcut(i)=nverts
              verts(:,nverts)=p1
              npois(iplan)=npois(iplan)+1
              ifaces(npois(iplan),iplan)=nverts
              xb(npois(iplan))=p1(1)
              yb(npois(iplan))=p1(2)
              zb(npois(iplan))=p1(3)
            else
              npois(iplan)=npois(iplan)+1
              ifaces(npois(iplan),iplan)=ipoi
              lcut(i)=icut(i)
              xb(npois(iplan))=p1(1)
              yb(npois(iplan))=p1(2)
              zb(npois(iplan))=p1(3)
            endif
          enddo

          nface1=nface1+1
          nface2=nface2+1

          n1=0
          n2=0

          do i=1,npois(iplan)
            k=ifaces(i,iplan)
            if(abs(yb(i)-cut).le.hulltiny) then
c              if (k.eq.lcut(1).or.k.eq.lcut(2)) then
                n1=n1+1
                ifaces1(n1,nface1)=k
                n2=n2+1
                ifaces2(n2,nface2)=k
c              else
c                n1=n1+1
c                ifaces1(n1,nface1)=ifaces(i,iplan)
c                n2=n2+1
c                ifaces2(n2,nface2)=ifaces(i,iplan)
c              endif
            else if(yb(i).le.cut+hulltiny) then
c              if (k.eq.lcut(1).or.k.eq.lcut(2)) then
                n1=n1+1
                ifaces1(n1,nface1)=k
c              else
c                n1=n1+1
c                ifaces1(n1,nface1)=ifaces(i,iplan)
c              endif
            else if(yb(i).ge.cut-hulltiny) then
c              if (k.eq.lcut(1).or.k.eq.lcut(2)) then
                n2=n2+1
                ifaces2(n2,nface2)=k
c              else
c                n2=n2+1
c                ifaces2(n2,nface2)=ifaces(i,iplan)
c              endif
            endif
          enddo
          npois1(nface1)=n1
          npois2(nface2)=n2
        endif
      enddo !nface

      do i=1,nverts
        call util_mat_mul_vec_3x3(rotinv,verts(:,i),vwork(:,i))
      enddo
      verts=vwork

      n1=0
      npoimax=0
      do iplan=1,nface1
        npoi=npois1(iplan)
        if (npoi.gt.npoimax) npoimax=npoi
        do ipoi=1,npoi
          n1=n1+1
          i=ifaces1(ipoi,iplan)
          xb(n1)=verts(1,i)
          yb(n1)=verts(2,i)
          zb(n1)=verts(3,i)
        enddo
      enddo

      call util_weed_points(n1,xb,yb,zb,hulltiny**2)

      if (n1.gt.tc1%nverts) then
        allocate(tc1%verts(3,n1))
        deallocate(tc1%verts)
      endif
      tc1%nverts=n1

      do ipoi=1,n1
        if (idebug.ne.0) then
          write(81,*)xb(ipoi),yb(ipoi),zb(ipoi),ipoi
          flush(81)
        endif
        tc1%verts(:,ipoi)=[xb(ipoi),yb(ipoi),zb(ipoi)]
      enddo

      if (nface1.gt.tc1%tmag%nface.or.npoimax.gt.tc1%npoimax) then
        if(tc1%tmag%nface.gt.0) then
          deallocate(tc1%ifaces)
          deallocate(tc1%lifaces,tc1%npois)
        endif
        allocate(tc1%ifaces(npoimax,nface1))
        allocate(tc1%lifaces(nface1),tc1%npois(nface1))
      endif
      tc1%tmag%nface=nface1
      tc1%npoimax=npoimax

      n1=1
      do iplan=1,nface1
        npoi=npois1(iplan)
        tc1%npois(iplan)=npoi
        tc1%ifaces(1:npoi,iplan)=ifaces1(1:npoi,iplan)
        lifaces(iplan)=n1
        n1=n1+npoi
      enddo

      n2=0
      npoimax=0
      do iplan=1,nface2
        npoi=npois2(iplan)
        if (npoi.gt.npoimax) npoimax=npoi
        do ipoi=1,npoi
          n2=n2+1
          i=ifaces2(ipoi,iplan)
          xb(n2)=verts(1,i)
          yb(n2)=verts(2,i)
          zb(n2)=verts(3,i)
        enddo
      enddo

      call util_weed_points(n2,xb,yb,zb,hulltiny**2)

      if (n2.gt.tc2%nverts) then
        if (tc2%nverts.gt.0) deallocate(tc2%verts)
        allocate(tc2%verts(3,n2))
      endif

      tc2%nverts=n2

      !all util_break

      do ipoi=1,n2
        if (idebug.ne.0) then
          write(82,*)xb(ipoi),yb(ipoi),zb(ipoi),ipoi
          flush(82)
        endif
        tc2%verts(:,ipoi)=[xb(ipoi),yb(ipoi),zb(ipoi)]
      enddo

      if (nface2.gt.tc2%tmag%nface.or.npoimax.gt.tc2%npoimax) then
        if (tc2%tmag%nface.gt.0) then
          deallocate(tc2%ifaces)
          deallocate(tc2%lifaces,tc2%npois)
        endif
        allocate(tc2%ifaces(npoimax,nface2))
        allocate(tc2%lifaces(nface2),tc2%npois(nface2))
      endif
      tc2%tmag%nface=nface2
      tc2%npoimax=npoimax

      n2=1
      do iplan=1,nface2
        npoi=npois2(iplan)
        tc2%npois(iplan)=npoi
        tc2%ifaces(1:npoi,iplan)=ifaces2(1:npoi,iplan)
        lifaces(iplan)=n2
        n2=n2+npoi
      enddo

      return
      end
