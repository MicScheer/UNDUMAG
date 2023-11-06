*CMZ :  2.05/03 05/11/2023  17.47.42  by  Michael Scheer
*CMZ :  2.05/02 05/11/2023  14.55.18  by  Michael Scheer
*CMZ :  2.05/01 11/10/2023  17.25.06  by  Michael Scheer
*-- Author :    Michael Scheer   02/10/2023
      subroutine clc_concave_to_convex

      use bpolyederf90m
      use undumagf90m
      use magnets_structure
      use utilmod

      implicit none

      Type(T_concave), dimension(:), allocatable :: tconc,heap

      character(512), dimension(:), allocatable :: cbuff
      character(512) cline,cfile,cword
      character(128) ckey

      double precision, dimension (:), allocatable :: xp,yp,zp

      double precision x,y,z,br(5),undumag_variable_getval,
     &  vnor(3),p1(3),p2(3),p3(3),q(3)

      integer, dimension (:,:), allocatable :: kedge
      integer, dimension (:), allocatable :: khull,kface,ibuff

      integer :: i,iconcave,l,ipoi,npoi,luni,nplan,iplan,nline,
     &  nverts,nbuff,nface,nedge,nhull,kfacelast,kfail,kcut,nconv,nheap,
     &  idebug=0,k,ifound,il,ibrn,ksolid,nsolid,j,mhull

*KEEP,hulldim.
      include 'hulldim.cmn'
*KEND.

      integer :: ipos(2,100),nwords,istat,nallo=1000,nfacemax,npoimax,
     &  nalloconc,kalloconc=0

      Type(T_Concave) tc1,tc2

      allocate(cbuff(nallo),ibuff(nallo),
     &  xp(nallo),yp(nallo),zp(nallo))
      allocate(khull(nallo),kface(nallo),kedge(4,nallo))

      lenhull=nallo
      lenedge=nallo
      lenface=nallo
      nverhullmax=nallo

      nconcave_t=0
      do i=1,nclccave_t
        cline=clcconcave(i)
        call util_string_split(cline,100,nwords,ipos,istat)
        if (cline(1:1).eq.'&') then
          nconcave_t=nconcave_t+1
        endif
      enddo

      nalloconc=nconcave_t*10
      allocate(t_concaves(nalloconc),heap(nalloconc))

      allocate(brnmat(2,nconcave_t))

      nconv=0
      iconcave=0

      do il=1,nclccave_t,6

        iconcave=iconcave+1

        cline=clcconcave(il)
        call util_string_split(cline,100,nwords,ipos,istat)

        if (idebug.ne.0) call util_break

        tc1%tmag%IwasConcave=iconcave
        tc1%tmag%ispole=0
        tc1%tmag%isspecial=0

        ckey=cline(ipos(1,2):ipos(2,2))
        call util_lower_case(ckey)

        if (ckey.eq.'special_concave_magnet'.or.
     &    ckey.eq.'special_concave_pole') tc1%tmag%isspecial=1

        if (ckey.eq.'convex_pole'.or.
     &    ckey.eq.'special_concave_pole') tc1%tmag%ispole=1

        cline=clcconcave(il+1)
        call util_string_split(cline,100,nwords,ipos,istat)
        tc1%tmag%cnam=cline(ipos(1,2):ipos(2,2))
        tc1%tmag%cmoth=cline(ipos(1,3):ipos(2,3))

        ckey=cline(ipos(1,1):ipos(2,1))
        call util_lower_case(ckey)

        if (ckey.ne.'file'.and.ckey.ne.'stl_ascii') then
          print*,"*** Error in CLC_CONCAVE_TO_CONVEX: Concave items must be read from file ***"
          print*,"*** Valid key words are 'File' and STL_Ascii ***"
          stop "UNDUMAG aborted"
        endif

        cword=cline(ipos(1,4):ipos(2,4))
        if (cword(1:1).eq.'$') then
          tc1%tmag%icol=int(undumag_variable_getval(cword))
        else
          read(cword,*)tc1%tmag%icol
        endif

        if (tc1%tmag%isspecial.eq.0) then
          tc1%tmag%kmodule=magmodulecav(iconcave)
        else
          tc1%tmag%kmodule=0
        endif

        cline=clcconcave(il+2)
        call util_string_split(cline,10,nwords,ipos,istat)
        do i=1,3
          cword=cline(ipos(1,i):ipos(2,i))
          if (cword(1:1).eq.'$') then
            tc1%tmag%xyz(i)=undumag_variable_getval(cword)
          else
            read(cword,*)tc1%tmag%xyz(i)
          endif
        enddo

        cline=clcconcave(il+3)
        call util_string_split(cline,10,nwords,ipos,istat)

        if (tc1%tmag%IsPole.eq.0) then
          do i=1,5
            cword=cline(ipos(1,i):ipos(2,i))
            if (cword(1:1).eq.'$') then
              cword=cline(ipos(1,i):ipos(2,i))
              Br(i)=undumag_variable_getval(cword)
            else
              read(cword,*)br(i)
            endif
          enddo

          if (norm2(br(2:4)).ne.0.0d0) br(2:4)=br(2:4)/norm2(br(2:4))

          tc1%tmag%brn=br(1)
          tc1%tmag%br=br(2:4)*br(1)
          tc1%tmag%imat=nint(br(5))
          tc1%tmag%IsBlock=0
          tc1%tmag%IsRotated=0
          tc1%tmag%ctype='File'

          ifound=0
          do ibrn=1,nbrnmat
            if (br(1).eq.brnmat(2,ibrn).and.br(5).eq.brnmat(1,ibrn)) then
              ifound=1
              exit
            endif
          enddo
          if (ifound.eq.0) then
            nbrnmat=nbrnmat+1
            brnmat(1,nbrnmat)=br(5) !material index
            brnmat(2,nbrnmat)=br(1) !Br
          endif

        else

          cword = cline(ipos(1,1):ipos(2,1))
          if (cword(1:1).eq.'$') then
            tc1%tmag%imat=nint(undumag_variable_getval(cword))
          else
            read(cword,*)tc1%tmag%imat
          endif

        endif !Pole

        cline=clcconcave(il+4)
        call util_string_split(cline,10,nwords,ipos,istat)

        cword = cline(ipos(1,1):ipos(2,1))
        if (cword(1:1).eq.'$') then
          tc1%tmag%nxdiv=nint(undumag_variable_getval(cword))
        else
          read(cword,*)tc1%tmag%nxdiv
        endif

        cword = cline(ipos(1,2):ipos(2,2))
        if (cword(1:1).eq.'$') then
          tc1%tmag%nxdiv=nint(undumag_variable_getval(cword))
        else
          read(cword,*)tc1%tmag%nydiv
        endif

        cword = cline(ipos(1,3):ipos(2,3))
        if (cword(1:1).eq.'$') then
          tc1%tmag%nxdiv=nint(undumag_variable_getval(cword))
        else
          read(cword,*)tc1%tmag%nzdiv
        endif

        cword = cline(ipos(1,4):ipos(2,4))
        if (cword(1:1).eq.'$') then
          tc1%tmag%yfracdiv=undumag_variable_getval(cword)
        else
          read(cword,*)tc1%tmag%yfracdiv
        endif

        cword = cline(ipos(1,5):ipos(2,5))
        if (cword(1:1).eq.'$') then
          tc1%tmag%zfracdiv=undumag_variable_getval(cword)
        else
          read(cword,*)tc1%tmag%zfracdiv
        endif

        if (idebug.ne.0) call util_break

        tc2=tc1

        nfacemax=0
        nplanmax=0

        cline=clcconcave(il+5)
        call util_string_split(cline,100,nwords,ipos,istat)
        cfile=cline(ipos(1,1):ipos(2,1))
        if (nwords.gt.1) then
          cword=cline(ipos(1,2):ipos(2,2))
          if(cword(1:1).eq.'!') then
            ksolid=1
          else
            read(cword,*) ksolid
          endif
        else
          ksolid=1
        endif

        nline=0
        nsolid=0

        open(newunit=luni,file=cfile,status='old')
        do while (.true.)
          read(luni,'(a)',iostat=istat) cline
          if (ckey.eq.'stl_ascii') then
            if (cline(1:5).eq.'solid') then
              nsolid=nsolid+1
            endif
            if (nsolid.ne.ksolid) cycle
          else if (nsolid.eq.ksolid.and.cline(1:8).eq.'endsolid') then
            exit
          endif
          if (istat.eq.0) then
            nline=nline+1
            if (nline.gt.nallo) then
              deallocate(cbuff,ibuff,kface,kedge,khull)
              nallo=nline*3
              allocate(cbuff(nallo),ibuff(nallo))
              allocate(khull(nallo),kface(nallo),kedge(4,nallo))
              lenhull=nallo
              lenedge=nallo
              lenface=nallo
              nverhullmax=nallo
            endif
            cbuff(nline)=cline
          else
            exit
          endif
        enddo

        close(luni)

        if (ckey.eq.'file') then

          i=1
          read(cbuff(i),*)nverts
          tc1%nverts=nverts

          allocate(tc1%verts(3,nverts))

          do ipoi=1,nverts
            i=i+1
            read(cbuff(i),*) x,y,z
            tc1%verts(:,ipoi)=[x,y,z]
          enddo

          i=i+1
          read(cbuff(i),*)nplan
          if(nplan.gt.nplanmax) nplanmax=nplan

          tc1%tmag%nface=nplan

          allocate(tc1%lifaces(nplan),tc1%npois(nplan))
          npoimax=0

          do iplan=1,nplan
            i=i+1
            read(cbuff(i),*)npoi
            if(npoi.gt.npoimax) npoimax=npoi
            i=i+1
            tc1%lifaces(iplan)=i
            tc1%npois(iplan)=npoi
          enddo

          tc1%npoimax=npoimax
          allocate(tc1%ifaces(npoimax,nplan))

          nbuff=0
          do iplan=1,nplan
            l=tc1%lifaces(iplan)
            npoi=tc1%npois(iplan)
            if (npoi.gt.npoimax) npoimax=npoi
            read(cbuff(l),*) tc1%ifaces(1:npoi,iplan)
            read(cbuff(l),*) ibuff(nbuff+1:nbuff+npoi)
            nbuff=nbuff+npoi
            if (nbuff*2*npoimax.gt.nallo) then
              nallo=nbuff*2*npoimax*2
              deallocate(cbuff,ibuff,kface,kedge,khull)
              allocate(cbuff(nallo),ibuff(nallo))
              allocate(khull(nallo),kface(nallo),kedge(4,nallo))
              lenhull=nallo
              lenedge=nallo
              lenface=nallo
              nverhullmax=nallo
            endif
          enddo

        else if (ckey.eq.'stl_ascii') then

          nplan=nline/7
          if(nplan.gt.nplanmax) nplanmax=nplan

          nverts=nplan*3

          allocate(tc1%verts(3,nverts))
          allocate(tc1%lifaces(nplan),tc1%npois(nplan),tc1%ifaces(3,nplan))
          tc1%npois=3
          tc1%npoimax=3

          nbuff=0
          i=1

          do iplan=1,nplan
            i=i+1
            call util_string_split(cbuff(i),100,nwords,ipos,istat)
            read(cbuff(i)(ipos(1,3):ipos(2,5)),*) vnor
            i=i+1
            do j=1,3
              i=i+1
              nbuff=nbuff+1
              call util_string_split(cbuff(i),100,nwords,ipos,istat)
              read(cbuff(i)(ipos(1,2):ipos(2,4)),*) tc1%verts(1:3,nbuff)
              tc1%ifaces(j,nplan)=nbuff
              if (nbuff*2*npoimax.gt.nallo) then
                nallo=nbuff*2*npoimax*2
                deallocate(cbuff,ibuff,kface,kedge,khull)
                allocate(cbuff(nallo),ibuff(nallo))
                allocate(khull(nallo),kface(nallo),kedge(4,nallo))
                lenhull=nallo
                lenedge=nallo
                lenface=nallo
                nverhullmax=nallo
              endif
              ibuff(nbuff)=nbuff
            enddo
            p1=tc1%verts(:,nbuff-2)
            p2=tc1%verts(:,nbuff-1)
            p3=tc1%verts(:,nbuff)
            call util_vcross(p1,p2,q)
            if (dot_product(q,vnor).lt.0.0d0) then
              tc1%verts(:,nbuff-1)=p3
              tc1%verts(:,nbuff)=p2
            endif
            i=i+2
          enddo !nplan

          nverts=nbuff

        endif !(ckey.eq.'file') then

        if (idebug.ne.0) then
          do i=1,nbuff
            write(43,*)tc1%verts(:,ibuff(i)),i,ibuff(i)
          enddo
        endif

        call util_hull_check_convex(
     &    nverts,tc1%verts(:,1:nverts),khull,kedge,kface,
     &    nhull,nedge,nface,kfacelast,nbuff,ibuff,hulltiny,kfail)

        tc1%IsConvex=1
        tc1%nconcave=0

        if (nhull.ne.nverts.and.maxval(ibuff(1:nbuff)).gt.0) then
          tc1%nconcave=nbuff
          allocate(tc1%kconcave(nbuff))
          do i=1,nbuff
            if (ibuff(i).gt.0) then
              tc1%IsConvex=0
              exit
            endif
          enddo
          tc1%kconcave(1:nbuff)=ibuff(1:nbuff)
        endif

        if (tc1%IsConvex.ne.0) then

          do i=1,nhull
            l=khull(i)
            xp(i)=tc1%verts(1,l)
            yp(i)=tc1%verts(2,l)
            zp(i)=tc1%verts(3,l)
          enddo

          mhull=nhull
          call util_convex_hull_3d(mhull,
     &      xp,yp,zp,
     &      khull,kedge,kface,
     &      nhull,nedge,nface,kfacelast,hulltiny,kfail)

          nconv=nconv+1

          if(nconv.gt.nalloconc) then
            nalloconc=nalloconc*2
            if (kalloconc.ne.0) deallocate(tconc)
            allocate(tconc(nalloconc))
            kalloconc=1
            tconc(1:nconv-1)=t_concaves(1:nconv-1)
            deallocate(t_concaves)
            allocate(t_concaves(nalloconc))
            t_concaves(1:nconv-1)=tconc(1:nconv-1)
          endif

          allocate(tc1%tmag%xhull0(nhull),tc1%tmag%yhull0(nhull),
     &      tc1%tmag%zhull0(nhull))
          allocate(tc1%tmag%xhull(nhull),tc1%tmag%yhull(nhull),
     &      tc1%tmag%zhull(nhull))

          tc1%tmag%gcen=0.0d0
          do i=1,nhull
            l=khull(i)
            tc1%tmag%xhull0(i)=xp(l)+tc1%tmag%xyz(1)
            tc1%tmag%yhull0(i)=yp(l)+tc1%tmag%xyz(2)
            tc1%tmag%zhull0(i)=zp(l)+tc1%tmag%xyz(3)
            tc1%tmag%gcen=tc1%tmag%gcen+[xp(l),yp(l),zp(l)]
          enddo

          tc1%tmag%gcen=tc1%tmag%gcen/dble(nhull)+tc1%tmag%xyz
          tc1%tmag%xhull=tc1%tmag%xhull0
          tc1%tmag%yhull=tc1%tmag%yhull0
          tc1%tmag%zhull=tc1%tmag%zhull0

          tc1%tmag%nface=nface
          tc1%tmag%nedge=nedge
          tc1%tmag%nhull=nhull
          tc1%tmag%kfacelast=kfacelast

          allocate(tc1%tmag%khull(nhull))
          tc1%tmag%khull(1:nhull)=khull(1:nhull)
          allocate(tc1%tmag%lface(nface))
          allocate(tc1%tmag%kface(kfacelast))
          tc1%tmag%kface(1:kfacelast)=kface(1:kfacelast)
          allocate(tc1%tmag%kedge(4,nedge))
          tc1%tmag%kedge(:,1:nedge)=kedge(:,1:nedge)

          k=1
          do i=1,nface
            npoi=kface(k)
            if (npoi.gt.npoimax) npoimax=npoi
            tc1%tmag%lface(i)=k
            k=k+npoi+1
          enddo

          call util_volume(nhull,
     &      tc1%tmag%xhull,tc1%tmag%yhull,tc1%tmag%zhull,
     &      hulltiny,tc1%tmag%volume,kfail)

          t_concaves(nconv)=tc1

          if(nface.gt.nplanmax) nplanmax=nface

          cycle
        endif

        nheap=1
        heap(1)=tc1

        do while (nheap.gt.0)

          tc1=heap(nheap)

          if (idebug.ne.0) call util_break

          call clc_cut_concave(tc1,tc2,kcut)

          call util_string_trim(tc1%tmag%cnam,k,l)
          tc2%tmag%cnam(1:l)=tc1%tmag%cnam(1:l) // '&'
          tc2%tmag%cnam(l+1:l+1)='&'

          nverts=tc1%nverts

          if (nverts*3.gt.nallo) then
            nallo=nverts*6
            deallocate(cbuff,ibuff,kface,kedge,khull)
            allocate(cbuff(nallo),ibuff(nallo))
            allocate(khull(nallo),kface(nallo),kedge(4,nallo))
            lenhull=nallo
            lenedge=nallo
            lenface=nallo
            nverhullmax=nallo
          endif

          call util_hull_check_convex(
     &      nverts,tc1%verts(:,1:nverts),khull,kedge,kface,
     &      nhull,nedge,nface,kfacelast,nbuff,ibuff,hulltiny,kfail)

          if (nface.gt.nplanmax) nplanmax=nface

          if (maxval(ibuff(1:nbuff)).le.0) then
            tc1%IsConvex=1
          endif

          if (tc1%IsConvex.ne.0) then

            nheap=nheap-1
            nconv=nconv+1

            if(nconv.gt.nalloconc) then
              nalloconc=nalloconc*2
              if (kalloconc.ne.0) deallocate(tconc)
              allocate(tconc(nalloconc))
              kalloconc=1
              tconc(1:nconv-1)=t_concaves(1:nconv-1)
              deallocate(t_concaves)
              allocate(t_concaves(nalloconc))
              t_concaves(1:nconv-1)=tconc(1:nconv-1)
            endif

            do i=1,nhull
              l=khull(i)
              xp(i)=tc1%verts(1,l)
              yp(i)=tc1%verts(2,l)
              zp(i)=tc1%verts(3,l)
            enddo

            mhull=nhull
            call util_convex_hull_3d(mhull,
     &        xp,yp,zp,
     &        khull,kedge,kface,
     &        nhull,nedge,nface,kfacelast,hulltiny,kfail)

            allocate(tc1%tmag%xhull0(nhull),tc1%tmag%yhull0(nhull),
     &        tc1%tmag%zhull0(nhull))
            allocate(tc1%tmag%xhull(nhull),tc1%tmag%yhull(nhull),
     &        tc1%tmag%zhull(nhull))

            tc1%tmag%gcen=0.0d0
            do i=1,nhull
              l=khull(i)
              tc1%tmag%xhull0(i)=xp(l)+tc1%tmag%xyz(1)
              tc1%tmag%yhull0(i)=yp(l)+tc1%tmag%xyz(2)
              tc1%tmag%zhull0(i)=zp(l)+tc1%tmag%xyz(3)
              tc1%tmag%gcen=tc1%tmag%gcen+[xp(l),yp(l),zp(l)]
            enddo

            tc1%tmag%gcen=tc1%tmag%gcen/dble(nhull)+tc1%tmag%xyz
            tc1%tmag%xhull=tc1%tmag%xhull0
            tc1%tmag%yhull=tc1%tmag%yhull0
            tc1%tmag%zhull=tc1%tmag%zhull0

            mhull=nhull
            call util_convex_hull_3d(mhull,
     &        tc1%tmag%xhull,tc1%tmag%yhull,tc1%tmag%zhull,
     &        khull,kedge,kface,
     &        nhull,nedge,nface,kfacelast,hulltiny,kfail)

            tc1%tmag%nhull=nhull
            tc1%tmag%nface=nface
            tc1%tmag%nedge=nedge
            tc1%tmag%kfacelast=kfacelast

            allocate(tc1%tmag%khull(nhull))
            tc1%tmag%khull(1:nhull)=khull(1:nhull)
            allocate(tc1%tmag%lface(nface))
            allocate(tc1%tmag%kface(kfacelast))
            tc1%tmag%kface(1:kfacelast)=kface(1:kfacelast)
            allocate(tc1%tmag%kedge(4,nedge))
            tc1%tmag%kedge(:,1:nedge)=kedge(:,1:nedge)

            k=1
            do i=1,nface
              npoi=kface(k)
              if (npoi.gt.npoimax) npoimax=npoi
              tc1%tmag%lface(i)=k
              k=k+npoi+1
            enddo

            call util_volume(nhull,
     &        tc1%tmag%xhull,tc1%tmag%yhull,tc1%tmag%zhull,
     &        hulltiny,tc1%tmag%volume,kfail)
            k=1

            t_concaves(nconv)=tc1

          endif

          nverts=tc2%nverts

          if (nverts*3.gt.nallo) then
            nallo=nverts*6
            deallocate(cbuff,ibuff,kface,kedge,khull)
            allocate(cbuff(nallo),ibuff(nallo))
            allocate(khull(nallo),kface(nallo),kedge(4,nallo))
            lenhull=nallo
            lenedge=nallo
            lenface=nallo
            nverhullmax=nallo
          endif

          call util_hull_check_convex(
     &      nverts,tc2%verts(:,1:nverts),khull,kedge,kface,
     &      nhull,nedge,nface,kfacelast,nbuff,ibuff,hulltiny,kfail)

          if (nface.gt.nplanmax) nplanmax=nface

          if (maxval(ibuff(1:nbuff)).le.0) then
            tc2%IsConvex=1
          endif

          if(tc2%isconvex.ne.0) then

            do i=1,nhull
              l=khull(i)
              xp(i)=tc2%verts(1,l)
              yp(i)=tc2%verts(2,l)
              zp(i)=tc2%verts(3,l)
            enddo

            mhull=nhull
            call util_convex_hull_3d(mhull,
     &        xp,yp,zp,
     &        khull,kedge,kface,
     &        nhull,nedge,nface,kfacelast,hulltiny,kfail)

            nconv=nconv+1

            if(nconv.gt.nalloconc) then
              nalloconc=nalloconc*2
              if (kalloconc.ne.0) deallocate(tconc)
              allocate(tconc(nalloconc))
              kalloconc=1
              tconc(1:nconv-1)=t_concaves(1:nconv-1)
              deallocate(t_concaves)
              allocate(t_concaves(nalloconc))
              t_concaves(1:nconv-1)=tconc(1:nconv-1)
            endif

            allocate(tc2%tmag%xhull0(nhull),tc2%tmag%yhull0(nhull),
     &        tc2%tmag%zhull0(nhull))
            allocate(tc2%tmag%xhull(nhull),tc2%tmag%yhull(nhull),
     &        tc2%tmag%zhull(nhull))

            tc2%tmag%gcen=0.0d0
            do i=1,nhull
              l=khull(i)
              tc2%tmag%xhull0(i)=xp(l)+tc2%tmag%xyz(1)
              tc2%tmag%yhull0(i)=yp(l)+tc2%tmag%xyz(2)
              tc2%tmag%zhull0(i)=zp(l)+tc2%tmag%xyz(3)
              tc2%tmag%gcen=tc2%tmag%gcen+[xp(l),yp(l),zp(l)]
            enddo

            tc2%tmag%gcen=tc2%tmag%gcen/dble(nhull)+tc2%tmag%xyz
            tc2%tmag%xhull=tc2%tmag%xhull0
            tc2%tmag%yhull=tc2%tmag%yhull0
            tc2%tmag%zhull=tc2%tmag%zhull0

            mhull=nhull
            call util_convex_hull_3d(mhull,
     &        tc2%tmag%xhull,tc2%tmag%yhull,tc2%tmag%zhull,
     &        khull,kedge,kface,
     &        nhull,nedge,nface,kfacelast,hulltiny,kfail)

            tc2%tmag%nface=nface
            tc2%tmag%nhull=nhull
            tc2%tmag%nedge=nedge
            tc2%tmag%kfacelast=kfacelast

            allocate(tc2%tmag%khull(nhull),tc2%tmag%lface(nface))
            tc2%tmag%khull(1:nhull)=khull(1:nhull)
            allocate(tc2%tmag%kface(kfacelast))
            tc2%tmag%kface(1:kfacelast)=kface(1:kfacelast)
            allocate(tc2%tmag%kedge(4,nedge))
            tc2%tmag%kedge(:,1:nedge)=kedge(:,1:nedge)

            k=1
            do i=1,nface
              npoi=kface(k)
              if (npoi.gt.npoimax) npoimax=npoi
              tc2%tmag%lface(i)=k
              k=k+npoi+1
            enddo

            call util_volume(nhull,
     &        tc2%tmag%xhull,tc2%tmag%yhull,tc2%tmag%zhull,
     &        hulltiny,tc2%tmag%volume,kfail)

            t_concaves(nconv)=tc2

          else

            nheap=nheap+1

            if(nheap.gt.nalloconc) then
              nalloconc=nalloconc*2
              if (kalloconc.ne.0) deallocate(tconc)
              allocate(tconc(nalloconc))
              kalloconc=1
              if(nconv.gt.0) tconc(1:nconv-1)=t_concaves(1:nconv-1)
              deallocate(t_concaves)
              allocate(t_concaves(nalloconc))
              if(nconv.gt.0) t_concaves(1:nconv-1)=tconc(1:nconv-1)
            endif

            heap(nheap)=tc2

          endif

        enddo

      enddo !nclccave_t

      deallocate(cbuff,ibuff,xp,yp,zp,khull,kface,kedge)

      if (kalloconc.gt.0) deallocate(tconc)

      nconcave_t=nconv
      ncornmax=npoimax

      if(ntransrotcop.gt.0) call clc_trcconcave

      deallocate(magmodulecav)

      return
      end
