*CMZ :  2.05/01 11/10/2023  17.25.06  by  Michael Scheer
*-- Author :    Michael Scheer   02/10/2023
      subroutine clc_concave_to_convex

      use bpolyederf90m
      use undumagf90m
      use magnets_structure
      use utilmod

      implicit none

      character(512), dimension(:), allocatable :: cbuff
      character(512) cline,cfile
      character(128) ckey

      double precision, dimension (:), allocatable :: xp,yp,zp,
     &  xcuts,ycuts,zcuts

      double precision x,y,z

      integer, dimension (:,:), allocatable :: kedge
      integer, dimension (:), allocatable :: khull,kface,
     &  lconcave,ibuff

      integer i,iconcave,nconcave,l,ipoi,npoi,luni,nplan,iplan,nline,
     &  nverts,nbuff,nface,nedge,nhull,kfacelast,kfail,kcut

*KEEP,HULLDIM.
      integer lenhull,lenedge,lenface,nverhullmax
      common/uhullc/lenhull,lenedge,lenface,nverhullmax
*KEND.

      integer :: ipos(2,1000),nwords,istat,nallo=1000,nfacemax,npoimax

      Type(T_Concave) tc1,tc2

      allocate(lconcave(nconcave_t/5))
      allocate(xp(nallo),yp(nallo),zp(nallo),cbuff(nallo),ibuff(nallo),
     &  xcuts(nallo),ycuts(nallo),zcuts(nallo))
      allocate(khull(nallo),kface(nallo),kedge(4,nallo))
      lenhull=nallo
      lenedge=nallo
      lenface=nallo
      nverhullmax=nallo

      nconcave=0
      do i=1,nconcave_t
        cline=clcconcave(i)
        call util_string_split(cline,1000,nwords,ipos,istat)
        if (cline(1:1).eq.'&') then
          nconcave=nconcave+1
          lconcave(nconcave)=i
        endif
      enddo

      nconcave_t=nconcave

      allocate(t_concaves(nconcave_t))

      do iconcave=1,nconcave_t

        i=lconcave(iconcave)

        cline=clcconcave(i)
        call util_string_split(cline,1000,nwords,ipos,istat)

        if (cline(1:1).eq.'&') then

          tc1%ispole=0
          tc1%isspecial=1
          ckey=cline(ipos(1,2):ipos(2,2))
          call util_lower_case(ckey)

          if (ckey.eq.'special_convex_magnet'.or.
     &      ckey.eq.'special_convex_pole') tc1%isspecial=1

          if (ckey.eq.'convex_pole'.or.
     &      ckey.eq.'special_convex_pole') tc1%ispole=1

          cline=clcconcave(i+1)
          call util_string_split(cline,1000,nwords,ipos,istat)
          ckey=cline(ipos(1,1):ipos(2,1))
          call util_lower_case(ckey)

          if (ckey.ne.'file') then
            print*,"*** Error in CLC_CONCAVE_TO_CONVEX: Concave items must be read from file ***"
            stop "UNDUMAG aborted"
          endif

          nfacemax=0
          nplanmax=0

          cline=clcconcave(i+5)
          call util_string_split(cline,1000,nwords,ipos,istat)
          cfile=cline(ipos(1,1):ipos(2,1))
          nline=0
          open(newunit=luni,file=cfile,status='old')

          do while (.true.)
            read(luni,'(a)',iostat=istat) cline
            if (istat.eq.0) then
              nline=nline+1
              if (nline.gt.nallo) then
                deallocate(xp,yp,zp,cbuff,ibuff,kface,kedge,khull)
                nallo=nline*2
                allocate(xp(nallo),yp(nallo),zp(nallo),cbuff(nallo),
     &            ibuff(nallo))
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
          tc1%nface=nplan

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
            read(cbuff(l),*) tc1%ifaces(1:npoi,iplan)
            read(cbuff(l),*) ibuff(nbuff+1:nbuff+npoi)
            nbuff=nbuff+npoi
          enddo

          xp(1:nverts)=tc1%verts(1,1:nverts)
          yp(1:nverts)=tc1%verts(2,1:nverts)
          zp(1:nverts)=tc1%verts(3,1:nverts)

          if (nbuff*2*npoimax.gt.nallo) then
            nallo=nbuff*2*npoimax*2
            deallocate(xp,yp,zp,cbuff,ibuff,kface,kedge,khull)
            allocate(xp(nallo),yp(nallo),zp(nallo),cbuff(nallo),ibuff(nallo))
            allocate(khull(nallo),kface(nallo),kedge(4,nallo))
            lenhull=nallo
            lenedge=nallo
            lenface=nallo
            nverhullmax=nallo
          endif

          call util_hull_check_convex(
     &      nverts,tc1%verts(:,1:nverts),khull,kedge,kface,
     &      nhull,nedge,nface,kfacelast,nbuff,ibuff,hulltiny,kfail)

          allocate(tc1%kface(kfacelast),tc1%kedge(4,nedge))

          tc1%IsConvex=1
          tc1%nconcave=0

          if (nhull.ne.nverts) then
            tc1%nconcave=nbuff
            allocate(tc1%Kconcave(nbuff))
            do i=1,nbuff
              if (ibuff(i).gt.0) then
                tc1%IsConvex=0
                exit
              endif
            enddo
            tc1%kconcave(1:nbuff)=ibuff(1:nbuff)
          endif

          if (tc1%IsConvex.ne.0) then
            t_concaves(iconcave)=tc1
            cycle
          endif

          call clc_cut_concave(tc1,tc2,kcut)

          t_concaves(iconcave)=tc1
        endif !&

      enddo !iconcave

      deallocate(lconcave,xp,yp,zp,cbuff,ibuff,xcuts,ycuts,zcuts,
     &  khull,kface,kedge)

      return
      end
