*CMZ :  2.05/02 03/11/2023  15.59.50  by  Michael Scheer
*CMZ :  2.04/05 22/08/2023  09.03.52  by  Michael Scheer
*CMZ :  2.02/01 26/01/2022  11.19.15  by  Michael Scheer
*-- Author :    Michael Scheer   20/04/2021
      subroutine clcmag_copy(kcopy)

      use commandlinef90m
      use bpolyederf90m
      use undumagf90m
      use magnets_structure
      use displacement

      implicit none

      integer i,k,ifound,kcopy,nold
      integer ipos(2,100),nwords,istat

      character(32) cnam,cnamnew,cmoth

      Type(T_Magnet), dimension(:), allocatable ::  tmc

      i=kcopy

      call util_string_split(clccop(i),100,nwords,ipos,istat)

      cnam=clccop(i)(ipos(1,1):ipos(2,1))
      cnamnew=clccop(i)(ipos(1,2):ipos(2,2))
      cmoth=clccop(i)(ipos(1,3):ipos(2,3))

      do k=1,nmagtot_t
        if (t_magnets(k)%cmoth.eq.cnam) then
          print*,"*** Error in clcmag_copy: Can copy magnets only, not mother volumes, check " // cnam // " ***"
          return
        endif
      enddo

      do k=1,nmagtot_t
        ifound=0
        if (t_magnets(k)%cnam.eq.cnam) then
          ifound=k

          nmagtot_t=nmagtot_t+1

          if (nmagtot_t.gt.nallotmag_t) then
            allocate(tmc(nallotmag_t))
            tmc=t_magnets
            nold=size(t_magnets)
            nallotmag_t=nold+ntransrotcop
            deallocate(t_magnets)
            allocate(t_magnets(nallotmag_t))
            t_magnets(1:nold)=tmc(1:nold)
          endif

          if (t_magnets(k)%IsSpecial.ne.0) then
            nspecmag_t=nspecmag_t+1
            t_magnets(nmagtot_t)=t_magnets(k)
            t_magnets(nmagtot_t)%cnam=cnamnew
            t_magnets(nmagtot_t)%cmoth=cmoth
            t_magnets(nmagtot_t)%IwasConcave=-t_magnets(nmagtot_t)%IwasConcave
          else if (nspecmag_t.eq.0) then
            nmag_t=nmag_t+1
            t_magnets(nmag_t)=t_magnets(k)
            t_magnets(nmag_t)%cnam=cnamnew
            t_magnets(nmag_t)%cmoth=cmoth
            t_magnets(nmag_t)%IwasConcave=-t_magnets(nmag_t)%IwasConcave
          else
            nmag_t=nmag_t+1
            t_magnets(nmagtot_t)=t_magnets(nmag_t)
            t_magnets(nmag_t)=t_magnets(k)
            t_magnets(nmag_t)%cnam=cnamnew
            t_magnets(nmag_t)%cmoth=cmoth
            t_magnets(nmag_t)%IwasConcave=-t_magnets(nmag_t)%IwasConcave
          endif
          exit
        endif
      enddo

      if (ifound.eq.0) then
        print*,"*** Error in clcmag_copy: Cannot copy " // cnam // ", not found ***"
        return
      endif

      return
      end
