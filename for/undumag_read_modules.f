*CMZ :  2.02/01 28/12/2021  09.10.09  by  Michael Scheer
*-- Author :    Michael Scheer   15/10/2021
      subroutine undumag_read_modules

      use bpolyederf90m
      use undumagf90m
      use commandlinef90m
      use magnets_structure

      implicit none

      double precision undumag_variable_getval,space

      integer ib,ipos(2,10),istat,nwords,i,j,imodul

      character(512) cword

*KEEP,grarad.
      include 'grarad.cmn'
*KEND.


      allocate(t_modules(nmodule_t))

      ib=0
      nmodule_t=0
      do while (ib.lt.nclcmod)
        ib=ib+1
        if (clcmod(ib)(1:8).eq.'& Module') cycle
        nmodule_t=nmodule_t+1
        call util_string_split(clcmod(ib),10,nwords,ipos,istat)
        do i=1,3
          cword=clcmod(ib)(ipos(1,i):ipos(2,i))
          if (cword(1:1).eq.'$') then
            t_modules(nmodule_t)%offset(i)=undumag_variable_getval(cword)
          else
            read(cword,*)t_modules(nmodule_t)%offset(i)
          endif
        enddo
        ib=ib+1
        call util_string_split(clcmod(ib),10,nwords,ipos,istat)
        cword=clcmod(ib)(ipos(1,1):ipos(2,1))
        if (cword(1:1).eq.'$') then
          t_modules(nmodule_t)%ncopy=nint(undumag_variable_getval(cword))
        else
          read(cword,*)t_modules(nmodule_t)%ncopy
        endif
        ib=ib+1
        call util_string_split(clcmod(ib),10,nwords,ipos,istat)
        cword=clcmod(ib)(ipos(1,1):ipos(2,1))
        if (cword(1:1).eq.'$') then
          space=undumag_variable_getval(cword)
        else
          read(cword,*)space
        endif
        do i=2,4
          cword=clcmod(ib)(ipos(1,i):ipos(2,i))
          if (cword(1:1).eq.'$') then
            t_modules(nmodule_t)%vspace(i-1)=undumag_variable_getval(cword)
          else
            read(cword,*)t_modules(nmodule_t)%vspace(i-1)
          endif
        enddo
        t_modules(nmodule_t)%vspace=space*t_modules(nmodule_t)%vspace/
     &    norm2(t_modules(nmodule_t)%vspace)
        cword=clcmod(ib)(ipos(1,5):ipos(2,5))
        if (cword(1:1).eq.'$') then
          t_modules(nmodule_t)%phi=undumag_variable_getval(cword)
        else
          read(cword,*)t_modules(nmodule_t)%phi
        endif
        ib=ib+1
        call util_string_split(clcmod(ib),10,nwords,ipos,istat)
        do i=1,3
          cword=clcmod(ib)(ipos(1,i):ipos(2,i))
          if (cword(1:1).eq.'$') then
            t_modules(nmodule_t)%scalmag(i)=undumag_variable_getval(cword)
          else
            read(cword,*)t_modules(nmodule_t)%scalmag(i)
          endif
        enddo
        ib=ib+1
      enddo

      do i=1,nmodule_t
        call util_rotmat(t_modules(i)%vspace,t_modules(i)%phi*grarad1,
     &    t_modules(i)%rotmat,istat)
      enddo

      return
      end
