*CMZ :  2.02/02 16/02/2022  14.22.41  by  Michael Scheer
*CMZ :  2.02/01 06/02/2022  11.21.39  by  Michael Scheer
*-- Author :    Michael Scheer   27/04/2021
      subroutine clcstring_to_vars(cin)

      use commandlinef90m
      use magnets_structure

      implicit none

      double precision val,undumag_variable_getval

      integer i,ipos(2,100),nwords,istat

      character(*) cin
      character(2048) cout
      character(128) cword
      character(16) cvar

      call util_string_split(cin,100,nwords,ipos,istat)

      cout=""
      do i=1,nwords
        cword=cin(ipos(1,i):ipos(2,i))
        if (cword(1:1).eq.'$') then
          val=undumag_variable_getval(cword)
          if (val.eq.-9999.d29) then
            stop "*** Program UNDUMAG stopped in clcstring_to_vars ***"
          endif
          write(cvar,'(g16.9)') val
        else
          cvar=adjustl(cword(1:16))
        endif
        cout=trim(cout) // " " // adjustl(trim(cvar))
      enddo

      cin=cout

      return
      end
