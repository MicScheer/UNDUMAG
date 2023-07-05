*CMZ :  2.04/00 13/01/2023  20.03.17  by  Michael Scheer
*CMZ :  2.02/01 06/02/2022  11.21.39  by  Michael Scheer
*-- Author :    Michael Scheer   27/04/2021
      function undumag_variable_getval(cin)

      use commandlinef90m
      use magnets_structure

      implicit none

      double precision undumag_variable_getval

      integer i,istat,ireturn

      character(*) cin
      character(128) cname,tvar

      read(cin,*,iostat=istat) undumag_variable_getval
      if (istat.eq.0) return

      undumag_variable_getval=-9999.0d29

      if (cin(1:1).eq.'$') then
        cname=cin(2:)
      else
        cname=cin
      endif

      call util_lower_case(cname)

      ireturn=0
      do i=1,nvar_t
        tvar=t_variables(i)%cname
        call util_lower_case(tvar)
        if (tvar == cname) then
          undumag_variable_getval=t_variables(i)%val
c          return
          ireturn=1
        endif
      enddo

      if (ireturn.eq.1) return

      if (nowarnugv.eq.0)
     &  write(lun6,*)"*** Error in undumag_variable_getval: Variable "//trim(cname)//" not found ***"

      return
      end
