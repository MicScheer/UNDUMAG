*CMZ :          16/03/2025  11.32.05  by  Michael Scheer
*-- Author :    Michael Scheer   15/03/2025
      subroutine undumag_system

      use iso_c_binding
      use iso_fortran_env !, only: int64

      implicit none

*KEEP,usystem.
      include 'usystem.cmn'
*KEND.

      integer isystem,istatus,luni,ipos(2,100),nwords
      character(5) :: chspacer='     '

      ihavepython=0
      chpythonpath=''
      iwinlin=0

      call getlog(chuser)
      call get_environment_variable("HOME", chuserhome)
      call getcwd(chworkingdir)
      call hostnm(chhost)
      chcompiler=compiler_version()

      istatus=isystem('where python > where_python 2>&1')

      if (istatus.eq.0) then
        open(newunit=luni,file='where_python')
        read(luni,'(a)') chpythonpath
        close(luni)
        istatus=isystem('del where_python')
        ihavepython=1
        iwinlin=2
        call get_environment_variable("OS", chplatform)
      else

        istatus=isystem('which python > where_python 2>&1')

        if (istatus.eq.0) then
          open(newunit=luni,file='where_python')
          read(luni,'(a)') chpythonpath
          close(luni)
          istatus=isystem('rm where_python')
          ihavepython=1
          iwinlin=1
          istatus=isystem('uname -a > .platform 2>&1')
          open(newunit=luni,file='.platform')
          read(luni,'(a)') chplatform
          close(luni)
        endif

      endif

      print*,''
      print*,chspacer,'Platform:'
      call util_string_split(chplatform,100,nwords,ipos,istatus)
      print*,chspacer,'  ',chplatform(1:ipos(2,6))
      print*,chspacer,'  ',chplatform(ipos(1,7):len_trim(chplatform))
      print*,''
      print*,chspacer,'Compiler:'
      print*,chspacer,'  ',trim(chcompiler)
      print*,''
      print*,chspacer,'Run-time environmet:'
      print*,''
      print*,chspacer,'  Hostname: ',trim(chhost)
      print*,chspacer,'  User: ',trim(chuser)
      print*,chspacer,'  User home: ',trim(chuserhome)
      print*,chspacer,'  Working directory: ',trim(chworkingdir)

      open(newunit=luni,file='undumag.env')

      write(luni,'(a)')trim(chplatform)
      write(luni,'(a)')trim(chcompiler)
      write(luni,'(a)')trim(chhost)
      write(luni,'(a)')trim(chuser)
      write(luni,'(a)')trim(chuserhome)
      write(luni,'(a)')trim(chworkingdir)

      close(luni)

      end
