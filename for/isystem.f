*CMZ :          16/03/2025  16.49.34  by  Michael Scheer
*-- Author :    Michael Scheer   11/03/2025
      integer function isystem(com)

      implicit none

      integer kstat
      character(*) com
      character(2024) cmdmsg

      call execute_command_line(com,.true.,kstat,isystem,cmdmsg)

      return
      end
