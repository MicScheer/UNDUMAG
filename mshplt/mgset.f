*CMZ :  1.03/02 08/12/2015  13.47.56  by  Michael Scheer
*CMZ :  1.02/00 03/10/2014  10.05.12  by  Michael Scheer
*CMZ :  0.00/06 19/08/2014  14.25.00  by  Michael Scheer
*CMZ :  1.17/00 26/05/2014  11.00.23  by  Michael Scheer
*CMZ :  1.16/04 16/04/2014  15.13.52  by  Michael Scheer
*-- Author :    Michael Scheer   16/04/2014
      subroutine mgset(pname,rval)

      implicit none

      character(4) pname
      real rval



      if (pname.eq.'MSCF') then
        call mshplt_igset(pname,rval/10.)
      else
        call mshplt_igset(pname,rval)
      endif
      return
      end
