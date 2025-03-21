*CMZ :          19/03/2025  11.41.06  by  Michael Scheer
*CMZ :  1.04/00 12/02/2025  08.54.28  by  Michael Scheer
*CMZ :  1.03/03 06/08/2018  08.40.38  by  Michael Scheer
*-- Author :    Michael Scheer   06/08/2018
      subroutine mshplt_graph(n,x,y,xtit,ytit,chopt)

      implicit none

      integer, parameter :: nsplinep=1001
      integer n,ispline,istatus,ianf,iend,imark,iline
      real x(n),y(n),xspline(nsplinep),yspline(nsplinep)

      character(*) xtit,ytit,chopt
      character(128) copt

      copt=adjustl(trim(chopt))

      if (len_trim(copt).eq.0) then
        if (n.le.20) then
          copt='splinemarker'
        else
          copt='spline'
        endif
      endif

      call util_lower_case(copt)

      call util_string_substring(copt,'spline',ianf,iend,istatus)
      if (istatus.eq.0) then
        ispline=1
        copt(ianf:iend)=''
      else
        ispline=0
      endif

      call util_string_substring(copt,'line',ianf,iend,istatus)
      if (istatus.eq.0) then
        iline=1
        copt(ianf:iend)=''
      else
        iline=0
      endif

      call util_string_substring(copt,'mark',ianf,iend,istatus)
      if (istatus.eq.0) then
        imark=1
      else
        imark=0
      endif

      if (ispline.ne.0) then
        call util_spline_real4(n,x,y,nsplinep,xspline,yspline)
        call mshplt_frame_auto(nsplinep,xspline,yspline,xtit,ytit,'')
        call mshplt_pline(nsplinep,xspline,yspline)
      endif

      if (iline.ne.0) then
        call mshplt_frame_auto(n,x,y,xtit,ytit,'')
        call mshplt_pline(n,x,y)
      endif

      if (imark.ne.0) then
        call mshplt_marker(n,x,y)
      endif

      return
      end
