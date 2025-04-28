*CMZ :  1.04/00 12/02/2025  14.59.12  by  Michael Scheer
*CMZ :  1.03/03 06/08/2018  08.40.38  by  Michael Scheer
*-- Author :    Michael Scheer   06/08/2018
      subroutine mshplt_graph_3d(n,x,y,z,xtit,ytit,ztit,chopt)

      implicit none

      integer, parameter :: nsplinep=1001
      integer n,ispline,istatus,ianf,iend,imark,ic,ir,ig,ib
      real x(n),y(n),z(n),xspline(nsplinep),yspline(nsplinep),zspline(nsplinep)

      character(*) xtit,ytit,ztit,chopt
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

      call util_string_substring(copt,'mark',ianf,iend,istatus)
      if (istatus.eq.0) then
        imark=1
      else
        imark=0
      endif

      if (ispline.ne.0) then
        call util_spline_real4(n,x,y,nsplinep,xspline,yspline)
        call util_spline_real4(n,x,z,nsplinep,xspline,zspline)
        call mshplt_frame3d_auto(nsplinep,xspline,yspline,zspline,xtit,ytit,ztit,'',istatus)
        call mshplt_pline_3d(nsplinep,xspline,yspline,zspline)
      else
        call mshplt_frame3d_auto(n,x,y,z,xtit,ytit,ztit,'',istatus)
        call mshplt_pline_3d(n,x,y,z)
      endif

      if (imark.ne.0) then
        call mshplt_marker_3d(n,x,y,z)
      endif

      return
      end
