*CMZ :  0.01/02 11/09/2014  17.22.55  by  Michael Scheer
*CMZ :  0.00/06 22/08/2014  15.31.43  by  Michael Scheer
*CMZ :  0.00/04 07/08/2014  08.43.21  by  Michael Scheer
*CMZ :  0.00/03 05/08/2014  15.53.35  by  Michael Scheer
*CMZ :  0.00/02 08/07/2014  11.10.50  by  Michael Scheer
*CMZ :  0.00/01 07/07/2014  12.00.23  by  Michael Scheer
*CMZ : 00.00/02 04/07/2014  21.11.57  by  Michael Scheer
*-- Author :    Michael Scheer   11/06/2014
      subroutine mshplt_world_log_axis(
     &  wxmin,wxmax,wymin,wymax,smin,smax,title,
     &  iticside,ilabside,titang,offtit,offlab
     &  )

c Draw an axis from (wxmin,wxmax) to (wymin,wymax) on pad
c with labeling from smin to smax, and title title

      implicit none

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.

      real wxmin,wxmax,wymin,wymax,smin,smax,xlength,ang,chhe,tiche,
     &  titang,offtit,offlab
      integer iticside,ilabside

      character(*) title

      write(lun_ps,*)'% begin of mshplt_world_log_axis'

      ilabmod_ps=ilabside

      xlength=sqrt((wxmax-wxmin)**2+(wymax-wymin)**2)

      if (xlength.eq.0.0.or.smin.eq.smax) then
        print*,'*** Error in mshplt_world_log_axis: Zero-length axis!'
        return
      endif

      ang=atan2(wymax-wymin,wxmax-wxmin)*57.29578

      if (ihigzmode_ps.eq.0) then
        chhe=chhe_ps*scaletxt_ps
        tiche=ticsiz_ps
      else
        chhe=chhe_ps*scaletxt_ps*1.5
        tiche=ticsiz_ps
      endif

      call mshplt_log_axis(wxmin,wymin,xlength,smin,smax,ang,chhe,
     &  tiche,title,iticside,ilabside,titang,offtit,offlab)

      write(lun_ps,*)'% end of mshplt_world_log_axis'

      return
      end
