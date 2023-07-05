*CMZ :  0.01/02 22/09/2014  14.18.27  by  Michael Scheer
*CMZ :  0.01/00 22/08/2014  18.11.33  by  Michael Scheer
*CMZ :  0.00/02 09/07/2014  13.34.47  by  Michael Scheer
*-- Author :    Michael Scheer   07/07/2014
      subroutine mshplt_fill_buff(cline)

      implicit none

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.

      character(*) cline

      if(ibuffpos_ps.lt.nbuffsize_ps) then
        ibuffpos_ps=ibuffpos_ps+1
        chbuff_ps(ibuffpos_ps)=cline(1:len_trim(cline))
      else
        call mshplt_flush_buff
        if (len_trim(cline).gt.0) then
          ibuffpos_ps=ibuffpos_ps+1
          chbuff_ps(ibuffpos_ps)=cline(1:len_trim(cline))
        endif
      endif

c      kzone_ps=abs(kzone_ps) !to indicate that pad is touched

      return
      end
