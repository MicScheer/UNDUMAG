*CMZ :          26/05/2017  10.10.47  by  Michael Scheer
*CMZ :  1.03/02 25/04/2016  09.27.22  by  Michael Scheer
*CMZ :  0.01/02 05/09/2014  15.41.43  by  Michael Scheer
*CMZ :  0.00/06 19/08/2014  14.33.17  by  Michael Scheer
*CMZ :  0.00/04 11/08/2014  17.53.18  by  Michael Scheer
*CMZ :  0.00/02 07/07/2014  12.22.09  by  Michael Scheer
*-- Author :    Michael Scheer   07/07/2014
      subroutine mshplt_igset(pname,val)

      implicit none

*KEEP,mshpltincl.
      include 'mshplt.cmn'
*KEND.

      character(*) pname
      real val

      if (pname.eq.'PLCI'.or.pname.eq.'plci') then
        call mshplt_set_line_color(int(val),0,0,0)
      else if (pname.eq.'TXCI'.or.pname.eq.'txci') then
        call mshplt_set_text_color(int(val),0,0,0)
      else if (pname.eq.'PMCI'.or.pname.eq.'pmci') then
        call mshplt_set_marker_color(int(val),0,0,0)
      else if (pname.eq.'MTYP'.or.pname.eq.'mtyp') then
        call mshplt_set_marker_type(int(val))
      else if (pname.eq.'MSCF'.or.pname.eq.'mscf') then
        call mshplt_set_marker_size(val)
      else if (pname.eq.'LWID'.or.pname.eq.'lwid') then
        call mshplt_set_line_width(val)
      else if (pname.eq.'CHHE'.or.pname.eq.'chhe') then
        call mshplt_set_character_height(val)
      else
        print*,'*** WARNING in mshplt_igset: Parameter ',
     &    pname(1:len_trim(pname)),' not available! ***'
      endif

      return
      end
