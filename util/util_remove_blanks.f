*CMZ :  2.02/01 16/01/2022  12.17.07  by  Michael Scheer
*CMZ : 00.00/20 01/11/2016  17.21.24  by  Michael Scheer
*CMZ : 00.00/15 26/11/2013  13.28.58  by  Michael Scheer
*CMZ : 00.00/07 19/03/2010  16.41.49  by  Michael Scheer
*CMZ : 00.00/05 07/03/2007  10.40.20  by  Michael Scheer
*-- Author :    Michael Scheer   07/03/2007
      subroutine util_remove_blanks(chin,chout,lenout)

      implicit none

        integer lenin,lenout,k,i

        character(*) chin,chout
        character c1

        lenin=len_trim(chin)
        chout=''

        k=0

        do i=1,lenin
          c1=chin(i:i)
          if (c1.ne.' ') then
            k=k+1
            chout(k:k)=c1
          endif
        enddo

        lenout=k

      return
      end
