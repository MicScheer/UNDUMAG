*CMZ :  1.10/00 28/10/2016  15.06.31  by  Michael Scheer
*-- Author :    Michael Scheer   13/10/2016
      subroutine util_oper(x,cop,y,res)

      implicit none

      double precision x,y,res
      character(*) cop


      if (cop(1:2).eq.'**') then
        call util_pow(x,y,res)
      else if (cop(1:1).eq.'+') then
        call util_plus(x,y,res)
      else if (cop(1:1).eq.'-') then
        call util_minus(x,y,res)
      else if (cop(1:1).eq.'*') then
        call util_mul(x,y,res)
      else if (cop(1:1).eq.'/') then
        call util_div(x,y,res)
      else
        print*,"*** Error in util_oper: Undefined operation ",trim(cop)
      endif

c      print*,x,cop,y,res

      return
      end
