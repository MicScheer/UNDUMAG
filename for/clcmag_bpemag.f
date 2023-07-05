*CMZ :  2.02/01 01/11/2021  06.19.19  by  Michael Scheer
*-- Author :    Michael Scheer   01/04/2016
      subroutine clcmag_bpemag(dx,dy,dz,bpe346,xyz)

      implicit none

      double precision dx,dy,dz,bpe346(3,5,6),xyz(1:3)
      integer iplan,icorn

      iplan=1
      icorn=1
      bpe346(1,icorn,iplan)=-dx/2.0d0
      bpe346(2,icorn,iplan)=-dy/2.0d0
      bpe346(3,icorn,iplan)=-dz/2.0d0
      icorn=2
      bpe346(1,icorn,iplan)=-dx/2.0d0
      bpe346(2,icorn,iplan)=-dy/2.0d0
      bpe346(3,icorn,iplan)=+dz/2.0d0
      icorn=3
      bpe346(1,icorn,iplan)=-dx/2.0d0
      bpe346(2,icorn,iplan)=+dy/2.0d0
      bpe346(3,icorn,iplan)=+dz/2.0d0
      icorn=4
      bpe346(1,icorn,iplan)=-dx/2.0d0
      bpe346(2,icorn,iplan)=+dy/2.0d0
      bpe346(3,icorn,iplan)=-dz/2.0d0

      iplan=2
      icorn=4
      bpe346(1,icorn,iplan)=+dx/2.0d0
      bpe346(2,icorn,iplan)=-dy/2.0d0
      bpe346(3,icorn,iplan)=-dz/2.0d0
      icorn=3
      bpe346(1,icorn,iplan)=+dx/2.0d0
      bpe346(2,icorn,iplan)=-dy/2.0d0
      bpe346(3,icorn,iplan)=+dz/2.0d0
      icorn=2
      bpe346(1,icorn,iplan)=+dx/2.0d0
      bpe346(2,icorn,iplan)=+dy/2.0d0
      bpe346(3,icorn,iplan)=+dz/2.0d0
      icorn=1
      bpe346(1,icorn,iplan)=+dx/2.0d0
      bpe346(2,icorn,iplan)=+dy/2.0d0
      bpe346(3,icorn,iplan)=-dz/2.0d0

      iplan=3
      icorn=4
      bpe346(1,icorn,iplan)=-dx/2.0d0
      bpe346(2,icorn,iplan)=-dy/2.0d0
      bpe346(3,icorn,iplan)=-dz/2.0d0
      icorn=3
      bpe346(1,icorn,iplan)=-dx/2.0d0
      bpe346(2,icorn,iplan)=-dy/2.0d0
      bpe346(3,icorn,iplan)=+dz/2.0d0
      icorn=2
      bpe346(1,icorn,iplan)=+dx/2.0d0
      bpe346(2,icorn,iplan)=-dy/2.0d0
      bpe346(3,icorn,iplan)=+dz/2.0d0
      icorn=1
      bpe346(1,icorn,iplan)=+dx/2.0d0
      bpe346(2,icorn,iplan)=-dy/2.0d0
      bpe346(3,icorn,iplan)=-dz/2.0d0

      iplan=4
      icorn=1
      bpe346(1,icorn,iplan)=-dx/2.0d0
      bpe346(2,icorn,iplan)=+dy/2.0d0
      bpe346(3,icorn,iplan)=-dz/2.0d0
      icorn=2
      bpe346(1,icorn,iplan)=-dx/2.0d0
      bpe346(2,icorn,iplan)=+dy/2.0d0
      bpe346(3,icorn,iplan)=+dz/2.0d0
      icorn=3
      bpe346(1,icorn,iplan)=+dx/2.0d0
      bpe346(2,icorn,iplan)=+dy/2.0d0
      bpe346(3,icorn,iplan)=+dz/2.0d0
      icorn=4
      bpe346(1,icorn,iplan)=+dx/2.0d0
      bpe346(2,icorn,iplan)=+dy/2.0d0
      bpe346(3,icorn,iplan)=-dz/2.0d0

      iplan=5
      icorn=1
      bpe346(1,icorn,iplan)=-dx/2.0d0
      bpe346(2,icorn,iplan)=-dy/2.0d0
      bpe346(3,icorn,iplan)=-dz/2.0d0
      icorn=2
      bpe346(1,icorn,iplan)=-dx/2.0d0
      bpe346(2,icorn,iplan)=+dy/2.0d0
      bpe346(3,icorn,iplan)=-dz/2.0d0
      icorn=3
      bpe346(1,icorn,iplan)=+dx/2.0d0
      bpe346(2,icorn,iplan)=+dy/2.0d0
      bpe346(3,icorn,iplan)=-dz/2.0d0
      icorn=4
      bpe346(1,icorn,iplan)=+dx/2.0d0
      bpe346(2,icorn,iplan)=-dy/2.0d0
      bpe346(3,icorn,iplan)=-dz/2.0d0

      iplan=6
      icorn=4
      bpe346(1,icorn,iplan)=-dx/2.0d0
      bpe346(2,icorn,iplan)=-dy/2.0d0
      bpe346(3,icorn,iplan)=+dz/2.0d0
      icorn=3
      bpe346(1,icorn,iplan)=-dx/2.0d0
      bpe346(2,icorn,iplan)=+dy/2.0d0
      bpe346(3,icorn,iplan)=+dz/2.0d0
      icorn=2
      bpe346(1,icorn,iplan)=+dx/2.0d0
      bpe346(2,icorn,iplan)=+dy/2.0d0
      bpe346(3,icorn,iplan)=+dz/2.0d0
      icorn=1
      bpe346(1,icorn,iplan)=+dx/2.0d0
      bpe346(2,icorn,iplan)=-dy/2.0d0
      bpe346(3,icorn,iplan)=+dz/2.0d0

      do iplan=1,6
        do icorn=1,4
          bpe346(1:3,icorn,iplan)=bpe346(1:3,icorn,iplan)+xyz(1:3)
        enddo
      enddo

      bpe346(1:3,5,1:6)=bpe346(1:3,1,1:6)

      return
      end
