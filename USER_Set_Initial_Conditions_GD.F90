      Subroutine USER_Set_Initial_Conditions_GD(f,fm,wsm)

      USE Model_dim
      USE STATES 

      implicit none
 
      real f(im,jm,nsl,nf)
      integer ii,i,j,k,fm(im,jm)
      real wsm(im,jm) 


! Read in from "InitialConditions_GD.txt" which has 1 value for each state variable.
! This is for a single cell.  There is no header line.
      open(19,file="InitialConditions_GD.txt",status='old')
      do ii = 1,nf
         read(19,*) f(1,1,1,ii)
      enddo
      close(19)

!Values on shelf will be erroneous, set to -9999 as 'land' marker:
      do ii = 1, nf
       do j=1,jm
        do i=1,im
          if(wsm(i,j).eq.0.) then !if shelf
           f(i,j,nsl,ii) = -9999
          endif
        enddo
       enddo
      enddo


      end subroutine USER_Set_Initial_Conditions_GD
