      Subroutine USER_Set_Initial_Conditions(filename)

      USE Model_dim, ONLY: nf
      USE State_Vars
      USE Fill_Value

      implicit none
 
      integer ii
      character(200),intent(in) :: filename

      f=fill(0)  !Fill values for netCDF

! Read in from "InitialConditions.txt" which has 1 value for each state variable.
! This is for a single cell.  There is no header line.
      open(19,file=filename,status='old')
      do ii = 1,nf
         read(19,*) f(1,1,1,ii)
      enddo
      close(19)

      return

      end subroutine USER_Set_Initial_Conditions

