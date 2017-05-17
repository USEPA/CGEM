      Subroutine USER_Set_Initial_Conditions(filename)

      USE Model_dim
      USE State_Vars !f
      USE Fill_Value

      implicit none
 
      integer ii,i,j,k,nz
      character(*),intent(in) :: filename
      real init(nf)

      f=fill(0)  !Fill values for netCDF

! Read in from "InitialConditions.txt" which has 1 value for each state variable.
! There is no header line.
      open(19,file=filename,status='old')
      do ii = 1,nf
         read(19,*) init(ii)
      enddo
      close(19)

      do j=1,jm
       do i=1,im
             nz = nza(i,j)
             do k = 1, nz
                f(i,j,k,:) = init(:)
             enddo
       enddo
      enddo

      return

      end subroutine USER_Set_Initial_Conditions

