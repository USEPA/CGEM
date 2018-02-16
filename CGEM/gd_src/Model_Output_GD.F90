      Subroutine Model_Output_GD(istep_out,myid,numprocs)

      USE Model_dim
      USE State_Vars
      USE OUTPUT_NETCDF_GD
      USE Grid
      USE states 
      USE OUTPUT

      IMPLICIT NONE

      integer,intent(in)  :: istep_out !current output counter
      integer,intent(in) :: myid,numprocs
      integer :: i,j,k,myi,nz
      real :: dumf(myim,jm,km,nf)

        dumf = f
        do j=1,jm
         myi = 1
         do i=myi_start,myi_end
          nz=nza(i,j)
          do k=1,nz
            dumf(myi,j,k,JTR) = f(myi,j,k,JTR) * Vol(i,j,k)
          enddo
          myi = myi + 1
         enddo
        enddo

        CALL WRITE_DATA( myi_start,myim, 1, jm, 1, km, nf, istep_out, dumf)
        CALL WRITE_EXTRA_DATA( myi_start, myIM,1, JM, 1,KM, EXTRA_VARIABLES, istep_out) 

      return

      End Subroutine
