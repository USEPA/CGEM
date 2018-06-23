      Subroutine Model_Output_CGEM(istep_out,myid,numprocs)

      USE Model_dim
      USE State_Vars
      USE OUTPUT_NETCDF_CGEM
      USE Grid
      USE CGEM_vars

      IMPLICIT NONE

      integer,intent(in)  :: istep_out !current output counter
      integer, intent(in) :: myid,numprocs
      real :: dumf(myim,jm,km,nf)
      integer :: i,j,k,nz,myi

        dumf = f(1:myim,:,:,:)

        do j=1,jm
        myi = 1
        do i=myi_start,myi_end
          nz=nza(i,j)
          do k=1,nz
            dumf(myi,j,k,iTr) = f(myi,j,k,iTr) * Vol(i,j,k)
          enddo
          myi = myi + 1
        enddo
        enddo

        CALL WRITE_DATA( myi_start,myim, 1,jm, 1, km, istep_out, dumf)

      return

      End Subroutine
