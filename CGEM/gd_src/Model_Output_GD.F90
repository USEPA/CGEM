      Subroutine Model_Output_GD(istep_out)
      !
      ! 02/21/2020  Wilson Melendez, Commented out loop that was
      !             recalcualting dumf for the tracer case for
      !             unknown reason.

      USE Model_dim
      USE State_Vars
      USE OUTPUT_NETCDF_GD
      USE Grid
      USE states 

      IMPLICIT NONE

      integer,intent(in)  :: istep_out !current output counter
      integer :: i,j,k,nz
      real :: dumf(im,jm,km,nf)

        dumf = f
!        do j=1,jm
!        do i=1,im
!          nz=nza(i,j)
!          do k=1,nz
!            dumf(i,j,k,JTR) = f(i,j,k,JTR) * Vol(i,j,k)
!          enddo
!         enddo
!        enddo

        CALL WRITE_DATA( im, jm, km, nf, istep_out, dumf)
        CALL WRITE_EXTRA_DATA( IM, JM, KM, EXTRA_VARIABLES, istep_out) 

      return

      End Subroutine
