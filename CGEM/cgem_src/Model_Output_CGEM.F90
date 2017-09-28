      Subroutine Model_Output_CGEM(istep_out)

      USE Model_dim
      USE State_Vars
      USE OUTPUT_NETCDF_CGEM
      USE Grid
      USE CGEM_vars

      IMPLICIT NONE

      integer,intent(in)  :: istep_out !current output counter
      real :: dumf(im,jm,km,nf)
      integer :: i,j,k,nz

        dumf = f
        do j=1,jm
        do i=1,im
          nz=nza(i,j)
          do k=1,nz
           dumf(i,j,k,iTr) = f(i,j,k,iTr) * Vol(i,j,k)
          enddo
         enddo
        enddo

        CALL WRITE_DATA( im, jm, km, nf, istep_out, dumf)

      return

      End Subroutine
