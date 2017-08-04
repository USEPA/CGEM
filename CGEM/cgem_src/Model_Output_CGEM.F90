      Subroutine Model_Output_CGEM(istep_out)

      USE Model_dim
      USE State_Vars
      USE OUTPUT_NETCDF_CGEM

      IMPLICIT NONE

      integer,intent(in)  :: istep_out !current output counter

        CALL WRITE_DATA( im, jm, nsl, nf, istep_out, f)

      return

      End Subroutine
