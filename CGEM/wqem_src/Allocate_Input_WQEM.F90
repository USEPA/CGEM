Subroutine Allocate_Input_WQEM()

USE Model_dim
USE OUTPUT_NETCDF_WQEM
USE EUT
USE InRemin
USE MASS_BALANCE_WQEM
USE OUTPUT

IMPLICIT NONE

         nf = 23
         EXTRA_VARIABLES = 24
         STATE_VARIABLES = nf
         call OUTPUT_NETCDF_WQEM_allocate()
         call eut_allocate()
         call InRemin_allocate()
         call MASS_BALANCE_WQEM_allocate()

return
END SUBROUTINE Allocate_Input_WQEM
