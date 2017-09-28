Subroutine Allocate_Input_CGEM()

USE Model_dim
USE CGEM_VARS
USE INPUT_VARS_CGEM
USE CGEM_Flux
USE TEMP_VARS
USE STOICH_VARS
USE MASS_BALANCE_CGEM
USE DailyRad
USE SDM
USE JWMod
USE OUTPUT_NETCDF_CGEM

IMPLICIT NONE

         nf = nospA*3+nospZ+17  !CGEM
        !Calculate EXTRA_VARIABLES for netCDF:
        !ir,irfrac,uN(nospA),uP(nospA),uE(nospA),uA(nospA),Chla,s_xy(8),uSi(nospA),pH,ChlC(nospA),RN2,RO2
         EXTRA_VARIABLES = 14 + 6*nospA
         call CGEM_vars_allocate()
         call INPUT_VARS_CGEM_allocate()
         call TEMP_VARS_allocate()
         call STOICH_VARS_allocate()
         call MASS_BALANCE_CGEM_allocate()
         call DailyRad_allocate()
         call SDM_allocate()
         call JWMod_allocate()
         call OUTPUT_NETCDF_CGEM_allocate()
         call Allocate_CGEM_Flux()

return
END SUBROUTINE Allocate_Input_CGEM

