subroutine USER_Calculates_Loads()
  
  USE Model_dim
  USE DATE_TIME
  USE RiverLoad
  USE Grid
  USE INPUT_VARS
  
  IMPLICIT NONE

  INTEGER :: iriv
  REAL, DIMENSION(4) :: NLoad

  DO iriv = 1, nRiv
      ! Calculate nutrient loadings
      ! Flow is assumed to be in units of m3/s.
      ! Concentration is assumed to be in units of mg/L.
      ! The factor of 1.0E-03 converts mg/L to kg/m3.  
      NLoad = River_InFlow(iriv) * (River_Conc(iriv,:) * 1.0E-03)

      ! Assign nutrient loadings to Riv_*'s arrays.
      Riv_NO3(iriv) = NLoad(1)
      Riv_NH3(iriv) = NLoad(2)
      Riv_DIP(iriv) = NLoad(3)
      Riv_DO(iriv)  = NLoad(4)

      ! Update volume of grid cell
      Vol(1,1,1) = Vol(1,1,1) + (River_InFlow(iriv) - River_OutFlow(iriv)) * dT

      ! Check if volume is negative
      IF (Vol(1,1,1) <= 0.0) THEN
         WRITE(6,*) "Volume of grid cell is less than or equal to zero: ", Vol(1,1,1) 
         WRITE(6,*) "Exiting"
         STOP
      ENDIF

  END DO

RETURN
END SUBROUTINE USER_Calculates_Loads
