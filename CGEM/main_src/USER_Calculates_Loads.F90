subroutine USER_Calculates_Loads()
  
  USE Model_dim
  USE DATE_TIME
  USE RiverLoad
  USE Grid
  USE INPUT_VARS
  
  IMPLICIT NONE

  REAL, DIMENSION(4) :: NLoad

  ! Calculate nutrient loadings
  ! Flow is assumed to be in units of m3/s.
  ! Concentration is assumed to be in units of mg/L.
  ! The factor of 1.0E-03 converts mg/L to kg/m3.  
  NLoad = River_InFlow(1) * (River_Conc * 1.0E-03)

  ! Assign nutrient loadings to Riv_*'s arrays.
  Riv_NO3(1) = NLoad(1)
  Riv_NH3(1) = NLoad(2)
  Riv_DIP(1) = NLoad(3)
  Riv_DO(1) = NLoad(4)

  ! Update volume of grid cell
  Vol(1,1,1) = Vol(1,1,1) + (River_InFlow(1) - River_OutFlow(1)) * dT

  ! Check if volume is negative
  IF (Vol(1,1,1) <= 0.0) THEN
     WRITE(6,*) "Volume of grid cell is less than or equal to zero: ", Vol(1,1,1) 
     WRITE(6,*) "Exiting"
     STOP
  ENDIF
  
  RETURN
END SUBROUTINE USER_Calculates_Loads
