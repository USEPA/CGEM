!***********************************************************************
      FUNCTION T_adj( T ) RESULT( RES )  
      
  !--------------------------------------------------------------------------
  ! INPUT:  
  !   T = temperature [degree C]
  !
  ! OUTPUT:
  !   Temperature Adjustment 
  ! 
  ! REFERENCES:
  !------------------------------------------------------------------------
    IMPLICIT NONE

    REAL, INTENT(IN) :: T    ! Temperature (deg C)
    REAL             :: RES
    REAL, PARAMETER  :: f0    = 0.1
    REAL, PARAMETER  :: r     = 0.3
    REAL, PARAMETER  :: f1    = 1.0/f0 - 1.0  
    REAL, PARAMETER  :: r1    = r*(46.5/18.0) 
    REAL             :: denom

    denom = 1.0 + f1*exp(-r1*( T - 17.0)) 
    RES   = 0.3 *(1.0/denom) + 0.7            
               
    RETURN
  END FUNCTION T_adj  
!***********************************************************************
