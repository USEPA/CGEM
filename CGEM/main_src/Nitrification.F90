!--------------------------------------------------------------------
  SUBROUTINE Nitrification( O2, NH4, KO2, KNH4, nitmax, TEMP, R_11 )     
  !-------------------------------------------------------------------  
  ! Provides Nitrification rate for the model.
  ! Note that the time units are in days, not seconds. 
  !-------------------------------------------------------------------   
    IMPLICIT NONE
    
    REAL, INTENT(IN) :: O2
    REAL, INTENT(IN) :: NH4
    REAL, INTENT(IN) :: KO2
    REAL, INTENT(IN) :: KNH4
    REAL, INTENT(IN) :: nitmax 
    REAL, INTENT(IN) :: TEMP
    REAL, INTENT(OUT):: R_11  

    REAL :: FACTOR
    REAL :: TQ1
    REAL :: TQ2
    REAL :: RQ1
    REAL :: RQ11
!-------------------------------------------------
    ! Use the Q10 relationship to determine the rates.

    TQ1  = 25.0
    TQ2  = TEMP
    FACTOR = LOG10( 2.0 ) * 0.1 * ( TQ1 - TQ2 )

    RQ1  = nitmax
    RQ11 = LOG10( RQ1 ) - FACTOR
    RQ11 = 10.0 ** RQ11

    R_11 = RQ11 * O2/(KO2+O2) * NH4/(KNH4+NH4)

    RETURN
  END SUBROUTINE Nitrification 
