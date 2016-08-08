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

    R_11 = nitmax* O2/(KO2+O2) * NH4/(KNH4+NH4)

    RETURN
  END SUBROUTINE Nitrification 
