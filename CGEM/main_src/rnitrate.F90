!***********************************************************************
  FUNCTION rnitrate( O2, KO2 ) RESULT( RES ) 

  !--------------------------------------------------------------------  
  ! Monod type function and feedbacks for NO3 - as an electron acceptor.
  !--------------------------------------------------------------------   
    IMPLICIT NONE
    
    REAL,INTENT(IN) :: O2  ! Oxygen concentration
    REAL,INTENT(IN) :: KO2 ! Inhibition constant
    REAL            :: RES
    REAL:: PO2

    PO2  = MAX( 0.0, O2 )

    RES  = KO2 / ( KO2 + PO2 )  

    RETURN
  END FUNCTION rnitrate  
!***********************************************************************
