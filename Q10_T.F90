!---------------------------------------------------------------------
   FUNCTION   Q10_T(T,K)               RESULT(Tadj)
!--------------------------------------------------------------------- 
! Q10 rate adjustment, taken from reaction subroutine
!---------------------------------------------------------------------    
   
    IMPLICIT NONE

!----------------------------------------------------------------------
    REAL, INTENT(IN) :: T  ! Temperature (degrees C) 
    REAL, INTENT(IN) :: K  ! Constant     
!----------------------------------------------------------------------    
    REAL, PARAMETER  :: Tk = 25.
    REAL             :: FACTOR    
    REAL             :: Tadj ! Temperature adjusted rate function 
!----------------------------------------------------------------------       
    FACTOR = LOG10( 2.0 ) * 0.1 * ( Tk - T )
    Tadj = LOG10( K ) - FACTOR
    Tadj = 10.0 ** Tadj 

    RETURN

  END FUNCTION Q10_T  
