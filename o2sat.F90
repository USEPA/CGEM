!---------------------------------------------------------------------
   FUNCTION   o2sat(S, T)               RESULT(OSAT)
  
!--------------------------------------------------------------------- 
! Oxygen concentration at saturation at one atmosphere (umol/kg).
! Source: "The Solubility Of Nitrogen, Oxygen And Argon In Water And
!         Seawater" - Weiss (1970) _Deep Sea Research_ V17(4): 721-735.
! Based on MATLAB code by Edward T Peltzer, MBARI, revised: 1998-01-2   
!---------------------------------------------------------------------    
   
    IMPLICIT NONE

    REAL        , INTENT(IN) :: S  ! Salinity 
    REAL        , INTENT(IN) :: T  ! Temperature (degrees C) 
        
    ! Declare Locals:
!----------------------------------------------------------------------    
    REAL, PARAMETER:: CONSTANT1  =  273.15      
    REAL, PARAMETER:: CONSTANT2  =    0.01     
    REAL, PARAMETER:: CONSTANT3  = -177.7888  
    REAL, PARAMETER:: CONSTANT4  =  255.5907 
    REAL, PARAMETER:: CONSTANT5  =  146.4813
    REAL, PARAMETER:: CONSTANT6  =   22.204     
    REAL, PARAMETER:: CONSTANT7  =   -0.037362 
    REAL, PARAMETER:: CONSTANT8  =    0.016504
    REAL, PARAMETER:: CONSTANT9  =    0.0020564 
    REAL, PARAMETER:: ML_PER_UM  =   44.658    
    REAL          :: T1     
    REAL          :: OSAT ! Oxygen (umol/kg) --function RESULT 
!----------------------------------------------------------------------       

    T1 = ( T + CONSTANT1 ) * CONSTANT2
    
    OSAT = CONSTANT3 + CONSTANT4 /                                     &
    &      T1 + CONSTANT5 * LOG( T1 ) - CONSTANT6 * T1
    
    OSAT = OSAT + S * (CONSTANT7 + T1 * (CONSTANT8 - CONSTANT9 * T1))
    OSAT = EXP( OSAT )
    OSAT = OSAT * ML_PER_UM ! Oxygen (umol/kg)

    RETURN
  END FUNCTION o2sat  
