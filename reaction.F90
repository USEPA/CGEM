!***********************************************************************
  SUBROUTINE reaction ( OM1, OM2, O2, NO3, KG1, KG2, KO2, KstarO2, &
  &   KNO3, X1, Y1, Z1, X2, Y2, Z2, TEMP, RC )

  !----------------------------------------------------------
  ! Provides all reaction rates for the model.
  ! Note that the time units are in years, not days.
  !---------------------------------------------------------- 
    IMPLICIT NONE
    
    REAL, INTENT(IN) :: OM1
    REAL, INTENT(IN) :: OM2
    REAL, INTENT(IN) :: O2
    REAL, INTENT(IN) :: NO3
    REAL, INTENT(IN) :: KG1
    REAL, INTENT(IN) :: KG2
    REAL, INTENT(IN) :: KO2
    REAL, INTENT(IN) :: KstarO2
    REAL, INTENT(IN) :: KNO3
    REAL, INTENT(IN) :: X1
    REAL, INTENT(IN) :: Y1
    REAL, INTENT(IN) :: Z1
    REAL, INTENT(IN) :: X2
    REAL, INTENT(IN) :: Y2
    REAL, INTENT(IN) :: Z2
    REAL, INTENT(IN) :: TEMP
    REAL, DIMENSION(10),INTENT(OUT):: RC
    
    ! Begin Locals:
!---------------------------------------------    
    REAL:: FACTOR         
    REAL:: RQ1           
    REAL:: RQ2          
    REAL:: TQ1         
    REAL:: TQ2        
    REAL:: RQ21      
    REAL:: RQ22     
    REAL:: RCT1           
    REAL:: RCT2          
    REAL:: R11          
    REAL:: R12         
    REAL:: R21        
    REAL:: R22       
    REAL:: R1_SUM    
    REAL:: R2_SUM        
    REAL:: FBNO3         
    REAL:: GAM14      
    REAL:: GAM24
    REAL:: BET14
    REAL:: BET24
    REAL:: A14
    REAL:: A24     
    REAL:: RCH2O1         
    REAL:: RCH2O2        
    REAL:: RO2          
    REAL:: RNO3        
    REAL:: RPO4       
    REAL:: RTC       
    REAL:: RNH4
    REAL:: RSi
    REAL:: RALK
    REAL:: rnitrate
    REAL:: RN2
    REAL,DIMENSION(2) :: R   
!---------------------------------------------     
    ! End Locals   
    

    ! Use the Q10 relationship to determine the rates.
    ! Assume that TEMP is the maximum temperature.

    RQ1  = KG1
    RQ2  = KG2
    TQ1  = 25.0
    TQ2  = TEMP
    FACTOR = LOG10( 2.0 ) * 0.1 * ( TQ1 - TQ2 )
    RQ21 = LOG10( RQ1 ) - FACTOR
    RQ21 = 10.0 ** RQ21
    RQ22 = LOG10( RQ2 ) - FACTOR
    RQ22 = 10.0 ** RQ22

    ! Lets oxidants determine the rate of organic degradation using
    ! the full Monod relationship.
    ! Calculate the concentration of OMs from Flux.
    ! Calculate the fluxes using the concentration of OMs and 
    !    reaction rates.

    ! Provide Monod Ks, i.e. KNO3,  for denitrification--see
    ! parameter statement
    RCT1     = RQ21 * OM1
    RCT2     = RQ22 * OM2
    R( 1 ) = O2 / ( KO2 + O2 )
    R( 1 ) = MAX( 0.0, R( 1 ) )

    FBNO3 = rnitrate( O2, KstarO2 ) ! Feedback on denitrification by O2.    

    R( 2 ) = NO3 / ( KNO3 + NO3 ) * FBNO3
    R( 2 ) = MAX( 0.0, R( 2 ) )


    R11 = RCT1 * R( 1 )
    R12 = RCT1 * R( 2 )
    R1_SUM = R11 + R12
    
    R21 = RCT2 * R( 1 )
    R22 = RCT2 * R( 2 )
    R2_SUM = R21 + R22


    ! Reaction rates from TABLE 5 of Van Cappellen and Wang (1996).
    ! Use J. Lehrter's updated equations, where OM is first converted
    ! to NH3 (instead of converting directly to NO3)
    ! Use the stoichiometry from OM to determine O2 and NO3 use.

    GAM14 = ( 4.0 * X1 + 3.0 * Y1 ) / 5.0 / X1 ! denitrification NO3-/CH20
    GAM24 = ( 4.0 * X2 + 3.0 * Y2 ) / 5.0 / X2 ! denitrification NO3-/CH20

    BET14 = ( 2.0 * X1 + 4.0 * Y1 ) / 5.0 / X1 ! denitrification NO3-/CH20
    BET24 = ( 2.0 * X2 + 4.0 * Y2 ) / 5.0 / X2 ! denitrification NO3-/CH20
    RN2 = ( BET14 * R12 + BET24 * R22 )

    A14 = ( 4.0 * X1 + 3.0 * Y1 - 10.0 * Z1 ) / 5.0 / X1
    A24 = ( 4.0 * X2 + 3.0 * Y2 - 10.0 * Z2 ) / 5.0 / X2

    RO2 = -( R11 + R21 )

    RNO3 = - ( GAM14 * R12 + GAM24 * R22 )
 
    RALK = (A14 * R12 + A24 * R22)

    RNH4 = ( Y1 / X1 * R11 + Y2 / X2 * R21 )

    RSi    =  R1_SUM * Y1/X1 + R2_SUM * Y2/X2 

    RPO4   =  R1_SUM * Z1/X1 + R2_SUM * Z2/X2
    RCH2O1 = -R1_SUM   
    RCH2O2 = -R2_SUM   
    RTC    =  R1_SUM + R2_SUM   

    RC( 1 ) = RCH2O1
    RC( 2 ) = RCH2O2
    RC( 3 ) = RO2
    RC( 4 ) = RNO3
    RC( 5 ) = RPO4
    RC( 6 ) = RTC
    RC( 7 ) = RNH4
    RC( 8 ) = RSi 
    RC( 9 ) = RALK
    RC( 10 ) = RN2
 
    RETURN

  END SUBROUTINE reaction 
!***********************************************************************
