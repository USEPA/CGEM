!------------------------------------------------------------------
  FUNCTION gas_exchange( T, S, TC, H, PH, PCO2 ) RESULT( RES ) 
!------------------------------------------------------------------
! Gas exchange model from Eldridge and Cifuentes (2000).
! Provides and estimate of the loss or gain of CO2 from the atmosphere
! The atmosphere pCo2 concentration was from Whitfield and Turner (1986)
! and may have changes by now.  D/z is in cm s-1 and depth is in meters.
! Multiplication by 1000 and division by 100 provides units of mmol C m-2
! s-1.  Final units are mmol C m-2 d-1.
!------------------------------------------------------------------
! T        - Temperature (deg C) of upper model layer.
! S        - Salinity (psu) of upper model layer.
! TC       - Dissolved Inorganic Carbon (DIC) (mmol m-3) in upper 
!            model layer
! H        -- Thickness of the upper model layer (m.) contacting the 
!             atmosphere
! PH       -- pH concentration of seawater.
! PCO2     -- CO2 concentration in atmosphere in ppmv.  
!             Table 1 in Huang et al. 2015 for LA shelf, average=380 
!------------------------------------------------------------------ 
   
    USE MOD_EPACOM_GEM_UTILITIES
   
    IMPLICIT NONE   
   
    REAL   ,INTENT(IN)  :: T
    REAL   ,INTENT(IN)  :: S
    REAL   ,INTENT(IN)  :: TC
    REAL   ,INTENT(IN)  :: H
    REAL   ,INTENT(IN)  :: PCO2    
    REAL   ,INTENT(IN)  :: PH
!--------------------------------------------     
    REAL, PARAMETER      :: DZ      = 0.005  
                                        ! combines diffusion through 
                                        ! water-atm layer and 
				        ! thickness(cm s-1). 
    REAL                 :: ATM_CO2           
    REAL                 :: H2CO3C             
    REAL                 :: PW                 
    REAL                 :: RES      
    REAL                 :: RK0    
    REAL                 :: sw_dens0
    REAL   ,DIMENSION(6) :: SPC    
   
    ! Use an updated salclosed function to get the distribution of 
    ! carbonate alkalinity species and H+.  Whitman and Turner (1986).
    SPC     = salclosed( S, TC, T, PH )  ! SPC units Concentration mmol/m3 
 
    H2CO3C  = SPC( 1 )   ! Concentration of H2CO3 in units of mmol/m3 
    RK0     = SPC( 6 )   ! Henry's constant (mol kg-1 atm-1) 
    PW      = sw_dens0( S, T )              ! water density  [kg/m3]
     
    ATM_CO2  = PCO2 * RK0 * (PW * 1.0E-3)  
   
    RES = -DZ  * ( ATM_CO2 - H2CO3C ) / ( H * 100.0) !mmol C m-2 s-1 
 
    RETURN
  END FUNCTION gas_exchange 
