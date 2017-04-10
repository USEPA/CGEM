SUBROUTINE EXCHANGE(f,area,Vol,DLT,i,j,SETRATE)
!------------------------------------------------------------------------------
!-
!-  $Id: exchange.F90,v 1.0.6.1 2014/08/26 22:54:04 wmelende Exp wmelende $
!-  $Locker: wmelende $
!-
!-  Description :  This subroutine calculates the mass rate transferred across
!-                 the bottom boundary using the bottom vertical flows and
!-                 bottom vertical mixing coefficients.
!-
!-  Inputs  :  None
!-  Outputs :  None
!-
!-  Calls:      None
!-  Called by:  DERIV
!- 
!-  Created: 11/06/13  W. Melendez
!-
!- Revised:  07/23/14  W. Melendez : Updated the instant mineralization:
!-                                   added Monod function to MASSBC; added
!-                                   code that prevents the sediment NO3
!-                                   mass rate from being lower than the
!-                                   minimum observed NO3 mass flux; added
!-                                   C_SOD_FLUX_MAX and SED_NO3_FLUX_MAX
!-                                   parameters. Name change: OXYD --> OXID.
!-                                   Upated documentation.
!- Revised:  03/14/14  W. Melendez : Added capping of the NO3 sediment bed 
!-                                   flux for the case of negative fluxes.
!-                                   Added linear extrapolation for the 
!-                                   estimation of the concentration at the 
!-                                   bottom boundary cell face when the
!-                                   bottom boundary flow is negative.
!- Revised:  03/04/14  W. Melendez : Bug fix --> changed multiplication to
!-                                   division by term DENIT_CN_RATIO in
!-                                   nitrate flux equation. Turned on the
!-                                   passing of the settling rate loss,
!-                                   through the bottom boundary, to the
!-                                   transport routine.
!- Revised:  02/27/14  W. Melendez : Switched back to passing the bottom
!-                                   boundary advection and diffusion to the
!-                                   transport routine as a load term.
!- Revised:  02/26/14  W. Melendez : Bug fix --> removed RSODNTR paramater
!-                                   from SED_NO3_RATE equation in the ELSE
!-                                   part of the IF-THEN-ELSE statement 
!-                                   found at the bottom of this subroutine.
!- Revised:  02/25/14  W. Melendez : Took settling loss rate out of 
!-                                   transport routine.  Added update
!-                                   of mass derivative for bottom boundary
!-                                   advection and diffusion. Added 
!-                                   WRITE_MASSBD flag.
!- Revised:  02/13/14  W. Melendez : Bug fix --> corrected erroneous 
!-                                   accumulation of MASS_BTBDOUT under 
!-                                   diffusion: it had MASS_BTBDIN in
!-                                   the right hand side of the equation;
!-                                   changed to MASS_BTBDOUT as it should be.
!- Revised:  02/07/14  W. Melendez : Renamed some of the variables by adding
!-                                   "RATE" to their names.  Added more 
!-                                   comments.
!- Revised:  01/31/14  W. Melendez : Added calculation of SOD, ammonia, 
!-                                   and nitrate sediment bed fluxes.
!-
!------------------------------------------------------------------------------

USE Model_dim
USE STATES
USE EUT
USE InRemin
USE INPUT_VARS_GD, ONLY:ws

IMPLICIT NONE 

REAL, INTENT(IN) :: f(nf),area,Vol,DLT
INTEGER, INTENT(IN) :: i,j
REAL, INTENT(OUT) :: SETRATE(nf) !Settling loss in kg/s

REAL :: SED_CARBON_RATE_OXID
REAL :: SED_NO3_RATE_NITR
REAL :: TOC_SETRATE, TON_SETRATE

REAL :: MASSBC              ! Mass of bottom grid cell

REAL :: MASS_NO3R           ! NO3 mass obtained by using the calculated
                            ! sediment NO3 mass rate.
REAL :: MASS_NO3F           ! NO3 mass obtained by using the minimum observed
                            ! NO3 mass flux.
REAL :: MASS_NO3            ! The minimum value of MASS_NO3R and MASS_NO3F.
REAL :: C_SOD               ! Carbon contribution to SOD in units of kg/s
REAL :: C_SOD_PerArea       ! Carbon contribution to SOD in units of kg/(m^2 s)
REAL :: Available_Carbon    ! Availabe carbon for denitrification
REAL :: Carbon_Consumed_Denitr  !  Carbon consumed by the denitrification process

!
!------------------------------------------------------------------------------
!
! Mass loss due to settling in kg/s
  SETRATE(:) = f(:)*area*(-ws(:))

!------------------------------------------------------------------------------
! Calculate sediment oxygen demand, ammonia, and nitrate mass rates at
! each bottom water grid cell that has a sediment bed interface.
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!-  Calculate particulate organic carbon and particulate organic nitrogen
!-  settling rates.
!-  [TOC_SETRATE] = kg/s
!-  [TON_SETRATE] = kg/s
!------------------------------------------------------------------------------
    TOC_SETRATE = SETRATE(JLOC) + SETRATE(JROC) + &
                & SETRATE(JDIA) + SETRATE(JGRE) + &
                & SED_CARBON_RATE(i,j)

    TON_SETRATE = SETRATE(JLON) + SETRATE(JRON) + &
                & (SETRATE(JDIA) + SETRATE(JGRE)) * ANCP


!------------------------------------------------------------------------------
!-  Calculate the organic carbon mass rate that is oxidized in the sediment
!-  bed.  [SED_CARBON_RATE_OXID] = kg/s
!------------------------------------------------------------------------------
    SED_CARBON_RATE_OXID = TOC_SETRATE * ( f(JDO2) / &
                         & ( f(JDO2) + KHODOC_SED ) )

!------------------------------------------------------------------------------
!-  We are assuming that particulate organic nitrogen gets instantenously
!-  converted to ammonia when it enters the sediment bed.
!-  Some of the ammonia gets converted to nitrate due to nitrification.  The 
!-  nitrate resulting from nitrification gets calculated as a mass rate in 
!-  units of kg/s.  The ammonia that remains in the sediment bed
!-  is then calculated also as a mass rate.
!-  [SED_NO3_RATE_NITR] = kg/s
!-  [SED_NH3_RATE] = kg/s 
!------------------------------------------------------------------------------
    SED_NO3_RATE_NITR = TON_SETRATE * ( f(JDO2) / ( f(JDO2) + KHDONT_SED ) )

    SED_NH3_RATE(i,j) = TON_SETRATE - SED_NO3_RATE_NITR


!------------------------------------------------------------------------------
!  Cap the SOD coming from carbon oxidation to 25 mmol/(m^2 day) and
!  cap the carbon oxidized rate accordingly.
!  25 mmol of O2/(m^2 day) = (9.259E-09 kg of O2)/(m^2 s)
!  The value of (9.259E-09 kg of O2)/(m^2 s) is represented by the FIREAD
!  key C_SOD_FLUX_MAX.
!  [C_SOD] = (kg of O2)/s
!  [C_SOD_PerArea] = (kg of O2)/(m^2 s)
!  RCDO = (mol of O2)/(mol of C) = 2.67
!------------------------------------------------------------------------------
    C_SOD = SED_CARBON_RATE_OXID * RCDO
    C_SOD_PerArea = C_SOD / area 

    IF (C_SOD_PerArea > C_SOD_FLUX_MAX) THEN
        C_SOD = C_SOD_FLUX_MAX * area 
        SED_CARBON_RATE_OXID = C_SOD / RCDO
    ENDIF

!------------------------------------------------------------------------------
!  Calculate SOD as the sum of the nitrate mass rate coming from 
!  nitrification and the carbon mass rate coming from oxidation. 
!  [TSOD] = kg/s
!------------------------------------------------------------------------------
    TSOD(i,j) = (SED_NO3_RATE_NITR * RNTO) + C_SOD 
    TSOD(i,j) = AMAX1(TSOD(i,j),0.)

!------------------------------------------------------------------------------
!  Calculate the available carbon for denitrification by subtracting the
!  carbon mass rate lost due to oxidation from the total carbon mass rate in
!  the sediment bed.
!  [Available_Carbon] = kg/s
!  [TOC_SETRATE] = kg/s
!  [SED_CARBON_RATE_OXID] = kg/s
!------------------------------------------------------------------------------
   Available_Carbon = TOC_SETRATE - SED_CARBON_RATE_OXID
   !write(6,*) "AC=",TOC_SETRATE,SED_CARBON_RATE_OXID
!------------------------------------------------------------------------------
!  Calculate the NO3 mass rate that will be moved across the water-sediment
!  interface by subtracting the NO3 mass rate lost due to denitrification from
!  the NO3 mass rate that had been previously generated by nitrification.
!  [SED_NO3_RATE] = kg/s
!  [SED_NO3_RATE_NITR] = kg/s
!------------------------------------------------------------------------------
   SED_NO3_RATE(i,j) = SED_NO3_RATE_NITR -  (Available_Carbon / DENIT_CN_RATIO)

   !write(6,*) "SNR,SNRN,AC/DCN",SED_NO3_RATE(i,j),SED_NO3_RATE_NITR,Available_Carbon/DENIT_CN_RATIO


   IF ( SED_NO3_RATE(i,j) < 0.0 ) THEN

!--------------------------------------------------------------------------------
!   Limit the amount of the overlying NO3 that will be available for
!   denitrification by multipliyng the overlying NO3 mass by a Monod equation.
!   The Monod equation is a function of the overlying NO3 concentration.
!--------------------------------------------------------------------------------
        MASSBC = f(JNO3) * Vol * (f(JNO3) / (f(JNO3) + KHDENITR_SED))

!--------------------------------------------------------------------------------
!   Calculate the amount of overlying NO3 mass needed by denitrification: this
!   is the NO3 mass that denitrification is trying to draw from the water column.
!   The calculation is done in three steps.  They are:
!   1) Calculate an NO3 mass term, MASS_NO3R, by using the calculated NO3 mass
!      rate.
!   2) Calculate a second NO3 mass term, MASS_NO3F, by using the minimum
!      observed NO3 mass flux.
!   3) Determine the minimum of these two mass terms.
!--------------------------------------------------------------------------------
        MASS_NO3R = -(SED_NO3_RATE(i,j) * DLT)

        MASS_NO3F = -(SED_NO3_FLUX_MIN * area * DLT)

        MASS_NO3 = MIN(MASS_NO3R,MASS_NO3F)

        !write(6,*) "R,F,NO3,BC,DLT,area",MASS_NO3R,MASS_NO3F,MASS_NO3,MASSBC,area,DLT

!--------------------------------------------------------------------------------
!  If the amount of NO3 required by denitrification is greater than the
!  amount of NO3 in the bottom water layer, cap the sediment NO3 mass rate
!  that will be moved across the water-sediment interface to the NO3 mass
!  per time step that is available in the bottom water layer.
!--------------------------------------------------------------------------------
        IF (MASS_NO3 > MASSBC) THEN

            SED_NO3_RATE(i,j) = -(MASSBC / DLT)

!--------------------------------------------------------------------------------
!  The carbon mass rate consumed by denitrification is the sum of the NO3
!  that is available in the sediment bed (as a result of nitrification) and the
!  NO3 mass rate that is being drawn from the bottom water layer, converted to
!  carbon units using a denitrification carbon-to-nitrogen ratio (DENIT_CN_RATIO).
!  [Carbon_Consumed_Denitr] = Kg/s
!--------------------------------------------------------------------------------
            Carbon_Consumed_Denitr = (SED_NO3_RATE_NITR - SED_NO3_RATE(i,j)) * DENIT_CN_RATIO

!--------------------------------------------------------------------------------
!  The inventory of carbon in the sediment bed is calculated by subtracting the
!  carbon mass rate consumed by denitrification from the available carbon mass
!  rate.
!  [SED_CARBON_RATE] = kg/s
!--------------------------------------------------------------------------------
            SED_CARBON_RATE(i,j) = Available_Carbon - Carbon_Consumed_Denitr


        ELSE IF (MASS_NO3F < MASS_NO3R) THEN
!--------------------------------------------------------------------------------
!  Cap the NO3 mass rate that will be moved across the water-sediment interface
!  to the mass of NO3 calculated using the minimum observed NO3 mass flux
!  divided by the time step.
!--------------------------------------------------------------------------------
            SED_NO3_RATE(i,j) = -(MASS_NO3F / DLT)

!--------------------------------------------------------------------------------
! The carbon mass rate consumed by denitrification is the sum of the NO3
! that is available in the sediment bed (as a result of nitrification) and the
! NO3 mass rate that is being drawn from the bottom water layer, converted to
! carbon units using a denitrification carbon-to-nitrogen ratio (DENIT_CN_RATIO).
!--------------------------------------------------------------------------------
            Carbon_Consumed_Denitr = (SED_NO3_RATE_NITR - SED_NO3_RATE(i,j)) * DENIT_CN_RATIO

!--------------------------------------------------------------------------------
!  The inventory of carbon in the sediment bed is calculated by subtracting the
!  carbon mass rate consumed by denitrification from the available carbon mass
!  rate.
!  [SED_CARBON_RATE] = kg/s
!--------------------------------------------------------------------------------
            SED_CARBON_RATE(i,j) = Available_Carbon - Carbon_Consumed_Denitr

        ELSE

            SED_CARBON_RATE(i,j) = 0.0

        ENDIF

   ELSE

        SED_CARBON_RATE(i,j) = 0.0

   ENDIF



!
!------------------------------------------------------------------------------
!

RETURN 

END SUBROUTINE EXCHANGE
