SUBROUTINE GREENS_DROOP(f,DTM,TEMP,IOPpar,Vol,dT,i,j,k)
!------------------------------------------------------------------------------
!-
!-   Purpose and Methods : Perform non-diatoms Droop kinetics calculations
!-
!-   Inputs  :   None
!-   Outputs :   None
!-   Controls:   Unknown
!-
!-   Calls:      REPORT
!-   Called by:  EUTRO
!-
!-   Created:    May 13, 2016
!-   Authors:    Wilson Melendez
!-
!------------------------------------------------------------------------------
!

USE Model_dim
USE EUT
USE FLAGS, ONLY: SILIM
USE STATES
USE INPUT_VARS_GD, ONLY : Read_Solar

IMPLICIT NONE


REAL, INTENT(IN) :: f(nf),dT
REAL, INTENT(IN) :: TEMP,IOPpar,Vol
REAL, INTENT(INOUT) :: DTM(nf)
INTEGER, INTENT(IN) :: i,j,k


REAL :: FN               ! Nutrient limitation factor
REAL :: PAR              ! Photosynthetic active radiation

!
!------------------------------------------------------------------------------
!

!------------------------------------------------------------------------------
!                             Light limitation
!------------------------------------------------------------------------------
! Reference: Jassby, Alan D. and Trevor Platt. July 1976. 
!            Mathematical formulation of the relationship between
!            photosynthesis and light for phytoplankton.
!            Limnology and Oceanography. Vol. 21(4). 
!
! ALPHA_GRE = slope of light saturation curve at low light levels, i.e. in 
!             the linear range.  Light saturation curve is also known as a
!             Photosynthesis-Light (P-I) curve.
!             Its units are g C [g Chl a]^-1 h^-1 (umol quanta)^-1 m^-2 s^-1.
!
! PBMAX_GRE = specific photosynthetic rate at optimal illumination, i.e the
!             assimilation number. It's the maximum assimilation number 
!             normalized to phytoplankton biomass; biomass is abbreviated as
!             B. Its units are g C [g Chl a]^-1 h^-1.
!
! The light limitation equation is parametrized in terms of PAR.
! PAR = photosynthetic active radiation = 400nm - 700nm.  The part
! of the electromagnetic spectrum that can be utilized for photosynthesis
! is between 400 and 700 nm and the energy in this part of the spectrum
! is called photosynthetic active radiation or PAR. Short-wave radiation 
! or solar radiation is the energy of wavelengths in the solar spectrum.
! The solar spectrum is between 300 and 3000 nm. Approximately 48% of the
! energy in the solar spectrum is PAR: see Brad Penta et al., 
! "An underwater light attenuation scheme for marine ecosystem models"  
! Optics Express, vol. 16, no. 21 (2008). 
! The units of PAR are umol/m2/s. 
!
! IAVG = average solar radiation in Watts/m^2. We need to convert Watts/m^2
! to umol/(m^2 s).  This is done by using the factor 4.57 which converts 
! W/m^2 to umol/(m^2 s).  
! See website "http://www.egc.com/useful_info_lighting.php" for more
! details regarding the conversion factor of 4.57.
!
!------------------------------------------------------------------------------

   PAR = IOPpar * 0.48 * 4.57
   if(Read_Solar.eq.1) PAR = IOPpar

   IFG(i,j,k)  = TANH (ALPHA_GRE * PAR / PBMAX_GRE)

!------------------------------------------------------------------------------
!  Greens metabolism rate: respiration and mortality are included with this
!  term.
!------------------------------------------------------------------------------
   BMG(i,j,k) = BMRG * EXP(KTBG * (TEMP - TRG))

!------------------------------------------------------------------------------
!  Temperature limitation
!------------------------------------------------------------------------------
   IF (TEMP < TMG) THEN
       TFG(i,j,k) = EXP(-KTGG1 * (TEMP - TMG)**2)                       
   ELSE
       TFG(i,j,k) = EXP(-KTGG2 * (TMG - TEMP)**2)
   ENDIF

!------------------------------------------------------------------------------
! Nutrient limitations
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!  Nitrogen limitation
!------------------------------------------------------------------------------
   QNG(i,j,k) = 0.0
   NFG(i,j,k) = 0.0
   IF (f(JGRE) > 0.0) QNG(i,j,k) = f(JGREN) / f(JGRE) 
   IF (QNG(i,j,k) > 0.0) NFG(i,j,k) = MAX(0.0, QNG(i,j,k) - QminNG) / QNG(i,j,k)

!------------------------------------------------------------------------------
! Phosphorous limitation
!------------------------------------------------------------------------------
  QPG(i,j,k) = 0.0
  PFG(i,j,k) = 0.0
  IF (f(JGRE) > 0.0) QPG(i,j,k) = f(JGREP) / f(JGRE)           
  IF (QPG(i,j,k) > 0.0) PFG(i,j,k) = MAX(0.0, QPG(i,j,k) - QminPG) / QPG(i,j,k)

!------------------------------------------------------------------------------
!  Nutrient limitation
!------------------------------------------------------------------------------
   IF (SILIM == 3) THEN 
       FN = PFG(i,j,k)                      ! Phosphorus limitation only
   ELSE
       FN = MIN(NFG(i,j,k),PFG(i,j,k))       ! Nutrient limitation
   ENDIF

   PG(i,j,k) = PMG * FN * IFG(i,j,k) * TFG(i,j,k)      ! Production/Growth
   IF (PG(i,j,k) < 0.0) PG(i,j,k) = 0.0


!------------------------------------------------------------------------------
!  Update green algae time derivative
!------------------------------------------------------------------------------
   DTM(JGRE) = DTM(JGRE) &                  
                  + (PG(i,j,k) - BMG(i,j,k)) * f(JGRE) &
                  - PRG(i,j,k)


   PG_AVG(i,j,k)  = PG_AVG(i,j,k)  + PG(i,j,k) *  f(JGRE) * Vol * real(dT,4)




!
!------------------------------------------------------------------------------
!

RETURN

END SUBROUTINE GREENS_DROOP

