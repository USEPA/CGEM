SUBROUTINE DIATOMS_DROOP(f,DTM,TEMP,IOPpar,i,j,k,myi,Vol,dT)
!------------------------------------------------------------------------------
!-   
!-   Purpose and Methods : Diatoms Droop kinetics calculations
!-
!-   Inputs  :   None
!-   Outputs :   None
!-   Controls:   Unknown
!-
!-   Calls:      None
!-   Called by:  EUTRO
!-
!-   Created:    May 13, 2016
!------------------------------------------------------------------------------
!

USE Model_dim
USE EUT
USE FLAGS, ONLY: SILIM
USE STATES
!USE Extra_Output

IMPLICIT NONE

REAL, INTENT(IN) :: f(nf),dT
REAL, INTENT(IN) :: TEMP,IOPpar,Vol
REAL, INTENT(INOUT) :: DTM(nf)
INTEGER, INTENT(IN) :: i,j,k,myi


REAL :: FN  ! Nutrient limitation factor

REAL :: SA                ! Available silica
REAL :: PAR               ! Photosynthetic active radiation

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
! ALPHA_DIA = slope of light saturation curve at low light levels, i.e. in 
!             the linear range. Light saturation curve is also known as a
!             Photosynthesis-Light (P-I) curve.
!             Its units are g C [g Chl a]^-1 h^-1 (umol quanta)^-1 m^-2 s^-1.
!
! PBMAX_DIA = specific photosynthetic rate at optimal illumination, i.e the
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
   IFD(i,j,k) = TANH (ALPHA_DIA * PAR / PBMAX_DIA)

!------------------------------------------------------------------------------
!  Diatom metabolism: respiration and mortality are included with this term.
!------------------------------------------------------------------------------
   BMD(i,j,k) = BMRD * EXP(KTBD * (TEMP - TRD))  

!------------------------------------------------------------------------------
!  Temperature limitation
!------------------------------------------------------------------------------
   IF (TEMP < TMD) THEN
      TFD(i,j,k) = EXP(-KTGD1 * (TEMP - TMD)**2)     
   ELSE
      TFD(i,j,k) = EXP(-KTGD2 * (TMD - TEMP)**2)
   ENDIF

!------------------------------------------------------------------------------
!  Nutrient limitations
!------------------------------------------------------------------------------
                           
!------------------------------------------------------------------------------
!  Nitrogen limitation
!------------------------------------------------------------------------------
   QND(i,j,k) = 0.0
   NFD(i,j,k) = 0.0
   IF (f(JDIA) > 0.0) QND(i,j,k) = f(JDIAN) / f(JDIA) 
   IF (QND(i,j,k) > 0.0) NFD(i,j,k) = MAX(0.0, QND(i,j,k) - QminND) / QND(i,j,k)

!------------------------------------------------------------------------------
! Phosphorous limitation
!------------------------------------------------------------------------------
  QPD(i,j,k) = 0.0
  PFD(i,j,k) = 0.0
  IF (f(JDIA) > 0.0) QPD(i,j,k) = f(JDIAP) / f(JDIA)           
  IF (QPD(i,j,k) > 0.0) PFD(i,j,k) = MAX(0.0, QPD(i,j,k) - QminPD) / QPD(i,j,k)
        
!------------------------------------------------------------------------------
! Silica limitation
!------------------------------------------------------------------------------
   SA = f(JSA)               ! Available silica
   SFD(i,j,j) = SA / (KHSD + SA)              

!------------------------------------------------------------------------------
! Nutrient limitation 
!------------------------------------------------------------------------------
   IF (SILIM == 1) FN = SFD(i,j,k) * MIN(NFD(i,j,k),PFD(i,j,k))   
   IF (SILIM == 2) FN = MIN( NFD(i,j,k), PFD(i,j,k), SFD(i,j,k) )   
   IF (SILIM == 3) FN = SFD(i,j,k) * PFD(i,j,k)

!------------------------------------------------------------------------------
!  Production
!------------------------------------------------------------------------------
   PD(i,j,k) = PMD * FN * IFD(i,j,k) * TFD(i,j,k)               

!------------------------------------------------------------------------------
!  Set production to zero if it's negative
!------------------------------------------------------------------------------
   IF (PD(i,j,k) < 0.0) PD(i,j,k) = 0.0

!------------------------------------------------------------------------------
!  Update diatom time derivatives 
!------------------------------------------------------------------------------
   DTM(JDIA) = DTM(JDIA)                          &     
                  + (PD(i,j,k) - BMD(i,j,k)) * f(JDIA)  &
                  - PRD(i,j,k) 


!   IF (DO_PHYTO_PROCESSES) THEN
!------------------------------------------------------------------------------
!  Convert metabolism, production, and predation rates to mass terms
!  and add up mass terms over the time span of a writing interval.
!------------------------------------------------------------------------------
!!       BMD_AVG(myi,j,k) = BMD_AVG(myi,j,k) + BMD(i,j,k) * f(JDIA) * Vol * dT 
!!       PD_AVG(myi,j,k)  = PD_AVG(myi,j,k)  + PD(i,j,k) * f(JDIA) * Vol * dT
!       PRD_AVG(ISEG) = PRD_AVG(ISEG) + (PRD(ISEG) * V1(ISEG) * DLT)

!   ENDIF




!
!------------------------------------------------------------------------------

RETURN

END SUBROUTINE DIATOMS_DROOP
