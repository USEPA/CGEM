SUBROUTINE GREENS(f,DTM,TEMP,PAR,Vol,dT,i,j,k)
!------------------------------------------------------------------------------
!-
!-   $Id: greens.F90,v 1.0.6.1 2014/08/26 22:54:04 wmelende Exp wmelende $
!-   $Locker: wmelende $
!-
!-   Purpose and Methods : Greens kinetics calculations
!-
!-   Inputs  :   None
!-   Outputs :   None
!-   Controls:   Unknown
!-
!-   Calls:      REPORT
!-   Called by:  EUTRO
!-
!-   Created:    July 28, 1992
!-   Authors:    Carl F. Cerco      : Water quality scheme
!-               Raymond S. Chapman : Numerical solution scheme
!-               Thomas M. Cole     : Computer algorithms & coding
!-               Hydroqual          : Sediment compartment
!-
!-               Water Quality Modeling Group
!-               U.S. Army Corps of Engineers
!-               Waterways Experiment Station
!-               Vicksburg, Mississippi 39180
!-
!------------------------------------------------------------------------------
!

USE Model_dim
USE EUT
USE FLAGS, ONLY: SILIM
USE STATES

IMPLICIT NONE

REAL, INTENT(IN) :: f(nf),Vol
REAL, INTENT(IN) :: TEMP,PAR, dT
REAL, INTENT(INOUT) :: DTM(nf)
INTEGER, INTENT(IN) :: i,j,k


REAL :: FN               ! Nutrient limitation factor
REAL :: PO4AVL           ! Available phosphate
REAL :: NDIS             ! Dissolved nitrogen
!
!------------------------------------------------------------------------------
!

!------------------------------------------------------------------------------
!  Greens metabolism rate
!------------------------------------------------------------------------------
BMG(i,j,k) = BMRG * EXP(KTBG * (TEMP - TRG))


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
   IFG(i,j,k)  = TANH (ALPHA_GRE * PAR / PBMAX_GRE)


!------------------------------------------------------------------------------
! Nutrient limitations
!------------------------------------------------------------------------------

!-------------------------------------
! Dissolved phosphorous
!-------------------------------------
   PO4AVL = f(JSRP) + AVFRAC * f(JDOP)   

!-------------------------------------
! Dissolved nitrogen
!-------------------------------------
   NDIS = f(JNH4) + f(JNO3) + AVFRACDON *  f(JDON) 

!-------------------------------------
! Phosphorous limitation
!-------------------------------------
   PFG(i,j,k) = PO4AVL / (KHPG + PO4AVL)             

!-------------------------------------
! Nitrogen limitation
!-------------------------------------
   IF (NDIS > 1.0E-20) THEN
       NFG(i,j,k) = NDIS / (KHNG + NDIS)            
   ELSE
       NFG(i,j,k) = 0.0
   ENDIF



!------------------------------------------------------------------------------
!  Temperature and nutrient limitations
!------------------------------------------------------------------------------
   IF (TEMP < TMG) THEN
       TFG(i,j,k) = EXP(-KTGG1 * (TEMP - TMG)**2)                       
   ELSE
       TFG(i,j,k) = EXP(-KTGG2 * (TMG - TEMP)**2)
   ENDIF
   IF (SILIM == 3) THEN 
       FN = PFG(i,j,k)                      ! Phosphorus limitation only
   ELSE
       FN = MIN(NFG(i,j,k),PFG(i,j,k))       ! Nutrient limitation
   ENDIF
   PG(i,j,k) = PMG * FN * IFG(i,j,k) * TFG(i,j,k)      ! Production
   
   IF (PG(i,j,k) < 0.0) PG(i,j,k) = 0.0


!------------------------------------------------------------------------------
!  Update green algae time derivative
!------------------------------------------------------------------------------
   DTM(JGRE) = DTM(JGRE) + (PG(i,j,k) - BMG(i,j,k)) * f(JGRE) - PRG(i,j,k)                  

   PG_AVG(i,j,k)  = PG_AVG(i,j,k)  + PG(i,j,k) *  f(JGRE) * Vol * real(dT,4)

!
!------------------------------------------------------------------------------
!

RETURN

END SUBROUTINE GREENS

