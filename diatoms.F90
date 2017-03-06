SUBROUTINE DIATOMS(f,DTM,TEMP,IOPpar,Vol,dT,i,j,k)
!------------------------------------------------------------------------------
!-
!-   $Id: diatoms.F90,v 1.0.6.1 2014/08/26 22:54:04 wmelende Exp wmelende $
!-   $Locker: wmelende $
!-
!-   Purpose and Methods : Diatoms kinetics calculations
!-
!-   Inputs  :   None
!-   Outputs :   None
!-   Controls:   Unknown
!-
!-   Calls:      None
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
!-
!------------------------------------------------------------------------------
!

USE Model_dim
USE EUT
USE FLAGS, ONLY: SILIM
USE STATES
USE INPUT_VARS_GD, ONLY : Read_Solar

IMPLICIT NONE


REAL, INTENT(IN) :: f(nf),Vol
REAL, INTENT(IN) :: TEMP,IOPpar
REAL, INTENT(INOUT) :: DTM(nf)
INTEGER, INTENT(IN) :: i,j,k,dT

REAL :: FN  ! Nutrient limitation factor

REAL :: PO4AVL            ! Dissolved phosphorous
REAL :: SA                ! Available silica
REAL :: PAR               ! Photosynthetic active radiation
REAL :: NDIS              ! Dissolved nitrogen

!
!------------------------------------------------------------------------------
!

!------------------------------------------------------------------------------
!  Diatom metabolism
!------------------------------------------------------------------------------
BMD(i,j,k) = BMRD * EXP(KTBD * (TEMP - TRD))                           

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
   if(Read_Solar.eq.2) PAR = IOPpar
   !write(6,*) "PAR",PAR

   IFD(i,j,k) = TANH (ALPHA_DIA * PAR / PBMAX_DIA)

!------------------------------------------------------------------------------
!  Nutrient limitations
!------------------------------------------------------------------------------

!-------------------------------------
! Total phosphate
!-------------------------------------
   PO4AVL = f(JSRP) + AVFRAC * f(JDOP)   

!-------------------------------------
! Available silica
!-------------------------------------
   SA = f(JSA)                              

!-------------------------------------
! Dissolved mitrogen
!-------------------------------------
   NDIS = f(JNH4) + f(JNO3) + AVFRACDON *  f(JDON)

!-------------------------------------
! Phosphorous limitation
!-------------------------------------
   PFD(i,j,k) = PO4AVL / (KHPD + PO4AVL)            

!-------------------------------------
! Silica limitation
!-------------------------------------
   SFD(i,j,k) = SA / (KHSD + SA)              

!-------------------------------------
! Nitrogen limitation
!-------------------------------------
   IF (NDIS > 1.0E-20) THEN
       NFD(i,j,k) = NDIS / (KHND + NDIS)            
   ELSE
       NFD(i,j,k) = 0.0
   ENDIF


!------------------------------------------------------------------------------
!  Temperature limitation
!------------------------------------------------------------------------------
   IF (TEMP < TMD) THEN
      TFD(i,j,k) = EXP(-KTGD1 * (TEMP - TMD)**2)     
   ELSE
      TFD(i,j,k) = EXP(-KTGD2 * (TMD - TEMP)**2)
   ENDIF

!------------------------------------------------------------------------------
! Nutrient limitation 
!------------------------------------------------------------------------------
IF (SILIM == 1) FN = SFD(i,j,k) * MIN(NFD(i,j,k),PFD(i,j,k))   
IF (SILIM == 2) FN = MIN(NFD(i,j,k),SFD(i,j,k),PFD(i,j,k))   
IF (SILIM == 3) FN = SFD(i,j,k) * PFD(i,j,k)

!------------------------------------------------------------------------------
!  Production
!------------------------------------------------------------------------------
PD(i,j,k) = PMD * FN * IFD(i,j,k) * TFD(i,j,k)               
!write(6,*) PD(i,j,k),PMD,FN,IFD(i,j,k),TFD(i,j,k)
!------------------------------------------------------------------------------
!  Set production to zero if it's negative
!------------------------------------------------------------------------------
   IF (PD(i,j,k) < 0.0) PD(i,j,k) = 0.0


!------------------------------------------------------------------------------
!  Update diatom time derivatives 
!------------------------------------------------------------------------------
   DTM(JDIA) = DTM(JDIA) + (PD(i,j,k) - BMD(i,j,k)) * f(JDIA) - PRD(i,j,k)     

   PD_AVG(i,j,k)  = PD_AVG(i,j,k)  + PD(i,j,k) * f(JDIA) * Vol * real(dT,4)


!     write(6,*)  PD(i,j,k),PD_AVG(i,j,k),add
!    write(6,*) PD(i,j,k)* f(JDIA) * Vol * real(dT,4),PD_AVG(i,j,k)



!
!------------------------------------------------------------------------------
!

RETURN

END SUBROUTINE DIATOMS

