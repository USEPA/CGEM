SUBROUTINE EUT_allocate
!------------------------------------------------------------------------------
!-
!-  $Id: eut.F90,v 1.0.6.1 2014/08/26 22:54:04 wmelende Exp $
!-  $Locker:  $
!-
!-  Purpose and Methods : Include file for eutrophication variables.
!-
!-  Created: 12/09/97  M. Settles
!-
!- Revised: 07/23/14  W. Melendez :    Added C_S0D_FLUX_MAX, SED_NO3_FLUX_MIN,
!-                                     and KHDENITR_SED parematers.
!- Revised: 03/04/14  W. Melendez :    Updated default values of DENIT_CN_RATIO,
!-                                     KHDONT_SED, KHODOC_SED, and RSODNTR.
!- Revised: 02/03/14  W. Melendez :    Added NH4WSED array. Added 
!-                                     KHODOC_SED, KHDONT_SED, RSODNTR,
!-                                     and DENIT_CN_RATIO parameters.
!- Revised: 09/12/13  W. Melendez :    Added DAILY_PAR output variable.
!- Revised: 06/26/13  W. Melendez :    Added declarations of arrays DOCBMDLDO2,
!-                                     DOCBMGLDO2, DOCPRDLDO2, DOCPRGLDO2,
!-                                     and DOCZOOLDO2.
!- Revised: 04/30/13  W. Melendez :    Added declaration of ZOOMORT array.
!- Revised: 04/30/13  W. Melendez :    Added declaration of DOCMINRL array.
!- Revised: 04/25/13  W. Melendez :    Added declarations of the
!-                                     DENITR and NO3WSED arrays.
!- Revised: 02/28/13  W. Melendez :    Added AVFRACDON variable -->
!-                                     this is the fraction of DON
!-                                     available to phytoplankton.
!- Revised: 01/25/13  W. Melendez :    Added new arrays that are used
!-                                     to store dissoved oxygen kinetic
!-                                     processes.
!- Revised: 12/19/12  W. Melendez :    Added SOD reference temperature
!-                                     variable TREF.
!- Revised: 06/27/12  W. Melendez :    Added the following new arrays:
!-                                     BMD_AVG, BMG_AVG, PD_AVG, PG_AVG,
!-                                     PRD_AVG, and PRG_AVG.
!- Revised: 04/12/12  W. Melendez :    Removed ISMIN, KE, and KECHL parameters.
!- Revised: 04/10/12  W. Melendez :    Updated documentation.
!- Revised: 04/10/12  W. Melendez :    Added light limitation related 
!-                                     parameters:  ALPHA_DIA,  ALPHA_GRE,
!-                                     PBMAX_DIA, and PBMAX_GRE. Added
!-                                     segment-depth averaged irradiance
!-                                     array IAVG. Removed parameters
!-                                     I0WT, I1WT, I2WT, and ILUM0.
!-                                     Removed parameters ATMP1, ATMP2, 
!-                                     DOPTD, and DOPTG. Removed arrays
!-                                     I1 and I2.
!- Revised: 08/04/10  W. Melendez :    Set default value of KREA_VEL_DO to
!-                                     1.1574E-05 m/s.
!- Revised: 02/02/10  W. Melendez :    Added parameter KREA_VEL_DO.
!- Revised: 01/25/10  W. Melendez :    Added variables KCOD and RNTO.
!- Revised: 01/25/10  W. Melendez :    Added denitrification-related
!-                                     parameters (and thus D.O. related 
!-                                     as well): KDENITR and KHDENITR.
!-                                     Added declaration of array KRDO.
!- Revised: 09/17/09  W. Melendez :    Added dissolved oxygen related 
!-                                     parameters: KHDONT, KHODOC, KHOCOD, 
!-                                     and RCDO.  
!- Revised:  06/22/09  W. Melendez  : ----> GOMDOM. This application is based
!-                                     on LM3-Eutro (LM3 version 3.2.13) and 
!-                                     its purpose is to serve as a dissolved
!-                                     oxygen model for the Gulf of Mexico
!-                                     hypoxia project. NetCDF is the library 
!-                                     used to handle input/output binary
!-                                     files.
!- Revised: 05/10/01  W. Melendez   :  Added the parameter AVFRAC.
!- Revised: 03/12/01  W. Melendez   :  Added four new green/diatom metabolism
!-                                     parameters.  They are: FCLD, FCLG, FCRD,
!-                                     and FCRG.  They refer to labile and 
!-                                     refractory particulate organic carbon 
!-                                     produced by diatoms and greens due to 
!-                                     metobolic and/or mortality rate.
!- Revised: 08/16/00  W. Melendez   :  Remove declarations for RFD, I0AVG,
!-                                     FSAZ, and FSUZ. Remove commented out
!-                                     line of declaration for FD.
!- Revised: 08/09/00  W. Melendez   :  Comment out declaration for FD
!- Revised: 06/09/00  M. Settles    :  Add phytoplankton productivity arrays
!- Revised: 01/06/00  M. Settles    :  Changes to default parameter values 
!- Revised: 12/21/99  M. Settles    :  Changes to mineralization and hydrolysis
!-                                     kinetics parameter default values
!- Revised: 12/17/99  M. Settles    :  Restore parameters to support Minod
!-                                     dependent calculation of carbon, 
!-                                     nitrogen, and phosphorous kinetics.
!- Revised: 09/14/99  M. Settles    :  New zooplankton grazing coefficient CGZ
!- Revised: 08/20/99  M. Settles    :  New default parameter values KTGD1, 
!-                                     KTGG1
!- Revised: 08/13/99  M. Settles    :  Remove diatom/green predation parameters
!- Revised: 07/14/99  M. Settles    :  PARTICLE flag allocated
!- Revised: 06/15/99  M. Settles    :  Add growth limitation factors
!- Revised: 06/04/99  M. Settles    :  Add spatially variable illumination
!- Revised: 05/07/99  M. Settles    :  Add ILMUL, illumination scale factor
!- Revised; 04/13/99  M. Settles    :  Change default value KECHL
!- Revised: 04/10/99  M. Settles    :  Update/correct eutro. parameters
!- Revised: 03/15/99  M. Settles    :  Update/correct eutro. parameters
!- Revised: 03/01/99  M. Settles    :  Misc. bug fixes
!- Revised: 01/15/99  M. Settles    :  Dynamically allocated arrays
!- Revised: 01/15/99  M. Settles    :  Number of boxes is NB
!- Revised: 03/05/98  M. Settles    :  Add eutro. parameters from knetic.inc
!- Revised: 03/03/98  M. Settles    :  Add several phyto. parameters
!-
!------------------------------------------------------------------------------

USE Model_dim 
USE EUT

IMPLICIT NONE

!------------------------------------------------------------------------------
!  Miscellaneous arrays/variables; not parameters
!------------------------------------------------------------------------------
  
ALLOCATE(BMD(im,jm,nsl))        ! Base metabolic rate for diatoms

ALLOCATE(BMD_AVG(im,jm,nsl))       ! Average base metabolic rate 
                                              ! for diatoms

ALLOCATE(BMG(im,jm,nsl))        ! Base metabolic rate for greens

ALLOCATE(BMG_AVG(im,jm,nsl))       ! Average base metabolic rate 
                                              ! for greens

ALLOCATE(FTHDR(im,jm,nsl))        ! Temperature correction for hydrolysis

ALLOCATE(FTMNL(im,jm,nsl))        ! Temperature correction for mineralization

ALLOCATE(IAVG(im,jm,nsl))        ! Segment-depth averaged irradiance

ALLOCATE(I0(im,jm,nsl))        ! Current light intensity

ALLOCATE(SWR(im,jm,nsl))        ! Shortwave radiation (illumination)

ALLOCATE(KESS(im,jm,nsl))        ! Light attenuation factor

ALLOCATE(DAILY_PAR(im,jm,nsl))     ! Daily cumulative PAR

ALLOCATE(PARTICLE(im,jm,nsl))   ! Particle/dissolved status flags

ALLOCATE(PD(im,jm,nsl))        ! Production rates for diatoms
PD = 0.
ALLOCATE(PD_AVG(im,jm,nsl))        ! Average production rates for 
PD_AVG = 0.                                              ! diatoms

ALLOCATE(PG(im,jm,nsl))        ! Production rates for greens
PG = 0.
ALLOCATE(PG_AVG(im,jm,nsl))        ! Average production rates for 
PG_AVG = 0.                                              ! greens

ALLOCATE(PPC(im,jm,nsl))        ! Level 2 phytoplankton production rates

ALLOCATE(WKFTIM(im,jm,nsl))        ! Phytoplankton productivity times

ALLOCATE(PRD(im,jm,nsl))        ! Predation time derivative for diatoms

ALLOCATE(PRD_AVG(im,jm,nsl))       ! Average predation time derivative
                                              ! for diatoms

ALLOCATE(PRG(im,jm,nsl))        ! Predation time derivative for greens

ALLOCATE(PRG_AVG(im,jm,nsl))       ! Average predation time derivative
                                              ! for greens

ALLOCATE(KRDO(im,jm,nsl))        ! D.O. reaeration coefficient [m/s] 

ALLOCATE(DENITR(im,jm,nsl))       ! Denitrification cumulative mass

ALLOCATE(NO3WSED(im,jm,nsl))       ! Water-sediment NO3 flux cumulative mass

ALLOCATE(NH4WSED(im,jm,nsl))     ! Water-sediment NH4 flux cumulative mass

ALLOCATE(DOCMINRL(im,jm,nsl))     ! DOC mineralization cumulative mass

ALLOCATE(ZOOMORT(im,jm,nsl))     ! C released from zooplankton mortality

ALLOCATE(DOCBMDLDO2(im,jm,nsl))       ! C generated by BMD at low D.O.

ALLOCATE(DOCBMGLDO2(im,jm,nsl))        ! C generated by BMG at low D.O.

ALLOCATE(DOCPRDLDO2(im,jm,nsl))        ! C generated by PRD at low D.O.

ALLOCATE(DOCPRGLDO2(im,jm,nsl))        ! C generated by PRG at low D.O.

ALLOCATE(DOCZOOLDO2(im,jm,nsl))        ! C generated by zoo's mortality at low D.O.


!------------------------------------------------------------------------------
!  Fine segmentation growth limitation factors
!------------------------------------------------------------------------------

ALLOCATE(PFD(im,jm,nsl))     ! Phosphorous limitation for diatoms

ALLOCATE(SFD(im,jm,nsl))     ! Silica limitation for diatoms

ALLOCATE(NFD(im,jm,nsl))     ! Nitrogen limitation for diatoms

ALLOCATE(IFD(im,jm,nsl))     ! Light limitation for diatoms

ALLOCATE(TFD(im,jm,nsl))     ! Temperature limitation for diatoms

ALLOCATE(PFG(im,jm,nsl))     ! Phosphorous limitation for greens

ALLOCATE(NFG(im,jm,nsl))     ! Nitrogen limitation for greens

ALLOCATE(IFG(im,jm,nsl))     ! Light limitation for greens

ALLOCATE(TFG(im,jm,nsl))     ! Temperature limitation for greens

!Droop
ALLOCATE(QND(im,jm,nsl))  
ALLOCATE(QPD(im,jm,nsl))  
ALLOCATE(QNG(im,jm,nsl))  
ALLOCATE(QPG(im,jm,nsl))  

!------------------------------------------------------------------------------
!  Dissolved Oxygen arrays used to store kinetic processes.
!------------------------------------------------------------------------------
ALLOCATE(DOPRODD_AVG(im,jm,nsl))
ALLOCATE(DOPRODG_AVG(im,jm,nsl))
ALLOCATE(NITDO2(im,jm,nsl)) 
ALLOCATE(DOMETD_ARR(im,jm,nsl)) 
ALLOCATE(DOMETG_ARR(im,jm,nsl))
ALLOCATE(DOPREDD_ARR(im,jm,nsl)) 
ALLOCATE(DOPREDG_ARR(im,jm,nsl))
ALLOCATE(DOZOO_ARR(im,jm,nsl)) 
ALLOCATE(DOMNLDOC_ARR(im,jm,nsl)) 
ALLOCATE(DOCOD_ARR(im,jm,nsl)) 
ALLOCATE(DOREAR_ARR(im,jm,nsl)) 
ALLOCATE(DOSOC_ARR(im,jm,nsl)) 
 
DOMETD_ARR = 0.;
DOMETG_ARR = 0.
DOPREDD_ARR = 0.
DOPREDG_ARR = 0.
DOZOO_ARR = 0.
DOMNLDOC_ARR = 0.
NITDO2 = 0.

!
!------------------------------------------------------------------------------
!

END SUBROUTINE EUT_allocate
