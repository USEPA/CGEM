MODULE EUT
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


IMPLICIT NONE

!------------------------------------------------------------------------------
!  Miscellaneous arrays/variables; not parameters
!------------------------------------------------------------------------------
  
REAL,ALLOCATABLE :: BMD(:,:,:)          ! Base metabolic rate for diatoms

REAL,ALLOCATABLE :: BMD_AVG(:,:,:)      ! Average base metabolic rate 
                                              ! for diatoms

REAL,ALLOCATABLE :: BMG(:,:,:)          ! Base metabolic rate for greens

REAL,ALLOCATABLE :: BMG_AVG(:,:,:)      ! Average base metabolic rate 
                                              ! for greens

REAL,ALLOCATABLE :: FTHDR(:,:,:)        ! Temperature correction for hydrolysis

REAL,ALLOCATABLE :: FTMNL(:,:,:)        ! Temperature correction for mineralization

REAL,ALLOCATABLE :: IAVG(:,:,:)         ! Segment-depth averaged irradiance

REAL,ALLOCATABLE :: I0(:,:,:)           ! Current light intensity

REAL,ALLOCATABLE :: SWR(:,:,:)          ! Shortwave radiation (illumination)

REAL,ALLOCATABLE :: KESS(:,:,:)         ! Light attenuation factor

REAL,ALLOCATABLE :: DAILY_PAR(:,:,:)    ! Daily cumulative PAR

INTEGER,ALLOCATABLE :: PARTICLE(:,:,:)  ! Particle/dissolved status flags

REAL,ALLOCATABLE :: PD(:,:,:)           ! Production rates for diatoms

REAL,ALLOCATABLE :: PD_AVG(:,:,:)       ! Average production rates for 
                                              ! diatoms

REAL,ALLOCATABLE :: PG(:,:,:)           ! Production rates for greens

REAL,ALLOCATABLE :: PG_AVG(:,:,:)       ! Average production rates for 
                                              ! greens

REAL,ALLOCATABLE :: PPC(:,:,:)          ! Level 2 phytoplankton production rates

REAL,ALLOCATABLE :: WKFTIM(:,:,:)       ! Phytoplankton productivity times

REAL,ALLOCATABLE :: PRD(:,:,:)          ! Predation time derivative for diatoms

REAL,ALLOCATABLE :: PRD_AVG(:,:,:)      ! Average predation time derivative
                                              ! for diatoms

REAL,ALLOCATABLE :: PRG(:,:,:)          ! Predation time derivative for greens

REAL,ALLOCATABLE :: PRG_AVG(:,:,:)      ! Average predation time derivative
                                              ! for greens

REAL,ALLOCATABLE :: KRDO(:,:,:)         ! D.O. reaeration coefficient [m/s] 

REAL,ALLOCATABLE :: DENITR(:,:,:)       ! Denitrification cumulative mass

REAL,ALLOCATABLE :: NO3WSED(:,:,:)      ! Water-sediment NO3 flux cumulative mass

REAL,ALLOCATABLE :: NH4WSED(:,:,:)      ! Water-sediment NH4 flux cumulative mass

REAL,ALLOCATABLE :: DOCMINRL(:,:,:)     ! DOC mineralization cumulative mass

REAL,ALLOCATABLE :: ZOOMORT(:,:,:)      ! C released from zooplankton mortality

REAL,ALLOCATABLE :: DOCBMDLDO2(:,:,:)       ! C generated by BMD at low D.O.

REAL,ALLOCATABLE :: DOCBMGLDO2(:,:,:)       ! C generated by BMG at low D.O.

REAL,ALLOCATABLE :: DOCPRDLDO2(:,:,:)       ! C generated by PRD at low D.O.

REAL,ALLOCATABLE :: DOCPRGLDO2(:,:,:)       ! C generated by PRG at low D.O.

REAL,ALLOCATABLE :: DOCZOOLDO2(:,:,:)       ! C generated by zoo's mortality at low D.O.


!------------------------------------------------------------------------------
!  Fine segmentation growth limitation factors
!------------------------------------------------------------------------------

REAL,ALLOCATABLE :: PFD(:,:,:)       ! Phosphorous limitation for diatoms

REAL,ALLOCATABLE :: SFD(:,:,:)       ! Silica limitation for diatoms

REAL,ALLOCATABLE :: NFD(:,:,:)       ! Nitrogen limitation for diatoms

REAL,ALLOCATABLE :: IFD(:,:,:)       ! Light limitation for diatoms

REAL,ALLOCATABLE :: TFD(:,:,:)       ! Temperature limitation for diatoms


REAL,ALLOCATABLE :: PFG(:,:,:)       ! Phosphorous limitation for greens

REAL,ALLOCATABLE :: NFG(:,:,:)       ! Nitrogen limitation for greens

REAL,ALLOCATABLE :: IFG(:,:,:)       ! Light limitation for greens

REAL,ALLOCATABLE :: TFG(:,:,:)       ! Temperature limitation for greens

!New for Droop
!diatoms
REAL,ALLOCATABLE :: QND(:,:,:)       !
REAL,ALLOCATABLE :: QPD(:,:,:)       !
REAL :: QminND = 0.003
REAL :: QminPD = 0.003
!greens
REAL,ALLOCATABLE :: QNG(:,:,:)       !
REAL,ALLOCATABLE :: QPG(:,:,:)       !
REAL :: QminNG = 0.003
REAL :: QminPG = 0.003
!nitrog
REAL :: QmaxND = 1.0
REAL :: QmaxNG = 1.0
REAL :: QmaxPD = 1.0
REAL :: QmaxPG = 1.0
REAL :: KhIntND = 2.0E-05
REAL :: KhIntNG = 2.0E-05
REAL :: KhIntPD = 2.0E-05
REAL :: KhIntPG = 2.0E-05
!eut.F90
REAL :: FIntNID = 0.0     ! Fraction of inorganic nitrogen produced by algal metabolism
REAL :: FIntNIG = 0.0     ! Fraction of inorganic nitrogen produced by algal metabolism
REAL :: FIntNDD = 0.0     ! Fraction of dissolved nitrogen produced by algal metabolism
REAL :: FIntNDG = 0.0     ! Fraction of dissolved nitrogen produced by algal metabolism
REAL :: FIntNLD = 0.0     ! Fraction of labile nitrogen produced by algal metabolism
REAL :: FIntNLG = 0.0     ! Fraction of labile nitrogen produced by algal metabolism
REAL :: FIntNRD = 0.0     ! Fraction of refractory nitrogen produced by algal metabolism
REAL :: FIntNRG = 0.0     ! Fraction of refractory nitrogen produced by algal metabolism
REAL :: FIntPID = 0.0     ! Fraction of inorganic phosphorus produced by algal metabolism
REAL :: FIntPIG = 0.0     ! Fraction of inorganic phosphorus produced by algal metabolism
REAL :: FIntPDD = 0.0     ! Fraction of dissolved phosphorus produced by algal metabolism
REAL :: FIntPDG = 0.0     ! Fraction of dissolved phosphorus produced by algal metabolism
REAL :: FIntPLD = 0.0     ! Fraction of labile phosphorus produced by algal metabolism
REAL :: FIntPLG = 0.0     ! Fraction of labile phosphorus produced by algal metabolism
REAL :: FIntPRD = 0.0     ! Fraction of refractory phosphorus produced by algal metabolism
REAL :: FIntPRG = 0.0     ! Fraction of refractory phosphorus produced by algal metabolism
REAL :: FIntLuxNIP = 0.0  ! Fraction of luxury inorganic nitrogen produced by predation
REAL :: FIntLuxNDP = 0.0  ! Fraction of luxury dissolved organic nitrogen produced by predation
REAL :: FIntLuxNLP = 0.0  ! Fraction of luxury labile nitrogen produced by predation
REAL :: FIntLuxNRP = 0.0  ! Fraction of luxury refractory nitrogen produced by predation
REAL :: FIntStrNIP = 0.0  ! Fraction of structural inorganic nitrogen produced by predation
REAL :: FIntStrNDP = 0.0  ! Fraction of structural dissolved organic nitrogen produced by predation
REAL :: FIntStrNLP = 0.0  ! Fraction of structural labile nitrogen produced by predation
REAL :: FIntStrNRP = 0.0  ! Fraction of structural refractory nitrogen produced by predation
REAL :: FIntLuxPIP = 0.0  ! Fraction of luxury inorganic phosphorus produced by predation
REAL :: FIntLuxPDP = 0.0  ! Fraction of luxury dissolved organic phosphorus produced by predation
REAL :: FIntLuxPLP = 0.0  ! Fraction of luxury labile phosphorus produced by predation
REAL :: FIntLuxPRP = 0.0  ! Fraction of luxury refractory phosphorus produced by predation
REAL :: FIntStrPIP = 0.0  ! Fraction of structural inorganic phosphorus produced by predation
REAL :: FIntStrPDP = 0.0  ! Fraction of structural dissolved organic phosphorus produced by predation
REAL :: FIntStrPLP = 0.0  ! Fraction of structural labile phosphorus produced by predation
REAL :: FIntStrPRP = 0.0  ! Fraction of structural refractory phosphorus produced by predation
!------------------------------------------------------------------------------
! Maximum uptake rate of nitrogen by diatoms.
! Units: g N / (g algal C) / s
!------------------------------------------------------------------------------
REAL :: UpNMaxD = 2.315E-06

!------------------------------------------------------------------------------
! Maximum uptake rate of nitrogen by greens.
! Units: g N / (g algal C) / s
!------------------------------------------------------------------------------
REAL :: UpNMaxG = 2.315E-06

!------------------------------------------------------------------------------
! Maximum uptake rate of phosphorus by diatoms.
! Units: g P / (g algal C) / s
!------------------------------------------------------------------------------
REAL :: UpPMaxD = 2.315E-06

!------------------------------------------------------------------------------
! Maximum uptake rate of phosphorus by greens.
! Units: g P / (g algal C) / s
!------------------------------------------------------------------------------
REAL :: UpPMaxG = 2.315E-06



!------------------------------------------------------------------------------
!  Eutro parameter declarations
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! ALPHA_DIA = slope of diatoms light saturation curve at low light levels.
!             Its units are gC [gChl-a]^-1 h^-1 [umol quanta/(m^2 s)]^-1.
!------------------------------------------------------------------------------
REAL :: ALPHA_DIA = 0.035

!------------------------------------------------------------------------------
! ALPHA_GRE = slope of greens light saturation curve at low light levels.
!             Its units are gC [gChl-a]^-1 h^-1 [umol quanta/(m^2 s)]^-1.
!------------------------------------------------------------------------------
REAL :: ALPHA_GRE = 0.035

REAL :: ANCP                         ! N:C ratio for plankton [-]
                                              
REAL :: APCP                         ! P:C ratio for plankton [-]
                                              
REAL :: ASCD                         ! Si:C ratio for diatoms [-]
                                              
REAL :: AVFRAC                       ! Fraction of DOP available
                                              ! to phytoplankton.

!------------------------------------------------------------------------------
! Fraction of DON available to phytoplankton.
!------------------------------------------------------------------------------
REAL :: AVFRACDON = 0.05
                                              

REAL :: BMRD                        ! Diatom base metabolic rates [s^-1]
                                              
REAL :: BMRG                        ! Green base metabolic rates [s^-1]
                                              
REAL :: CCHLD                       ! Carbon:chlorophyll ratio for diatoms [-]
                                              
REAL :: CCHLG                       ! Carbon:chlorophyll ratio for green algae [-]
                                              
REAL :: CGZ                         ! Zooplankton grazing coefficient [sec^-1]

REAL :: FCDD                        ! Fraction of basal metabolism exuded
                                              ! as dissolved organic carbon by diatoms [-] 

REAL :: FCDG                        ! Fraction of basal metabolism exuded
                                              ! as dissolved organic carbon by greens [-]

REAL :: FCDP                        ! Fraction of dissolved organic carbon
                                              ! produced by predation [-]

REAL :: FCDZ                        ! Fraction of DOC from 
                                              ! zooplankton mortality [-]

REAL :: FCLD                        ! Fraction of labile particulate organic
                                              ! carbon produced by diatoms metabolism.

REAL :: FCLG                        ! Fraction of labile particulate organic
                                              ! carbon produced by greens metabolism.

REAL :: FCLP                        ! Fraction of labile pdc from predation [-]

REAL :: FCLZ                        ! Fraction of labile pdc from 
                                              ! zooplankton mortality [-]

REAL :: FCRD                        ! Fraction of refractory particulate organic
                                              ! carbon produced by diatoms metabolism.

REAL :: FCRG                        ! Fraction of refractory particulate organic
                                              ! carbon produced by greens metabolism.

REAL :: FCRP                        ! Fraction of refractory pdc from
                                              ! predation [-]

REAL :: FCRZ                        ! Fraction of refractory pdc from 
                                              ! zooplankton mortality [-]

REAL :: FNDD                        ! Fraction dissolved organic nitrogen
                                              ! produced by diatom metabolism [-]

REAL :: FNDG                        ! Fraction dissolved organic nitrogen
                                              ! produced by green algae metabolism [-]

REAL :: FNDP                        ! Fraction of dissolved organic
                                              ! nitrogen produced by predation [-]              

REAL :: FNDZ                        ! Fraction of dissolved organic
                                              ! nitrogen produced by zoo. mortality [-]              

REAL :: FNID                        ! Fraction of inorganic nitrogren
                                              ! produced by diatom metabolism [-]

REAL :: FNIG                        ! Fraction of inorganic nitrogren
                                              ! produced by green algae metabolism [-]

REAL :: FNIP                        ! Fraction of inorganic nitrogen
                                              ! produced by predation [-]

REAL :: FNIZ                        ! Fraction of inorganic nitrogen
                                              ! produced by zoo. mortality [-]

REAL :: FNLD                        ! Fraction labile particulate nitrogen
                                              ! produced by diatom metabolism [-]

REAL :: FNLG                        ! Fraction labile particulate nitrogen
                                              ! produced by green algae metabolism [-]

REAL :: FNLP                        ! Fraction of labile organic
                                              ! nitrogen produced by predation [-]             

REAL :: FNLZ                        ! Fraction of labile organic
                                              ! nitrogen produced by zoo. mortality [-]             

REAL :: FNRD                        ! Fraction refractory particulate nitrogen
                                              ! produced by diatom metabolism [-]

REAL :: FNRG                        ! Fraction refractory particulate nitrogen
                                              ! produced by green algae metabolism [-]

REAL :: FNRP                        ! Fraction of refractory organic
                                              ! nitrogen produced by predation [-]            

REAL :: FNRZ                        ! Fraction of refractory organic nitrogen
                                              ! produced by zooplankton mortality [-]

REAL :: FPDD                        ! Fraction dissolved organic phosphorous
                                              ! produced by diatom metabolism [-]

REAL :: FPDG                        ! Fraction dissolved organic phosphorous
                                              ! produced by green metabolism [-]

REAL :: FPDP                        ! Fraction of dissolved organic phosphorous
                                              ! produced by predation [-]

REAL :: FPDZ                        ! Fraction of dissolved organic
                                              ! phosphorous produced by zoo. mortality [-]

REAL :: FPID                        ! Fraction of inorganic phosphorous
                                              ! produced by diatom metabolism [-]

REAL :: FPIG                        ! Fraction of inorganic phosphorous
                                              ! produced by green metabolism [-]

REAL :: FPIP                        ! Fraction of inorganic phosphorous
                                              ! produced by diatom metabolism [-]

REAL :: FPIZ                        ! Fraction of inorganic phosphorous
                                              ! produced by zooplankton mortality [-]

REAL :: FPLD                        ! Fraction of labile organic
                                              ! phosphorous produced by diatom 
                                              ! metabolism [-]

REAL :: FPLG                        ! Fraction of labile organic
                                              ! phosphorous produced by green 
                                              ! algae metabolism [-]

REAL :: FPLP                        ! Fraction of labile organic
                                              ! phosphorous produced by predation [-]

REAL :: FPLZ                        ! Fraction of labile organic phosphorous
                                              ! produced by zoo. mortality [-]

REAL :: FPRD                        ! Fraction of refractory organic
                                              ! phosphorous produced by diatom metabolism [-]

REAL :: FPRG                        ! Fraction of refractory organic
                                              ! phosphorous produced by greeen 
                                              ! algae metabolism [-]

REAL :: FPRP                        ! Fraction of refractory organic
                                              ! phosphorous produced by predation [-]

REAL :: FPRZ                        ! Fraction of refractory organic
                                              ! phosphorous produced by zoo. mortality [-]

REAL :: FSAP                        ! Fraction of silica made available
                                              ! through predation [-]

REAL :: GREFF                       ! Zooplankton grazing efficiency [-]

REAL :: ILMUL                       ! Scaling for surface illumination [-]

REAL :: KDC                         ! Minimum DOC mineralization rate [1/sec]

REAL :: KDCALG                      ! Mineralization algal dependence factor [m^3-kg^-1-s^1]

REAL :: KDN                         ! Minimum dissolved organic nitrogen
                                              ! mineralization rate [s^-1]

REAL :: KDNALG                      ! Dissolved organic nitrogen mineralization
                                              ! algal proportionality dependence [m^3-kg^-1-s^-1]

REAL :: KDP                         ! Minimum dissolved organic phosphorous
                                              ! mineralization rate [s^-1]

REAL :: KDPALG                      ! Dissolved phosphorous mineralization
                                              ! algal proportionality dependence [m^3-kg^-1-s^-1] 


REAL :: KHN                         ! Organic nitrogen decay
                                              ! half saturation constant [kg-m^-3]

REAL :: KHND                        ! Mean half-saturation constant
                                              ! for nitrogen uptake by diatoms [kg-m^-3]

REAL :: KHNG                        ! Mean half-saturation constant
                                              ! for nitrogen uptake by greens [kg-m^-3]

REAL :: KHNNT                       ! Half saturation concentration of NH4
                                              ! required for nitrification [kg-m^-3]

REAL :: KHP                         ! Organic phosphorous decay
                                              ! half saturation constant [kg-m^-3]

REAL :: KHPD                        ! Mean half-saturation constant
                                              ! for algal phosphorous uptake [kg-m^-3]

REAL :: KHPG                        ! Mean half-saturation constant
                                              ! for algal phosphorous uptake [kg-m^-3] 

REAL :: KHSD                        ! Mean half-saturation constant
                                              ! for diatom silica uptake [kg-m^-3]

REAL :: KLC                         ! Min. hydrolysis rate of labile PDC [s^-1]

REAL :: KLCALG                      ! Labile hydrol. algal proportionality
                                              ! dependence [m^3-kg^-1-s^-1]

REAL :: KLN                         ! Minimum labile particulate nitrogen
                                              ! hydrolysis rate [s^-1]

REAL :: KLNALG                      ! Labile nitrogen hydrol. algal proportionality
                                              ! dependence [m^3-kg^-1-s^-1]

REAL :: KLP                         ! Minimum labile organic phosphorous
                                              ! hydrolysis rate [s^-1]

REAL :: KLPALG                      ! Labile phosphorous hydrol. algal proportionality
                                              ! dependence [m^3-kg^-1-s^-1]

REAL :: KRC                         ! Min. hydrolysis rate of refractory PDC [s^-1]

REAL :: KRCALG                      ! Refractory hydrol. algal proportionality
                                              ! dependence [m^3-kg^-1-s^-1]

REAL :: KRN                         ! Minimum refractory particulate nitrogen
                                              ! hydrolysis rate [s^-1]

REAL :: KRNALG                      ! Refractory nitrogen hydrol. algal proportionality
                                              ! dependence [m^3-kg^-1-s^-1]

REAL :: KRP                         ! Minimum refractory organic phosphorous
                                              ! hydrolysis rate [s^-1]

REAL :: KRPALG                      ! Refractory phosphorous hydrol. algal proportionality
                                              ! dependence [m^3-kg^-1-s^-1]

REAL :: KSUA                        ! Particulate silica dissolution
                                              ! rate constant [s^-1]

REAL :: KSZ                         ! half saturation coeff. of zooplankton  
                                              ! for phytoplankton [kg-m^-3]

REAL :: KTBD                        ! Metabolism temperature dependence
                                              ! factor for diatoms [deg-C]

REAL :: KTBG                        ! Metabolism temperature dependence
                                              ! factor for greens [deg-C]

REAL :: KTGD1                       ! Effect of temperature below optimal
                                              ! temperature for diatoms [deg-C^2]

REAL :: KTGD2                       ! Effect of temperature above optimal
                                              ! temperature for diatoms [deg-C^2]

REAL :: KTGG1                       ! Effect of temperature below optimal
                                              ! temperature for greens [deg-C^2]

REAL :: KTGG2                       ! Effect of temperature above optimal
                                              ! temperature for greens [deg-C^2]

REAL :: KTHDR                       ! Hydrolysis temperature dependence 
                                              ! [deg-C^-1]

REAL :: KTMNL                       ! Mineralization temperature 
                                              ! dependence [deg-C^-1]

REAL :: KTNT1                       ! Effect of temperature below optimal T
                                              ! for nitrification [deg-C^-2]

REAL :: KTNT2                       ! Effect of temperature above optimal T
                                              ! for nitrification [deg-C^-2]

REAL :: KTSUA                       ! Silica dissolution temperature
                                              ! rate constant [deg-C^-1

REAL :: NTM                         ! Nitrification rate coefficient at
                                              ! optimal temperature [kg-m^-3-s-1]


!------------------------------------------------------------------------------
! PBMAX_DIA = specific diatoms photosynthetic rate at optimal 
!             illumination, i.e the assimilation number. It's the maximum
!             assimilation number normalized to diatoms biomass.
!             Its units are gC [gChl-a]^-1 h^-1.
!------------------------------------------------------------------------------
REAL :: PBMAX_DIA = 10.5

!------------------------------------------------------------------------------
! PBMAX_GRE = specific greens photosynthetic rate at optimal 
!             illumination, i.e the assimilation number. It's the maximum
!             assimilation number normalized to greens biomass.
!             Its units are gC [gChl-a]^-1 h^-1.
!------------------------------------------------------------------------------
REAL :: PBMAX_GRE = 10.5


REAL :: PMD                         ! Diatom production under
                                              ! optimal conditions [s^-1]

REAL :: PMG                         ! Green algae production under
                                              ! optimal conditions [s^-1]

REAL :: TMD                         ! Temperature of optimum growth
                                              ! for diatoms [deg-C] 

REAL :: TMG                         ! Temperature of optimum growth
                                              ! for greens [deg-C]

REAL :: TMNT                        ! Optimal temperature for
                                              ! nitrification [deg-C]

REAL :: TRD                         ! Metabolism reference temperature
                                              ! for diatoms [deg-C]

REAL :: TRG                         ! Metabolism reference temperature
                                              ! for greens [deg-C]

REAL :: TRHDR                       ! Reference temperature for
                                              ! hydrolysis [deg-C]

REAL :: TRMNL                       ! Reference temperature for
                                              ! mineralization [deg-C]

REAL :: TRSUA                       ! Silica dissolution reference
                                              ! temperature [deg-C]

REAL :: TZREF                       ! Reference predation temperature

REAL :: ZDTH                        ! Zooplankton death/die-off coefficient

REAL :: ZTHET                       ! temperature coefficient 
                                              ! for predation [-]




!------------------------------------------------------------------------------
!  Default values of the eutro parameters
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!  Eutrophication parameters
!------------------------------------------------------------------------------

DATA ANCP/0.2/                       ! N:C ratio for plankton [-]
  
DATA APCP/0.01/                      ! P:C ratio for plankton [-] 
  
DATA ASCD/0.8/                       ! Si:C ratio for diatoms [-]
  
DATA AVFRAC/0.05/                   ! Fraction of DOP available to
                                              ! phytoplankton.

DATA BMRD/8.6E-07/                   ! Diatom base metabolic rates [s^-1]
  
DATA BMRG/8.6E-07/                   ! Green base metabolic rates [s^-1]
  
DATA CCHLD/50.0/                    ! Carbon:chlorophyll ratio for diatoms [-]
  
DATA CCHLG/50.0/                    ! Carbon:chlorophyll ratio for green algae [-]
  
DATA CGZ/4.6E-06/                     ! Zooplankton grazing coefficient [sec^-1]

DATA FCDD/0.1/                       ! Fraction of basal metabolism exuded
                                              ! as dissolved organic carbon by diatoms [-] 
  
DATA FCDG/0.1/                       ! Fraction of basal metabolism exuded
                                              ! as dissolved organic carbon by greens [-]
  
DATA FCDP/0.1/                       ! Fraction of dissolved organic carbon
                                              ! produced by predation [-]

DATA FCDZ/0.1/                       ! Fraction of DOC from
                                              ! zooplankton mortality [-]

DATA FCLD/0.10/                      ! Fraction of labile particulate
                                              ! organic carbon produced from
                                              ! diatoms metabolic and/or
                                              ! mortality rate.

DATA FCLG/0.10/                      ! Fraction of labile particulate
                                              ! organic carbon produced from
                                              ! greens metabolic and/or
                                              ! mortality rate.

DATA FCLP/0.55/                      ! Fraction of labile pdc 
                                              ! from predation [-]

DATA FCLZ/0.55/                      ! Fraction of labile pdc from
                                              ! zooplankton mortality [-]

DATA FCRD/0.10/                      ! Fraction of refractory 
                                              ! particulate organic carbon
                                              ! produced from diatoms   
                                              ! metabolic and/or mortality
                                              ! rate.

DATA FCRG/0.10/                      ! Fraction of refractory 
                                              ! particulate organic carbon
                                              ! produced from greens   
                                              ! metabolic and/or mortality
                                              ! rate.

DATA FCRP/0.35/                      ! Fraction of refractory pdc from
                                              ! predation [-]

DATA FCRZ/0.35/                      ! Fraction of refractory pdc from 
                                              ! zooplankton mortality [-]

DATA FNDD/0.8/                       ! Fraction dissolved organic nitrogen
                                              ! produced by diatom metabolism [-]

DATA FNDG/0.8/                       ! Fraction dissolved organic nitrogen
                                              ! produced by green algae metabolism [-]

DATA FNDP/0.1/                       ! Fraction of dissolved organic
                                              ! nitrogen produced by predation [-]              

DATA FNDZ/0.4/                       ! Fraction of dissolved organic
                                              ! nitrogen produced by zoo. mortality [-]              

DATA FNID/0.1/                       ! Fraction of inorganic nitrogren
                                              ! produced by diatom metabolism [-]

DATA FNIG/0.1/                       ! Fraction of inorganic nitrogren
                                              ! produced by green algae metabolism [-]

DATA FNIP/0.0/                       ! Fraction of inorganic nitrogen
                                              ! produced by predation [-]

DATA FNIZ/0.1/                       ! Fraction of inorganic nitrogen
                                              ! produced by zoo. mortality [-]

DATA FNLD/0.1/                       ! Fraction labile particulate nitrogen
                                              ! produced by diatom metabolism [-]

DATA FNLG/0.1/                       ! Fraction labile particulate nitrogen
                                              ! produced by green algae metabolism [-]

DATA FNLP/0.55/                      ! Fraction of labile organic
                                              ! nitrogen produced by predation [-]             

DATA FNLZ/0.35/                      ! Fraction of labile organic
                                              ! nitrogen produced by zoo. mortality [-]             

DATA FNRD/0.0/                       ! Fraction refractory particulate nitrogen
                                              ! produced by diatom metabolism [-]

DATA FNRG/0.0/                       ! Fraction refractory particulate nitrogen
                                              ! produced by green algae metabolism [-]

DATA FNRP/0.35/                      ! Fraction of refractory organic
                                              ! nitrogen produced by predation [-]            

DATA FNRZ/0.15/                      ! Fraction of refractory organic nitrogen
                                              ! produced by zooplankton mortality [-]

DATA FPDD/0.8/                       ! Fraction dissolved organic phosphorous
                                              ! produced by diatom metabolism [-]

DATA FPDG/0.8/                       ! Fraction dissolved organic phosphorous
                                              ! produced by green metabolism [-]

DATA FPDP/0.1/                       ! Fraction of dissolved organic phosphorous
                                              ! produced by predation [-]

DATA FPDZ/0.4/                       ! Fraction of dissolved organic 
                                              ! phosphorous produced by zoo. mortality [-]

DATA FPID/0.1/                       ! Fraction inorganic phosphorous
                                              ! produced by diatom metabolism [-]

DATA FPIG/0.1/                       ! Fraction inorganic phosphorous
                                              ! produced by green metabolism [-]

DATA FPIP/0.0/                       ! Fraction inorganic phosphorous
                                              ! produced by predation  [-]

DATA FPIZ/0.1/                       ! Fraction inorganic phosphorous
                                              ! produced by zooplankton mortality [-]

DATA FPLD/0.1/                       ! Fraction of labile organic
                                              ! phosphorous produced by diatom 
                                              ! metabolism [-]

DATA FPLG/0.1/                       ! Fraction of labile organic
                                              ! phosphorous produced by green 
                                              ! algae metabolism [-]

DATA FPLP/0.55/                      ! Fraction of labile organic
                                              ! phosphorous produced by predation [-]

DATA FPLZ/0.35/                      ! Fraction of labile organic phosphorous
                                              ! produced by zoo. mortality [-]

DATA FPRD/0.0/                       ! Fraction of refractory organic
                                              ! phosphorous produced by diatom 
                                              ! metabolism [-]

DATA FPRG/0.0/                       ! Fraction of refractory organic
                                              ! phosphorous produced by greeen 
                                              ! algae metabolism [-]

DATA FPRP/0.35/                      ! Fraction of refractory organic
                                              ! phosphorous produced by predation [-]

DATA FPRZ/0.15/                      ! Fraction of refractory organic
                                              ! phosphorous produced by zoo. mortality [-]

DATA FSAP/0.0/                       ! Fraction silica made available
                                              ! through predation [-]


DATA GREFF/0.6/                     ! Zooplankton grazing efficiency [-]

DATA ILMUL/1.0/                     ! Scaling for surface illumination [-]


DATA KDC/1.2E-07/                     ! Respiration rate of DOC [sec^-1]

DATA KDCALG/0.0/                   ! Mineralization algal dependence factor [m^3-kg^-1-s^1]

DATA KDN/2.3E-07/                     ! Minimum dissolved organic nitrogen
                                              ! mineralization rate [sec^-1]

DATA KDNALG/0.0/                   ! Minimum dissolved organic nitrogen
                                              ! mineralization rate [m^3-kg^-1-s^1]

DATA KDP/5.0E-07/                     ! Minimum dissolved organic phosphorous
                                              ! mineralization rate [s^-1]

DATA KDPALG/2.3E-03/               ! Dissolved phosphorous mineralization
                                              ! algal proportionality dependence [m^3-kg^-1-s^-1] 


DATA KHN/2.5E-05/                     ! Organic nitrogen decay
                                              ! half saturation constant [kg-m^-3]

DATA KHND/2.5E-05/                   ! Mean half-saturation constant
                                              ! for nitrogen uptake by diatoms [kg-m^-3]

DATA KHNG/2.5E-05/                   ! Mean half-saturation constant
                                              ! for nitrogen uptake by greens [kg-m^-3]

DATA KHNNT/1.0E-04/                 ! Half saturation concentration of NH4
                                              ! required for nitrification [kg-m^-3]

DATA KHP/1.0E-06/                     ! Organic phosphorous decay
                                              ! half saturation constant [kg-m^-3]

DATA KHPD/1.0E-06/                   ! Mean half-saturation constant
                                              ! for algal phosphorous uptake [kg-m^-3]

DATA KHPG/1.0E-06/                   ! Mean half-saturation constant
                                              ! for algal phosphorous uptake [kg-m^-3] 

DATA KHSD/3.0E-05/                   ! Mean half-saturation constant
                                              ! for diatom silica uptake [kg-m^-3]

DATA KLC/8.6E-07/                     ! Hydrolysis rate of labile PDC [s^-1]

DATA KLCALG/0.0/                   ! Labile hydrol. algal proportionality
                                              ! dependence [m^3-kg^-1-s^-1]

DATA KLN/2.3E-07/                     ! Minimum labile particulate nitrogen
                                              ! hydrolysis rate [s^-1]

DATA KLNALG/0.0/                   ! Labile nitrogen hydrol. algal proportionality
                                              ! dependence [m^3-kg^-1-s^-1]

DATA KLP/2.3E-07/                    ! Minimum labile organic phosphorous
                                              ! hydrolysis rate [s^-1]

DATA KLPALG/2.3E-03/                   ! Labile phosphorous hydrol. algal proportionality
                                              ! dependence [m^3-kg^-1-s^-1]

DATA KRC/5.0E-08/                     ! Min. hydrolysis rate of refractory PDC 
                                              ! [s^-1]

DATA KRCALG/0.0/                   ! Refractory hydrol. algal proportionality
                                              ! dependence [m^3-kg^-1-s^-1]

DATA KRN/5.7E-08/                     ! Minimum refractory particulate nitrogen
                                              ! hydrolysis rate [s^-1]

DATA KRNALG/0.0/                   ! Refractory nitrogen hydrol. algal proportionality
                                              ! dependence [m^3-kg^-1-s^-1]

DATA KRP/5.7E-08/                     ! Minimum labile organic phosphorous
                                              ! hydrolysis rate [s^-1]

DATA KRPALG/2.3E-03/               ! Refractory phosphorous hydrol. algal proportionality
                                              ! dependence [m^3-kg^-1-s^-1]

DATA KSUA/5.0E-08/                   ! Particulate silica dissolution
                                              ! rate constant [s^-1]

DATA KSZ/7.5E-04/                     ! half saturation coeff. of zooplankton 
                                              ! for phytoplankton [kg-m^-3]
 
DATA KTBD/6.9E-02/                   ! Metabolism temperature dependence
                                              ! factor for diatoms [deg-C]

DATA KTBG/6.9E-02/                   ! Metabolism temperature dependence
                                              ! factor for greens [deg-C]

DATA KTGD1/2.5E-03/                 ! Effect of temperature below optimal
                                              ! temperature for diatoms [deg-C^2]

DATA KTGD2/6.0E-03/                 ! Effect of temperature above optimal
                                              ! temperature for diatoms [deg-C^2]

DATA KTGG1/2.5E-03/                 ! Effect of temperature below optimal
                                              ! temperature for greens [deg-C^2]

DATA KTGG2/1.0E-02/                 ! Effect of temperature above optimal
                                              ! temperature for greens [deg-C^2]

DATA KTHDR/6.9E-02/                 ! Hydrolysis temperature dependence 
                                              ! [deg-C^-1]

DATA KTMNL/6.9E-02/                 ! Mineralization temperature 
                                              ! dependence [deg-C^-1]

DATA KTNT1/9.0E-02/                 ! Effect of temperature below optimal T
                                              ! for nitrification [deg-C^-2]

DATA KTNT2/9.0E-02/                 ! Effect of temperature above optimal T
                                              ! for nitrification [deg-C^-2]

DATA KTSUA/1.1E-06/                 ! Silica dissolution temperature
                                              ! rate constant [deg-C^-1]

DATA NTM/1.2E-12/                     ! Nitrification rate coefficient at
                                              ! optimal temperature [kg-m^-3-s-1]

DATA PMD/2.4E-05/                     ! Diatom production under
                                              ! optimal conditions [s^-1]

DATA PMG/2.3E-05/                     ! Green algae production under
                                              ! optimal conditions [s^-1]

DATA TMD/28.0/                        ! Temperature of optimum growth
                                              ! for diatoms [deg-C] 

DATA TMG/28.0/                        ! Temperature of optimum growth
                                              ! for greens [deg-C]

DATA TMNT/25.0/                      ! Optimal temperature for
                                              ! nitrification [deg-C]

DATA TRD/25.0/                        ! Metabolism reference temperature
                                              ! for diatoms [deg-C]

DATA TRG/25.0/                        ! Metabolism reference temperature
                                              ! for greens [deg-C]

DATA TRHDR/25.0/                    ! Reference temperature for
                                              ! hydrolysis [deg-C]

DATA TRMNL/25.0/                    ! Reference temperature for
                                              ! mineralization [deg-C]

DATA TRSUA/25.0/                    ! Silica dissolution reference
                                              ! temperature [deg-C]

DATA TZREF/25.0/                    ! Predation reference 
                                              ! temperature [deg-C]

DATA ZDTH/3.5E-07/                   ! Zooplankton death/die-off 
                                              ! coefficient [sec^-1]

DATA ZTHET/1.05/                    ! temperature coefficient 
                                              ! for predation [-]


!------------------------------------------------------------------------------
!  Dissolved Oxygen parameters
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!  Maximum observed carbon sod mass flux.
!  It has units of (kg of O2)/(m^2 s).
!------------------------------------------------------------------------------
REAL :: C_SOD_FLUX_MAX = 9.259E-09

!------------------------------------------------------------------------------
!  Denitrification carbon-to-nitrogen ratio
!------------------------------------------------------------------------------
REAL :: DENIT_CN_RATIO = 1.07

!------------------------------------------------------------------------------
!  COD rate coefficient
!------------------------------------------------------------------------------
REAL :: KCOD = 1.0

!------------------------------------------------------------------------------
!  Maximum denitrification rate coefficient [s^-1]
!------------------------------------------------------------------------------
REAL :: KDENITR = 1.0

!------------------------------------------------------------------------------
!  Half-Saturation concentration of NO3 required for 
!  denitrification [kg NO3 / m^-3]
!------------------------------------------------------------------------------
REAL :: KHDENITR = 1.0

!------------------------------------------------------------------------------
!  Half-Saturation concentration of NO3 required for
!  denitrification in the sediment bed [kg NO3 / m^-3].
!------------------------------------------------------------------------------
REAL :: KHDENITR_SED = 1.0E-03

!------------------------------------------------------------------------------
!  Half-Saturation concentration of dissolved oxygen required for 
!  nitrification [kg O2 / m^-3]
!------------------------------------------------------------------------------
REAL :: KHDONT = 1.0

!------------------------------------------------------------------------------
!  Half-Saturation concentration of dissolved oxygen required for
!  nitrification [kg O2 / m^-3] in the sediment bed.
!------------------------------------------------------------------------------
REAL :: KHDONT_SED = 2.0E-3

!------------------------------------------------------------------------------
!  Half-Saturation concentration of dissolved oxygen required for oxic
!  respiration [kg O2 / m^-3]
!------------------------------------------------------------------------------
REAL :: KHODOC = 1.0

!------------------------------------------------------------------------------
!  Half-Saturation concentration of dissolved oxygen required for oxic
!  respiration [kg O2 / m^-3] in the sediment bed.
!------------------------------------------------------------------------------
REAL :: KHODOC_SED = 2.0E-03

!------------------------------------------------------------------------------
!  Half-Saturation concentration of dissolved oxygen required for
!  exertion of chemical oxygen demand [kg O2 / m^-3]
!------------------------------------------------------------------------------
REAL :: KHOCOD = 1.0

!------------------------------------------------------------------------------
! D.O. reaeration velocity (m/s)
!------------------------------------------------------------------------------
REAL :: KREA_VEL_DO = 1.1574E-05


!------------------------------------------------------------------------------
!  Minimum observed NO3 mass flux across the water-sediment interface.
!  The number is negative because it represents a mass flux from the water
!  to the sediment bed.
!  It has units of (kg of N2)/(m^2 s)
!------------------------------------------------------------------------------
REAL :: SED_NO3_FLUX_MIN = -1.62118E-10


!------------------------------------------------------------------------------
!  Dissolved Oxygen-to-Carbon ratio in respiration = 2.67 grams O2/grams C
!------------------------------------------------------------------------------
REAL :: RCDO = 2.67

!------------------------------------------------------------------------------
!  Nitrogen to Dissolved Oxygen ratio
!------------------------------------------------------------------------------
REAL :: RNTO = 1.0

!------------------------------------------------------------------------------
! SOD to nitrification ratio?? 
!------------------------------------------------------------------------------
REAL :: RSODNTR = 0.24

!------------------------------------------------------------------------------
!  SOD reference temperature (degrees Celsius)
!------------------------------------------------------------------------------
REAL :: TREF = 25.0

!------------------------------------------------------------------------------
!  Dissolved Oxygen arrays used to store kinetic processes.
!------------------------------------------------------------------------------
REAL, ALLOCATABLE :: DOPRODD_AVG(:,:,:) 
REAL, ALLOCATABLE :: DOPRODG_AVG(:,:,:) 
REAL, ALLOCATABLE :: NITDO2(:,:,:)
REAL, ALLOCATABLE :: DOMETD_ARR(:,:,:)
REAL, ALLOCATABLE :: DOMETG_ARR(:,:,:)
REAL, ALLOCATABLE :: DOPREDD_ARR(:,:,:)
REAL, ALLOCATABLE :: DOPREDG_ARR(:,:,:)
REAL, ALLOCATABLE :: DOZOO_ARR(:,:,:)
REAL, ALLOCATABLE :: DOMNLDOC_ARR(:,:,:)
REAL, ALLOCATABLE :: DOCOD_ARR(:,:,:)
REAL, ALLOCATABLE :: DOREAR_ARR(:,:,:)
REAL, ALLOCATABLE :: DOSOC_ARR(:,:,:)
 

!
!------------------------------------------------------------------------------
!

END MODULE EUT
