MODULE EUT
!------------------------------------------------------------------------------
!-
!-  $Id: eut.F90,v 1.0.6.1 2014/08/26 22:54:04 wmelende Exp $
!-  $Locker:  $
!-
!-  Purpose and Methods : Include file for eutrophication variables.
!-
!-  Created: 12/09/97  M. Settles
!-  Revised: 11/12/2018 W. Melendez : Added KE and KECHL parameters.
!------------------------------------------------------------------------------

IMPLICIT NONE

SAVE

!------------------------------------------------------------------------------
!  Miscellaneous arrays/variables; not parameters
!------------------------------------------------------------------------------

!calculated in diatoms, used in carbon, dissolved oxygen, nitrog, phosph, silica 
REAL,ALLOCATABLE :: BMD(:,:,:)          ! Base metabolic rate for diatoms

!calculated in greens, used in carbon, dissolved oxygen, nitrog, phosph, silica
REAL,ALLOCATABLE :: BMG(:,:,:)          ! Base metabolic rate for greens

!Calculated in carbon, used in carbon, nitrog, and phosph
REAL,ALLOCATABLE :: FTHDR(:,:,:)        ! Temperature correction for hydrolysis

!Calculated in carbon, used in carbon, dissolved_oxygen, nitrog, phosph
REAL,ALLOCATABLE :: FTMNL(:,:,:)        ! Temperature correction for mineralization

!Used for output, calculated in diatoms
REAL,ALLOCATABLE :: OUTPUT_PAR(:,:,:)    ! Daily cumulative PAR

!calculated in diatoms, used in dissolved oxygen, nitrog, phosph, silica
REAL,ALLOCATABLE :: PD(:,:,:)           ! Production rates for diatoms

!Used for output, calculated in diatoms
REAL,ALLOCATABLE :: PD_AVG(:,:,:)       ! Average production rates for 
                                              ! diatoms

!calculated in diatoms, used in dissolved oxygen, nitrog, phosph
REAL,ALLOCATABLE :: PG(:,:,:)           ! Production rates for greens

!Used for output, calculated in greens
REAL,ALLOCATABLE :: PG_AVG(:,:,:)       ! Average production rates for 
                                              ! greens

!Calculated in zoo, used in carbon, dissolved oxygen, nitrog, phosph, silica
REAL,ALLOCATABLE :: PRD(:,:,:)          ! Predation time derivative for diatoms

!Calculated in zoo, used in carbon, dissolved oxygen, nitrog, phosph
REAL,ALLOCATABLE :: PRG(:,:,:)          ! Predation time derivative for greens


!------------------------------------------------------------------------------
!  Fine segmentation growth limitation factors
!------------------------------------------------------------------------------
!Calculated and used in diatoms, used in output
REAL,ALLOCATABLE :: PFD(:,:,:)       ! Phosphorous limitation for diatoms

!Calculated and used in diatoms, used in output
REAL,ALLOCATABLE :: SFD(:,:,:)       ! Silica limitation for diatoms

!Calculated and used in diatoms, used in output
REAL,ALLOCATABLE :: NFD(:,:,:)       ! Nitrogen limitation for diatoms

!Calculated and used in diatoms, used in output
REAL,ALLOCATABLE :: IFD(:,:,:)       ! Light limitation for diatoms

!Calculated and used in diatoms, used in output
REAL,ALLOCATABLE :: TFD(:,:,:)       ! Temperature limitation for diatoms

!Calculated and used in greens, used in output
REAL,ALLOCATABLE :: PFG(:,:,:)       ! Phosphorous limitation for greens

!Calculated and used in greens, used in output
REAL,ALLOCATABLE :: NFG(:,:,:)       ! Nitrogen limitation for greens

!Calculated and used in greens, used in output
REAL,ALLOCATABLE :: IFG(:,:,:)       ! Light limitation for greens

!Calculated and used in greens, used in output
REAL,ALLOCATABLE :: TFG(:,:,:)       ! Temperature limitation for greens

!New for Droop
!diatoms
!Calculated in diatoms, used in nitrog
REAL,ALLOCATABLE :: QND(:,:,:)       !
!Calculated in diatoms, used in phosph
REAL,ALLOCATABLE :: QPD(:,:,:)       !
REAL :: QminND = 0.003
REAL :: QminPD = 0.003
!greens
!Calculated in greens, used in nitrog
REAL,ALLOCATABLE :: QNG(:,:,:)       !
!Calculated in greens, used in phosph
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

REAL :: KE                          ! Background light attenuation

REAL :: KECHL                       ! Light attenuation factor for chlorophyll-a [m^2-kg^-1]


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

DATA KE/0.15/                      ! Background light attenuation [m^-1]

DATA KECHL/1.7E+04/                ! Light attenuation factor for chlorophyll-a [m^2-kg^-1]


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
! D.O. reaeration coefficient (m/s)
!------------------------------------------------------------------------------
REAL :: KRDO = 2.77E-05

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
!Calculated in dissolved oxygen for output purposes
REAL, ALLOCATABLE :: NITDO2(:,:,:)
!Calculated in dissolved oxygen for output purposes
REAL, ALLOCATABLE :: DOMETD_ARR(:,:,:)
!Calculated in dissolved oxygen for output purposes
REAL, ALLOCATABLE :: DOMETG_ARR(:,:,:)
!Calculated in dissolved oxygen for output purposes
REAL, ALLOCATABLE :: DOPREDD_ARR(:,:,:)
!Calculated in dissolved oxygen for output purposes
REAL, ALLOCATABLE :: DOPREDG_ARR(:,:,:)
!Calculated in dissolved oxygen for output purposes
REAL, ALLOCATABLE :: DOZOO_ARR(:,:,:)
!Calculated in dissolved oxygen for output purposes
REAL, ALLOCATABLE :: DOMNLDOC_ARR(:,:,:)
 

!
!------------------------------------------------------------------------------
!
contains

SUBROUTINE EUT_allocate
!------------------------------------------------------------------------------
!-
!-  $Id: eut.F90,v 1.0.6.1 2014/08/26 22:54:04 wmelende Exp $
!-  $Locker:  $
!-
!-  Purpose and Methods : Include file for eutrophication variables.
!-
!-  Created: 12/09/97  M. Settles
!------------------------------------------------------------------------------

USE Model_dim 

IMPLICIT NONE

!------------------------------------------------------------------------------
!  Miscellaneous arrays/variables; not parameters
!------------------------------------------------------------------------------
  
ALLOCATE(BMD(im,jm,km))        ! Base metabolic rate for diatoms

ALLOCATE(BMG(im,jm,km))        ! Base metabolic rate for greens

ALLOCATE(FTHDR(im,jm,km))        ! Temperature correction for hydrolysis

ALLOCATE(FTMNL(im,jm,km))        ! Temperature correction for mineralization

ALLOCATE(OUTPUT_PAR(im,jm,km))     ! Daily cumulative PAR
OUTPUT_PAR = 0.


ALLOCATE(PD(im,jm,km))        ! Production rates for diatoms
PD = 0.
ALLOCATE(PD_AVG(im,jm,km))        ! Average production rates for 
PD_AVG = 0.                                              ! diatoms

ALLOCATE(PG(im,jm,km))        ! Production rates for greens
PG = 0.
ALLOCATE(PG_AVG(im,jm,km))        ! Average production rates for 
PG_AVG = 0.                                              ! greens

ALLOCATE(PRD(im,jm,km))        ! Predation time derivative for diatoms

ALLOCATE(PRG(im,jm,km))        ! Predation time derivative for greens


!------------------------------------------------------------------------------
!  Fine segmentation growth limitation factors
!------------------------------------------------------------------------------

ALLOCATE(PFD(im,jm,km))     ! Phosphorous limitation for diatoms

ALLOCATE(SFD(im,jm,km))     ! Silica limitation for diatoms

ALLOCATE(NFD(im,jm,km))     ! Nitrogen limitation for diatoms

ALLOCATE(IFD(im,jm,km))     ! Light limitation for diatoms

ALLOCATE(TFD(im,jm,km))     ! Temperature limitation for diatoms

ALLOCATE(PFG(im,jm,km))     ! Phosphorous limitation for greens

ALLOCATE(NFG(im,jm,km))     ! Nitrogen limitation for greens

ALLOCATE(IFG(im,jm,km))     ! Light limitation for greens

ALLOCATE(TFG(im,jm,km))     ! Temperature limitation for greens

!------------------------------------------------------------------------------
! Initialize arrays to zero.
!------------------------------------------------------------------------------
PFD = 0.0
SFD = 0.0
NFD = 0.0
IFD = 0.0
TFD = 0.0
PFG = 0.0
NFG = 0.0
IFG = 0.0
TFG = 0.0

!Droop
ALLOCATE(QND(im,jm,km))  
ALLOCATE(QPD(im,jm,km))  
ALLOCATE(QNG(im,jm,km))  
ALLOCATE(QPG(im,jm,km))  

!------------------------------------------------------------------------------
!  Dissolved Oxygen arrays used to store kinetic processes.
!------------------------------------------------------------------------------
ALLOCATE(NITDO2(im,jm,km)) 
ALLOCATE(DOMETD_ARR(im,jm,km)) 
ALLOCATE(DOMETG_ARR(im,jm,km))
ALLOCATE(DOPREDD_ARR(im,jm,km)) 
ALLOCATE(DOPREDG_ARR(im,jm,km))
ALLOCATE(DOZOO_ARR(im,jm,km)) 
ALLOCATE(DOMNLDOC_ARR(im,jm,km)) 
 
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
return
END SUBROUTINE EUT_allocate

END Module EUT
