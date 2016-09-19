Module INPUT_VARS

IMPLICIT NONE

!--Code Run Identifier-----------
character*150 code_ID
!--Run Specifics---------------
integer iYrS,iMonS,iDayS,iHrS,iMinS,iSecS
integer iYrE,iMonE,iDayE,iHrE,iMinE,iSecE
integer RESTART_FILE_TIMESTEP
integer dT, dT_out, dT_restart
integer icent,jcent
!--Switches in GEM---------
integer Which_fluxes(8)
integer Which_uptake
integer Which_quota
integer Which_irradiance 
integer Which_chlaC
integer Which_photosynthesis 
integer Which_growth
integer SolarRad
integer InitializeHow
integer Which_hydro
integer Which_Output
integer MC
!--Optics-----------------------
real Kw
real Kcdom
real Kspm
real Kchla
real PARfac
!---Phytoplankton 
real, allocatable :: ediblevector(:,:)
real, allocatable :: umax(:) 
real, allocatable :: alpha(:)
real, allocatable :: beta(:)
real, allocatable :: respg(:)
real, allocatable :: respb(:)
real, allocatable :: QminN(:)
real, allocatable :: QminP(:)
real, allocatable :: QmaxN(:)
real, allocatable :: QmaxP(:)
real, allocatable :: Kn(:)
real, allocatable :: Kp(:)
real, allocatable :: Ksi(:)
real, allocatable :: KQn(:)
real, allocatable :: KQp(:)
real, allocatable :: nfQs(:)
real, allocatable :: vmaxN(:)
real, allocatable :: vmaxP(:)
real, allocatable :: vmaxSi(:)
real, allocatable :: aN(:)
real, allocatable :: volcell(:)
real, allocatable :: Qc(:)
real, allocatable :: Athresh(:)
real, allocatable :: mA(:)
real, allocatable :: A_wt(:)
!---Zooplankton
real, allocatable :: Zeffic(:)
real, allocatable :: Zslop(:)
real, allocatable :: Zvolcell(:)
real, allocatable :: ZQc(:)
real, allocatable :: ZQn(:)
real, allocatable :: ZQp(:)
real, allocatable :: ZKa(:)
real, allocatable :: Zrespg(:)
real, allocatable :: Zrespb(:)
real, allocatable :: Zumax(:)
real, allocatable :: Zm(:)
!---Organic Matter		
real KG1
real KG2
real KG1_R
real KG2_R
real KG1_BC
real KG2_BC
real KNH4
real nitmax
real KO2
real KstarO2
real KNO3
real pCO2
!real, allocatable :: pH(im,jm,nsl)
real stoich_x1R
real stoich_y1R
real stoich_z1R
real stoich_x2R
real stoich_y2R
real stoich_z2R
real stoich_x1BC
real stoich_y1BC
real stoich_z1BC
real stoich_x2BC
real stoich_y2BC
real stoich_z2BC
real KGcdom
real CF_SPM
!----Sinking Terms----------------------------------
real, allocatable :: ws(:) 
!----River Params-----------------------------------
real rcNO3
real rcNH4
real rcPO4
real rcSi
!----Other including Boundary Conditions------------
integer Which_VMix
real KH_coeff 
integer Which_Outer_BC
real wt_l, wt_o
real wt_pl, wt_po
real m_OM_init,m_OM_BC,m_OM_sh
real Stoich_x1A_init,Stoich_y1A_init,Stoich_z1A_init
real Stoich_x2A_init,Stoich_y2A_init,Stoich_z2A_init
real Stoich_x1Z_init,Stoich_y1Z_init,Stoich_z1Z_init
real Stoich_x2Z_init,Stoich_y2Z_init,Stoich_z2Z_init
real KG_bot

!Light curve parameters
real, allocatable :: alphad(:) ! Initial slope of photosynthesis-irradiance curve / Vmax
real, allocatable :: betad(:)  ! Photoinhibition constant / Vmax

END MODULE INPUT_VARS
