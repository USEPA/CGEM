Module INPUT_VARS_CGEM

IMPLICIT NONE

SAVE

integer MC
!--Switches in GEM---------
integer Which_fluxes(8)
integer Which_uptake
integer Which_quota
integer Which_irradiance 
integer Which_chlaC
integer Which_photosynthesis 
integer Which_growth
!--Optics-----------------------
real Kw
real Kcdom
real Kspm
real Kchla
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
!----Other including Boundary Conditions------------
integer Which_VMix
real KH_coeff 
integer Which_Outer_BC
real wt_l, wt_o
real wt_pl, wt_po
real m_OM_init,m_OM_BC,m_OM_sh
!real Stoich_x1A_init,Stoich_y1A_init,Stoich_z1A_init
!real Stoich_x2A_init,Stoich_y2A_init,Stoich_z2A_init
!real Stoich_x1Z_init,Stoich_y1Z_init,Stoich_z1Z_init
!real Stoich_x2Z_init,Stoich_y2Z_init,Stoich_z2Z_init
real KG_bot

!Light curve parameters
real, allocatable :: alphad(:) ! Initial slope of photosynthesis-irradiance curve / Vmax
real, allocatable :: betad(:)  ! Photoinhibition constant / Vmax

!Diatom array
integer, allocatable :: is_diatom(:)
 
contains

Subroutine INPUT_VARS_CGEM_allocate

USE Fill_Value
USE Model_dim, ONLY : nospA, nospZ

IMPLICIT NONE

integer ierr

!---Phytoplankton 
ALLOCATE( ediblevector(nospZ,nospA),stat=ierr )
if(ierr.ne.0) write(6,*) "error allocate"

ALLOCATE( umax(nospA),stat=ierr  )
if(ierr.ne.0) write(6,*) "error allocate"

ALLOCATE( alpha(nospA),stat=ierr )
if(ierr.ne.0) write(6,*) "error allocate"
ALLOCATE( beta(nospA),stat=ierr )
if(ierr.ne.0) write(6,*) "error allocate"
ALLOCATE( respg(nospA),stat=ierr )
if(ierr.ne.0) write(6,*) "error allocate"
ALLOCATE( respb(nospA),stat=ierr )
if(ierr.ne.0) write(6,*) "error allocate"
ALLOCATE( QminN(nospA),stat=ierr )
if(ierr.ne.0) write(6,*) "error allocate"
ALLOCATE( QminP(nospA),stat=ierr )
if(ierr.ne.0) write(6,*) "error allocate"
ALLOCATE( QmaxN(nospA),stat=ierr )
if(ierr.ne.0) write(6,*) "error allocate"
ALLOCATE( QmaxP(nospA),stat=ierr )
if(ierr.ne.0) write(6,*) "error allocate"
ALLOCATE( Kn(nospA),stat=ierr )
if(ierr.ne.0) write(6,*) "error allocate"
ALLOCATE( Kp(nospA),stat=ierr )
if(ierr.ne.0) write(6,*) "error allocate"
ALLOCATE( Ksi(nospA),stat=ierr )
if(ierr.ne.0) write(6,*) "error allocate"
ALLOCATE( KQn(nospA),stat=ierr )
if(ierr.ne.0) write(6,*) "error allocate"
ALLOCATE( KQp(nospA),stat=ierr )
if(ierr.ne.0) write(6,*) "error allocate"
ALLOCATE( nfQs(nospA),stat=ierr )
if(ierr.ne.0) write(6,*) "error allocate"
ALLOCATE( vmaxN(nospA),stat=ierr )
if(ierr.ne.0) write(6,*) "error allocate"
ALLOCATE( vmaxP(nospA),stat=ierr )
if(ierr.ne.0) write(6,*) "error allocate"
ALLOCATE( vmaxSi(nospA),stat=ierr )
if(ierr.ne.0) write(6,*) "error allocate"
ALLOCATE( aN(nospA),stat=ierr )
if(ierr.ne.0) write(6,*) "error allocate"
ALLOCATE( volcell(nospA),stat=ierr )
if(ierr.ne.0) write(6,*) "error allocate"
ALLOCATE( Qc(nospA),stat=ierr )
if(ierr.ne.0) write(6,*) "error allocate"
ALLOCATE( Athresh(nospA),stat=ierr )
if(ierr.ne.0) write(6,*) "error allocate"
ALLOCATE( mA(nospA),stat=ierr )
if(ierr.ne.0) write(6,*) "error allocate"
ALLOCATE( A_wt(nospA),stat=ierr )
if(ierr.ne.0) write(6,*) "error allocate"
!---Zooplankton
if(ierr.ne.0) write(6,*) "error allocate"
ALLOCATE( Zeffic(nospZ),stat=ierr )
if(ierr.ne.0) write(6,*) "error allocate"
ALLOCATE( Zslop(nospZ),stat=ierr )
if(ierr.ne.0) write(6,*) "error allocate"
ALLOCATE( Zvolcell(nospZ),stat=ierr )
if(ierr.ne.0) write(6,*) "error allocate"
ALLOCATE( ZQc(nospZ),stat=ierr )
if(ierr.ne.0) write(6,*) "error allocate"
ALLOCATE( ZQn(nospZ),stat=ierr )
if(ierr.ne.0) write(6,*) "error allocate"
ALLOCATE( ZQp(nospZ),stat=ierr )
if(ierr.ne.0) write(6,*) "error allocate"
ALLOCATE( ZKa(nospZ),stat=ierr )
if(ierr.ne.0) write(6,*) "error allocate"
ALLOCATE( Zrespg(nospZ),stat=ierr )
if(ierr.ne.0) write(6,*) "error allocate"
ALLOCATE( Zrespb(nospZ),stat=ierr )
if(ierr.ne.0) write(6,*) "error allocate"
ALLOCATE( Zumax(nospZ),stat=ierr )
if(ierr.ne.0) write(6,*) "error allocate"
ALLOCATE( Zm(nospZ),stat=ierr )
if(ierr.ne.0) write(6,*) "error allocate"

!Light curve parameters
ALLOCATE( alphad(nospA),stat=ierr ) ! Initial slope of photosynthesis-irradiance curve / Vmax
if(ierr.ne.0) write(6,*) "error allocate"
ALLOCATE( betad(nospA),stat=ierr )  ! Photoinhibition constant / Vmax
if(ierr.ne.0) write(6,*) "error allocate"

!Diatom array
ALLOCATE( is_diatom(nospA),stat=ierr )
if(ierr.ne.0) write(6,*) "error allocate"

return
END Subroutine INPUT_VARS_CGEM_allocate

END MODULE INPUT_VARS_CGEM

