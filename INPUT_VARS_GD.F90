Module INPUT_VARS_GD

USE Model_dim, ONLY: nf

IMPLICIT NONE

!--Code Run Identifier-----------
character*150 code_ID
!--Run Specifics---------------
integer iYrS,iMonS,iDayS,iHrS,iMinS,iSecS
integer iYrE,iMonE,iDayE,iHrE,iMinE,iSecE
integer dT, dT_out, dT_restart
integer icent,jcent
real*8 DayS_8, DayE_8
!--Switches in GEM---------
integer Which_Fluxes(9)
integer Which_hydro
integer Read_Solar,Read_Wind,Read_T,Read_Sal 
integer InitializeHow
integer Which_irradiance
!----Sinking Terms----------------------------------
real,allocatable :: ws(:) 
!----River Params-----------------------------------
real rcNO3
real rcNH4
real rcPO4
real rcSi
!----Other including Boundary Conditions------------
integer Which_VMix
real KH_coeff 
integer Which_Outer_BC
real m_OM_init,m_OM_bc,m_OM_sh

END MODULE INPUT_VARS_GD
