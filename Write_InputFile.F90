Subroutine Write_InputFile(filename)

USE Model_dim
USE INPUT_VARS
USE LIGHT_VARS
USE TEMP_VARS
USE CGEM_vars 

IMPLICIT NONE

character*120 filename

!--Code Identifier--------------
open(unit=999,file=filename,form='formatted',status='unknown')
write(999,*) "code_ID",code_ID
write(999,*)
!--Simulation specifics------
write(999,*)
write(999,*) "iYrS,iMonS,iDayS,iHrS,iMinS,iSecS",iYrS,iMonS,iDayS,iHrS,iMinS,iSecS
write(999,*) "iYrE,iMonE,iDayE,iHrE,iMinE,iSecE",iYrE,iMonE,iDayE,iHrE,iMinE,iSecE
write(999,*) "RESTART_FILE_TIMESTEP",RESTART_FILE_TIMESTEP
write(999,*) "dT, dT_out", dT, dT_out
write(999,*) "icent,jcent",icent,jcent
write(999,*)
!--Switches in GEM---------
write(999,*)
write(999,*) "Which_fluxes",Which_fluxes
write(999,*) "Which_temperature",Which_temperature
write(999,*) "Which_uptake",Which_uptake
write(999,*) "Which_quota",Which_quota
write(999,*) "Calibration",Calibration
write(999,*) "Out_1D",Out_1D
write(999,*) "Which_irradiance",Which_irradiance
write(999,*) "Which_chlaC",Which_chlaC
write(999,*) "Which_photosynthesis",Which_photosynthesis
write(999,*) "Which_growth",Which_growth
write(999,*) "SolarRad",SolarRad
write(999,*) "InitializeHow",InitializeHow
write(999,*) 
!--Optics-----------------------
write(999,*)
write(999,*) "Kw",Kw
write(999,*) "Kcdom",Kcdom
write(999,*) "Kspm",Kspm
write(999,*) "Kchla",Kchla
write(999,*) "astar490",astar490
write(999,*) "aw490",aw490
write(999,*) "astarOMA",astarOMA
write(999,*) "astarOMZ",astarOMZ
write(999,*) "astarOMR",astarOMR
write(999,*) "astarOMBC",astarOMBC
write(999,*) "ws(iCDOM)",ws(iCDOM)
write(999,*)
!--Temperature-------------------
write(999,*)
write(999,*) "Tref",Tref
write(999,*) "KTg1",KTg1
write(999,*) "KTg2",KTg2
write(999,*) "Ea_R",Ea_R
write(999,*)
!--Phytoplankton-----------------
write(999,*)
write(999,*) "edibilevector for Z1",ediblevector(1,:)
write(999,*) "edibilevector for Z2",ediblevector(2,:)
write(999,*) "umax",umax
write(999,*) "alpha",alpha
write(999,*) "beta",beta
write(999,*) "respg",respg
write(999,*) "respb",respb
write(999,*) "QminN",QminN
write(999,*) "QminP",QminP
write(999,*) "QmaxN",QmaxN
write(999,*) "QmaxP,",QmaxP
write(999,*) "Kn",Kn
write(999,*) "Kp",Kp
write(999,*) "Ksi",Ksi
write(999,*) "KQn",KQn
write(999,*) "KQp",KQp
write(999,*) "nfQs",nfQs
write(999,*) "vmaxN",vmaxN
write(999,*) "vmaxP",vmaxP
write(999,*) "vmaxSi",vmaxSi
write(999,*) "aN",aN
write(999,*) "volcell",volcell
write(999,*) "Qc",Qc
write(999,*) "Athresh",Athresh
write(999,*) "ws(iA1:iA(nospA))",ws(iA(1):iA(nospA))
write(999,*) "mA",mA
write(999,*)
!--Zooplankton---------------------
write(999,*)
write(999,*) "Zeffic",Zeffic
write(999,*) "Zslop",Zslop
write(999,*) "Zvolcell",Zvolcell
write(999,*) "ZQc",ZQc
write(999,*) "ZQn",ZQn
write(999,*) "ZQp",ZQp
write(999,*) "ZKa",ZKa
write(999,*) "Zrespg",Zrespg
write(999,*) "Zrespb",Zrespb
write(999,*) "Zumax",Zumax
write(999,*) "Zm",Zm
write(999,*)
!--Organic Matter--------------------
write(999,*) 
write(999,*) "KG1",KG1
write(999,*) "KG2",KG2
write(999,*) "KG1_R",KG1_R
write(999,*) "KG2_R",KG2_R
write(999,*) "KG1_BC",KG1_BC
write(999,*) "KG2_BC",KG2_BC
write(999,*) "KNH4",KNH4
write(999,*) "nitmax",nitmax
write(999,*) "KO2",KO2
write(999,*) "KstarO2",KstarO2
write(999,*) "KNO3",KNO3
write(999,*) "pCO2",pCO2
write(999,*) "stoich_x1R",stoich_x1R
write(999,*) "stoich_y1R",stoich_y1R
write(999,*) "stoich_x2R",stoich_x2R
write(999,*) "stoich_y2R",stoich_y2R
write(999,*) "stoich_x1BC",stoich_x1BC
write(999,*) "stoich_y1BC",stoich_y1BC
write(999,*) "stoich_x2BC",stoich_x2BC
write(999,*) "stoich_y2BC",stoich_y2BC
write(999,*) "ws(iOM1_A)",ws(iOM1_A)
write(999,*) "ws(iOM2_A)",ws(iOM2_A)
write(999,*) "ws(iOM1_Z)",ws(iOM1_Z)
write(999,*) "ws(iOM2_Z)",ws(iOM2_Z)
write(999,*) "ws(iOM1_R)",ws(iOM1_R)
write(999,*) "ws(iOM2_R)",ws(iOM2_R)
write(999,*) "ws(iOM1_BC)",ws(iOM1_BC)
write(999,*) "ws(iOM2_BC)",ws(iOM2_BC)
write(999,*) "KGcdom", KGcdom
write(999,*) "CF_SPM", CF_SPM
write(999,*)
!----River Params-----------------------------------
write(999,*)
write(999,*) "rcNO3",rcNO3
write(999,*) "rcNH4",rcNH4
write(999,*) "rcPO4",rcPO4
write(999,*) "rcSi",rcSi
write(999,*)
!----Other Params-----------------------------------
write(999,*)
write(999,*) "Which_VMix",Which_VMix     
write(999,*) "KH_coeff",KH_coeff         
write(999,*) "Which_Outer_BC",Which_Outer_BC
write(999,*) "m_OM_init,m_OM_BC,m_OM_sh",m_OM_init,m_OM_BC,m_OM_sh
write(999,*) "Stoich_x1A_init,Stoich_y1A_init",Stoich_x1A_init,Stoich_y1A_init
write(999,*) "Stoich_x2A_init,Stoich_y2A_init",Stoich_x2A_init,Stoich_y2A_init
write(999,*) "Stoich_x1Z_init,Stoich_y1Z_init",Stoich_x1Z_init,Stoich_y1Z_init
write(999,*) "Stoich_x2Z_init,Stoich_y2Z_init",Stoich_x2Z_init,Stoich_y2Z_init
write(999,*) "KG_bot",KG_bot
write(999,*)
write(999,*) "stoich_z1R,stoich_z1R",stoich_z1R,stoich_z1R
write(999,*) "stoich_z2BC,stoich_z2BC",stoich_z2BC,stoich_z2BC
!---------------------------------------------------
close(999)
return
END SUBROUTINE Write_InputFile 
