Module CGEM_vars

!CGEM STATE VARIABLES
IMPLICIT NONE

SAVE

!---------------------------------------------------------      
!-A; Phytoplankton number density (cells/m3);
!---------------------------------------------------------  
      integer, dimension(:), ALLOCATABLE :: iA(:)
!----------------------------------------------------------------------
!-Qn: Phytoplankton Nitrogen Quota (mmol-N/cell)
!----------------------------------------------------------------------
      integer, dimension(:), ALLOCATABLE :: iQn(:)
!----------------------------------------------------------------------
!-Qp: Phytoplankton Phosphorus Quota (mmol-P/cell)
!----------------------------------------------------------------------
      integer, dimension(:), ALLOCATABLE :: iQp(:)
!--------------------------------------------------------------------
!-Z: Zooplankton number density (individuals/m3);
!--------------------------------------------------------------------
      integer, dimension(:), ALLOCATABLE :: iZ(:)
!-------------------------------
!-NO3; Nitrate (mmol-N/m3)
!-------------------------------
      integer :: iNO3 
!--------------------------------      
!-NH4; Ammonium (mmol-N/m3)
!--------------------------------
      integer :: iNH4
!-------------------------------------------        
!-PO4: Phosphate (mmol-P/m3)
!--------------------------------------
      integer :: iPO4 
!---------------------------------------------------------
!-DIC: Dissolved Inorganic Carbon (mmol-C/m3) 
!---------------------------------------------------------
      integer :: iDIC 
!-------------------------------------------        
!-O2: Molecular Oxygen (mmol-O2/m3)
!------------------------------
      integer :: iO2 
!-------------------------------------------------------------
!-OM1_A: (mmol-C/m3--particulate)
!        -- Particulate Organic Matter arising from 
!           dead Phytoplankton
!-------------------------------------------------------------
      integer :: iOM1_A 
!-----------------------------------------------------------------
!-OM2_A: (mmol-C/m3--dissolved)
!        -- Dissolved Organic Matter arising from 
!           dead Phytoplankton 
!------------------------------------------------------------------
      integer :: iOM2_A 
!-------------------------------------------------------------
!-OM1_Z:(mmol-C/m3--particulate)
!        -- Particulate Organic Matter arising from 
!           Zooplankton fecal pellets.
!-------------------------------------------------------------
      integer :: iOM1_Z 
!-------------------------------------------------        
!-OM2_Z:(mmol-C/m3--dissolved)
!        -- Dissolved Organic Matter arising from 
!          Zooplankton fecal pellets.
!-----------------------------------------------
      integer :: iOM2_Z 
!--------------------------------------------------------------------
!-OM1_R: (mmol-C/m3--particulate)
!         -- Particulate Organic Matter arising from river outflow
!--------------------------------------------------------------------
      integer :: iOM1_R  
!-------------------------------------------------      
!-OM2_R: (mmol-C/m3--dissolved)
!         -- Dissolved Organic Matter arising from river outflow
!--------------------------------------------------------------------
      integer :: iOM2_R 
!-------------------------------------------
!-CDOM: (ppb) 
!        -- Colored Dissolved Organic Matter
!-------------------------------------------
      integer :: iCDOM  
!---------------------------------------------
!-Silica: (mmol-Si/m3) 
!        -- Silica
!-------------------------------------------
      integer :: iSi
!--------------------------------------------------------------------
!-OM1_BC: (mmol-C/m3--particulate)
!         -- Particulate Organic Matter in initial and boundary 
!            conditions 
!--------------------------------------------------------------------
      integer :: iOM1_BC 
!-------------------------------------------------
!-OM2_BC: (mmol-C/m3--dissolved)
!         -- Dissolved Organic Matter in initial and boundary
!            conditions
!--------------------------------------------------------------------
      integer :: iOM2_BC
!-------------------------------------------
!-ALK:  (mmol-HCO3/m3)?
!        -- Alkalinity
!-------------------------------------------
      integer :: iALK 
!Tracer
      integer :: iTR

contains

Subroutine CGEM_vars_allocate()

USE Model_dim, ONLY: nospA, nospZ

integer i
integer :: counter = 0 
!---------------------------------------------------------      
!-A; Phytoplankton number density (cells/m3);
!---------------------------------------------------------  
       ALLOCATE (iA(nospA))
       do i=1,nospA
          counter = counter+1
          iA(i) = counter 
       enddo
!----------------------------------------------------------------------
!-Qn: Phytoplankton Nitrogen Quota (mmol-N/cell)
!----------------------------------------------------------------------
      ALLOCATE (iQn(nospA))
       do i=1,nospA
          counter = counter+1
          iQn(i) = counter
       enddo
!----------------------------------------------------------------------
!-Qp: Phytoplankton Phosphorus Quota (mmol-P/cell)
!----------------------------------------------------------------------
      ALLOCATE (iQp(nospA))
       do i=1,nospA
          counter = counter+1
          iQp(i) = counter
       enddo
!--------------------------------------------------------------------
!-Z: Zooplankton number density (individuals/m3);
!--------------------------------------------------------------------
      ALLOCATE (iZ(nospZ))
       do i=1,nospZ
          counter = counter+1
          iZ(i) = counter
       enddo
!-------------------------------
!-NO3; Nitrate (mmol-N/m3)
!-------------------------------
      iNO3 = counter+1 
!--------------------------------      
!-NH4; Ammonium (mmol-N/m3)
!--------------------------------
      iNH4 = counter+2 
!-------------------------------------------        
!-PO4: Phosphate (mmol-P/m3)
!--------------------------------------
      iPO4 = counter+3 
!---------------------------------------------------------
!-DIC: Dissolved Inorganic Carbon (mmol-C/m3) 
!---------------------------------------------------------
      iDIC = counter+4 
!-------------------------------------------        
!-O2: Molecular Oxygen (mmol-O2/m3)
!------------------------------
      iO2 = counter+5 
!-------------------------------------------------------------
!-OM1_A: (mmol-C/m3--particulate)
!        -- Particulate Organic Matter arising from 
!           dead Phytoplankton
!-------------------------------------------------------------
      iOM1_A = counter+6  
!-----------------------------------------------------------------
!-OM2_A: (mmol-C/m3--dissolved)
!        -- Dissolved Organic Matter arising from 
!           dead Phytoplankton 
!------------------------------------------------------------------
      iOM2_A = counter+7 
!-------------------------------------------------------------
!-OM1_Z:(mmol-C/m3--particulate)
!        -- Particulate Organic Matter arising from 
!           Zooplankton fecal pellets.
!-------------------------------------------------------------
      iOM1_Z = counter+8 
!-------------------------------------------------        
!-OM2_Z:(mmol-C/m3--dissolved)
!        -- Dissolved Organic Matter arising from 
!          Zooplankton fecal pellets.
!-----------------------------------------------
      iOM2_Z = counter+9 
!--------------------------------------------------------------------
!-OM1_R: (mmol-C/m3--particulate)
!         -- Particulate Organic Matter arising from river outflow
!--------------------------------------------------------------------
      iOM1_R = counter+10   
!-------------------------------------------------      
!-OM2_R: (mmol-C/m3--dissolved)
!         -- Dissolved Organic Matter arising from river outflow
!--------------------------------------------------------------------
      iOM2_R = counter+11 
!-------------------------------------------
!-CDOM: (ppb) 
!        -- Colored Dissolved Organic Matter
!-------------------------------------------
      iCDOM = counter+12 
!---------------------------------------------
!-Silica: (mmol-Si/m3) 
!        -- Silica
!-------------------------------------------
      iSi = counter+13 
!--------------------------------------------------------------------
!-OM1_BC: (mmol-C/m3--particulate)
!         -- Particulate Organic Matter in initial and boundary 
!            conditions 
!--------------------------------------------------------------------
      iOM1_BC = counter+14
!-------------------------------------------------
!-OM2_BC: (mmol-C/m3--dissolved)
!         -- Dissolved Organic Matter in initial and boundary
!            conditions
!--------------------------------------------------------------------
      iOM2_BC = counter+15
!-------------------------------------------
!-ALK:  (mmol-HCO3/m3)?
!        -- Alkalinity
!-------------------------------------------
      iALK = counter+16
!Tracer
      iTR = counter+17

return
END Subroutine CGEM_vars_allocate

END MODULE CGEM_vars

