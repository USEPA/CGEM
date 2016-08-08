Module EPA_GEM_Params

USE Model_dim

!---------------------------------------------------------      
!-A; Phytoplankton number density (cells/m3);
!---------------------------------------------------------  
      
      integer iA(nospA)
!---------------------------
      data iA/1,2,3,4,5,6/      
!-------------------------------------------------------------      
      integer, parameter :: iA1 = 1, iA2 = 2, iA3 = 3, iA4 = 4
      integer, parameter :: iA5 = 5, iA6 = 6          
!--------------------------------------------------------------------

!----------------------------------------------------------------------
!-Qn: Phytoplankton Nitrogen Quota (mmole-N/cell)
!----------------------------------------------------------------------
      integer iQn(nospA)
      data iQn/7,8,9,10,11,12/      
!----------------------------------------------------------------  
      integer, parameter :: iQn1 =  7, iQn2 =  8, iQn3 =  9, iQn4 = 10
      integer, parameter :: iQn5 = 11, iQn6 = 12      
!--------------------------------------------------------------------
!--------------------------------- 
      integer iQp(nospA)
      data iQp/13,14,15,16,17,18/
!----------------------------------------------------------------
      integer, parameter :: iQp1 = 13, iQp2 = 14, iQp3 = 15, iQp4 = 16     
      integer, parameter :: iQp5 = 17, iQp6 = 18    
!--------------------------------------------------------------------
!-G: Zooplankton number density (individuals/m3);
      integer iG(nospG)
      data iG/19,20/			       
      integer, parameter :: iG1 = 19, iG2 = 20       

!-------------------------------
!-NO3; Nitrate (mmole-N/m3)
!-------------------------------
      integer, parameter :: iNO3 = 21  
!--------------------------------      
!-NH4; Ammonium (mmole-N/m3)
!--------------------------------
      integer, parameter :: iNH4 = 22 
!-------------------------------------------        
!-PO4: Phosphate (mmolE-P/m3)
!--------------------------------------
      integer, parameter :: iPO4 = 23
!---------------------------------------------------------
!-DIC: Dissolved Inorganic Carbon (mmolE-C/m3) 
!---------------------------------------------------------
      integer, parameter :: iDIC = 24 
!-------------------------------------------        
!-O2: Molecular Oxygen (mmolE-C/m3)
!------------------------------
      integer, parameter :: iO2 = 25 
!-------------------------------------------------------------
!-OM1_A: (mmole-C/m3--particulate)
!        -- Particulate Organic Matter arising from 
!           dead Phytoplankton
!-------------------------------------------------------------
      integer, parameter :: iOM1_A = 26  
!-----------------------------------------------------------------
!-OM2_A: (mmole-C/m3--dissolved)
!        -- Dissolved Organic Matter arising from 
!           dead Phytoplankton 
!------------------------------------------------------------------
      integer, parameter :: iOM2_A = 27 
!-------------------------------------------------------------
!-OM1_fp:(mmole-C/m3--particulate)
!        -- Particulate Organic Matter arising from 
!           Zooplankton fecal pellets.
!-------------------------------------------------------------
      integer, parameter :: iOM1_fp = 28 
!-------------------------------------------------        
!-OM2_fp:(mmole-C/m3--dissolved)
!        -- Dissolved Organic Matter arising from 
!          Zooplankton fecal pellets.
!-----------------------------------------------
      integer, parameter :: iOM2_fp = 29 
!--------------------------------------------------------------------
!-OM1_rp: (mmole-C/m3--particulate)
!         -- Particulate Organic Matter arising from river
!            outflow into the Gulf
!--------------------------------------------------------------------
      integer, parameter :: iOM1_rp = 30   
!-------------------------------------------------      
!-OM2_rp: (mmole-C/m3--dissolved)
!         -- Dissolved Organic Matter arising from river
!            outflow
!--------------------------------------------------------------------
      integer, parameter :: iOM2_rp = 31 
!-------------------------------------------
!-CDOM: (ppb) 
!        -- Colored Dissolved Organic Matter
!-------------------------------------------
      integer, parameter :: iCDOM = 32 
!---------------------------------------------
!-Silica: (mmole-Si/m3) 
!        -- Silica
!-------------------------------------------
      integer, parameter :: iSi = 33
!--------------------------------------------------------------------
!-OM1_bc: (mmole-C/m3--particulate)
!         -- Particulate Organic Matter in initial and boundary 
!            conditions 
!--------------------------------------------------------------------
      integer, parameter :: iOM1_bc = 34
!-------------------------------------------------
!-OM2_bc: (mmole-C/m3--dissolved)
!         -- Dissolved Organic Matter in initial and boundary
!            conditions
!--------------------------------------------------------------------
      integer, parameter :: iOM2_bc = 35


    REAL, PARAMETER :: C2_chla_mg       = 3.00203293024339E-09 
    REAL, PARAMETER :: C2_chla_mg_inv   = 1./C2_chla_mg

END MODULE EPA_GEM_Params   
