! ------------------------------------------------------------------------
      SUBROUTINE calc_Agrow_ij( E, T_ij, Qn, Qp, Si, A_ij, Agrow_ij, &
     & uA_ij, ArespTot_ij, un_k, up_k, uadj_k )       
! ------------------------------------------------------------------------

    USE Model_dim
    USE INPUT_VARS
    USE RKVectorMod

      IMPLICIT NONE

!-------------------------------------------------------------------------
! CALL SUBROUTINE calc_Agrow_ij TO EXECUTE THE DESIRED PHYTOPLANKTON
! PHOTOSYNTHESIS MODEL to calculate the one-D array Agrow_ij. for
! vertical grid column (i,j)
!
! Agrow_ij(k) is the growth-rate  for
! vertical grid column (i,j) at cell (i,j,k).
!-----------------------------------------------------------------------

      
! Declare variables coming thru the interface.
!--------------------------------------------------------------------------
      REAL   , INTENT(IN)  ::  E(nzp1)           ! Irradiance (quanta/cm2/sec) 
                                                 ! at middle of layer k
                                       
      REAL   , INTENT(IN)  ::  T_ij(nzp1)        ! Water temperature. T_ij(k)
                                                 ! is temperature for cell i,j,k

      REAL   , INTENT(IN)  ::  Qn(nospA,nzp1) ! Phytoplankton Nitrogen Quota (mmol-N/cell)         
      REAL   , INTENT(IN)  ::  Qp(nospA,nzp1) ! Phytoplankton Phosphorous Quota (mmol-P/cell)      
      REAL   , INTENT(IN)  ::  Si(nzp1)       ! Silica (mmol-Si/m3)
      
      REAL   , INTENT(IN)  ::  A_ij(nospA,nzp1)  ! A_ij(isp,k) is the number 
                                                 ! density of phytoplankton 
					         ! group isp for cell i,j,k  

      REAL   , INTENT(OUT) ::  Agrow_ij(nospA,nzp1)      
                                               ! Agrow_ij(isp,k) is the 
					       ! temporal "specific"
					       ! growth rate 
					       ! (1/A)*(partial A/partial t)
					       ! of phytoplankton group
                                               ! isp at grid cell i,j,k 
                                               
     REAL   , INTENT(OUT) ::  uA_ij(nospA,nzp1)  ! uA is the temperature adjusted
                                                 ! light factor used in the GEM
                                                 ! model. uA_ij(isp,k) is the
                                                 ! value of uA for phytoplankton
                                                 ! group isp in cell (i,j,k)
                                                                            
      REAL   , INTENT(OUT) ::  ArespTot_ij(nospA,nzp1)       	
                                               ! Total phytoplankton
					       ! respiration, including
					       ! Dark respiration. 
      	                                       ! ArespTot_ij(isp,k) is the
					       ! total respiration for phyto.
					       ! group isp in cell (i,j,k).

      real,intent(OUT)     :: un_k(nospA,nsl)
      real,intent(OUT)     :: up_k(nospA,nsl)
      real,intent(OUT)     :: uadj_k(nospA,nsl)

! --------------------------------------------------------------------------   
      INTEGER  ::  k, isp ! loop indicies     
!------------------------------------------------------	
! Declare real scalar variables
!------------------------------------------------------
      real,dimension(nospA+nospG)     :: Tadj ! Temperature adjustment factor, variable and function 
      real,dimension(nospA)           :: uA   ! Specific growth, 1/d      
!--------------------------------------------- 
      real, dimension(nospA) :: f_E             ! Light growth function 
      real, dimension(nospA) :: f_N, f_P, f_Si  ! Nutrient growth functions
      real, dimension(nospA) :: min_S           ! Limiting substrate values
!------------------------------------------------------------------------
!----------------     
! Begin main code
!---------------- 
!******************************************************************
!-------------------------------
! Begin growth rate calculations
!-------------------------------

         do k = 1, nz

                 call func_T( T_ij(k), Tadj ) ! Temperature adjustment
                 call func_S( Qn(:,k), Qp(:,k), Si(k), f_N, f_P, f_Si ) ! Nutrient dependent growth function
                 do isp = 1, nospA
                    min_S(isp) = AMIN1( f_N(isp), f_P(isp), f_Si(isp) )
                 enddo
                 call func_E( E(k), min_S, f_E ) ! Light growth function


             !Output variables for netCDF to examine light vs. nutrient limitations 
                 un_k(:,k) = f_N(:) 
                 up_k(:,k) = f_P(:)
                 uadj_k(:,k) = f_E(:) 

!--------------------------------------------------------------------------
                if(Specific_Growth.eq.1) then
                   do isp=1,nospA
                      uA(isp) = mumax(isp) * Tadj(isp) * AMIN1(min_S(isp),f_E(isp)) ! Minimum Formulation
                   enddo
                else if(Specific_Growth.eq.2.or.Specific_Growth.eq.3) then
                   uA(:) = mumax(:) * Tadj(1:nospA) * f_E(:) * min_S(:)   ! Product Formultation
                else !Let default be Minimum Formulation
                   do isp=1,nospA
                      uA(isp) = mumax(isp) * Tadj(isp) * AMIN1(min_S(isp),f_E(isp)) ! Minimum Formulation
                   enddo
                endif

                 uA_ij(:,k) = uA(:)                            ! Save specific growth rate to array, 1/d
                 Agrow_ij(:,k)   = A_ij(:,k)*uA(:)  !BUG: *Tadj(1:nospA)     ! Phytoplankton growth, cells/m3/d

!-----------------------------------------      
! Calculate the total respiration ArespTot 
!-----------------------------------------
                 ArespTot_ij(:,k) =  Agrow_ij(:,k) * respAp(:)               &  ! Growth dependent respiration (loss of cells), cells/m3/d
     &                                  + Tadj(1:nospA)  * respApDark(:) * A_ij(:,k)  ! Basal respiration (loss of cells) , cells/m3/d



                 
      ENDDO      ! End of the "DO   k = 1, nz   " block DO 	  	 

           
      RETURN
      END SUBROUTINE  calc_Agrow_ij
!***********************************************************************
