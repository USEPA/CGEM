! ------------------------------------------------------------------------
      Subroutine calc_Agrow( E, T_k, Qn, Qp, Si, A_k, Agrow_k, &
     & uA_k, Aresp_k, uN_k, uP_k, uE_k, uSi_k )       
! ------------------------------------------------------------------------

      USE Model_dim
      USE INPUT_VARS

      IMPLICIT NONE

!-------------------------------------------------------------------------
! Call subroutine calc_Agrow to execute the desired phytoplankton 
! growth model to calculate the one-D array (water column) Agrow_k 
!-----------------------------------------------------------------------
! -- Declare variables coming thru the interface ---------------------
      real,intent(in)  ::  E(nsl)        ! Irradiance (quanta/cm2/sec) 
                                         ! at middle of layer k
                                       
      real,intent(in)  ::  T_k(nsl)      ! Water temperature in Celsius

      real,intent(in)  ::  Qn(nospA,nsl) ! Phytoplankton Nitrogen Quota (mmol-N/cell)         
      real,intent(in)  ::  Qp(nospA,nsl) ! Phytoplankton Phosphorous Quota (mmol-P/cell)      
      real,intent(in)  ::  Si(nsl)       ! Silica (mmol-Si/m3)
      
      real,intent(in)  ::  A_k(nospA,nsl)      ! Number density of phytoplankton group isp 
      real,intent(out) ::  Agrow_k(nospA,nsl)  ! Specific growth rate    
					       ! of phytoplankton group isp
                                               
      real,intent(out) ::  uA_k(nsl,nospA)     ! Temperature adjusted light factor
                                               ! phytoplankton group isp
                                                                            
      real,intent(out) ::  Aresp_k(nospA,nsl)    ! Phytoplankton respiration of group       	
					         ! isp, including dark respiration. 

      real,intent(out)     :: uN_k(nsl,nospA)  ! Nitrogen limited growth rate (1/d)
      real,intent(out)     :: uP_k(nsl,nospA)  ! Phosphorus limited growth rate (1/d)
      real,intent(out)     :: uE_k(nsl,nospA)  ! Light limited growth rate (1/d)
      real,intent(out)     :: uSi_k(nsl,nospA) ! Silica limited growth rate (1/d)

! -- Local variables --------------------------------------------------------------   
      integer :: k, isp ! loop indices     
      real,dimension(nospA+nospZ) :: Tadj      ! Temperature adjustment factor, variable and function 
      real,dimension(nospA) :: uA              ! Specific growth, 1/d      
      real,dimension(nospA) :: f_E             ! Light growth function 
      real,dimension(nospA) :: f_N, f_P, f_Si  ! Nutrient growth functions
      real,dimension(nospA) :: min_S           ! Limiting substrate values
      real,dimension(nospA) :: respg2          ! Actual respiration coefficient
!------------------------------------------------------------------------
!-------------------------------
! Begin growth rate calculations
!-------------------------------

       do k = 1, nz

          call func_T( T_k(k), Tadj ) ! Temperature adjustment
          call func_S( Qn(:,k), Qp(:,k), Si(k), f_N, f_P, f_Si ) ! Nutrient dependent growth function
          do isp = 1, nospA
             min_S(isp) = AMIN1( f_N(isp), f_P(isp), f_Si(isp) )
          enddo
          call func_E( E(k), min_S, f_E ) ! Light growth function

      !Output variables for netCDF to examine light vs. nutrient limitations 
          uN_k(k,:)   = f_N(:)  * umax(:) * Tadj(1:nospA) 
          uP_k(k,:)   = f_P(:)  * umax(:) * Tadj(1:nospA)
          uE_k(k,:)   = f_E(:)  * umax(:) * Tadj(1:nospA) 
          uSi_k(k,:)  = f_Si(:) * umax(:) * Tadj(1:nospA)

         if(Which_growth.eq.1) then
            do isp=1,nospA
               uA(isp) = umax(isp) * Tadj(isp) * AMIN1(min_S(isp),f_E(isp)) ! Minimum Formulation
            enddo
         else if(Which_growth.eq.2) then
            uA(:) = umax(:) * Tadj(1:nospA) * f_E(:) * min_S(:)   ! Product Formulation
         else if(Which_growth.eq.3) then
            uA(:) = umax(:) * Tadj(1:nospA) * f_E(:) * min_S(:) ! Nutrient dependence is in f_E
         else !Let default be Minimum Formulation
            do isp=1,nospA
               uA(isp) = umax(isp) * Tadj(isp) * AMIN1(min_S(isp),f_E(isp)) ! Minimum Formulation
            enddo
         endif

          uA_k(k,:)      = uA(:)           ! Save specific growth rate to array for netCDF, 1/d
          Agrow_k(:,k)   = A_k(:,k)*uA(:)  ! Phytoplankton growth, cells/m3/d

      ! If uA < 0.25d-1, set respiration to zero; Laws and Bannister(1980) 
         do isp=1,nospA
            if(uA(isp).lt.0.25) then
              respg2(isp) = 0.
            else
              respg2(isp) = respg(isp)
            endif
         enddo

      !-----------------------------------------      
      ! Calculate the total respiration Aresp
      !-----------------------------------------
          Aresp_k(:,k) =  Agrow_k(:,k) * respg2(:)         &     ! Growth dependent respiration (loss of cells), cells/m3/d
     &                   + Tadj(1:nospA)  * respb(:) * A_k(:,k)  ! Basal respiration (loss of cells) , cells/m3/d

      enddo    
         
  
      RETURN
      END Subroutine calc_Agrow
