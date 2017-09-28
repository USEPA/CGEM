Subroutine Salinity_Regression_Init_CGEM()

  USE Model_dim 
  USE CGEM_vars 
  USE INPUT_VARS
  USE INPUT_VARS_CGEM
  USE mdepth2press
  USE mrhoinsitu
  USE mvars
  USE State_Vars
  USE Grid
  USE Fill_Value
  USE Hydro, ONLY:S,T

  implicit none

  real temp,temp_OM1,temp_OM2,total_chl(nospA)
  real, dimension(1) :: pdbar, rhois !pressure, density
  integer i,j,k,isp

    !f(:,:,:,iTr) = 0.

    do j = 1,jm
      do i = 1,im 

        do k=1,nza(i,j)


            !Chla
            temp = -0.67 * S(i,j,k) + 0.01 * d_sfc(i,j,k) + 24.50
            total_chl(:) = AMAX1(temp,0.01)*A_wt(:) ! in mg.

            ! Convert Chla to A
            ! Convert total Chl to cells
            ! Divide by number of groups
            do isp=1,nospA
             f( i, j, k, iA(isp) ) = total_chl(isp) * CChla(isp)/12./Qc(isp) 
            enddo

            ! Zooplankton based on E&R ratio (1000 zooplankton)
            f( i, j, k, iZ(1) ) = 2.e-6 * SUM(f( i, j, k, iA(:)))/3./real(nospZ)   
            do isp=2,nospZ
             f( i, j, k, iZ(isp) ) = 2.e-5 * SUM(f( i, j, k, iA(:)))/3./real(nospZ)
            enddo

            !NO3
            temp = -7.57*S(i,j,k) + 0.04*d_sfc(i,j,k) + 0.11*S(i,j,k)*S(i,j,k) + 125.32
            f( i, j, k, iNO3 ) = AMAX1(temp,0.01)

            !NH4
            temp = -0.06*S(i,j,k) + 3.14
            f( i, j, k, iNH4 ) = AMAX1(temp,0.01)

            !Si
            temp = -5.08*S(i,j,k) + 0.07*d_sfc(i,j,k) + 0.08*S(i,j,k)*S(i,j,k) - 0.00029*d_sfc(i,j,k)*d_sfc(i,j,k) + 86.97
            f( i, j, k, iSi ) = AMAX1(temp,0.01)

            !PO4 = DIP
            temp = -0.11*S(i,j,k) + 1.77
            f( i, j, k, iPO4 ) = AMAX1(temp,0.01)

            !DIC
            temp = -36.89*S(i,j,k) + 3.06*d_sfc(i,j,k) + 0.81*S(i,j,k)*S(i,j,k) - 0.04*d_sfc(i,j,k)*d_sfc(i,j,k) + 2438.05
            f( i, j, k, iDIC ) = AMAX1(temp,0.01)

            !O2
            temp = -18.02*S(i,j,k) -0.37*d_sfc(i,j,k) +0.23*S(i,j,k)*S(i,j,k) + 521.72
            temp = AMAX1(temp,0.01)
            f( i, j, k, iO2 ) = AMIN1(temp,415.)

            !CDOM
            temp = -0.36*S(i,j,k) + 1.37
            temp = AMAX1(temp,0.01)
            !CONVERT CDOM 412 to 312
            !a312 = a412 X exp(-S*(312-412)), where S = 0.016 (S was obtained from D’Sa and Dimarco 2009).
            temp = temp * exp(-0.016*(312.-412.))
            !CONVERT CDOM 312 to ppb:
            ! calculate CDOM (QSE ppb): CDOM (QSE ppb) = a(312)*2.933 + 0.538
            f(i, j, k, iCDOM) = temp * 2.933 + 0.538

            f( i, j, k, iOM1_A ) = 0.
            f( i, j, k, iOM2_A ) = 0.
            f( i, j, k, iOM1_Z ) = 0.
            f( i, j, k, iOM2_Z ) = 0.
            f( i, j, k, iOM1_R ) = 0.
            f( i, j, k, iOM2_R ) = 0.

            !Initialize POC and DIC into OM_BC terms times a multiplier specified at input
            !PC==OM1
            temp = -3.97 * S(i,j,k) - 0.01 * d_sfc(i,j,k) + 157.42
            temp_OM1 = AMAX1(temp,0.01)
            f( i, j, k, iOM1_BC ) = temp_OM1 * m_OM_init
            !write(6,*) "INIT",temp_OM1 , m_OM_init

            !DOC==OM2
            temp = -6.39 * S(i,j,k) - 0.40 * d_sfc(i,j,k) + 338.89
            temp_OM2 = AMAX1(temp,0.01)
            f( i, j, k, iOM2_BC )= temp_OM2 * m_OM_init

            do isp=1,nospA
             f( i, j, k, iQn(isp) ) = 15.*QmaxP(isp)
             f( i, j, k, iQp(isp) ) = QmaxP(isp)
            enddo

            !Set Alkalinity, use Figure 2 from Cai 2003, in mmol/kg, convert to mmol/m3
            ! y = -(0.01586)x + 2.9573
            call depth2press(d_sfc(i,j,k), lat(i,j), pdbar, 1)

            call rhoinsitu(S(i,j,k), T(i,j,k), pdbar, 1, rhois)

            f(i,j,k,iALK) = (-0.01586*S(i,j,k) + 2.9573)*rhois(1)

            f(i, j, k, iTr) = 1./Vol(i,j,k)
          
      enddo
    enddo
    enddo

END Subroutine Salinity_Regression_Init_CGEM


!Regression y  =  b1*Salinity + b2*Depth + b3*Salinity^2 + b4*Depth^2 + Intercept
!For depths > 200 m (100 m for DIC), set y = mean_deep									
!				coeffs						
!	Var	n	Intercept	b1	b2	b3	b4	mean_deep*	R2	
!	O2	31,573	521.72	      -18.02	-0.37	0.23	0.00	112.77	     0.15	
!	NO3	2,423	125.32	      -7.57	0.04	0.11	0.00	21.85	     0.50	
!	NH4	2,404	1.88	       0.04	0.00	0.00	0.00	0.22	     0.01	
!	DIN	2,392	127.42	      -7.55	0.04	0.11	0.00	22.06	     0.46	
!	DON	1,977	6.80	       0.73	0.03	-0.02	0.00	10.04	     0.09	
!	PN	1,825	13.69	       0.12	0.01	-0.01	0.00	0.36	     0.16	
!	DIP	2,424	1.77	      -0.11	0.00	0.00	0.00	1.23	     0.06	
!	DOP	2,179	0.27	       0.01	0.00	0.00	0.00	0.10	     0.02	
!	PP	1,810	0.91	      -0.01	0.00	0.00	0.00	0.04	     0.22	
!	DIC	1,670	2438.05	      -36.89	3.06	0.81	-0.04	2205.33	     0.35	
!	DOC	1,469	189.23	       4.66	0.03	-0.20	0.00	188.61	     0.29	
!	PC	1,833	127.32	      -1.64	0.11	-0.04	0.00	7.30	     0.31	
!	Chla	2,110	30.06	      -1.05	0.02	0.01	0.00	0.08	     0.36	
!	SPM	1,073	-6.94	       1.15	0.05	-0.03	0.00	3.57	     0.03	
!	CDOM**			                0.01					
!	*mean concentration for depths > 200 m (100 m for DIC)									
!	**best guess for CDOM(a488) at the open boundary									
! Set any negative values to 0.01; in a few cases the regressions still produce negative values
!CDOM, instead, use: a412 = -0.36*Salinity + 1.37;
!  	Si	2324	86.96666104	-5.079115752	0.070930806	0.079130394	-0.000286726	11.67499995	0.436898807
