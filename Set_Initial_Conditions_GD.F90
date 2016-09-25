      Subroutine Set_Initial_Conditions_GD(f,S,D,fm)

      USE Model_dim
      USE STATES 
      USE EUT, ONLY:CCHLD,CCHLG

      implicit none
 
      real f(im,jm,nsl,nf),S(im,jm,nsl),D(im,jm,nsl)!S=Salinity,D=Depth
      real temp
      integer i,j,k,fm(im,jm)

      do k = 1,nsl
      do j = 1,jm
      do i = 1,im
         if(fm(i,j).eq.1) then
!Regression y  =  b1*Salinity + b2*Depth + b3*Salinity^2 + b4*Depth^2 + Intercept
     !DOC
      temp = -6.39*S(i,j,k)  -0.40*D(i,j,k) + 338.89
      temp = AMAX1(temp,0.01)
      f( i, j, k, JDOC) = temp*12.e-6 !mmol/m3->kg/m3

     !Chla==DIA,GRE, Chla is in mg
      temp = -0.67*S(i,j,k) + 0.01*D(i,j,k) + 24.50
      temp = AMAX1(temp,0.01)
      f( i, j, k, JDIA) = 0.9*temp*1.e-6*CCHLD !mg/m3->kg/m3
      f( i, j, k, JGRE) = 0.1*temp*1.e-6*CCHLG !mg/m3->kg/m3

     !ZOO
      f( i, j, k, JZOO) = 22.6*1.e-6 !mg/m3==kg/m3

     !PC==LOC,ROC
      temp = -3.97*S(i,j,k)  -0.01*D(i,j,k) + 157.42
      temp = AMAX1(temp,0.01)
      f( i, j, k, JLOC) = 0.9*temp*12.e-6 !mmol/m3->kg/m3
      f( i, j, k, JROC) = 0.1*temp*12.e-6 !mmol/m3->kg/m3

     !DIP==SRP
      temp = -0.11*S(i,j,k) + 1.77
      temp = AMAX1(temp,0.01)
      f( i, j, k, JSRP ) = temp*31.e-6 !mmol/m3->kg/m3

     !DOP
      temp = 0.01*S(i,j,k) + 0.27 
      temp = AMAX1(temp,0.01)
      f( i, j, k, JDOP) = temp*31.e-6 !mmol/m3->kg/m3

     !PP==LOP,ROP
      temp = -0.01*S(i,j,k) + 0.91 
      temp = AMAX1(temp,0.01)
      f( i, j, k, JLOP) = 0.9*temp*31.e-6 !mmol/m3->kg/m3
      f( i, j, k, JROP) = 0.1*temp*31.e-6 !mmol/m3->kg/m3

     !NH4
      temp = -0.06*S(i,j,k) + 3.14
      temp = AMAX1(temp,0.01)
      f( i, j, k, JNH4 ) = temp*14.e-6 !mmol/m3->kg/m3 

     !NO3
      temp = -7.57*S(i,j,k) + 0.04*D(i,j,k) + 0.11*S(i,j,k)*S(i,j,k) + 125.32 
      temp = AMAX1(temp,0.01)
      f( i, j, k, JNO3 ) = temp*14.e-6 !mmol/m3->kg/m3

     !DON
      temp = 0.73*S(i,j,k) + 0.03*D(i,j,k) -0.02*S(i,j,k)*S(i,j,k) + 6.80
      temp = AMAX1(temp,0.01)
      f( i, j, k, JDON) = temp*14.e-6 !mmol/m3->kg/m3

     !PN==LON,RON
      temp = 0.12*S(i,j,k) + 0.01*D(i,j,k) -0.01*S(i,j,k)*S(i,j,k) + 13.69
      temp = AMAX1(temp,0.01)
      f( i, j, k, JLON) = 0.9*temp*14.e-6 !mmol/m3->kg/m3
      f( i, j, k, JRON) = 0.1*temp*14.e-6 !mmol/m3->kg/m3

     !Si==SA
      temp = -5.08*S(i,j,k) + 0.07*D(i,j,k) + 0.08*S(i,j,k)*S(i,j,k) - 0.00029*D(i,j,k)*D(i,j,k) + 86.97
      temp = AMAX1(temp,0.01)
      f( i, j, k, JSA ) = temp*28.e-6 !mmol/m3->kg/m3

     !SU
      f( i, j, k, JSU ) = 0.275*0.001 !mg/L->kg/m3 

     !DO2
      temp = -18.02*S(i,j,k) -0.37*D(i,j,k) +0.23*S(i,j,k)*S(i,j,k) + 521.72  
      temp = AMAX1(temp,0.01) 
      f( i, j, k, JDO2 )    = AMIN1(temp,415.)*32.e-6 !mmol/m3->kg/m3

     !TR
      f( i, j, k, JTR ) = 1. 

      else

       f( i, j, k, : ) = -9999.

      endif
      enddo
      enddo
      enddo





      end subroutine Set_Initial_Conditions_GD


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
