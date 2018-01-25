Subroutine Read_InputFile_CGEM(filename)

USE Model_dim
USE INPUT_VARS
USE INPUT_VARS_CGEM
USE LIGHT_VARS
USE CGEM_vars 
USE TEMP_VARS
USE DATE_TIME
USE STOICH_VARS

IMPLICIT NONE


integer i,j,k,icent_jcent_units
integer isp,isz
real i_in,j_in,tot,x
real, parameter :: SDay = 86400.0  ! # of sec in 24 hr day
character(120) filename
real eps

ws(1:nf) = 0.
!--Code Identifier--------------
open(unit=999,file=filename,form='formatted',status='old')
read(999,*) code_ID
read(999,*)
!--Simulation specifics------
read(999,*)
read(999,*) iYrS,iMonS,iDayS,iHrS,iMinS,iSecS
read(999,*) iYrE,iMonE,iDayE,iHrE,iMinE,iSecE
read(999,*) dT, dT_out
read(999,*) icent_jcent_units
read(999,*) i_in, j_in
read(999,*)
!--Switches in GEM---------
read(999,*)
read(999,*) Which_fluxes
read(999,*)  !Comment line
read(999,*) Which_temperature
read(999,*) Which_uptake
read(999,*) Which_quota
read(999,*) Which_irradiance 
read(999,*) Which_chlaC
read(999,*) Which_photosynthesis 
read(999,*) Which_growth
read(999,*) Read_Solar,Read_Wind,Read_T,Read_Sal 
read(999,*) InitializeHow
read(999,*) 
!--Optics-----------------------
read(999,*)
read(999,*) Kw
read(999,*) Kcdom
read(999,*) Kspm
read(999,*) Kchla

read(999,*) astar490
read(999,*) aw490
read(999,*) astarOMA
read(999,*) astarOMZ
read(999,*) astarOMR
read(999,*) astarOMBC
read(999,*) PARfac
read(999,*) ws(iCDOM)
read(999,*)
!--Temperature-------------------
read(999,*)
read(999,*) (Tref(i), i=1,nospA+nospZ)
read(999,*) (KTg1(i), i=1,nospA+nospZ)
read(999,*) (KTg2(i), i=1,nospA+nospZ)
read(999,*) (Ea(i), i=1,nospA+nospZ)
read(999,*)
!--Phytoplankton-----------------
read(999,*)
do isz=1,nospZ
read(999,*) (ediblevector(isz,i), i=1,nospA)
enddo
read(999,*) (umax(i), i=1,nospA)
read(999,*) (CChla(i), i=1,nospA)
read(999,*) (alpha(i), i=1,nospA)
read(999,*) (beta(i), i=1,nospA)
read(999,*) (respg(i), i=1,nospA)
read(999,*) (respb(i), i=1,nospA)
read(999,*) (QminN(i), i=1,nospA)
read(999,*) (QminP(i), i=1,nospA)
read(999,*) (QmaxN(i), i=1,nospA)
read(999,*) (QmaxP(i), i=1,nospA)
read(999,*) (Kn(i), i=1,nospA)
read(999,*) (Kp(i), i=1,nospA)
read(999,*) (Ksi(i), i=1,nospA)
read(999,*) (KQn(i), i=1,nospA)
read(999,*) (KQp(i), i=1,nospA)
read(999,*) (nfQs(i), i=1,nospA)
read(999,*) (vmaxN(i), i=1,nospA)
read(999,*) (vmaxP(i), i=1,nospA)
read(999,*) (vmaxSi(i), i=1,nospA)
read(999,*) (aN(i), i=1,nospA)
read(999,*) (volcell(i), i=1,nospA)
read(999,*) (Qc(i), i=1,nospA)
read(999,*) (Athresh(i), i=1,nospA)
read(999,*) (ws(i), i=1,nospA)
read(999,*) (mA(i), i=1,nospA)
read(999,*) (A_wt(i), i=1,nospA)
read(999,*)
!--Zooplankton---------------------
read(999,*)
read(999,*) (Zeffic(i), i=1,nospZ)
read(999,*) (Zslop(i), i=1,nospZ)
read(999,*) (Zvolcell(i), i=1,nospZ)
read(999,*) (ZQc(i), i=1,nospZ)
read(999,*) (ZQn(i), i=1,nospZ)
read(999,*) (ZQp(i), i=1,nospZ)
read(999,*) (ZKa(i), i=1,nospZ)
read(999,*) (Zrespg(i), i=1,nospZ)
read(999,*) (Zrespb(i), i=1,nospZ)
read(999,*) (Zumax(i), i=1,nospZ)
read(999,*) (Zm(i), i=1,nospZ)
read(999,*)
!--Organic Matter--------------------
read(999,*) 
read(999,*) KG1
read(999,*) KG2
read(999,*) KG1_R
read(999,*) KG2_R
read(999,*) KG1_BC
read(999,*) KG2_BC
read(999,*) KNH4
read(999,*) nitmax
read(999,*) KO2
read(999,*) KstarO2
read(999,*) KNO3
read(999,*) pCO2
read(999,*) stoich_x1R
read(999,*) stoich_y1R
read(999,*) stoich_x2R
read(999,*) stoich_y2R
read(999,*) stoich_x1BC
read(999,*) stoich_y1BC
read(999,*) stoich_x2BC
read(999,*) stoich_y2BC
read(999,*) ws(iOM1_A:iOM1_A)
read(999,*) ws(iOM2_A)
read(999,*) ws(iOM1_Z)
read(999,*) ws(iOM2_Z)
read(999,*) ws(iOM1_R)
read(999,*) ws(iOM2_R)
read(999,*) ws(iOM1_BC)
read(999,*) ws(iOM2_BC)
read(999,*) KGcdom
read(999,*) CF_SPM
read(999,*)

!----Other including Boundary Conditions-------------
read(999,*)
read(999,*) Which_Vmix,Which_Adv 
read(999,*) KH_coeff  
read(999,*) Which_Outer_BC 
read(999,*) !Comment Line
read(999,*) wt_pl,wt_po
read(999,*) wt_l,wt_o
read(999,*) m_OM_init,m_OM_BC,m_OM_sh 
read(999,*) Stoich_x1A_init,Stoich_y1A_init
read(999,*) Stoich_x2A_init,Stoich_y2A_init
read(999,*) Stoich_x1Z_init,Stoich_y1Z_init
read(999,*) Stoich_x2Z_init,Stoich_y2Z_init
read(999,*) KG_bot
read(999,*) MC
read(999,*) Which_Output

!---------------------------------------------------
close(999)

 call Check_InputFile()

stoich_z1R = 1.
stoich_z2R = 1.
stoich_z1BC = 1.
stoich_z2BC = 1.
stoich_z1A_init = 1.
stoich_z2A_init = 1.
stoich_z1Z_init = 1.
stoich_z2Z_init = 1.

Athresh  = Athresh*volcell   ! Threshold for grazing, um^3/m3
eps=0
do isp=1,nospA
   eps=0
   if(umax(isp).eq.0) eps=1.e-18
   alphad(isp) = alpha(isp)/(umax(isp)+eps) ! Initial slope of photosynthesis-irradiance curve / Vmax
   betad(isp)  = beta(isp)/(umax(isp)+eps)  ! Photoinhibition constant / Vmax
enddo

!Convert relative proportions of phytoplankton to percentage of total chlA
tot = SUM(A_wt)
if(tot.le.0) then
 write(6,*) "Error in A_wt, A_wt.le.0"
 stop
endif

do isp=1,nospA
   A_wt(isp) = A_wt(isp)/tot
enddo

!Diatom/non-Diatom array
do isp=1,nospA
   if(KSi(isp).le.tiny(x)) then
      is_diatom(isp) = 0
   else
      is_diatom(isp) = 1
   endif
enddo

    ! Compute starting time of run in seconds since Model_dim::iYr0:
      START_SECONDS = &
      TOTAL_SECONDS( iYr0, iYrS, iMonS, iDayS, iHrS, iMinS, iSecS )
#ifdef DEBUG
      write(6,*) "In ReadInput"
      write(6,*) "StartSeconds",START_SECONDS
#endif
      END_SECONDS = &
      TOTAL_SECONDS( iYr0, iYrE, iMonE, iDayE, iHrE, iMinE, iSecE )

      nstep = ( END_SECONDS - START_SECONDS ) / dT !number of timesteps in a run
      iout = dT_out/dT !output time-interval in timesteps

! --- sinking speed: converted from m/d downward positive to m/s negative
      ws = -ws/SDay
      if(Which_Adv.eq.2.or.Which_adv.eq.3) ws = 0.
      write(6,*) "Which_Adv=",Which_Adv
      write(6,*) "Which_VMix=",Which_VMix

  do j = 1,jm
    do i = 1,im
      do k=1,nza(i,j)
       s_x1A(i,j,k)= Stoich_x1A_init
       s_x2A(i,j,k)= Stoich_x2A_init
       s_y1A(i,j,k)= Stoich_y1A_init
       s_y2A(i,j,k)= Stoich_y2A_init
       s_z1A(i,j,k)= Stoich_z1A_init
       s_z2A(i,j,k)= Stoich_z2A_init
       s_x1Z(i,j,k)= Stoich_x1Z_init
       s_x2Z(i,j,k)= Stoich_x2Z_init
       s_y1Z(i,j,k)= Stoich_y1Z_init
       s_y2Z(i,j,k)= Stoich_y2Z_init
       s_z1Z(i,j,k)= Stoich_z1Z_init
       s_z2Z(i,j,k)= Stoich_z2Z_init
    enddo
   enddo
  enddo


return
END SUBROUTINE Read_InputFile_CGEM
