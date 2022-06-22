Subroutine Read_InputFile_CGEM(filename,myid,numprocs)

USE Model_dim
USE INPUT_VARS
USE INPUT_VARS_CGEM
USE LIGHT_VARS
USE CGEM_vars 
USE TEMP_VARS
USE DATE_TIME
USE STOICH_VARS
use mpi_interface

IMPLICIT NONE

integer, intent(in) :: myid, numprocs
character(120),intent(in) :: filename

integer i,j,k,icent_jcent_units,mpierr
integer isp,isz,myi
real tot,x,icent_in,jcent_in
real, parameter :: SDay = 86400.0  ! # of sec in 24 hr day
real eps

ws(1:nf) = 0.

if(myid.eq.0) then
!--Code Identifier--------------
open(unit=999,file=filename,form='formatted',status='old')
read(999,*) code_ID
read(999,*)
!--Simulation specifics------
read(999,*)
read(999,*) iYrS,iMonS,iDayS,iHrS,iMinS,iSecS
read(999,*) iYrE,iMonE,iDayE,iHrE,iMinE,iSecE
read(999,*) dT, dT_out, dT_sed
read(999,*) icent_jcent_units
read(999,*) icent_in, jcent_in
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
endif

if(numprocs.gt.1) then
call MPI_BCAST(code_ID,50,MPI_CHARACTER,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(iDayE,1,MPI_INTEGER,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(iDayS,1,MPI_INTEGER,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(iHrE,1,MPI_INTEGER,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(iHrS,1,MPI_INTEGER,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(iMinE,1,MPI_INTEGER,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(iMinS,1,MPI_INTEGER,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(iMonE,1,MPI_INTEGER,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(iMonS,1,MPI_INTEGER,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(iSecE,1,MPI_INTEGER,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(iSecS,1,MPI_INTEGER,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(iYrE,1,MPI_INTEGER,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(iYrS,1,MPI_INTEGER,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(dT,1,MPI_INTEGER,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(dT_out,1,MPI_INTEGER,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(dT_sed,1,MPI_INTEGER,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(icent,1,MPI_INTEGER,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(jcent,1,MPI_INTEGER,0,MPI_COMM_WORLD,mpierr)

call MPI_BCAST(Which_fluxes,8,MPI_INTEGER,0,MPI_COMM_WORLD,mpierr)

call MPI_BCAST(Which_temperature,1,MPI_INTEGER,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(Which_uptake,1,MPI_INTEGER,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(Which_quota,1,MPI_INTEGER,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(Which_irradiance,1,MPI_INTEGER,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(Which_chlaC,1,MPI_INTEGER,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(Which_photosynthesis,1,MPI_INTEGER,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(Which_growth,1,MPI_INTEGER,0,MPI_COMM_WORLD,mpierr)

call MPI_BCAST(Read_Solar,1,MPI_INTEGER,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(Read_Wind,1,MPI_INTEGER,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(Read_T,1,MPI_INTEGER,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(Read_Sal,1,MPI_INTEGER,0,MPI_COMM_WORLD,mpierr)

call MPI_BCAST(InitializeHow,1,MPI_INTEGER,0,MPI_COMM_WORLD,mpierr)

call MPI_BCAST(Kw,1,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(Kcdom,1,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(Kspm,1,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(Kchla,1,MPI_REAL,0,MPI_COMM_WORLD,mpierr)

call MPI_BCAST(astar490,1,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(aw490,1,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(astarOMA,1,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(astarOMZ,1,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(astarOMR,1,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(astarOMBC,1,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(ws,nf,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(PARfac,1,MPI_REAL,0,MPI_COMM_WORLD,mpierr)

call MPI_BCAST(Tref,nospA+nospZ,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(KTg1,nospA+nospZ,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(KTg2,nospA+nospZ,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(Ea,nospA+nospZ,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(ediblevector,nospA*nospZ,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(umax,nospA,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(CChla,nospA,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(alpha,nospA,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(beta,nospA,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(respg,nospA,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(respb,nospA,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(QminN,nospA,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(QminP,nospA,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(QmaxN,nospA,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(QmaxP,nospA,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(Kn,nospA,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(Kp,nospA,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(Ksi,nospA,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(KQn,nospA,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(KQp,nospA,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(nfQs,nospA,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(vmaxN,nospA,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(vmaxP,nospA,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(vmaxSi,nospA,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(aN,nospA,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(volcell,nospA,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(Qc,nospA,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(Athresh,nospA,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(mA,nospA,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(A_wt,nospA,MPI_REAL,0,MPI_COMM_WORLD,mpierr)

call MPI_BCAST(Zeffic,nospZ,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(Zslop,nospZ,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(Zvolcell,nospZ,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(ZQc,nospZ,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(ZQn,nospZ,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(ZQp,nospZ,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(ZKa,nospZ,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(Zrespg,nospZ,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(Zrespb,nospZ,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(Zumax,nospZ,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(Zm,nospZ,MPI_REAL,0,MPI_COMM_WORLD,mpierr)

call MPI_BCAST(KG1,1,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(KG2,1,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(KG1_R,1,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(KG2_R,1,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(KG1_BC,1,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(KG2_BC,1,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(KNH4,1,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(nitmax,1,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(KO2,1,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(KstarO2,1,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(KNO3,1,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(pCO2,1,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(stoich_x1R,1,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(stoich_y1R,1,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(stoich_x2R,1,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(stoich_y2R,1,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(stoich_x1BC,1,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(stoich_y1BC,1,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(stoich_x2BC,1,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(stoich_y2BC,1,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(KGcdom,1,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(CF_SPM,1,MPI_REAL,0,MPI_COMM_WORLD,mpierr)

call MPI_BCAST(Which_Vmix,1,MPI_INTEGER,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(Which_Adv,1,MPI_INTEGER,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(KH_coeff,1,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(Which_Outer_BC,1,MPI_INTEGER,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(wt_l,1,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(wt_o,1,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(wt_pl,1,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(wt_po,1,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(m_OM_init,1,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(m_OM_BC,1,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(m_OM_sh,1,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(Stoich_x1A_init,1,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(Stoich_y1A_init,1,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(Stoich_x2A_init,1,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(Stoich_y2A_init,1,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(Stoich_x1Z_init,1,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(Stoich_y1Z_init,1,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(Stoich_x2Z_init,1,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(Stoich_y2Z_init,1,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(KG_bot,1,MPI_REAL,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(MC,1,MPI_INTEGER,0,MPI_COMM_WORLD,mpierr)
call MPI_BCAST(Which_Output,1,MPI_INTEGER,0,MPI_COMM_WORLD,mpierr)

endif


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
      nstep_sed = dT_sed / dT   ! number of steps in-between calls to sediment diagenesis

! --- sinking speed: converted from m/d downward positive to m/s negative
      ws = -ws/SDay
      if(Which_Adv.eq.2.or.Which_adv.eq.3) ws = 0.

  do j = 1,jm
    myi = 1
    do i = myi_start,myi_end
      do k=1,nza(i,j)
       s_x1A(myi,j,k)= Stoich_x1A_init
       s_x2A(myi,j,k)= Stoich_x2A_init
       s_y1A(myi,j,k)= Stoich_y1A_init
       s_y2A(myi,j,k)= Stoich_y2A_init
       s_z1A(myi,j,k)= Stoich_z1A_init
       s_z2A(myi,j,k)= Stoich_z2A_init
       s_x1Z(myi,j,k)= Stoich_x1Z_init
       s_x2Z(myi,j,k)= Stoich_x2Z_init
       s_y1Z(myi,j,k)= Stoich_y1Z_init
       s_y2Z(myi,j,k)= Stoich_y2Z_init
       s_z1Z(myi,j,k)= Stoich_z1Z_init
       s_z2Z(myi,j,k)= Stoich_z2Z_init
    enddo
    myi = myi + 1
   enddo
  enddo


return
END SUBROUTINE Read_InputFile_CGEM
