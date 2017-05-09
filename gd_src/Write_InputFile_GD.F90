Subroutine Write_InputFile_GD()

USE INPUT_VARS
USE INPUT_VARS_GD
USE STATES
USE EUT
USE FLAGS

IMPLICIT NONE


integer icent_jcent_units
real i_in,j_in

!--Code Identifier--------------
open(unit=999,file="GD_debug.txt",form='formatted',status='unknown')
write(999,*) "code_ID",code_ID
write(999,*)
!--Simulation specifics------
write(999,*)
write(999,*) "iYrS,iMonS,iDayS,iHrS,iMinS,iSecS",iYrS,iMonS,iDayS,iHrS,iMinS,iSecS
write(999,*) "iYrE,iMonE,iDayE,iHrE,iMinE,iSecE",iYrE,iMonE,iDayE,iHrE,iMinE,iSecE
write(999,*) "dT, dT_out",dT, dT_out
write(999,*) "icent_jcent_units",icent_jcent_units
write(999,*) "i_in, j_in",i_in, j_in
write(999,*)
!--Switches in GEM---------
write(999,*)
write(999,*) "Which_Fluxes",Which_Fluxes
write(999,*) "solarRadKo",Read_Solar
write(999,*) "InitializeHow",InitializeHow
write(999,*) 
!----Other including Boundary Conditions-------------
write(999,*)
write(999,*) "Which_Vmix",Which_Vmix   
write(999,*) "KH_coeff",KH_coeff  
write(999,*) "Which_Outer_BC",Which_Outer_BC 
write(999,*) "m_OM_init,m_OM_bc,m_OM_sh",m_OM_init,m_OM_bc,m_OM_sh 
write(999,*) "DoDroop",DoDroop
!---------------------------------------------------

!--GOMDOM params------------------------------------------------
write(999,*)             !C
write(999,*)             !C  FIREAD file
write(999,*)             !C
write(999,*) 
write(999,*) ALPHA_DIA     ! ALPHA_DIA: Initial slope of diatom light sat curve 
write(999,*) ALPHA_GRE     ! ALPHA_GRE: Initial slope of greens light sat curve
write(999,*) 
write(999,*) ANCP          ! ANCP: N:C ratio
write(999,*) APCP          ! APCP: P:C ratio
write(999,*) ASCD          ! ASCD: Si:C ratio
write(999,*) AVFRAC        ! AVFRAC: Available DOP fraction
write(999,*) AVFRACDON     ! AVFRACDON: Available DON fraction
write(999,*) 
write(999,*) BMRD          ! BMRD: diatom mortality
write(999,*) BMRG          ! BMRG: greens mortality
write(999,*) 
write(999,*) CCHLD         ! CCHLD: C:chl ratio for diatoms
write(999,*) CCHLG         ! CCHLG: C:chl ratio for greens
write(999,*) 
write(999,*) CGZ           ! CGZ: zoo grazing rate
write(999,*)
write(999,*) DENIT_CN_RATIO ! DENIT_CN_RATIO 
write(999,*)
write(999,*) FCDD          ! FCDD: Frac mort = DOC (d)
write(999,*) FCDG          ! FCDG: Frac mort = DOC (g)
write(999,*) FCDP          ! FCDP: Frac pred = DOC
write(999,*) FCDZ          ! FCDZ: Frac z-mort = DOC
write(999,*) FCLD          ! FCLD: Frac mort = LOC (d)
write(999,*) FCLG          ! FCLG: Frac mort = LOC (g)
write(999,*) FCLP          ! FCLP: Frac LPDC (pred)
write(999,*) FCLZ          ! FCLZ: Frac LPDC z-mort
write(999,*) FCRD          ! FCRD: Frac mort = ROC (d)
write(999,*) FCRG          ! FCRG: Frac mort = ROC (g)
write(999,*) FCRP          ! FCRP: Frac RPDC (pred)
write(999,*) FCRZ          ! FCRZ: Frac RPDC z-mort
write(999,*) FNDD          ! FNDD: Frac mort = DON(d)
write(999,*) FNDG          ! FNDG: Frac mort = DON (g)
write(999,*) FNDP          ! FNDP: Frac pred= DON
write(999,*) FNDZ          ! FNDZ: Frac z-mort = DON
write(999,*) FNID          ! FNID: Frac mort=DIN (d)
write(999,*) FNIG          ! FNIG: Frac mort = DIN(g)
write(999,*) FNIP          ! FNIP: Frac pred = DIN
write(999,*) FNIZ          ! FNIZ: Frac DIN z-mort
write(999,*) FNLD          ! FNLD: Frac mort=LON(d)
write(999,*) FNLG          ! FNLG: Frac mort= LON(g)
write(999,*) FNLP          ! FNLP: Frac pred=LON
write(999,*) FNLZ          ! FNLZ: Frac z-mort=LON
write(999,*) FNRD          ! FNRD: Frac mort=RON(d)
write(999,*) FNRG          ! FNRG: Frac mort=RON(g)
write(999,*) FNRP          ! FNRP: Frac pred=RON
write(999,*) FNRZ          ! FNRZ: Frac z-mort= RON
write(999,*) FPDD          ! FPDD: Frac mort=DOP(d)
write(999,*) FPDG          ! FPDG: Frac mort = DOP(g)
write(999,*) FPDP          ! FPDP: Frac pred=DOP
write(999,*) FPDZ          ! FPDZ: Frac z-mort DOP
write(999,*) FPID          ! FPID: Frac mort=DIP(d)
write(999,*) FPIG          ! FPIG: Frac mort=DIP(g)
write(999,*) FPIP          ! FPIP: Frac pred=DIP
write(999,*) FPIZ          ! FPIZ: Frac z-mort=DIP
write(999,*) FPLD          ! FPLD: Frac mort=LOP(d)
write(999,*) FPLG          ! FPLG: Frac mort= LOP(g)
write(999,*) FPLP          ! FPLP: Fract pred=LOP
write(999,*) FPLZ          ! FPLZ: Frac z-mort LOP
write(999,*) FPRD          ! FPRD: Frac mort=ROP(d)
write(999,*) FPRG          ! FPRG: Frac mort=ROP(g)
write(999,*) FPRP          ! FPRP: Frac pred=ROP
write(999,*) FPRZ          ! FPRZ: Frac z-mort ROP
write(999,*) FSAP          ! FSAP: Frac pred = DSi
write(999,*) 
write(999,*) GREFF         ! GREFF: zoo grazing coeff
write(999,*) 
write(999,*) ILMUL         ! ILMUL: scaling factor for surface short wave radiation
write(999,*) 
write(999,*) KDC           ! KDC: min DOC mineraliz
write(999,*) KDCALG        ! KDCALG: DOC alg dependance
write(999,*) KDN           ! KDN: min DON mineraliz
write(999,*) KDNALG        ! KDNALG: DON alg depedence
write(999,*) KDP           ! KDP: min DOP mineral
write(999,*) KDPALG        ! KDPALG: DOP alg dependence
write(999,*) 
write(999,*) KDWD          ! KDWD: Specifies light attenuation eqn, default=0
write(999,*) 
write(999,*) KHDONT_SED    ! KHDONT_SED
write(999,*) KHN           ! KHN: Organic N Decay Half Saturation Constant
write(999,*) KHND          ! KHND: mean N half sat (dia)
write(999,*) KHNG          ! KHNG: mean N half sat (gre)
write(999,*) KHNNT         ! KHNNT: half sat of NH3 for nitr
write(999,*) KHODOC_SED    ! KHODOC_SED
write(999,*) KHP           ! KHP: Organic P Decay Half Saturation Constant
write(999,*) KHPD          ! KHPD: mean P half sat (dia)
write(999,*) KHPG          ! KHPG: mean P half sat (gre)
write(999,*) KHSD          ! KHSD: mean Si half sat (dia)
write(999,*) KLC           ! KLC: min hyd for LOC
write(999,*) KLCALG        ! KLCALG: LOC alg dependence
write(999,*) KLN           ! KLN: min hyd for LON
write(999,*) KLNALG        ! KLNALG: LON alg dependence
write(999,*) KLP           ! KLP: min hyd for LOP
write(999,*) KLPALG        ! KLPALG: LOP alg dependence
write(999,*) KRC           ! KRC: min hyd for ROC
write(999,*) KRCALG        ! KRCALG: POC alg dependence
write(999,*) KRN           ! KRN: min hyd for RON
write(999,*) KRNALG        ! KRNALG: RON alg dependence
write(999,*) KRP           ! KRP: min hyd for ROP
write(999,*) KRPALG        ! KRPALG: ROP alg dependence
write(999,*) KSUA          ! KSUA: BSi diss rate
write(999,*) KSZ           ! KSZ: zoo half sat phyto
write(999,*) KTBD          ! KTBD: dia mort temp coeff 
write(999,*) KTBG          ! KTBG: gre mort temp coeff 
write(999,*) KTGD1         ! KTGD1: temp coeff < (dia)
write(999,*) KTGD2         ! KTGD2: temp coeff > (dia)
write(999,*) KTGG1         ! KTGG1: temp coeff < (gre)
write(999,*) KTGG2         ! KTGG2: temp coeff > (gre)
write(999,*) KTHDR         ! KTHDR: hyd temp dep
write(999,*) KTMNL         ! KTMNL: min temp dep
write(999,*) KTNT1         ! KTNT1: temp coeff < nitr
write(999,*) KTNT2         ! KTNT2: temp coeff > nitr
write(999,*) KTSUA         ! KTSUA: silic diss temp coeff
write(999,*) NTM           ! NTM: nitrif rate 
write(999,*) PBMAX_DIA     ! PBMAX_DIA: photo rate at opt illum
write(999,*) PBMAX_GRE     ! PBMAX_GRE: photo rate at opt illum
write(999,*) PMD           ! PMD: dia prod
write(999,*) PMG           ! PMG: gre prod
write(999,*) 
write(999,*) RSODNTR       ! RSODNTR
write(999,*)
write(999,*) SILIM         ! SILIM: Minimum nutrient limits growth
write(999,*) 
write(999,*) TMD           ! TMD: opt temp dia
write(999,*) TMG           ! TMG: opt temp gre
write(999,*) TMNT          ! TMNT: opt temp nitr
write(999,*) TRD           ! TRD: opt metab temp (dia)
write(999,*) TRG           ! TRG: opt metab temp (gre)
write(999,*) TRHDR         ! TRHDR: opt hydr temp
write(999,*) TRMNL         ! TRMNL: opt mineral temp
write(999,*) TRSUA         ! TRSUA: opt Si diss temp
write(999,*) TZREF         ! TZREF: opt pred temp
write(999,*) TREF          ! TREF: SOD reference temp
write(999,*) ZDTH          ! ZDTH: zoo mort
write(999,*) ZTHET         ! ZTHET: temp coeff pred
write(999,*) 
write(999,*) !  Dissolved Oxygen related parameters
write(999,*) 
write(999,*) KCOD            ! KCOD: DOC oxidation rate
write(999,*) KDENITR         ! KDENITR: Max denitrification rate coefficient
write(999,*) KHDENITR        ! KHDENITR: Half-sat [NO3] for dentrification
write(999,*) KHDONT          ! KHDONT: Half-sat [D.O] required for nitrification
write(999,*) KHOCOD          ! KHOCOD: Half-sat [D.O] required for exertion of COD
write(999,*) KHODOC          ! KHODOC: Half-sat [D.O] required for oxic respiration
write(999,*) RCDO            ! RCDO: D.O. to carbon ratio in resp 
write(999,*) RNTO            ! RNTO: ratio of O2 consumed to nitrif
write(999,*) 
write(999,*) !Droop Kinetics
write(999,*)
write(999,*) FINTNID
write(999,*) FINTNDD
write(999,*) FINTNLD
write(999,*) FINTNRD
write(999,*)
write(999,*) FINTNIG
write(999,*) FINTNDG
write(999,*) FINTNLG
write(999,*) FINTNRG
write(999,*)
write(999,*) FINTLUXNIP
write(999,*) FINTSTRNIP
write(999,*) FINTLUXNDP
write(999,*) FINTSTRNDP
write(999,*) FINTLUXNLP
write(999,*) FINTSTRNLP
write(999,*) FINTLUXNRP
write(999,*) FINTSTRNRP
write(999,*)
write(999,*) FINTPID
write(999,*) FINTPDD
write(999,*) FINTPLD
write(999,*) FINTPRD
write(999,*)
write(999,*) FINTPIG
write(999,*) FINTPDG
write(999,*) FINTPLG
write(999,*) FINTPRG
write(999,*)
write(999,*) FINTLUXPIP
write(999,*) FINTSTRPIP
write(999,*) FINTLUXPDP
write(999,*) FINTSTRPDP
write(999,*) FINTLUXPLP
write(999,*) FINTSTRPLP
write(999,*) FINTLUXPRP
write(999,*) FINTSTRPRP
write(999,*)
write(999,*) KHINTND
write(999,*) KHINTNG
write(999,*) KHINTPD
write(999,*) KHINTPG
write(999,*)
write(999,*) QMINND
write(999,*) QMINNG
write(999,*) QMINPD
write(999,*) QMINPG
write(999,*) QMAXND
write(999,*) QMAXNG
write(999,*) QMAXPD
write(999,*) QMAXPG
write(999,*) UPNMAXD
write(999,*) UPNMAXG
write(999,*) UPPMAXD
write(999,*) UPPMAXG
write(999,*)
write(999,*) ! Settling rates
write(999,*) 
write(999,*) ws(JDIA)  ! VDIA
write(999,*) ws(JDIAN) ! VDIAN
write(999,*) ws(JDIAP) ! VDIAP
write(999,*) ws(JGRE)  ! VGRE	
write(999,*) ws(JGREN) ! VGREN
write(999,*) ws(JGREP) ! VGREP
write(999,*) ws(JLOC)  ! VLOC
write(999,*) ws(JROC)  ! VROC
write(999,*) ws(JLON)  ! VLON
write(999,*) ws(JRON)  ! VRON
write(999,*) ws(JLOP)  ! VLOP
write(999,*) ws(JROP)  ! VROP
write(999,*) ws(JSU)   ! VSU

END Subroutine Write_InputFile_GD
