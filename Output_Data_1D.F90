      Subroutine Output_Data_1D_text(f,istep,icent,jcent)

      USE Model_dim
      USE EPA_GEM_Params
      USE INPUT_VARS, ONLY:CODE_ID

      implicit none
 

      integer istep,icent,jcent
      real f(im,jm,nsl,nf) 
      integer, save :: iopen1 = 0

9000 format(i10,G16.8)

        if(iopen1.eq.0) then
      open(260,file="./R_PLOTS/A1.txt",status='unknown')
      open(261,file="./R_PLOTS/A2.txt",status='unknown')
      open(262,file="./R_PLOTS/A3.txt",status='unknown')
      open(263,file="./R_PLOTS/A4.txt",status='unknown')
      open(264,file="./R_PLOTS/A5.txt",status='unknown')
      open(265,file="./R_PLOTS/A6.txt",status='unknown')
      open(266,file="./R_PLOTS/Qn1.txt",status='unknown')
      open(267,file="./R_PLOTS/Qn2.txt",status='unknown')
      open(268,file="./R_PLOTS/Qn3.txt",status='unknown')
      open(269,file="./R_PLOTS/Qn4.txt",status='unknown')
      open(270,file="./R_PLOTS/Qn5.txt",status='unknown')
      open(271,file="./R_PLOTS/Qn6.txt",status='unknown')
      open(272,file="./R_PLOTS/Qp1.txt",status='unknown')
      open(273,file="./R_PLOTS/Qp2.txt",status='unknown')
      open(274,file="./R_PLOTS/Qp3.txt",status='unknown')
      open(275,file="./R_PLOTS/Qp4.txt",status='unknown')
      open(276,file="./R_PLOTS/Qp5.txt",status='unknown')
      open(277,file="./R_PLOTS/Qp6.txt",status='unknown')
      open(278,file="./R_PLOTS/G1.txt",status='unknown')
      open(279,file="./R_PLOTS/G2.txt",status='unknown')
      open(280,file="./R_PLOTS/NO3.txt",status='unknown')
      open(281,file="./R_PLOTS/NH4.txt",status='unknown')
      open(282,file="./R_PLOTS/PO4.txt",status='unknown')
      open(283,file="./R_PLOTS/DIC.txt",status='unknown')
      open(284,file="./R_PLOTS/O2.txt",status='unknown')
      open(294,file="./R_PLOTS/OM1_A.txt",status='unknown')
      open(295,file="./R_PLOTS/OM2_A.txt",status='unknown')
      open(296,file="./R_PLOTS/OM1_fp.txt",status='unknown')
      open(297,file="./R_PLOTS/OM2_fp.txt",status='unknown')
      open(298,file="./R_PLOTS/OM1_rp.txt",status='unknown')
      open(299,file="./R_PLOTS/OM2_rp.txt",status='unknown')
      open(2100,file="./R_PLOTS/CDOM.txt",status='unknown')
      open(2101,file="./R_PLOTS/Si.txt",status='unknown')
      open(2102,file="./R_PLOTS/OM1_bc.txt",status='unknown')
      open(2103,file="./R_PLOTS/OM2_bc.txt",status='unknown')
      ! PAR, irradiance and Chl-a are not stored in f();
      ! so they are printed from GEM_EPA() routine.
      open(300,file="./R_PLOTS/Rad.txt",status='unknown')
      open(301,file="./R_PLOTS/Chla.txt",status='unknown')
      open(302,file="./R_PLOTS/DailyRad.txt",status='unknown')
      open(303,file="./R_PLOTS/IOPpar.txt",status='unknown')
      open(304,file="./R_PLOTS/label.txt",status='unknown')
      write(304,*) '"',trim(CODE_ID),'"'

      open(305,file="./R_PLOTS/uN1.txt",status='unknown')
      open(405,file="./R_PLOTS/uN2.txt",status='unknown')
      open(505,file="./R_PLOTS/uN3.txt",status='unknown')
      open(605,file="./R_PLOTS/uN4.txt",status='unknown')
      open(705,file="./R_PLOTS/uN5.txt",status='unknown')
      open(805,file="./R_PLOTS/uN6.txt",status='unknown')

      open(306,file="./R_PLOTS/uP1.txt",status='unknown')
      open(406,file="./R_PLOTS/uP2.txt",status='unknown')
      open(506,file="./R_PLOTS/uP3.txt",status='unknown')
      open(606,file="./R_PLOTS/uP4.txt",status='unknown')
      open(706,file="./R_PLOTS/uP5.txt",status='unknown')
      open(806,file="./R_PLOTS/uP6.txt",status='unknown')

      open(307,file="./R_PLOTS/uE1.txt",status='unknown')
      open(407,file="./R_PLOTS/uE2.txt",status='unknown')
      open(507,file="./R_PLOTS/uE3.txt",status='unknown')
      open(607,file="./R_PLOTS/uE4.txt",status='unknown')
      open(707,file="./R_PLOTS/uE5.txt",status='unknown')
      open(807,file="./R_PLOTS/uE6.txt",status='unknown')

      open(308,file="./R_PLOTS/uA1.txt",status='unknown')
      open(408,file="./R_PLOTS/uA2.txt",status='unknown')
      open(508,file="./R_PLOTS/uA3.txt",status='unknown')
      open(608,file="./R_PLOTS/uA4.txt",status='unknown')
      open(708,file="./R_PLOTS/uA5.txt",status='unknown')
      open(808,file="./R_PLOTS/uA6.txt",status='unknown')

      open(309,file="./R_PLOTS/x1A.txt",status='unknown')
      open(310,file="./R_PLOTS/x2A.txt",status='unknown')
      open(311,file="./R_PLOTS/y1A.txt",status='unknown')
      open(312,file="./R_PLOTS/y2A.txt",status='unknown')
      open(313,file="./R_PLOTS/x1fp.txt",status='unknown')
      open(314,file="./R_PLOTS/x2fp.txt",status='unknown')
      open(315,file="./R_PLOTS/y1fp.txt",status='unknown')
      open(316,file="./R_PLOTS/y2fp.txt",status='unknown')
          iopen1 = 1
          endif

      write(260,9000) istep,f(icent,jcent,1,iA1)
      write(261,9000) istep,f(icent,jcent,1,iA2)
      write(262,9000) istep,f(icent,jcent,1,iA3)
      write(263,9000) istep,f(icent,jcent,1,iA4)
      write(264,9000) istep,f(icent,jcent,1,iA5)
      write(265,9000) istep,f(icent,jcent,1,iA6)
      write(266,9000) istep,f(icent,jcent,1,iQn1)
      write(267,9000) istep,f(icent,jcent,1,iQn2)
      write(268,9000) istep,f(icent,jcent,1,iQn3)
      write(269,9000) istep,f(icent,jcent,1,iQn4)
      write(270,9000) istep,f(icent,jcent,1,iQn5)
      write(271,9000) istep,f(icent,jcent,1,iQn6)
      write(272,9000) istep,f(icent,jcent,1,iQp1)
      write(273,9000) istep,f(icent,jcent,1,iQp2)
      write(274,9000) istep,f(icent,jcent,1,iQp3)
      write(275,9000) istep,f(icent,jcent,1,iQp4)
      write(276,9000) istep,f(icent,jcent,1,iQp5)
      write(277,9000) istep,f(icent,jcent,1,iQp6)
      write(278,9000) istep,f(icent,jcent,1,iG1)
      write(279,9000) istep,f(icent,jcent,1,iG2)
      write(280,9000) istep,f(icent,jcent,1,iNO3)
      write(281,9000) istep,f(icent,jcent,1,iNH4)
      write(282,9000) istep,f(icent,jcent,1,iPO4)
      write(283,9000) istep,f(icent,jcent,1,iDIC)
      write(284,9000) istep,f(icent,jcent,1,iO2)
      write(294,9000) istep,f(icent,jcent,1,iOM1_A)
      write(295,9000) istep,f(icent,jcent,1,iOM2_A)
      write(296,9000) istep,f(icent,jcent,1,iOM1_fp)
      write(297,9000) istep,f(icent,jcent,1,iOM2_fp)
      write(298,9000) istep,f(icent,jcent,1,iOM1_rp)
      write(299,9000) istep,f(icent,jcent,1,iOM2_rp)
      write(2100,9000) istep,f(icent,jcent,1,iCDOM)
      write(2101,9000) istep,f(icent,jcent,1,iSi)
      write(2102,9000) istep,f(icent,jcent,1,iOM1_bc)
      write(2103,9000) istep,f(icent,jcent,1,iOM2_bc)


      end subroutine Output_Data_1D_text 
