      Subroutine Output_Data_Calibration_open

      implicit none
 
      open(101,file="./R_PLOTS/A1k1growth.txt",status='unknown')
      open(105,file="./R_PLOTS/G1k1growth.txt",status='unknown')
      open(109,file="./R_PLOTS/DICk1growth.txt",status='unknown')
      open(113,file="./R_PLOTS/DICratek1.txt",status='unknown')
      open(117,file="./R_PLOTS/NH4k1growth.txt",status='unknown')
      open(121,file="./R_PLOTS/PO4k1growth.txt",status='unknown')
      open(125,file="./R_PLOTS/O2k1growth.txt",status='unknown')
      open(130,file="./R_PLOTS/Ntotk1growth.txt",status='unknown')
      open(131,file="./R_PLOTS/OM1_Ak1growth.txt",status='unknown')
      open(132,file="./R_PLOTS/OM2_Ak1growth.txt",status='unknown')
      open(133,file="./R_PLOTS/OM1_fpk1growth.txt",status='unknown')
      open(134,file="./R_PLOTS/OM2_fpk1growth.txt",status='unknown')
      open(135,file="./R_PLOTS/OM1_rpk1growth.txt",status='unknown')
      open(136,file="./R_PLOTS/OM2_rpk1growth.txt",status='unknown')
      open(137,file="./R_PLOTS/NO3k1growth.txt",status='unknown')
      open(142,file="./R_PLOTS/AllRatesk1.txt",status='unknown')
      open(143,file="./R_PLOTS/Qngrowk1.txt",status='unknown')

      return

      end subroutine Output_Data_Calibration_open
