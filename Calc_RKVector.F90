Subroutine Calc_RKVector

USE Model_dim
USE INPUT_VARS
USE RKVectorMod
USE EPA_GEM_Params
USE TEMP_VARS

IMPLICIT NONE

INTEGER         :: isp


! Multiply constants by rvector to differentiate between phytoplankton groups
    Athresh  = Athresh*VOLcell   ! Threshold for grazing, um^3/m3
    optNP = qnG/qpG              ! Optimal nutrient ratio for phytoplankton
    do isp=1,nospA
     alphad(isp) = alpha(isp)/mumax(isp) ! Initial slope of photosynthesis-irradiance curve / Vmax 
     betad(isp)  = beta(isp)/mumax(isp)  ! Photoinhibition constant / Vmax
    enddo

!Minima and Maxima
      fmin(iA1:iA6) = 1.0
      fmax(iA1:iA6) = 1.E32

      fmin(iQn1:iQn6) = QminN(:)
      fmax(iQn1:iQn6) = QmaxN(:)

      fmin(iQp1:iQp6) = QminP(:)
      fmax(iQp1:iQp6) = QmaxP(:)

      fmin(iG1:iG2) = 1.0
      fmax(iG1:iG2) = 1.E32

      fmin(iNO3) = 0.00
      fmax(iNO3) = 1.E32

      fmin(iNH4) = 0.00
      fmax(iNH4) = 1.E32

      fmin(iPO4) = 0.00
      fmax(iPO4) = 1.E32

      fmin(iDIC) = 0.00
      fmax(iDIC) = 1.E32

      fmin(iO2) = 0.00
      fmax(iO2) = 1.E32

      fmin(iOM1_A) = 0.00
      fmax(iOM1_A) = 1.E32

      fmin(iOM2_A) = 0.00
      fmax(iOM2_A) = 1.E32

      fmin(iOM1_fp) = 0.00
      fmax(iOM1_fp) = 1.E32

      fmin(iOM2_fp) = 0.00
      fmax(iOM2_fp) = 1.E32

      fmin(iOM1_rp) = 0.00
      fmax(iOM1_rp) = 1.E32

      fmin(iOM2_rp) = 0.00
      fmax(iOM2_rp) = 1.E32

      fmin(iCDOM) = 0.00
      fmax(iCDOM) = 1.E32

      fmin(iSi) = 0.00
      fmax(iSi) = 1.E32

      fmin(iOM1_bc) = 0.00
      fmax(iOM1_bc) = 1.E32

      fmin(iOM2_bc) = 0.00
      fmax(iOM2_bc) = 1.E32



End Subroutine Calc_RKVector
