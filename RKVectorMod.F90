MODULE RKVectorMod

USE Model_dim

  !---Minima and Maxima of all nf variables
   real :: fmin(nf),fmax(nf)
  !Zooplankton
    real, dimension(nospG)    :: optNP   ! optimal nutrient ratio for zooplankton
  !Light curve parameters
    real, dimension(nospA)    :: alphad ! Initial slope of photosynthesis-irradiance curve / Vmax
    real, dimension(nospA)    :: betad  ! Photoinhibition constant / Vmax

End MODULE RKVectorMod
