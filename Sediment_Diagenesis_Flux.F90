      SUBROUTINE Sediment_Diagenesis_Flux(A,f_nf,T,S,pH,sedflux,i,j,YY_init,ppH_init)

      USE Model_dim
      USE CGEM_vars 
      USE SDM, ONLY: Y

      IMPLICIT NONE

      real*8, dimension(100) :: A
      real, intent(in) :: f_nf(nf),T,S,pH ! State variables, Temp, Salinity
      real, intent(out)   :: sedflux(nf) ! Sediment fluxes
      integer, intent(in) :: i,j  
      real :: maxres ! Maximum residual
      integer :: ndays !Maximum amount of iterations
      integer, save :: init=1
      double precision ppH_init(2000)
      double precision YY_init(27000)

      if(init.eq.1) then
       Y=0.
       init=0
      endif

! Change A values according to state variables, T, and S
      call changeA(A,f_nf,T,S,pH,i,j)

! Run the simulation

! Total years is ndays*2
       ndays=400 !Iterations, Arrays are hardwired for 400(max)
!One iteration for testing:
!      ndays=1
!      ndays=20
      maxres=1e-2
     call model(A,ndays,sedflux,maxres,Y(i,j,:),YY_init,ppH_init)


      RETURN

      END SUBROUTINE Sediment_Diagenesis_Flux
