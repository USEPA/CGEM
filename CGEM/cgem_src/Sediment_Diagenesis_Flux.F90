      SUBROUTINE Sediment_Diagenesis_Flux(A,f_nf,T,S,pH,sedflux,i,j,Y,ppH_init)

      USE Model_dim
      USE CGEM_vars 

      IMPLICIT NONE

      real*8, dimension(100) :: A
      real, intent(in) :: f_nf(nf),T,S,pH ! State variables, Temp, Salinity
      real, intent(out)   :: sedflux(nf) ! Sediment fluxes
      integer, intent(in) :: i,j  
      real :: maxres ! Maximum residual
      integer :: ndays !Maximum amount of iterations
      integer, save :: init=1
      double precision ppH_init(2000)
      double precision Y(27000)
      integer, save :: initr=0

#ifdef DEBUG
      write(6,*) "before cA pph_init",ppH_init(1:3)
#endif

! Change A values according to state variables, T, and S
      call changeA(A,f_nf,T,S,pH,i,j)

#ifdef DEBUG
     write(6,*) "after cA pph_init",ppH_init(1:3)
#endif

! Run the simulation

! Total years is ndays*2
       ndays= 400 !Iterations, Arrays are hardwired for 400(max)
!One iteration for testing:
!      ndays=1
!      ndays=20
      maxres=1e-5
      if(init.eq.1) maxres=1e-5
      init=0
#ifdef DEBUG
     write(6,*) "A ndays ppH_init",ppH_init(1:3)
#endif
     call model(A,ndays,sedflux,maxres,Y,ppH_init)

#ifdef DEBUG
     write(6,*) "A model ppH_init",ppH_init(1:3)
#endif

      RETURN

      END SUBROUTINE Sediment_Diagenesis_Flux
