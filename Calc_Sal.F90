      subroutine Calc_Sal(START_SECONDS,TC_8,S)

      USE Model_dim

      IMPLICIT NONE

      integer*8, intent (in) :: START_SECONDS  !Integer seconds since 2002
      integer*8, intent (in) :: TC_8  !Integer seconds, simulation time
      real, intent (out) :: S(im,jm,nsl) !Salinity in psu
      integer, save :: init = 1


      if(init.eq.1) then  !Only read in data at the first timestep
       open (19,file='./data/S.dat',status='old')
       read (19,*)  !Assumes header file
       read (19,*) S
       close(19)
       init=0
      endif


      return 
      end subroutine Calc_Sal 
