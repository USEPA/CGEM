      subroutine USER_Read_Wind(TC_8,Wsp)

      USE Model_dim

      integer*8, intent (in) :: TC_8
      real, intent (out) :: Wsp(IM,JM)

      ! read Windspeed(i,j)
      Wsp = 5. 
 
      return 
      end subroutine USER_Read_Wind
