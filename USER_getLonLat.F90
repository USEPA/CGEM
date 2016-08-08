      Subroutine USER_getLonLat(lat,lon)

      USE Model_dim

      real, intent (out) :: lat(im)
      real, intent (out) :: lon(jm)

      open (19,file='./data/latlon.dat',status='old')
      read (19,*) !Header File
      read (19,*) lat
      read (19,*) lon
      close(19)

      return
      end subroutine USER_getLonLat
