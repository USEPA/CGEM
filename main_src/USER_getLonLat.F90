      Subroutine USER_getLonLat(lat,lon)

      USE Model_dim
      USE Fill_Value

      real, intent (out) :: lat(im,jm)
      real, intent (out) :: lon(im,jm)
      integer i,j
      character(200) filename


      write(filename,'(A, A)') trim(DATADIR),'/latlon.dat'
      open (19,file=filename,status='old')

      read (19,*) !Header, lat 
      do j=1,jm
       read (19,*) lat(:,j)
      enddo

      read (19,*) !Header, lon
      do j=1,jm
       read (19,*) lon(:,j)
      enddo
      close(19)

      do j=1,jm
       do i=1,im
        if(lat(i,j)<=-9999) lat(i,j)=fill(0)
        if(lon(i,j)<=-9999) lon(i,j)=fill(0)
       enddo
      enddo

      return
      end subroutine USER_getLonLat
