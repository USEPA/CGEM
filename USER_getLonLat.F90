      Subroutine USER_getLonLat(lat,lon)

      USE Model_dim

      real, intent (out) :: lat(im,jm)
      real, intent (out) :: lon(im,jm)
      integer i,j
      real fillvalue,one
      character(200) filename

      one = -1. 
      fillvalue = sqrt(one) 

      write(filename,'(A, A)') trim(DATADIR),'/latlon.dat'
      open (19,file=filename,status='old')
      read (19,*) !Header File
      do j=1,jm
       read (19,*) lat(:,j)
      enddo

      do j=1,jm
       read (19,*) lon(:,j)
      enddo
      close(19)

      do j=1,jm
       do i=1,im
        if(lat(i,j)<=-9999) lat(i,j)=fillvalue
        if(lon(i,j)<=-9999) lon(i,j)=fillvalue
       enddo
      enddo

      return
      end subroutine USER_getLonLat
