      Subroutine USER_getLonLat(lat,lon)

      USE Model_dim
      USE Fill_Value

      IMPLICIT NONE

      real, intent (out) :: lat(im,jm)
      real, intent (out) :: lon(im,jm)
      integer i,j
      character(200) filename

      real :: tmpi(im), tmpj(jm)

#ifdef map_code
write(6,*) "-----USER_getLonLat--------"
write(6,*) "  Reads in latlon.dat"
write(6,*) "  Sets NaN for cells with -9999"
write(6,*) 
#endif

      write(filename,'(A, A)') trim(DATADIR),'/latlon.dat'
      open (19,file=filename,status='old')

      if(Which_gridio.eq.2 .OR. Which_gridio.eq.3) then
        read (19,*) !Header, lat 
        read (19,*) tmpj
        do j=1,jm
         do i=1,im
           lat(i,j) = tmpj(j)
         enddo
        enddo
        read (19,*) tmpi
        do j=1,jm
         do i=1,im
           lon(i,j) = tmpi(i)
         enddo
        enddo
      else
        read (19,*) !Header, lat 
        do j=1,jm
         read (19,*) lat(:,j)
        enddo
        read (19,*) !Header, lon
        do j=1,jm
         read (19,*) lon(:,j)
        enddo
      endif

      close(19)

      do j=1,jm
       do i=1,im
        if(lat(i,j)<=-9999) lat(i,j)=fill(1)
        if(lon(i,j)<=-9999) lon(i,j)=fill(1)
       enddo
      enddo

      return
      end subroutine USER_getLonLat
