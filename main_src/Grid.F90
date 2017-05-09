      MODULE Grid

       USE Model_dim

       IMPLICIT NONE

      real,allocatable,save :: lat(:,:),lon(:,:) !Latitude and longitude of each grid cell
      real,allocatable,save :: d(:,:,:)          !Water depth at cell bottom
      real,allocatable,save :: d_sfc(:,:,:)      !Distance from surface to center of cell
      real,allocatable,save :: depth(:,:)      !Distance from surface to center of cell
      real,allocatable,save :: dz(:,:,:)         !Thickness of cell
      real,allocatable,save :: Vol(:,:,:)        !Volume of each cell
      real,allocatable,save :: area(:,:,:)       !Area of each cell
      integer,allocatable,save :: fm(:,:)        ! land(0)/sea(1) mask for NCOM, other for EFDC
      integer,allocatable,save :: wsm(:,:)       ! shelf(0)/open ocean(1) mask

      contains

      Subroutine Set_Grid()

      IMPLICIT NONE

      integer j
      character(200) filename

      call Grid_allocate()

      write(filename,'(A, A)') trim(DATADIR),'/nz.dat'
      open(unit=19,file=filename)
      read(19,*)
      do j=1,jm
        read(19,*) nza(:,j)
      enddo
      close(19)

!----------------------
! --- get grid location
!----------------------
      call USER_getLonLat(lat,lon)
      call USER_get_basic_grid(dz,depth,d,d_sfc,Vol,depth)
      area = Vol/dz
!--------------------------------
! --- get land/water and shelf masks
!--------------------------------
      call USER_get_masks(fm,wsm)

      return

      End Subroutine Set_Grid

      Subroutine Grid_allocate()

      USE Fill_Value

      IMPLICIT NONE

      ALLOCATE(lat(im,jm))
      ALLOCATE(lon(im,jm))
      ALLOCATE(depth(im,jm))
      ALLOCATE(d(im,jm,nsl))
      ALLOCATE(Vol(im,jm,nsl))
      ALLOCATE(area(im,jm,nsl))
      ALLOCATE(dz(im,jm,nsl))
      ALLOCATE(d_sfc(im,jm,nsl))
      ALLOCATE(fm(im,jm))
      ALLOCATE(wsm(im,jm))

      lat=fill(0)  !Fill values for netCDF
      lon=fill(0)  
      depth=fill(0) 
      d=fill(0)  
      Vol=fill(0)  
      area=fill(0)
      dz=fill(0) 
      d_sfc=fill(0)  
      fm=fill(0) 
      wsm=fill(0) 

      return

      End Subroutine Grid_allocate


      End Module Grid
