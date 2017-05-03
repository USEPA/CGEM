      MODULE Hydro 

      IMPLICIT NONE

      real,allocatable,save :: S(:,:,:) !Salinity
      real,allocatable,save :: T(:,:,:) !Temperature, C
      real,allocatable,save :: Wind(:,:) !Wind Speed, m/s
      real,allocatable,save :: Rad(:,:) !Solar Radiation, watts/m2
      real,allocatable,save :: Ux(:,:,:) !X- Horizonal Current Flux
      real,allocatable,save :: Vx(:,:,:) !Y- Horizonal Current Flux
      real,allocatable,save :: Wx(:,:,:) !Z- Vertical Current Flux
      real,allocatable,save :: Kh(:,:,:) !Diffusion Coefficients
      real,allocatable,save :: E(:,:) !Elevation


      contains

      Subroutine Allocate_Hydro()

      USE Model_dim
      USE Fill_Value

      IMPLICIT NONE

      ALLOCATE(S(im,jm,nsl))
      ALLOCATE(T(im,jm,nsl))
      ALLOCATE(Wind(im,jm))
      ALLOCATE(Rad(im,jm))
      ALLOCATE(Ux(im,jm,nsl))
      ALLOCATE(Vx(im,jm,nsl))
      ALLOCATE(Wx(im,jm,nsl))
      ALLOCATE(Kh(im,jm,nsl))
      ALLOCATE(E(im,jm))

      !Fill values for netCDF
      S=fill(0)  
      T=fill(0) 
      Wind=fill(0)  
      Rad=fill(0) 
      Ux=fill(0)
      Vx=fill(0)  
      Wx=fill(0) 
      Kh=fill(0)
      E=fill(0)  

      return

      End Subroutine Allocate_Hydro 


      End Module Hydro 
