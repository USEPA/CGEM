rm(list=ls())

#ENTER FILE PATH TO netCDF File
model_netcdf <- "cgem.000000.nc"

#-------------------------------------END USER INPUT--------------------------

sink("MB_N.txt",split=FALSE);
Sys.time()

#The Script For Plotting
library(ncdf4)

#Open netCDF file and get dimensions
nc <- nc_open(model_netcdf)
time <- ncvar_get(nc,"time")
iYr0 <- ncatt_get(nc,0,attname="iYr0")$value
time <- as.POSIXct(time, origin=paste(iYr0,"-01-01",sep=""), tz="GMT")
nt <- dim(time)
nx <- nc$dim$longitude$len
ny <- nc$dim$latitude$len
nz <- nc$dim$k$len

Vars <- names(nc$var)
A2_Exist <- "A2" %in% Vars
A3_Exist <- "A3" %in% Vars
A4_Exist <- "A4" %in% Vars
A5_Exist <- "A5" %in% Vars
A6_Exist <- "A6" %in% Vars

Qn2_Exist <- "Qn2" %in% Vars
Qn3_Exist <- "Qn3" %in% Vars
Qn4_Exist <- "Qn4" %in% Vars
Qn5_Exist <- "Qn5" %in% Vars
Qn6_Exist <- "Qn6" %in% Vars

R_11_Exist <- "R_11" %in% Vars

A2 <- array(0.0, dim = c(nx,ny,nz,1))
A3 <- array(0.0, dim = c(nx,ny,nz,1))
A4 <- array(0.0, dim = c(nx,ny,nz,1))
A5 <- array(0.0, dim = c(nx,ny,nz,1))
A6 <- array(0.0, dim = c(nx,ny,nz,1))

Qn2 <- array(0.0, dim = c(nx,ny,nz,1))
Qn3 <- array(0.0, dim = c(nx,ny,nz,1))
Qn4 <- array(0.0, dim = c(nx,ny,nz,1))
Qn5 <- array(0.0, dim = c(nx,ny,nz,1))
Qn6 <- array(0.0, dim = c(nx,ny,nz,1))

R_11 <- array(0.0, dim = c(nx,ny,nz,1))

Ntot <- array(0.0, dim = c(nt))
A1tot <- array(0.0, dim = c(nt))
A2tot <- array(0.0, dim = c(nt))
Ztot <- array(0.0, dim = c(nt))
OMA_tot <- array(0.0, dim = c(nt))
OM1Z_tot <- array(0.0, dim = c(nt))
OM2Z_tot <- array(0.0, dim = c(nt))
OMR_tot <- array(0.0, dim = c(nt))
OMBC_tot <- array(0.0, dim = c(nt))
R11_tot <- array(0.0, dim = c(nt))

ZQn <- ncatt_get(nc,0,"ZQn")$value
stoich_x1R <- ncatt_get(nc,0,"stoich_x1R")$value
stoich_x2R <- ncatt_get(nc,0,"stoich_x2R")$value
stoich_x1BC <- ncatt_get(nc,0,"stoich_x1BC")$value
stoich_x2BC <- ncatt_get(nc,0,"stoich_x2BC")$value
stoich_y1R <- ncatt_get(nc,0,"stoich_y1R")$value
stoich_y2R <- ncatt_get(nc,0,"stoich_y2R")$value
stoich_y1BC <- ncatt_get(nc,0,"stoich_y1BC")$value
stoich_y2BC <- ncatt_get(nc,0,"stoich_y2BC")$value


for (t in 1:nt) {
  NH4 <- ncvar_get(nc,"NH4",start=c(1,1,1,t),count=c(nx,ny,nz,1), collapse_degen = FALSE)
  NO3 <- ncvar_get(nc,"NO3",start=c(1,1,1,t),count=c(nx,ny,nz,1), collapse_degen = FALSE)
  A1 <- ncvar_get(nc,"A1",start=c(1,1,1,t),count=c(nx,ny,nz,1), collapse_degen = FALSE)
  if(A2_Exist) A2 <- ncvar_get(nc,"A2",start=c(1,1,1,t),count=c(nx,ny,nz,1), collapse_degen = FALSE)
  if(A3_Exist) A3 <- ncvar_get(nc,"A3",start=c(1,1,1,t),count=c(nx,ny,nz,1), collapse_degen = FALSE)
  Qn1 <- ncvar_get(nc,"Qn1",start=c(1,1,1,t),count=c(nx,ny,nz,1), collapse_degen = FALSE)
  if(Qn2_Exist) Qn2 <- ncvar_get(nc,"Qn2",start=c(1,1,1,t),count=c(nx,ny,nz,1), collapse_degen = FALSE)
  if(Qn3_Exist) Qn3 <- ncvar_get(nc,"Qn3",start=c(1,1,1,t),count=c(nx,ny,nz,1), collapse_degen = FALSE)
  if(A4_Exist) A4 <- ncvar_get(nc,"A4",start=c(1,1,1,t),count=c(nx,ny,nz,1), collapse_degen = FALSE)
  if(A5_Exist) A5 <- ncvar_get(nc,"A5",start=c(1,1,1,t),count=c(nx,ny,nz,1), collapse_degen = FALSE)
  if(A6_Exist) A6 <- ncvar_get(nc,"A6",start=c(1,1,1,t),count=c(nx,ny,nz,1), collapse_degen = FALSE)
  if(Qn4_Exist) Qn4 <- ncvar_get(nc,"Qn4",start=c(1,1,1,t),count=c(nx,ny,nz,1), collapse_degen = FALSE)
  if(Qn5_Exist) Qn5 <- ncvar_get(nc,"Qn5",start=c(1,1,1,t),count=c(nx,ny,nz,1), collapse_degen = FALSE)
  if(Qn6_Exist) Qn6 <- ncvar_get(nc,"Qn6",start=c(1,1,1,t),count=c(nx,ny,nz,1), collapse_degen = FALSE)
  Z1 <- ncvar_get(nc,"Z1",start=c(1,1,1,t),count=c(nx,ny,nz,1), collapse_degen = FALSE)
  Z2 <- ncvar_get(nc,"Z2",start=c(1,1,1,t),count=c(nx,ny,nz,1), collapse_degen = FALSE)
  OM1_A<-ncvar_get(nc,"OM1_A",start=c(1,1,1,t),count=c(nx,ny,nz,1), collapse_degen = FALSE)
  OM2_A<-ncvar_get(nc,"OM2_A",start=c(1,1,1,t),count=c(nx,ny,nz,1), collapse_degen = FALSE)
  s_x1A <- ncvar_get(nc,"s_x1A",start=c(1,1,1,t),count=c(nx,ny,nz,1), collapse_degen = FALSE)
  s_x2A <- ncvar_get(nc,"s_x2A",start=c(1,1,1,t),count=c(nx,ny,nz,1), collapse_degen = FALSE)
  s_y1A <- ncvar_get(nc,"s_y1A",start=c(1,1,1,t),count=c(nx,ny,nz,1), collapse_degen = FALSE)
  s_y2A <- ncvar_get(nc,"s_y2A",start=c(1,1,1,t),count=c(nx,ny,nz,1), collapse_degen = FALSE)
  OM1_Z<-ncvar_get(nc,"OM1_Z",start=c(1,1,1,t),count=c(nx,ny,nz,1), collapse_degen = FALSE)
  OM2_Z<-ncvar_get(nc,"OM2_Z",start=c(1,1,1,t),count=c(nx,ny,nz,1), collapse_degen = FALSE)
  s_x1Z <- ncvar_get(nc,"s_x1Z",start=c(1,1,1,t),count=c(nx,ny,nz,1), collapse_degen = FALSE)
  s_x2Z <- ncvar_get(nc,"s_x2Z",start=c(1,1,1,t),count=c(nx,ny,nz,1), collapse_degen = FALSE)
  s_y1Z <- ncvar_get(nc,"s_y1Z",start=c(1,1,1,t),count=c(nx,ny,nz,1), collapse_degen = FALSE)
  s_y2Z <- ncvar_get(nc,"s_y2Z",start=c(1,1,1,t),count=c(nx,ny,nz,1), collapse_degen = FALSE)
  OM1_R <- ncvar_get(nc,"OM1_R",start=c(1,1,1,t),count=c(nx,ny,nz,1), collapse_degen = FALSE)
  OM2_R <- ncvar_get(nc,"OM2_R",start=c(1,1,1,t),count=c(nx,ny,nz,1), collapse_degen = FALSE)
  OM1_BC <- ncvar_get(nc,"OM1_BC",start=c(1,1,1,t),count=c(nx,ny,nz,1), collapse_degen = FALSE)
  OM2_BC <- ncvar_get(nc,"OM2_BC",start=c(1,1,1,t),count=c(nx,ny,nz,1), collapse_degen = FALSE)
  if (R_11_Exist) R_11 <- ncvar_get(nc,"R_11",start=c(1,1,1,t),count=c(nx,ny,nz,1), collapse_degen = FALSE)
  
  
  for (i in 1:nx){
    for (j in 1:ny){
      for (k in 1:(nz)){
        if(s_x1A[i,j,k,1] < 0.) next 
        if(NH4[i,j,k,1] < 0.) next
        Ntot[t] = Ntot[t] + NH4[i,j,k,1] + NO3[i,j,k,1]
        A1tot[t] = A1tot[t] + A1[i,j,k,1]*Qn1[i,j,k,1] + A2[i,j,k,1]*Qn2[i,j,k,1] +A3[i,j,k,1]*Qn3[i,j,k,1] 
        A2tot[t] = A2tot[t] + A4[i,j,k,1]*Qn4[i,j,k,1] +A5[i,j,k,1]*Qn5[i,j,k,1] +A6[i,j,k,1]*Qn6[i,j,k,1]
        Ztot[t] = Ztot[t] + Z1[i,j,k,1]*ZQn[1] + Z2[i,j,k,1]*ZQn[2]
        OMA_tot[t] = OMA_tot[t] + OM1_A[i,j,k,1]/s_x1A[i,j,k,1]*s_y1A[i,j,k,1] + OM2_A[i,j,k,1]/s_x2A[i,j,k,1]*s_y2A[i,j,k,1] 
        OM1Z_tot[t] = OM1Z_tot[t] + OM1_Z[i,j,k,1]/s_x1Z[i,j,k,1]*s_y1Z[i,j,k,1] 
        OM2Z_tot[t] = OM2Z_tot[t] + OM2_Z[i,j,k,1]/s_x2Z[i,j,k,1]*s_y2Z[i,j,k,1]
        OMR_tot[t] = OMR_tot[t] + OM1_R[i,j,k,1]/stoich_x1R*stoich_y1R + OM2_R[i,j,k,1]/stoich_x2R*stoich_y2R 
        OMBC_tot[t] = OMBC_tot[t] + OM1_BC[i,j,k,1]/stoich_x1BC*stoich_y1BC + OM2_BC[i,j,k,1]/stoich_x2BC*stoich_y2BC
        R11_tot[t] = R11_tot[t] + R_11[i,j,k,1]
      }
    }
  }
  R11_tot[1] = 0
  cat(t,Ntot[t],A1tot[t]+A2tot[t],Ztot[t],OMA_tot[t],OM1Z_tot[t],OM2Z_tot[t],OMR_tot[t],OMBC_tot[t],R11_tot[t],Ntot[t] + A1tot[t] + A2tot[t] + Ztot[t] + OMA_tot[t] + OM1Z_tot[t] + OM2Z_tot[t] + OMR_tot[t] + OMBC_tot[t] - R11_tot[t],"\n")
}

Sys.time()
sink()
