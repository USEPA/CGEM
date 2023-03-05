rm(list=ls())

#ENTER FILE PATH TO netCDF File
model_netcdf <- "cgem.000000.nc"

#-------------------------------------END USER INPUT--------------------------

sink("MB_PO4.txt",split=FALSE);

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

Qp2_Exist <- "Qp2" %in% Vars
Qp3_Exist <- "Qp3" %in% Vars
Qp4_Exist <- "Qp4" %in% Vars
Qp5_Exist <- "Qp5" %in% Vars
Qp6_Exist <- "Qp6" %in% Vars

A2 <- array(0.0, dim = c(nx,ny,nz,1))
A3 <- array(0.0, dim = c(nx,ny,nz,1))
A4 <- array(0.0, dim = c(nx,ny,nz,1))
A5 <- array(0.0, dim = c(nx,ny,nz,1))
A6 <- array(0.0, dim = c(nx,ny,nz,1))

Qp2 <- array(0.0, dim = c(nx,ny,nz,1))
Qp3 <- array(0.0, dim = c(nx,ny,nz,1))
Qp4 <- array(0.0, dim = c(nx,ny,nz,1))
Qp5 <- array(0.0, dim = c(nx,ny,nz,1))
Qp6 <- array(0.0, dim = c(nx,ny,nz,1))

PO4tot <- array(0.0, dim = c(nt))
A1tot <- array(0.0, dim = c(nt))
A2tot <- array(0.0, dim = c(nt))
Ztot <- array(0.0, dim = c(nt))
OMA_tot <- array(0.0, dim = c(nt))
OMZ_tot <- array(0.0, dim = c(nt))
OMR_tot <- array(0.0, dim = c(nt))
OMBC_tot <- array(0.0, dim = c(nt))
Tot <- array(0.0, dim = c(nt))

ZQp <- ncatt_get(nc,0,"ZQp")$value
stoich_x1R <- ncatt_get(nc,0,"stoich_x1R")$value
stoich_x2R <- ncatt_get(nc,0,"stoich_x2R")$value
stoich_x1BC <- ncatt_get(nc,0,"stoich_x1BC")$value
stoich_x2BC <- ncatt_get(nc,0,"stoich_x2BC")$value


for (t in 1:nt){
  PO4 <- ncvar_get(nc,"PO4",start=c(1,1,1,t),count=c(nx,ny,nz,1), collapse_degen = FALSE)
  A1 <- ncvar_get(nc,"A1",start=c(1,1,1,t),count=c(nx,ny,nz,1), collapse_degen = FALSE)
  if(A2_Exist) A2 <- ncvar_get(nc,"A2",start=c(1,1,1,t),count=c(nx,ny,nz,1), collapse_degen = FALSE)
  if(A3_Exist) A3 <- ncvar_get(nc,"A3",start=c(1,1,1,t),count=c(nx,ny,nz,1), collapse_degen = FALSE)
  Qp1 <- ncvar_get(nc,"Qp1",start=c(1,1,1,t),count=c(nx,ny,nz,1), collapse_degen = FALSE)
  if(Qp2_Exist) Qp2 <- ncvar_get(nc,"Qp2",start=c(1,1,1,t),count=c(nx,ny,nz,1), collapse_degen = FALSE)
  if(Qp3_Exist) Qp3 <- ncvar_get(nc,"Qp3",start=c(1,1,1,t),count=c(nx,ny,nz,1), collapse_degen = FALSE)
  if(A4_Exist) A4 <- ncvar_get(nc,"A4",start=c(1,1,1,t),count=c(nx,ny,nz,1), collapse_degen = FALSE)
  if(A5_Exist) A5 <- ncvar_get(nc,"A5",start=c(1,1,1,t),count=c(nx,ny,nz,1), collapse_degen = FALSE)
  if(A6_Exist) A6 <- ncvar_get(nc,"A6",start=c(1,1,1,t),count=c(nx,ny,nz,1), collapse_degen = FALSE)
  if(Qp4_Exist) Qp4 <- ncvar_get(nc,"Qp4",start=c(1,1,1,t),count=c(nx,ny,nz,1), collapse_degen = FALSE)
  if(Qp5_Exist) Qp5 <- ncvar_get(nc,"Qp5",start=c(1,1,1,t),count=c(nx,ny,nz,1), collapse_degen = FALSE)
  if(Qp6_Exist) Qp6 <- ncvar_get(nc,"Qp6",start=c(1,1,1,t),count=c(nx,ny,nz,1), collapse_degen = FALSE)
  Z1 <- ncvar_get(nc,"Z1",start=c(1,1,1,t),count=c(nx,ny,nz,1), collapse_degen = FALSE)
  Z2 <- ncvar_get(nc,"Z2",start=c(1,1,1,t),count=c(nx,ny,nz,1), collapse_degen = FALSE)
  OM1_A <- ncvar_get(nc,"OM1_A",start=c(1,1,1,t),count=c(nx,ny,nz,1), collapse_degen = FALSE)
  OM2_A <- ncvar_get(nc,"OM2_A",start=c(1,1,1,t),count=c(nx,ny,nz,1), collapse_degen = FALSE)
  s_x1A  <-  ncvar_get(nc,"s_x1A",start=c(1,1,1,t),count=c(nx,ny,nz,1), collapse_degen = FALSE)
  s_x2A  <-  ncvar_get(nc,"s_x2A",start=c(1,1,1,t),count=c(nx,ny,nz,1), collapse_degen = FALSE)
  OM1_Z <- ncvar_get(nc,"OM1_Z",start=c(1,1,1,t),count=c(nx,ny,nz,1), collapse_degen = FALSE)
  OM2_Z <- ncvar_get(nc,"OM2_Z",start=c(1,1,1,t),count=c(nx,ny,nz,1), collapse_degen = FALSE)
  s_x1Z  <-  ncvar_get(nc,"s_x1Z",start=c(1,1,1,t),count=c(nx,ny,nz,1), collapse_degen = FALSE)
  s_x2Z  <-  ncvar_get(nc,"s_x2Z",start=c(1,1,1,t),count=c(nx,ny,nz,1), collapse_degen = FALSE)
  OM1_R <- ncvar_get(nc,"OM1_R",start=c(1,1,1,t),count=c(nx,ny,nz,1), collapse_degen = FALSE)
  OM2_R <- ncvar_get(nc,"OM2_R",start=c(1,1,1,t),count=c(nx,ny,nz,1), collapse_degen = FALSE)
  OM1_BC <- ncvar_get(nc,"OM1_BC",start=c(1,1,1,t),count=c(nx,ny,nz,1), collapse_degen = FALSE)
  OM2_BC <- ncvar_get(nc,"OM2_BC",start=c(1,1,1,t),count=c(nx,ny,nz,1), collapse_degen = FALSE)
  
  
  for (i in 1:nx){
    for (j in 1:ny){
      for (k in 1:(nz)){
        if(s_x1A[i,j,k,1] < 0.) next 
        if(PO4[i,j,k,1] < 0.) next
        PO4tot[t] = PO4tot[t] + PO4[i,j,k,1]
        A1tot[t] = A1tot[t] + A1[i,j,k,1]*Qp1[i,j,k,1] + A2[i,j,k,1]*Qp2[i,j,k,1] +A3[i,j,k,1]*Qp3[i,j,k,1] 
        A2tot[t] = A2tot[t] + A4[i,j,k,1]*Qp4[i,j,k,1] + A5[i,j,k,1]*Qp5[i,j,k,1] + A6[i,j,k,1]*Qp6[i,j,k,1]
        Ztot[t] = Ztot[t] + Z1[i,j,k,1]*ZQp[1] + Z2[i,j,k,1]*ZQp[2]
        OMA_tot[t] = OMA_tot[t] + OM1_A[i,j,k,1]/s_x1A[i,j,k,1] + OM2_A[i,j,k,1]/s_x2A[i,j,k,1] 
        OMZ_tot[t] = OMZ_tot[t] + OM1_Z[i,j,k,1]/s_x1Z[i,j,k,1] + OM2_Z[i,j,k,1]/s_x2Z[i,j,k,1]
        OMR_tot[t] = OMR_tot[t] + OM1_R[i,j,k,1]/stoich_x1R + OM2_R[i,j,k,1]/stoich_x2R 
        OMBC_tot[t] = OMBC_tot[t] + OM1_BC[i,j,k,1]/stoich_x1BC + OM2_BC[i,j,k,1]/stoich_x2BC
      }
    }
  }
  
  Tot[t] = PO4tot[t] + A1tot[t] + A2tot[t] + Ztot[t] + OMA_tot[t] + OMZ_tot[t] + OMR_tot[t] + OMBC_tot[t]
  if(t==1) Toti = Tot[1]
  cat(t,PO4tot[t],A1tot[t]+A2tot[t],Ztot[t],OMA_tot[t],OMZ_tot[t],OMR_tot[t],OMBC_tot[t],Tot[t],Tot[t]-Tot[max(t-1,1)],(Tot[t]-Toti)/Toti*100,"\n")
}

sink()
