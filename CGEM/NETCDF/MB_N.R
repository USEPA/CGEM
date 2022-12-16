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
nx <- 1 
ny <- 1 
nz <- 1 

Ntot <- 1:nt
dim(Ntot) <- c(nt)
A1tot <- 1:nt
dim(A1tot) <- c(nt)
A2tot <- 1:nt
dim(A2tot) <- c(nt)
Ztot <- 1:nt
dim(Ztot) <- c(nt)
OMA_tot <- 1:nt
dim(OMA_tot) <- c(nt)
OM1Z_tot <- 1:nt
dim(OM1Z_tot) <- c(nt)
OM2Z_tot <- 1:nt
dim(OM2Z_tot) <- c(nt)
OMR_tot <- 1:nt
dim(OMR_tot) <- c(nt)
OMBC_tot <- 1:nt
dim(OMBC_tot) <- c(nt)
R11_tot <- 1:nt
dim(R11_tot) <- c(nt)

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
  NH4<-ncvar_get(nc,"NH4",start=c(1,1,1,t),count=c(nx,ny,nz,1))
  NO3<-ncvar_get(nc,"NO3",start=c(1,1,1,t),count=c(nx,ny,nz,1))
  A1<-ncvar_get(nc,"A1",start=c(1,1,1,t),count=c(nx,ny,nz,1))
  A2<-ncvar_get(nc,"A2",start=c(1,1,1,t),count=c(nx,ny,nz,1))
  A3<-ncvar_get(nc,"A3",start=c(1,1,1,t),count=c(nx,ny,nz,1))
  Qn1<-ncvar_get(nc,"Qn1",start=c(1,1,1,t),count=c(nx,ny,nz,1))
  Qn2<-ncvar_get(nc,"Qn2",start=c(1,1,1,t),count=c(nx,ny,nz,1))
  Qn3<-ncvar_get(nc,"Qn3",start=c(1,1,1,t),count=c(nx,ny,nz,1))
  A4<-ncvar_get(nc,"A4",start=c(1,1,1,t),count=c(nx,ny,nz,1))
  A5<-ncvar_get(nc,"A5",start=c(1,1,1,t),count=c(nx,ny,nz,1))
  A6<-ncvar_get(nc,"A6",start=c(1,1,1,t),count=c(nx,ny,nz,1))
  Qn4<-ncvar_get(nc,"Qn4",start=c(1,1,1,t),count=c(nx,ny,nz,1))
  Qn5<-ncvar_get(nc,"Qn5",start=c(1,1,1,t),count=c(nx,ny,nz,1))
  Qn6<-ncvar_get(nc,"Qn6",start=c(1,1,1,t),count=c(nx,ny,nz,1))
  Z1<-ncvar_get(nc,"Z1",start=c(1,1,1,t),count=c(nx,ny,nz,1))
  Z2<-ncvar_get(nc,"Z2",start=c(1,1,1,t),count=c(nx,ny,nz,1))
  OM1_A<-ncvar_get(nc,"OM1_A",start=c(1,1,1,t),count=c(nx,ny,nz,1))
  OM2_A<-ncvar_get(nc,"OM2_A",start=c(1,1,1,t),count=c(nx,ny,nz,1))
  s_x1A <- ncvar_get(nc,"s_x1A",start=c(1,1,1,t),count=c(nx,ny,nz,1))
  s_x2A <- ncvar_get(nc,"s_x2A",start=c(1,1,1,t),count=c(nx,ny,nz,1))
  s_y1A <- ncvar_get(nc,"s_y1A",start=c(1,1,1,t),count=c(nx,ny,nz,1))
  s_y2A <- ncvar_get(nc,"s_y2A",start=c(1,1,1,t),count=c(nx,ny,nz,1))
  OM1_Z<-ncvar_get(nc,"OM1_Z",start=c(1,1,1,t),count=c(nx,ny,nz,1))
  OM2_Z<-ncvar_get(nc,"OM2_Z",start=c(1,1,1,t),count=c(nx,ny,nz,1))
  s_x1Z <- ncvar_get(nc,"s_x1Z",start=c(1,1,1,t),count=c(nx,ny,nz,1))
  s_x2Z <- ncvar_get(nc,"s_x2Z",start=c(1,1,1,t),count=c(nx,ny,nz,1))
  s_y1Z <- ncvar_get(nc,"s_y1Z",start=c(1,1,1,t),count=c(nx,ny,nz,1))
  s_y2Z <- ncvar_get(nc,"s_y2Z",start=c(1,1,1,t),count=c(nx,ny,nz,1))
  OM1_R<-ncvar_get(nc,"OM1_R",start=c(1,1,1,t),count=c(nx,ny,nz,1))
  OM2_R<-ncvar_get(nc,"OM2_R",start=c(1,1,1,t),count=c(nx,ny,nz,1))
  OM1_BC<-ncvar_get(nc,"OM1_BC",start=c(1,1,1,t),count=c(nx,ny,nz,1))
  OM2_BC<-ncvar_get(nc,"OM2_BC",start=c(1,1,1,t),count=c(nx,ny,nz,1))
  R_11<-ncvar_get(nc,"R_11",start=c(1,1,1,t),count=c(nx,ny,nz,1))
  
  Ntot[t] = 0.
  A1tot[t] = 0.
  A2tot[t] = 0.
  Ztot[t] = 0.
  OMA_tot[t] = 0.
  OM1Z_tot[t] = 0.
  OM2Z_tot[t] = 0.
  OMR_tot[t] = 0.
  OMBC_tot[t] = 0.
  R11_tot[t] = 0.
  
  for (i in 1:nx){
    for (j in 1:ny){
      for (k in 1:(nz)){
        if(s_x1A[k] < 0.) next 
        if(NH4[k] < 0.) next
        Ntot[t] = Ntot[t] + NH4[k] + NO3[k]
        A1tot[t] = A1tot[t] + A1[k]*Qn1[k] + A2[k]*Qn2[k] +A3[k]*Qn3[k] 
        A2tot[t] = A2tot[t] + A4[k]*Qn4[k] +A5[k]*Qn5[k] +A6[k]*Qn6[k]
        Ztot[t] = Ztot[t] + Z1[k]*ZQn[1] + Z2[k]*ZQn[2]
        OMA_tot[t] = OMA_tot[t] + OM1_A[k]/s_x1A[k]*s_y1A[k] + OM2_A[k]/s_x2A[k]*s_y2A[k] 
        OM1Z_tot[t] = OM1Z_tot[t] + OM1_Z[k]/s_x1Z[k]*s_y1Z[k] 
        OM2Z_tot[t] = OM2Z_tot[t] + OM2_Z[k]/s_x2Z[k]*s_y2Z[k]
        OMR_tot[t] = OMR_tot[t] + OM1_R[k]/stoich_x1R*stoich_y1R + OM2_R[k]/stoich_x2R*stoich_y2R 
        OMBC_tot[t] = OMBC_tot[t] + OM1_BC[k]/stoich_x1BC*stoich_y1BC + OM2_BC[k]/stoich_x2BC*stoich_y2BC
        R11_tot[t] = R11_tot[t] + R_11[k]
      }
    }
  }
  R11_tot[1] = 0
  cat(t,Ntot[t],A1tot[t]+A2tot[t],Ztot[t],OMA_tot[t],OM1Z_tot[t],OM2Z_tot[t],OMR_tot[t],OMBC_tot[t],R11_tot[t],Ntot[t] + A1tot[t] + A2tot[t] + Ztot[t] + OMA_tot[t] + OM1Z_tot[t] + OM2Z_tot[t] + OMR_tot[t] + OMBC_tot[t] - R11_tot[t],"\n")
}

Sys.time()
sink()
