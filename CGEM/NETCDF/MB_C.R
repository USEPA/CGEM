rm(list=ls())

#ENTER FILE PATH TO netCDF File
model_netcdf <- "cgem.000000.nc"

#-------------------------------------END USER INPUT--------------------------

sink("MB_C.txt",split=FALSE);

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

DICtot <- 1:nt
dim(DICtot) <- c(nt)
Atot <- 1:nt
dim(Atot) <- c(nt)
Ztot <- 1:nt
dim(Ztot) <- c(nt)
OMA_tot <- 1:nt
dim(OMA_tot) <- c(nt)
OMZ_tot <- 1:nt
dim(OMZ_tot) <- c(nt)
OMR_tot <- 1:nt
dim(OMR_tot) <- c(nt)
OMBC_tot <- 1:nt
dim(OMBC_tot) <- c(nt)
Tot <- 1:nt
dim(Tot) <- c(nt)

Qc <- ncatt_get(nc,0,"Qc")$value
ZQc <- ncatt_get(nc,0,"ZQc")$value

for (t in 1:nt){
  DIC<-ncvar_get(nc,"DIC",start=c(1,1,1,t),count=c(nx,ny,nz,1))
  A1<-ncvar_get(nc,"A1",start=c(1,1,1,t),count=c(nx,ny,nz,1))
  A2<-ncvar_get(nc,"A2",start=c(1,1,1,t),count=c(nx,ny,nz,1))
  A3<-ncvar_get(nc,"A3",start=c(1,1,1,t),count=c(nx,ny,nz,1))
  A4<-ncvar_get(nc,"A4",start=c(1,1,1,t),count=c(nx,ny,nz,1))
  A5<-ncvar_get(nc,"A5",start=c(1,1,1,t),count=c(nx,ny,nz,1))
  A6<-ncvar_get(nc,"A6",start=c(1,1,1,t),count=c(nx,ny,nz,1))
  Z1<-ncvar_get(nc,"Z1",start=c(1,1,1,t),count=c(nx,ny,nz,1))
  Z2<-ncvar_get(nc,"Z2",start=c(1,1,1,t),count=c(nx,ny,nz,1))
  OM1_A<-ncvar_get(nc,"OM1_A",start=c(1,1,1,t),count=c(nx,ny,nz,1))
  OM2_A<-ncvar_get(nc,"OM2_A",start=c(1,1,1,t),count=c(nx,ny,nz,1))
  OM1_Z<-ncvar_get(nc,"OM1_Z",start=c(1,1,1,t),count=c(nx,ny,nz,1))
  OM2_Z<-ncvar_get(nc,"OM2_Z",start=c(1,1,1,t),count=c(nx,ny,nz,1))
  OM1_R<-ncvar_get(nc,"OM1_R",start=c(1,1,1,t),count=c(nx,ny,nz,1))
  OM2_R<-ncvar_get(nc,"OM2_R",start=c(1,1,1,t),count=c(nx,ny,nz,1))
  OM1_BC<-ncvar_get(nc,"OM1_BC",start=c(1,1,1,t),count=c(nx,ny,nz,1))
  OM2_BC<-ncvar_get(nc,"OM2_BC",start=c(1,1,1,t),count=c(nx,ny,nz,1))
  
  DICtot[t] = 0.
  Atot[t] = 0.
  A2tot[t] = 0.
  Ztot[t] = 0.
  OMA_tot[t] = 0.
  OMZ_tot[t] = 0.
  OMR_tot[t] = 0.
  OMBC_tot[t] = 0.
  
  for (i in 1:nx){
    for (j in 1:ny){
      for (k in 1:nz){
        if(A1[k] < 0.) next
        DICtot[t] = DICtot[t] + DIC[k]
        Atot[t] = Atot[t] + A1[k]*Qc[1] + A2[k]*Qc[2] +A3[k]*Qc[3] 
        A2tot[t] = A2tot[t] + A4[k]*Qc[4] + A5[k]*Qc[5] + A6[k]*Qc[6]
        Ztot[t] = Ztot[t] + Z1[k]*ZQc[1] + Z2[k]*ZQc[2]
        OMA_tot[t] = OMA_tot[t] + OM1_A[k] + OM2_A[k] 
        OMZ_tot[t] = OMZ_tot[t] + OM1_Z[k] + OM2_Z[k]
        OMR_tot[t] = OMR_tot[t] + OM1_R[k] + OM2_R[k] 
        OMBC_tot[t] = OMBC_tot[t] + OM1_BC[k] + OM2_BC[k]
      }
    }
  }
  
  Tot[t] = DICtot[t] + Atot[t] + A2tot[t] + Ztot[t] + OMA_tot[t] + OMZ_tot[t] + OMR_tot[t] + OMBC_tot[t]
  if(t==1) Toti = Tot[i]
  cat(t,DICtot[t],Atot[t],+ A2tot[t],Ztot[t],OMA_tot[t],OMZ_tot[t],OMR_tot[t],OMBC_tot[t],Tot[t],Tot[t]-Tot[max(t-1,1)],(Tot[t]-Toti)/Toti*100,"\n")
}

sink()
