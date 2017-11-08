rm(list=ls())

#ENTER A TITLE FOR MODEL RUN
run<-"PO4"

#ENTER FILE PATH TO netCDF File
model_netcdf<-"output.1D.nc"

#-------------------------------------END USER INPUT--------------------------

sink("MB_PO4.txt",split=FALSE);

#The Script For Plotting
library(ncdf4)

#Open netCDF file and get dimensions
nc<-nc_open(model_netcdf)
nx <- 1 
ny <- 1 
nz <- 21
nt <- 365 

PO4tot <- 1:nt
dim(PO4tot) <- c(nt)
A1tot <- 1:nt
dim(A1tot) <- c(nt)
A2tot <- 1:nt
dim(A2tot) <- c(nt)
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


ZQp <- ncatt_get(nc,0,"ZQp")$value
stoich_x1R <- ncatt_get(nc,0,"stoich_x1R")$value
stoich_x2R <- ncatt_get(nc,0,"stoich_x2R")$value
stoich_x1BC <- ncatt_get(nc,0,"stoich_x1BC")$value
stoich_x2BC <- ncatt_get(nc,0,"stoich_x2BC")$value


for (t in 1:nt){

PO4<-ncvar_get(nc,"PO4",start=c(1,1,1,t),count=c(nx,ny,nz-1,1))
A1<-ncvar_get(nc,"A1",start=c(1,1,1,t),count=c(nx,ny,nz-1,1))
A2<-ncvar_get(nc,"A2",start=c(1,1,1,t),count=c(nx,ny,nz-1,1))
A3<-ncvar_get(nc,"A3",start=c(1,1,1,t),count=c(nx,ny,nz-1,1))
Qp1<-ncvar_get(nc,"Qp1",start=c(1,1,1,t),count=c(nx,ny,nz-1,1))
Qp2<-ncvar_get(nc,"Qp2",start=c(1,1,1,t),count=c(nx,ny,nz-1,1))
Qp3<-ncvar_get(nc,"Qp3",start=c(1,1,1,t),count=c(nx,ny,nz-1,1))
A4<-ncvar_get(nc,"A4",start=c(1,1,1,t),count=c(nx,ny,nz-1,1))
A5<-ncvar_get(nc,"A5",start=c(1,1,1,t),count=c(nx,ny,nz-1,1))
A6<-ncvar_get(nc,"A6",start=c(1,1,1,t),count=c(nx,ny,nz-1,1))
Qp4<-ncvar_get(nc,"Qp4",start=c(1,1,1,t),count=c(nx,ny,nz-1,1))
Qp5<-ncvar_get(nc,"Qp5",start=c(1,1,1,t),count=c(nx,ny,nz-1,1))
Qp6<-ncvar_get(nc,"Qp6",start=c(1,1,1,t),count=c(nx,ny,nz-1,1))
Z1<-ncvar_get(nc,"Z1",start=c(1,1,1,t),count=c(nx,ny,nz-1,1))
Z2<-ncvar_get(nc,"Z2",start=c(1,1,1,t),count=c(nx,ny,nz-1,1))
OM1_A<-ncvar_get(nc,"OM1_A",start=c(1,1,1,t),count=c(nx,ny,nz-1,1))
OM2_A<-ncvar_get(nc,"OM2_A",start=c(1,1,1,t),count=c(nx,ny,nz-1,1))
s_x1A <- ncvar_get(nc,"s_x1A",start=c(1,1,1,t),count=c(nx,ny,nz-1,1))
s_x2A <- ncvar_get(nc,"s_x2A",start=c(1,1,1,t),count=c(nx,ny,nz-1,1))
OM1_Z<-ncvar_get(nc,"OM1_Z",start=c(1,1,1,t),count=c(nx,ny,nz-1,1))
OM2_Z<-ncvar_get(nc,"OM2_Z",start=c(1,1,1,t),count=c(nx,ny,nz-1,1))
s_x1Z <- ncvar_get(nc,"s_x1Z",start=c(1,1,1,t),count=c(nx,ny,nz-1,1))
s_x2Z <- ncvar_get(nc,"s_x2Z",start=c(1,1,1,t),count=c(nx,ny,nz-1,1))
OM1_R<-ncvar_get(nc,"OM1_R",start=c(1,1,1,t),count=c(nx,ny,nz-1,1))
OM2_R<-ncvar_get(nc,"OM2_R",start=c(1,1,1,t),count=c(nx,ny,nz-1,1))
OM1_BC<-ncvar_get(nc,"OM1_BC",start=c(1,1,1,t),count=c(nx,ny,nz-1,1))
OM2_BC<-ncvar_get(nc,"OM2_BC",start=c(1,1,1,t),count=c(nx,ny,nz-1,1))

PO4tot[t] = 0.
A1tot[t] = 0.
A2tot[t] = 0.
Ztot[t] = 0.
OMA_tot[t] = 0.
OMZ_tot[t] = 0.
OMR_tot[t] = 0.
OMBC_tot[t] = 0.

for (i in 1:nx){
for (j in 1:ny){
for (k in 1:(nz-1)){
  if(s_x1A[k]<0.) next 
  if(PO4[k]<0.) next
  PO4tot[t] = PO4tot[t] + PO4[k]
  A1tot[t] = A1tot[t] + A1[k]*Qp1[k] + A2[k]*Qp2[k] +A3[k]*Qp3[k] 
  A2tot[t] = A2tot[t] + A4[k]*Qp4[k] +A5[k]*Qp5[k] +A6[k]*Qp6[k]
  Ztot[t] = Ztot[t] + Z1[k]*ZQp[1] + Z2[k]*ZQp[2]
  OMA_tot[t] = OMA_tot[t] + OM1_A[k]/s_x1A[k] + OM2_A[k]/s_x2A[k] 
  OMZ_tot[t] = OMZ_tot[t] + OM1_Z[k]/s_x1Z[k] + OM2_Z[k]/s_x2Z[k]
  OMR_tot[t] = OMR_tot[t] + OM1_R[k]/stoich_x1R + OM2_R[k]/stoich_x2R 
  OMBC_tot[t] = OMBC_tot[t] + OM1_BC[k]/stoich_x1BC + OM2_BC[k]/stoich_x2BC
}}}

Tot[t] = PO4tot[t] + A1tot[t] + A2tot[t] + Ztot[t] + OMA_tot[t] + OMZ_tot[t] + OMR_tot[t] + OMBC_tot[t]
if(t==1) Toti = Tot[i]
cat(t,PO4tot[t],A1tot[t]+A2tot[t],Ztot[t],OMA_tot[t],OMZ_tot[t],OMR_tot[t],OMBC_tot[t],Tot[t],Tot[t]-Tot[max(t-1,1)],(Tot[t]-Toti)/Toti*100,"\n")
}

sink()
