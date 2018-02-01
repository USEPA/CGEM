library(ncdf4)


dz <- ncvar_get(nc_open("LayerDepth.nc"),"LayerDepth")

#u<-ncvar_get(nc_open("UFlow.nc"),"UFlow")
#v<-ncvar_get(nc_open("VFlow.nc"),"VFlow")
#w<-ncvar_get(nc_open("WFlow.nc"),"WFlow")
u<-ncvar_get(nc_open("UFlow.nc"),"UFlow")
v<-ncvar_get(nc_open("VFlow.nc"),"VFlow")
w<-ncvar_get(nc_open("WFlow.nc"),"WFlow")

im <- dim(w)[1]
jm <- dim(w)[2]
km <- dim(w)[3]
tm <- dim(w)[4]


fr <- file(description="./WEEKS_INP/dxdy.inp",open="r")
#im <- 20
#jm <- 33
tmp <- readLines(fr,n=1)
tmp <- readLines(fr,n=1)
tmp <- readLines(fr,n=1)
tmp <- readLines(fr,n=1)
dx <- matrix(0,im,jm)
dy <- matrix(0,im,jm)
depth <- matrix(0,im,jm)
while (length(line <- scan(fr,quote="",nlines=1,quiet=TRUE))>0){
  dx[line[1],line[2]] <- line[3]
  dy[line[1],line[2]] <- line[4]
  depth[line[1],line[2]] <- line[5]
}
dxdy <- dx*dy

dT <- 300.

courant <- array(0,c(im,jm,km,tm))
umps <- array(0,c(im,jm,km,tm))
vmps <- array(0,c(im,jm,km,tm))
wmps <- array(0,c(im,jm,km,tm))
speed <- array(0,c(im,jm,km,tm))

vol <- array(1,c(im,jm,km,tm))

for(i in 1:im){
 for(j in 1:jm){
  for(k in 1:km){
   for(tt in 1:tm){
     vol[i,j,k,tt] <- dx[i,j]*dy[i,j]*dz[i,j,k,tt] 
     umps[i,j,k,tt] <- u[i,j,k,tt]/dy[i,j]/dz[i,j,k,tt]
     vmps[i,j,k,tt] <- v[i,j,k,tt]/dx[i,j]/dz[i,j,k,tt]
     wmps[i,j,k,tt] <- w[i,j,k,tt]/dx[i,j]/dy[i,j]
     speed[i,j,k,tt] <- sqrt(wmps[i,j,k,tt]*wmps[i,j,k,tt] + vmps[i,j,k,tt]*vmps[i,j,k,tt] + umps[i,j,k,tt]*umps[i,j,k,tt])
     courant[i,j,k,tt] <- abs(u[i,j,k,tt])*dT/vol[i,j,k,tt] + abs(v[i,j,k,tt])*dT/vol[i,j,k,tt] + abs(w[i,j,k,tt])*dT/vol[i,j,k,tt]
  }
 }
}
}

abs_courant <- abs(courant)
max_courant <- max(abs_courant,na.rm=TRUE)
which(abs_courant==max(abs_courant,na.rm=TRUE),arr.ind=TRUE)
cat("For dT=300, maximum value for courant condition is",max_courant,"\n")
#     dim1 dim2 dim3 dim4
#[1,]   24   16    6  889
#> max_courant
#[1] 10.21232

mean(umps,na.rm=TRUE)
#[1] 0.0005058567
max(umps,na.rm=TRUE)
#[1] 0.9118488
min(umps,na.rm=TRUE)
#[1] -0.8563608
which(umps==max(umps,na.rm=TRUE),arr.ind=TRUE)
#     dim1 dim2 dim3 dim4
#[1,]   26   15    1  842

mean(vmps,na.rm=TRUE)
#[1] -0.004077618
max(vmps,na.rm=TRUE)
#[1] 0.3224699
min(vmps,na.rm=TRUE)
#[1] -0.5168507
which(vmps==min(vmps,na.rm=TRUE),arr.ind=TRUE)
#     dim1 dim2 dim3 dim4
#[1,]   24   16    1  889

mean(wmps,na.rm=TRUE)
#[1] 5.197696e-06
max(wmps,na.rm=TRUE)
#[1] 0.01395397
min(wmps,na.rm=TRUE)
#[1] -0.01138307
which(wmps==max(wmps,na.rm=TRUE),arr.ind=TRUE)
#     dim1 dim2 dim3 dim4
#[1,]   24   16    7  555

mean(speed,na.rm=TRUE)
#[1] 0.0358987
max(speed,na.rm=TRUE)
#[1] 0.912701

#dimX <- ncdim_def("X","meters",(1:im))
#dimY <- ncdim_def("Y","meters",(1:jm))
#dimZ <- ncdim_def("Z","layer",(1:km))
#timeVar <- ncvar_get(nc_open("UFlow.nc"),"Time")
#dimT <- ncdim_def("Time","seconds",vals=timeVar)

#varid_umps <- ncvar_def(name="umps",units="m/s",dim=list(dimX, dimY,dimZ,dimT))
#con_umps <- nc_create("umps.nc",varid_umps)
#ncvar_put(con_umps,varid_umps,umps)
#nc_close(con_umps)

#varid_vmps <- ncvar_def(name="vmps",units="m/s",dim=list(dimX, dimY,dimZ,dimT))
#con_vmps <- nc_create("vmps.nc",varid_vmps)
#ncvar_put(con_vmps,varid_vmps,vmps)
#nc_close(con_vmps)

#varid_wmps <- ncvar_def(name="wmps",units="m/s",dim=list(dimX, dimY,dimZ,dimT))
#con_wmps <- nc_create("wmps.nc",varid_wmps)
#ncvar_put(con_wmps,varid_wmps,wmps)
#nc_close(con_wmps)

#varid <- ncvar_def(name="speed",units="m/s",dim=list(dimX, dimY,dimZ,dimT))
#con <- nc_create("speed.nc",varid)
#ncvar_put(con,varid,speed)
#nc_close(con)

#varid <- ncvar_def(name="vol",units="m/s",dim=list(dimX, dimY,dimZ,dimT))
#con <- nc_create("vol.nc",varid)
#ncvar_put(con,varid,vol)
#nc_close(con)


#vars <- list(varid_umps,varid_vmps,varid_wmps)

#con_vector <- nc_create("vector.nc",vars)

#ncvar_put(con_vector,varid_umps,umps)
#ncvar_put(con_vector,varid_vmps,vmps)
#ncvar_put(con_vector,varid_wmps,wmps)

#nc_close(con_vector)
#
