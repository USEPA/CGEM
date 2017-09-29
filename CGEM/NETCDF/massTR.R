library(ncdf4)

args = commandArgs(trailingOnly=TRUE)

if(length(args)==0){       #Default cgem

ncfile<-"cgem.000000.nc"
pdfname1<-"cgem"
pdfname2<-"efdc"
Var<-"Tr"
im <- 20
jm <- 33
km <- 7
weqns <- "cgem"
wgrid <- "efdc"

} 

if (length(args)>=1){
if(args[1]=="gomdom"){
ncfile<-"gomdom.000000.nc"
pdfname1<-"gomdom"
pdfname2<-"efdc"
Var<-"TR"
im <- 20
jm <- 33
km <- 7
weqns <- "gomdom"
wgrid <- "efdc"
}
else{
ncfile<-"cgem.000000.nc"
pdfname1<-"cgem"
pdfname2<-"efdc"
Var<-"Tr"
im <- 20
jm <- 33
km <- 7
weqns <- "cgem"
wgrid <- "efdc"
}
}

if (length(args)>=2)
{
if(args[2]=="efdc")
{
im <- 20
jm <- 33
km <- 7
wgrid <- "efdc"
pdfname2 <- "efdc"
}else if(args[2]=="efdc1d")
{
im <- 1 
jm <- 1 
km <- 7
wgrid <- "efdc1d"
pdfname2<-"efdc1d"
}else if(args[2]=="ncom1d"){
im <- 1
jm <-1
km <- 20 
wgrid <- "ncom1d"
pdfname2 <- "ncom1d"
}else{
im <- 1
jm <- 1
km <- 1
wgrid <- "cgem0D"
pdfname2 <- "cgem0D"
}
}

pdfname <- paste(pdfname1,pdfname2,"pdiffmass.pdf",sep=".")

cat("Assuming eqns=",weqns,",grid=",wgrid,".\n")

nc<-nc_open(ncfile)

rdata <- ncvar_get(nc,Var)

time <- ncvar_get(nc,"time")
tt <- length(time)

mass<-c(tt)

pdiff_mass<-c(tt-1)

for(t in 1:tt){
 mass[t]<-0

if(wgrid=="efdc"){
 for(i in 1:im){
 for(j in 1:jm){
 for(k in 1:km){
 if(rdata[i,j,k,t]>0. ){ 
  mass[t] <- mass[t] + rdata[i,j,k,t]
}}}}}else if(wgrid=="cgem0D"){
}
 if(rdata[t]>0. ){
  mass[t] <- mass[t] + rdata[t]
}
else{
}
 for(k in 1:km){
 if(rdata[k,t]>0. ){
  mass[t] <- mass[t] + rdata[k,t]
}}
}


for(i in 2:tt){
 pdiff_mass[i-1] <- (mass[i]-mass[1])/mass[1] * 100.
}


pdf(file=pdfname)
plot(time[1:(tt-1)],pdiff_mass)

dev.off()

cat("Percent difference of Tracer at last timestep=",pdiff_mass[tt-1],"\n")
