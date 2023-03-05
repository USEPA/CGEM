library(ncdf4)

args = commandArgs(trailingOnly=TRUE)

cat("This only works for cgem or wqem efdc\n")

ncfile <- "cgem.000000.nc"
pdfname2 <- "efdc"
Var <- "Tr"

if(length(args) == 0){
  ncfile <- "cgem.000000.nc"
  pdfname1 <- "cgem"
  Var <- "Tr"
} else if (length(args) >= 1) {
  if (args[1] == "wqem") {
    ncfile<-"wqem.000000.nc"
    pdfname1<-"wqem"
    Var<-"TR"
  } else {
    ncfile <- "cgem.000000.nc"
    pdfname1 <- "cgem"
    Var <- "Tr"
  }
}

pdfname <- paste(pdfname1,pdfname2,"pdiffmass.pdf",sep=".")

nc <- nc_open(ncfile)
# im <- nc$dim$longitude$len
# jm <- nc$dim$latitude$len
km <- nc$dim$k$len

rdata <- ncvar_get(nc,Var)

time <- ncvar_get(nc,"time")
tt <- length(time)

mass <- c(tt)

pdiff_mass <- c(tt-1)

for (t in 1:tt) {
  mass[t] <- 0
  for (k in 1:km) {
    if (rdata[k,t] > 0.) { 
      mass[t] <- mass[t] + rdata[k,t]
    }
  }
}


for(i in 2:tt){
 pdiff_mass[i-1] <- (mass[i]-mass[1])/mass[1] * 100.
}


pdf(file=pdfname)
plot(time[1:(tt-1)],pdiff_mass)

dev.off()

cat("Percent difference of Tracer at last timestep=",pdiff_mass[tt-1],"\n")
