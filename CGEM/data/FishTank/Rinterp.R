library('zoo')

rdata <- read.table("PAR.withNA.dat",header=TRUE)

time <- rdata[,1]
wind <- rdata[,2]
PAR <- rdata[,3]

PAR_interp <- na.approx(PAR)
Wind_interp <- na.approx(wind)

sink("Wind_PAR.dat")

end <- 288*365

for (i in 1:end){
 cat(time[i],Wind_interp[i],PAR_interp[i],"\n")
}
