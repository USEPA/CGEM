rdata <- read.table("PAR.copy.dat")
UTC <- rdata[,4]
PAR <- rdata[,11]
day <- rdata[,3]

end <- length(UTC)

for(i in 2:end-1){
 if(UTC[i+1]-UTC[i] != 5){
  if(UTC[i+1]-UTC[i] != 45){
   if(UTC[i+1]-UTC[i] != -2355){
 cat(i,UTC[i+1]-UTC[i],day[i+1]-day[i],"\n")
}}}
}
