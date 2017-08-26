is.leapyear=function(year){
  #http://en.wikipedia.org/wiki/Leap_year
  return(((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0))
}

which.month=function(day,ly=FALSE){
  days_per_month <- c( 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 )
  if(ly) days_per_month <- c( 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 )

  i <- 1
  while( day>sum(days_per_month[1:i]) ){
   i<-i+1
  }

  return(i)
}

which.day=function(day,month,ly=FALSE){
  days_per_month <- c( 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 )
  if(ly) days_per_month <- c( 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 )

  days_left <- day

  if(month!=1){ 
    days_left <- day - sum(days_per_month[1:(month-1)])
  } 

  return(days_left)
}


rdata <- read.table("PAR.copy.dat")
year <- rdata[,2]
day <- rdata[,3]
UTC <- rdata[,4]
Wind <- rdata[,6]
PAR <- rdata[,11]
UTC <- sprintf("%04d",UTC)
hour <- substr(UTC,1,2)
minute <- substr(UTC,3,4)
hour <- as.numeric(hour)
minute <- as.numeric(minute)


end <- length(UTC)


sink("ReformatWindPar.dat")

for(i in 1:end){
   Month <- which.month(day[i],is.leapyear(year[i]))
   Day   <- which.day(day[i],Month,is.leapyear(year[i])) 
   cat(year[i],Month,Day,hour[i],minute[i],Wind[i],PAR[i],"\n")
}
 
sink()
