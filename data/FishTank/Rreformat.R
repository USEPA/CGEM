is.leapyear=function(year){
  #http://en.wikipedia.org/wiki/Leap_year
  return(((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0))
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

end <- length(UTC)

ts <- 300 
time <- c(1:end)

#Times are relative to year 2002
#and it is in seconds after year 2002:
for(t in 1:end) {
time[t] <- (year[t]-2002)*60*60*24*365

#add seconds from leap years between 2002 and output year:
add_ly <- 0
for(i in 1:(year[t]-2002)){
  if(is.leapyear(2002+i)) add_ly = add_ly +1
}
time[t] <- time[t] + add_ly*60*60*24

#add seconds in hours and minutes:
time[t] <- time[t] + (day[t]-1)*60*60*24 + as.numeric(hour[t])*60*60 + as.numeric(minute[t])*60
}


sink("PAR.withNA.dat")

#start time
begin_ts <- time[1]
#expected time
exp_ts <- time[1]
#expected iterations with 365 days and every 5 minute (288/day)
exp_end <- 365*288 
j<-1
#hours begin at
ihour <- as.numeric(hour[1])

cat("Time","Wind_Speed","PAR","\n")


for(i in 1:exp_end){
  if(time[j] == exp_ts){
   cat(time[j],Wind[j],PAR[j],"\n")
   j <- j+1
}else{
   parval<-NA
   windval<-NA
   if(as.numeric(ihour>=2 && ihour<=9)) parval<-0
   cat(exp_ts,windval,parval,"\n")
   #cat(i,j,exp_ts,time[j],Wind[j],PAR[j],"\n")
   #break
}
 
 exp_ts = exp_ts+300

#keep track of hours
 if(i%%12 == 0) ihour = ihour +1
 if(ihour==24) ihour = 0

}
 
sink()
