rdata<-read.table("Wind_PAR.dat")

time_orig <- rdata[,1]
Wind <- rdata[,2]
PAR <- rdata[,3]


#data starts at 2006. 2002 is base year.  2004 is leap year
     #4 years + 1 leap day give ts from 2002:
ts <- 4*60*60*24*365 + 60*60*24
 
time <- time_orig - ts
#now time is seconds after 2002.  Make it be days after 2002:
time <- time/60./60./24.

Wind_ave <- c(1:365)
PAR_ave <- c(1:365)
first <- 1
last <- 288

for(i in 1:365){
  Wind_ave[i] <- mean(Wind[first:last])
  PAR_ave[i] <- mean(PAR[first:last])
  first <- first + 288
  last <- last + 288
}

Wind_ave_3 <- c(mean(Wind[1:35040]),mean(Wind[35040:70080]),mean(Wind[1:105120]))
PAR_ave_3 <- c(mean(PAR[1:35040]),mean(PAR[35040:70080]),mean(PAR[70080:105120]))

pdf("CheckAve.pdf");

par(pch=".",cex=3);#,cex.axis=.75);
par(mfrow=c(2,2));              #page with 4x4 plots

plot(time,Wind);
plot(time[1:1152],Wind[1:1152]);
plot(c(1:365),Wind_ave,type='l');
plot(c(1:3),Wind_ave_3,type='l');

par(mfrow=c(2,2));              #page with 4x4 plots
plot(time,PAR);
plot(time[1:1152],PAR[1:1152]);
plot(c(1:365),PAR_ave,type='l');
plot(c(1:3),PAR_ave_3,type='l');

dev.off();

sink("DailyAve.dat")
end <- 288*365
j <- 1
for(i in 1:end) {
   cat(time_orig[i],Wind_ave[j],PAR_ave[j],"\n")
   if(i%%288 == 0) j <- j+1
}
sink()

sink("ThreePointAve.dat")
j<-1
for(i in 1:end) {
   cat(time_orig[i],Wind_ave_3[j],PAR_ave_3[j],"\n")
   if(i == 35040) j <- j+1
   if(i == 70080) j <- j+1
}
sink()

pdf("CheckData.pdf");
par(mfrow=c(2,2));              #page with 4x4 plots
rdata <- read.table("DailyAve.dat")
plot(rdata[,2])
plot(rdata[,3])
rdata <- read.table("ThreePointAve.dat")
plot(rdata[,2])
plot(rdata[,3])
dev.off();



