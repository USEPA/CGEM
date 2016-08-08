## To use:
## source ("R_AllRates.R")

pdf("R_AllRatesk1.pdf");

par(pch=".",cex=3);


par(mfrow=c(4,4));              #page with 4x4 plots

rdata <- read.table("AllRatesk1.txt");
plot(rdata[,1]/288.,(rdata[,2]),xlab="days",ylab="-RO2b_A");

plot(rdata[,1]/288.,(rdata[,3]),xlab="days",ylab="-RO2b_fp");

plot(rdata[,1]/288.,(rdata[,4]),xlab="days",ylab="-RO2b_rp");

plot(rdata[,1]/288.,(rdata[,5]),xlab="days",ylab="RNO3b_A");

plot(rdata[,1]/288.,(rdata[,6]),xlab="days",ylab="RNO3b_fp");

plot(rdata[,1]/288.,(rdata[,7]),xlab="days",ylab="RNO3b_rp");

plot(rdata[,1]/288.,(rdata[,8]),xlab="days",ylab="RNH4b_A");

plot(rdata[,1]/288.,(rdata[,9]),xlab="days",ylab="RNH4b_fp");

plot(rdata[,1]/288.,(rdata[,10]),xlab="days",ylab="RNH4b_rp");

plot(rdata[,1]/288.,(rdata[,11]),xlab="days",ylab="RPO4b_A");

plot(rdata[,1]/288.,(rdata[,12]),xlab="days",ylab="RPO4b_fp");

plot(rdata[,1]/288.,(rdata[,13]),xlab="days",ylab="RPO4b_rp");

plot(rdata[,1]/288.,(rdata[,14]),xlab="days",ylab="RTCb_A");

plot(rdata[,1]/288.,(rdata[,15]),xlab="days",ylab="RTCb_fp");

plot(rdata[,1]/288.,(rdata[,16]),xlab="days",ylab="RTCb_rp");

dev.off();

