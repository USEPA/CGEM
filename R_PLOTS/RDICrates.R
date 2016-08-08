## To use:
## source ("RDICrates.R");

pdf("DICrates.pdf");
par(pch=".",cex=3)

par(mfrow=c(2,2));              #page with 4x4 plots

rdata <- read.table("DICratek1.txt");
plot(rdata[,1]/288.,(rdata[,2]),xlab="days",ylab="RTC, k1");

plot(rdata[,1]/288.,(rdata[,3]),xlab="days",ylab="RTCb_A, k1");

plot(rdata[,1]/288.,(rdata[,4]),xlab="days",ylab="RTCb_fp, k1");

plot(rdata[,1]/288.,(rdata[,5]),xlab="days",ylab="RTCb_rp, k1");

dev.off();
