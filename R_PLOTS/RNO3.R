## To use:
## source ("RNO3.R");

pdf("NO3.pdf");
par(pch=".",cex=3)

par(mfrow=c(1,2));              #page with 4x4 plots

rdata <- read.table("NO3k1growth.txt");
plot(rdata[,1]/288.,(rdata[,2]),xlab="days",ylab="NO3, k1");

plot(rdata[,1]/288.,(rdata[,3]),xlab="days",ylab="RNO3, k1");

dev.off();
