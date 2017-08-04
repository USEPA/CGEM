## To use:
## source ("RO2growth.R");

pdf("O2growth.pdf");
par(pch=".",cex=3)

par(mfrow=c(2,3));              #page with 4x4 plots

rdata <- read.table("O2k1growth.txt");

plot(rdata[,1]/288.,(rdata[,2]),xlab="days",ylab="O2, k1");

plot(rdata[,1]/288.,(rdata[,3]),xlab="days",ylab="photosyn, k1");

plot(rdata[,1]/288.,(rdata[,4]),xlab="days",ylab="-ArespTotC, k1");

plot(rdata[,1]/288.,(rdata[,5]),xlab="days",ylab="-decay, k1");

plot(rdata[,1]/288.,(rdata[,6]),xlab="days",ylab="-respG_CarcellG k1");

dev.off();
