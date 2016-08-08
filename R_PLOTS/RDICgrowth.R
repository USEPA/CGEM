## To use:
## source ("RDICgrowth.R");

pdf("DICgrowth.pdf");

par(pch=".",cex=3)

par(mfrow=c(2,2));              #page with 4x4 plots

rdata <- read.table("DICk1growth.txt");

plot(rdata[,1]/288.,(rdata[,2]),xlab="days",ylab="DIC, k1");

plot(rdata[,1]/288.,(rdata[,3]),xlab="days",ylab="RTC, k1");

plot(rdata[,1]/288.,(rdata[,4]),xlab="days",ylab="-SUM(Agrow*Carcell), k1");

plot(rdata[,1]/288.,(rdata[,5]),xlab="days",ylab="ArespTotC, k1");

plot(rdata[,1]/288.,(rdata[,6]),xlab="days",ylab="respG_CarcellG k1");

dev.off();
