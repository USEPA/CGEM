## To use:
## source ("RNtot.R");

pdf("Ntot.pdf");
par(pch=".",cex=3)

par(mfrow=c(3,3));              #page with 4x4 plots

rdata <- read.table("Ntotk1growth.txt");
plot(rdata[,1]/288.,(rdata[,2]),xlab="days",ylab="Ntot, k1");

plot(rdata[,1]/288.,(rdata[,3]),xlab="days",ylab="RNO3, k1");

plot(rdata[,1]/288.,(rdata[,4]),xlab="days",ylab="RNH4, k1");

plot(rdata[,1]/288.,(rdata[,5]),xlab="days",ylab="-AupN, k1");

plot(rdata[,1]/288.,(rdata[,6]),xlab="days",ylab="AexudN, k1");

plot(rdata[,1]/288.,(rdata[,7]),xlab="days",ylab="GexN_tot, k1");

plot(rdata[,1]/288.,(rdata[,8]),xlab="days",ylab="GmortN_tot, k1");

plot(rdata[,1]/288.,(rdata[,9]),xlab="days",ylab="GslopN_tot, k1");


dev.off();
