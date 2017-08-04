## To use:
## source ("RNH4growth.R");

pdf("NH4growth.pdf");
par(pch=".",cex=3)

par(mfrow=c(3,2));              #page with 4x4 plots

rdata <- read.table("NH4k1growth.txt");
plot(rdata[,1]/288.,(rdata[,2]),xlab="days",ylab="NH4, k1");

plot(rdata[,1]/288.,(rdata[,3]),xlab="days",ylab="-RNH4, k1");

plot(rdata[,1]/288.,(rdata[,4]),xlab="days",ylab="GexN_tot, k1");

plot(rdata[,1]/288.,(rdata[,5]),xlab="days",ylab="GmortN_tot, k1");

plot(rdata[,1]/288.,(rdata[,6]),xlab="days",ylab="GslopN_tot, k1");

dev.off();
