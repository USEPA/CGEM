## To use:
## source ("RPO4growth.R");

pdf("PO4growth.pdf");

par(pch=".",cex=3)

par(mfrow=c(3,3));              #page with 4x4 plots

rdata <- read.table("PO4k1growth.txt");
plot(rdata[,1]/288.,(rdata[,2]),xlab="days",ylab="PO4, k1");

plot(rdata[,1]/288.,(rdata[,3]),xlab="days",ylab="RPO4, k1");

plot(rdata[,1]/288.,(rdata[,4]),xlab="days",ylab="-AupP, k1");

plot(rdata[,1]/288.,(rdata[,5]),xlab="days",ylab="AexudP, k1");

plot(rdata[,1]/288.,(rdata[,6]),xlab="days",ylab="GexP_tot, k1");

plot(rdata[,1]/288.,(rdata[,7]),xlab="days",ylab="GmortP_tot, k1");

plot(rdata[,1]/288.,(rdata[,8]),xlab="days",ylab="GslopP_tot, k1");

dev.off();
