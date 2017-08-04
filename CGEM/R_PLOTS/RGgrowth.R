## To use:
## source ("RGgrowth.R");

pdf("G1growth.pdf");
par(pch=".",cex=3)

par(mfrow=c(4,3));              #page with 4x4 plots

rdata <- read.table("G1k1growth.txt");
plot(rdata[,1]/288.,(rdata[,2]),xlab="days",ylab="G1, k1");

plot(rdata[,1]/288.,(rdata[,3]),xlab="days",ylab="G1 growth, k1");

plot(rdata[,1]/288.,(rdata[,4]),xlab="days",ylab="G1 -respiration, k1");

plot(rdata[,1]/288.,(rdata[,5]),xlab="days",ylab="G1 -mortality, k1");

dev.off();
