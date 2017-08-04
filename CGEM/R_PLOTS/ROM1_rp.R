## To use:
## source ("ROM1_rp.R");

pdf("OM1_rp.pdf");
par(pch=".",cex=3)

par(mfrow=c(1,2));              #page with 4x4 plots

rdata <- read.table("OM1_rpk1growth.txt");

plot(rdata[,1]/288.,(rdata[,2]),xlab="days",ylab="OM1_rp, k1");

plot(rdata[,1]/288.,(rdata[,3]),xlab="days",ylab="ROM1b_rp, k1");

dev.off();
