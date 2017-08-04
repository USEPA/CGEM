## To use:
## source ("ROM2_rp.R");

pdf("OM2_rp.pdf");

par(pch=".",cex=3)

par(mfrow=c(1,2));              #page with 4x4 plots

rdata <- read.table("OM2_rpk1growth.txt");

plot(rdata[,1]/288.,(rdata[,2]),xlab="days",ylab="OM2_rp, k1");

plot(rdata[,1]/288.,(rdata[,3]),xlab="days",ylab="ROM2b_rp, k1");

dev.off();
