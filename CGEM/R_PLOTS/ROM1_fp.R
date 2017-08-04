## To use:
## source ("ROM1_fp.R");

pdf("OM1_fp.pdf");
par(pch=".",cex=3)

par(mfrow=c(2,2));              #page with 4x4 plots

rdata <- read.table("OM1_fpk1growth.txt");

plot(rdata[,1]/288.,(rdata[,2]),xlab="days",ylab="OM1_fp, k1");

plot(rdata[,1]/288.,(rdata[,3]),xlab="days",ylab="ROM1b_fp, k1");

plot(rdata[,1]/288.,(rdata[,4]),xlab="days",ylab="OM1_Cfp, k1");

dev.off();
