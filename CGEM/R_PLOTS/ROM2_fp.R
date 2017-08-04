## To use:
## source ("ROM2_fp.R");

pdf("OM2_fp.pdf");
par(pch=".",cex=3)

par(mfrow=c(2,2));              #page with 4x4 plots

rdata <- read.table("OM2_fpk1growth.txt");

plot(rdata[,1]/288.,(rdata[,2]),xlab="days",ylab="OM2_fp, k1");

plot(rdata[,1]/288.,(rdata[,3]),xlab="days",ylab="ROM2b_fp, k1");

plot(rdata[,1]/288.,(rdata[,4]),xlab="days",ylab="OM2_Cfp, k1");

dev.off();
