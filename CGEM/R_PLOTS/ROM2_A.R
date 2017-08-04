## To use:
## source ("ROM2_A.R");

pdf("OM2_A.pdf");
par(pch=".",cex=3)

par(mfrow=c(2,2));              #page with 4x4 plots

rdata <- read.table("OM2_Ak1growth.txt");

plot(rdata[,1]/288.,(rdata[,2]),xlab="days",ylab="OM2_A, k1");

plot(rdata[,1]/288.,(rdata[,3]),xlab="days",ylab="ROM2b_A, k1");

plot(rdata[,1]/288.,(rdata[,4]),xlab="days",ylab="OM2_AMort_total, k1");

dev.off();
