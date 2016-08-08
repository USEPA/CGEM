## To use:
## source ("ROM1_A.R");

pdf("OM1_A.pdf");
par(pch=".",cex=3)

par(mfrow=c(2,2));              #page with 4x4 plots

rdata <- read.table("OM1_Ak1growth.txt");

plot(rdata[,1]/288.,(rdata[,2]),xlab="days",ylab="OM1_A, k1");

plot(rdata[,1]/288.,(rdata[,3]),xlab="days",ylab="ROM1b_A, k1");

plot(rdata[,1]/288.,(rdata[,4]),xlab="days",ylab="OM1_AMort_total, k1");

dev.off();
