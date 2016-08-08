## To use:
## source ("RAgrowth.R");

pdf("A1growth.pdf");

par(pch=".",cex=3)


par(mfrow=c(2,3));              #page with 4x4 plots

rdata <- read.table("A1k1growth.txt");
plot(rdata[,1]/288.,log10(rdata[,2]),xlab="days",ylab="log10 A1, k1",xlim=c(100,365),ylim=c(-0.99,1.1));

plot(rdata[,1]/288.,log10(rdata[,3]),xlab="days",ylab="log10 A1 growth, k1");

plot(rdata[,1]/288.,log10(-rdata[,4]),xlab="days",ylab="log10 A1 respiration, k1");

plot(rdata[,1]/288.,log10(-rdata[,5]),xlab="days",ylab="log10 A1 grazing, k1");

plot(rdata[,1]/288.,log10(-rdata[,6]),xlab="days",ylab="log10 A1 mortality, k1",ylim=c(0,1));

dev.off();
