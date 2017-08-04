## To use:
## source ("R1DGulfBreeze_compare.R")

log_ylim_k <- function(indata,indata2) {
 ymin <- log10(min( min(indata), min(indata) ));
 ymax <- log10(max( max(indata2), max(indata2) ));
 ymid <- ymin + (ymax-ymin)/2.
 c(ymin-ymax*.001,ymax+ymax*.001)
}

log_get_lab <- function(indata,indata2){
 ymin <- log10(min( min(indata), min(indata2) ));
 ymax <- log10(max( max(indata), max(indata2) ));
 ymid <- ymin + (ymax-ymin)/2.
 ymin <- signif(ymin,digits=3)
 ymax <- signif(ymax,digits=3)
 ymid <- signif(ymid,digits=3)
 c(ymin,ymid,ymax)
}

ylim_k <- function(indata,indata2) {
 ymin <- (min( min(indata), min(indata2) ));
 ymax <- (max( max(indata), max(indata2) ));
 ymid <- ymin + (ymax-ymin)/2.
 c(ymin-ymax*.001,ymax+ymax*.001)
}

get_lab <- function(indata,indata2){
 ymin <- (min( min(indata), min(indata2) ));
 ymax <- (max( max(indata), max(indata2) ));
 ymid <- ymin + (ymax-ymin)/2.
 ymin <- signif(ymin,digits=3)
 ymax <- signif(ymax,digits=3)
 ymid <- signif(ymid,digits=3)
 c(ymin,ymid,ymax)
}

get_lab_4 <- function(indata,indata2){
 ymin <- (min( min(indata), min(indata2) ));
 ymax <- (max( max(indata), max(indata2) ));
 ymid <- ymin + (ymax-ymin)/2.
 ymin <- signif(ymin,digits=4)
 ymax <- signif(ymax,digits=4)
 ymid <- signif(ymid,digits=4)
 c(ymin,ymid,ymax)
}




pdf("1DGulfBreeze_compare.pdf");

par(pch=".",cex=3);#,cex.axis=.75);


par(mfrow=c(4,4));              #page with 4x4 plots

rdata  <- read.table("./Dir1/A1.txt");
rdata2 <- read.table("./Dir2/A1.txt");

plot(rdata[,1]/288.,log10(rdata[,2]),yaxt="n",xlab="days",ylab="",main="Log10 A1, k1",ylim=log_ylim_k(rdata[,2],rdata2[,2]));
lines(rdata2[,1]/288.,log10(rdata2[,2]),col="red");
axis(2, at=log_get_lab(rdata[,2],rdata2[,2]), labels=log_get_lab(rdata[,2],rdata2[,2]), las=2);

plot(rdata[,1]/288.,log10(rdata[,3]),yaxt="n",xlab="days",ylab="",main="Log10 A1, k6",ylim=log_ylim_k(rdata[,3],rdata2[,3]));
lines(rdata2[,1]/288.,log10(rdata2[,3]),col="red");
axis(2, at=log_get_lab(rdata[,3],rdata2[,3]), labels=log_get_lab(rdata[,3],rdata2[,3]), las=2);

plot(rdata[,1]/288.,log10(rdata[,4]),yaxt="n",xlab="days",ylab="",main="Log10 A1, k12",ylim=log_ylim_k(rdata[,4],rdata2[,4]));
lines(rdata2[,1]/288.,log10(rdata2[,4]),col="red");
axis(2, at=log_get_lab(rdata[,4],rdata2[,4]), labels=log_get_lab(rdata[,4],rdata2[,4]), las=2);

plot(rdata[,1]/288.,log10(rdata[,5]),yaxt="n",xlab="days",ylab="",main="Log10 A1, k20",ylim=log_ylim_k(rdata[,5],rdata2[,5]));
lines(rdata2[,1]/288.,log10(rdata2[,5]),col="red");
axis(2, at=log_get_lab(rdata[,5],rdata2[,5]), labels=log_get_lab(rdata[,5],rdata2[,5]), las=2);

#-------------
rdata  <- read.table("./Dir1/Qn1.txt");
rdata2 <- read.table("./Dir2/Qn1.txt");

plot(rdata[,1]/288.,(rdata[,2]),yaxt="n",xlab="days",ylab="",main="Qn1, k1",ylim=ylim_k(rdata[,2],rdata2[,2]));
lines(rdata2[,1]/288.,rdata2[,2],col="red");
axis(2,at=get_lab(rdata[,2],rdata2[,2]), labels=get_lab(rdata[,2],rdata2[,2]), las=2);

plot(rdata[,1]/288.,(rdata[,3]),yaxt="n",xlab="days",ylab="",main="Qn1, k6",ylim=ylim_k(rdata[,3],rdata2[,3]));
lines(rdata2[,1]/288.,rdata2[,3],col="red");
axis(2, at=get_lab(rdata[,3],rdata2[,3]), labels=get_lab(rdata[,3],rdata2[,3]), las=2);

plot(rdata[,1]/288.,(rdata[,4]),yaxt="n",xlab="days",ylab="",main="Qn1, k12",ylim=ylim_k(rdata[,4],rdata2[,4]));
lines(rdata2[,1]/288.,rdata2[,4],col="red");
axis(2, at=get_lab(rdata[,4],rdata2[,4]), labels=get_lab(rdata[,4],rdata2[,4]), las=2);

plot(rdata[,1]/288.,(rdata[,5]),yaxt="n",xlab="days",ylab="",main="Qn1, k20",ylim=ylim_k(rdata[,5],rdata2[,5]));
lines(rdata2[,1]/288.,rdata2[,5],col="red");
axis(2, at=get_lab(rdata[,5],rdata2[,5]), labels=get_lab(rdata[,5],rdata2[,5]), las=2);

#---------------------------------------------
rdata  <- read.table("./Dir1/Qp1.txt");
rdata2 <- read.table("./Dir2/Qp1.txt");

plot(rdata[,1]/288.,(rdata[,2]),yaxt="n",xlab="days",ylab="",main="Qp1, k1",ylim=ylim_k(rdata[,2],rdata2[,2]));
lines(rdata2[,1]/288.,rdata2[,2],col="red");
axis(2,at=get_lab(rdata[,2],rdata2[,2]), labels=get_lab(rdata[,2],rdata2[,2]), las=2);

plot(rdata[,1]/288.,(rdata[,3]),yaxt="n",xlab="days",ylab="",main="Qp1, k6",ylim=ylim_k(rdata[,3],rdata2[,3]));
lines(rdata2[,1]/288.,rdata2[,3],col="red");
axis(2, at=get_lab(rdata[,3],rdata2[,3]), labels=get_lab(rdata[,3],rdata2[,3]), las=2);

plot(rdata[,1]/288.,(rdata[,4]),yaxt="n",xlab="days",ylab="",main="Qp1, k12",ylim=ylim_k(rdata[,4],rdata2[,4]));
lines(rdata2[,1]/288.,rdata2[,4],col="red");
axis(2, at=get_lab(rdata[,4],rdata2[,4]), labels=get_lab(rdata[,4],rdata2[,4]), las=2);

plot(rdata[,1]/288.,(rdata[,5]),yaxt="n",xlab="days",ylab="",main="Qp1, k20",ylim=ylim_k(rdata[,5],rdata2[,5]));
lines(rdata2[,1]/288.,rdata2[,5],col="red");
axis(2, at=get_lab(rdata[,5],rdata2[,5]), labels=get_lab(rdata[,5],rdata2[,5]), las=2);

#----------------------------------------
rdata  <- read.table("./Dir1/G1.txt");
rdata2 <- read.table("./Dir2/G1.txt");

plot(rdata[,1]/288.,log10(rdata[,2]),yaxt="n",xlab="days",ylab="",main="Log10 G1, k1",ylim=log_ylim_k(rdata[,2],rdata2[,2]));
lines(rdata2[,1]/288.,log10(rdata2[,2]),col="red");
axis(2,at=log_get_lab(rdata[,2],rdata2[,2]), labels=log_get_lab(rdata[,2],rdata2[,2]), las=2);

plot(rdata[,1]/288.,log10(rdata[,3]),yaxt="n",xlab="days",ylab="",main="Log10 G1, k6",ylim=log_ylim_k(rdata[,3],rdata2[,3]));
lines(rdata2[,1]/288.,log10(rdata2[,3]),col="red");
axis(2,at=log_get_lab(rdata[,3],rdata2[,3]), labels=log_get_lab(rdata[,3],rdata2[,3]), las=2);

plot(rdata[,1]/288.,log10(rdata[,4]),yaxt="n",xlab="days",ylab="",main="Log10 G1, k12",ylim=log_ylim_k(rdata[,4],rdata2[,4]));
lines(rdata2[,1]/288.,log10(rdata2[,4]),col="red");
axis(2, at=log_get_lab(rdata[,4],rdata2[,4]), labels=log_get_lab(rdata[,4],rdata2[,4]), las=2);

plot(rdata[,1]/288.,log10(rdata[,5]),yaxt="n",xlab="days",ylab="",main="Log10 G1, k20",ylim=log_ylim_k(rdata[,5],rdata2[,5]));
lines(rdata2[,1]/288.,log10(rdata2[,5]),col="red");
axis(2, at=log_get_lab(rdata[,5],rdata2[,5]), labels=log_get_lab(rdata[,5],rdata2[,5]), las=2);

#---------------------------------------
par(mfrow=c(4,4));              #page with 4x4 plots

rdata  <- read.table("./Dir1/G2.txt");
rdata2 <- read.table("./Dir2/G2.txt");

plot(rdata[,1]/288.,log10(rdata[,2]),yaxt="n",xlab="days",ylab="",main="Log10 G2, k1",ylim=log_ylim_k(rdata[,2],rdata2[,2]));
lines(rdata2[,1]/288.,log10(rdata2[,2]),col="red");
axis(2,at=log_get_lab(rdata[,2],rdata2[,2]), labels=log_get_lab(rdata[,2],rdata2[,2]), las=2);

plot(rdata[,1]/288.,log10(rdata[,3]),yaxt="n",xlab="days",ylab="",main="Log10 G2, k6",ylim=log_ylim_k(rdata[,3],rdata2[,3]));
lines(rdata2[,1]/288.,log10(rdata2[,3]),col="red");
axis(2, at=log_get_lab(rdata[,3],rdata2[,3]), labels=log_get_lab(rdata[,3],rdata2[,3]), las=2);

plot(rdata[,1]/288.,log10(rdata[,4]),yaxt="n",xlab="days",ylab="",main="Log10 G2, k12",ylim=log_ylim_k(rdata[,4],rdata2[,4]));
lines(rdata2[,1]/288.,log10(rdata2[,4]),col="red");
axis(2, at=log_get_lab(rdata[,4],rdata2[,4]), labels=log_get_lab(rdata[,4],rdata2[,4]), las=2);

plot(rdata[,1]/288.,log10(rdata[,5]),yaxt="n",xlab="days",ylab="",main="Log10 G2, k20",ylim=log_ylim_k(rdata[,5],rdata2[,5]));
lines(rdata2[,1]/288.,log10(rdata2[,5]),col="red");
axis(2, at=log_get_lab(rdata[,5],rdata2[,5]), labels=log_get_lab(rdata[,5],rdata2[,5]), las=2);

#-----------------------------------
rdata  <- read.table("./Dir1/NO3.txt");
rdata2 <- read.table("./Dir2/NO3.txt");

plot(rdata[,1]/288.,rdata[,2],yaxt="n",xlab="days",ylab="",main="NO3, k1",ylim=ylim_k(rdata[,2],rdata2[,2]));
lines(rdata2[,1]/288.,rdata2[,2],col="red");
axis(2,at=get_lab(rdata[,2],rdata2[,2]), labels=get_lab(rdata[,2],rdata2[,2]), las=2);

plot(rdata[,1]/288.,rdata[,3],yaxt="n",xlab="days",ylab="",main="NO3, k6",ylim=ylim_k(rdata[,3],rdata2[,3]));
lines(rdata2[,1]/288.,rdata2[,3],col="red");
axis(2, at=get_lab(rdata[,3],rdata2[,3]), labels=get_lab(rdata[,3],rdata2[,3]), las=2);

plot(rdata[,1]/288.,rdata[,4],yaxt="n",xlab="days",ylab="",main="NO3, k12",ylim=ylim_k(rdata[,4],rdata2[,4]));
lines(rdata2[,1]/288.,rdata2[,4],col="red");
axis(2, at=get_lab(rdata[,4],rdata2[,4]), labels=get_lab(rdata[,4],rdata2[,4]), las=2);

plot(rdata[,1]/288.,rdata[,5],yaxt="n",xlab="days",ylab="",main="NO3, k20",ylim=ylim_k(rdata[,5],rdata2[,5]));
lines(rdata2[,1]/288.,rdata2[,5],col="red");
axis(2, at=get_lab(rdata[,5],rdata2[,5]), labels=get_lab(rdata[,5],rdata2[,5]), las=2);

#-------------------------------
rdata  <- read.table("./Dir1/NH4.txt");
rdata2 <- read.table("./Dir2/NH4.txt");

plot(rdata[,1]/288.,rdata[,2],yaxt="n",xlab="days",ylab="",main="NH4, k1",ylim=ylim_k(rdata[,2],rdata2[,2]));
lines(rdata2[,1]/288.,rdata2[,2],col="red");
axis(2,at=get_lab(rdata[,2],rdata2[,2]), labels=get_lab(rdata[,2],rdata2[,2]), las=2);

plot(rdata[,1]/288.,rdata[,3],yaxt="n",xlab="days",ylab="",main="NH4, k6",ylim=ylim_k(rdata[,3],rdata2[,3]));
lines(rdata2[,1]/288.,rdata2[,3],col="red");
axis(2, at=get_lab(rdata[,3],rdata2[,3]), labels=get_lab(rdata[,3],rdata2[,3]), las=2);

plot(rdata[,1]/288.,rdata[,4],yaxt="n",xlab="days",ylab="",main="NH4, k12",ylim=ylim_k(rdata[,4],rdata2[,4]));
lines(rdata2[,1]/288.,rdata2[,4],col="red");
axis(2, at=get_lab(rdata[,4],rdata2[,4]), labels=get_lab(rdata[,4],rdata2[,4]), las=2);

plot(rdata[,1]/288.,rdata[,5],yaxt="n",xlab="days",ylab="",main="NH4, k20",ylim=ylim_k(rdata[,5],rdata2[,5]));
lines(rdata2[,1]/288.,rdata2[,5],col="red");
axis(2, at=get_lab(rdata[,5],rdata2[,5]), labels=get_lab(rdata[,5],rdata2[,5]), las=2);

#------------------------------------------
rdata  <- read.table("./Dir1/PO4.txt");
rdata2 <- read.table("./Dir2/PO4.txt");

plot(rdata[,1]/288.,rdata[,2],yaxt="n",xlab="days",ylab="",main="PO4, k1",ylim=ylim_k(rdata[,2],rdata2[,2]));
lines(rdata2[,1]/288.,rdata2[,2],col="red");
axis(2,at=get_lab(rdata[,2],rdata2[,2]), labels=get_lab(rdata[,2],rdata2[,2]), las=2);

plot(rdata[,1]/288.,rdata[,3],yaxt="n",xlab="days",ylab="",main="PO4, k6",ylim=ylim_k(rdata[,3],rdata2[,3]));
lines(rdata2[,1]/288.,rdata2[,3],col="red");
axis(2, at=get_lab(rdata[,3],rdata2[,3]), labels=get_lab(rdata[,3],rdata2[,3]), las=2);

plot(rdata[,1]/288.,rdata[,4],yaxt="n",xlab="days",ylab="",main="PO4, k12",ylim=ylim_k(rdata[,4],rdata2[,4]));
lines(rdata2[,1]/288.,rdata2[,4],col="red");
axis(2, at=get_lab(rdata[,4],rdata2[,4]), labels=get_lab(rdata[,4],rdata2[,4]), las=2);

plot(rdata[,1]/288.,rdata[,5],yaxt="n",xlab="days",ylab="",main="PO4, k20",ylim=ylim_k(rdata[,5],rdata2[,5]));
lines(rdata2[,1]/288.,rdata2[,5],col="red");
axis(2, at=get_lab(rdata[,5],rdata2[,5]), labels=get_lab(rdata[,5],rdata2[,5]), las=2);

#----------------------------
par(mfrow=c(4,4));              #page with 4x4 plots

rdata  <- read.table("./Dir1/DIC.txt");
rdata2 <- read.table("./Dir2/DIC.txt");

plot(rdata[,1]/288.,rdata[,2],yaxt="n",xlab="days",ylab="",main="DIC, k1",ylim=ylim_k(rdata[,2],rdata2[,2]));
lines(rdata2[,1]/288.,rdata2[,2],col="red");
axis(2,at=get_lab_4(rdata[,2],rdata2[,2]), labels=get_lab_4(rdata[,2],rdata2[,2]), las=2);

plot(rdata[,1]/288.,rdata[,3],yaxt="n",xlab="days",ylab="",main="DIC, k6",ylim=ylim_k(rdata[,3],rdata2[,3]));
lines(rdata2[,1]/288.,rdata2[,3],col="red");
axis(2, at=get_lab_4(rdata[,3],rdata2[,3]), labels=get_lab_4(rdata[,3],rdata2[,3]), las=2);

plot(rdata[,1]/288.,rdata[,4],yaxt="n",xlab="days",ylab="",main="DIC, k12",ylim=ylim_k(rdata[,4],rdata2[,4]));
lines(rdata2[,1]/288.,rdata2[,4],col="red");
axis(2, at=get_lab_4(rdata[,4],rdata2[,4]), labels=get_lab_4(rdata[,4],rdata2[,4]), las=2);

plot(rdata[,1]/288.,rdata[,5],yaxt="n",xlab="days",ylab="",main="DIC, k20",ylim=ylim_k(rdata[,5],rdata2[,5]));
lines(rdata2[,1]/288.,rdata2[,5],col="red");
axis(2, at=get_lab_4(rdata[,5],rdata2[,5]), labels=get_lab_4(rdata[,5],rdata2[,5]), las=2);

#--------------------------------
rdata  <- read.table("./Dir1/O2.txt");
rdata2 <- read.table("./Dir2/O2.txt");

plot(rdata[,1]/288.,rdata[,2],yaxt="n",xlab="days",ylab="",main="O2, k1",ylim=ylim_k(rdata[,2],rdata2[,2]));
lines(rdata2[,1]/288.,rdata2[,2],col="red");
axis(2,at=get_lab(rdata[,2],rdata2[,2]), labels=get_lab(rdata[,2],rdata2[,2]), las=2);

plot(rdata[,1]/288.,rdata[,3],yaxt="n",xlab="days",ylab="",main="O2, k6",ylim=ylim_k(rdata[,3],rdata2[,3]));
lines(rdata2[,1]/288.,rdata2[,3],col="red");
axis(2, at=get_lab(rdata[,3],rdata2[,3]), labels=get_lab(rdata[,3],rdata2[,3]), las=2);

plot(rdata[,1]/288.,rdata[,4],yaxt="n",xlab="days",ylab="",main="O2, k12",ylim=ylim_k(rdata[,4],rdata2[,4]));
lines(rdata2[,1]/288.,rdata2[,4],col="red");
axis(2, at=get_lab(rdata[,4],rdata2[,4]), labels=get_lab(rdata[,4],rdata2[,4]), las=2);

plot(rdata[,1]/288.,rdata[,5],yaxt="n",xlab="days",ylab="",main="O2, k20",ylim=ylim_k(rdata[,5],rdata2[,5]));
lines(rdata2[,1]/288.,rdata2[,5],col="red");
axis(2, at=get_lab(rdata[,5],rdata2[,5]), labels=get_lab(rdata[,5],rdata2[,5]), las=2);

#--------------------
rdata  <- read.table("./Dir1/OM1_A.txt");
rdata2 <- read.table("./Dir2/OM1_A.txt");

plot(rdata[,1]/288.,rdata[,2],yaxt="n",xlab="days",ylab="",main="OM1_A, k1",ylim=ylim_k(rdata[,2],rdata2[,2]));
lines(rdata2[,1]/288.,rdata2[,2],col="red");
axis(2,at=get_lab(rdata[,2],rdata2[,2]), labels=get_lab(rdata[,2],rdata2[,2]), las=2);

plot(rdata[,1]/288.,rdata[,3],yaxt="n",xlab="days",ylab="",main="OM1_A, k6",ylim=ylim_k(rdata[,3],rdata2[,3]));
lines(rdata2[,1]/288.,rdata2[,3],col="red");
axis(2, at=get_lab(rdata[,3],rdata2[,3]), labels=get_lab(rdata[,3],rdata2[,3]), las=2);

plot(rdata[,1]/288.,rdata[,4],yaxt="n",xlab="days",ylab="",main="OM1_A, k12",ylim=ylim_k(rdata[,4],rdata2[,4]));
lines(rdata2[,1]/288.,rdata2[,4],col="red");
axis(2, at=get_lab(rdata[,4],rdata2[,4]), labels=get_lab(rdata[,4],rdata2[,4]), las=2);

plot(rdata[,1]/288.,rdata[,5],yaxt="n",xlab="days",ylab="",main="OM1_A, k20",ylim=ylim_k(rdata[,5],rdata2[,5]));
lines(rdata2[,1]/288.,rdata2[,5],col="red");
axis(2, at=get_lab(rdata[,5],rdata2[,5]), labels=get_lab(rdata[,5],rdata2[,5]), las=2);

#------------------------
rdata  <- read.table("./Dir1/OM2_A.txt");
rdata2 <- read.table("./Dir2/OM2_A.txt");

plot(rdata[,1]/288.,rdata[,2],yaxt="n",xlab="days",ylab="",main="OM2_A, k1",ylim=ylim_k(rdata[,2],rdata2[,2]));
lines(rdata2[,1]/288.,rdata2[,2],col="red");
axis(2,at=get_lab(rdata[,2],rdata2[,2]), labels=get_lab(rdata[,2],rdata2[,2]), las=2);

plot(rdata[,1]/288.,rdata[,3],yaxt="n",xlab="days",ylab="",main="OM2_A, k6",ylim=ylim_k(rdata[,3],rdata2[,3]));
lines(rdata2[,1]/288.,rdata2[,3],col="red");
axis(2, at=get_lab(rdata[,3],rdata2[,3]), labels=get_lab(rdata[,3],rdata2[,3]), las=2);

plot(rdata[,1]/288.,rdata[,4],yaxt="n",xlab="days",ylab="",main="OM2_A, k12",ylim=ylim_k(rdata[,4],rdata2[,4]));
lines(rdata2[,1]/288.,rdata2[,4],col="red");
axis(2, at=get_lab(rdata[,4],rdata2[,4]), labels=get_lab(rdata[,4],rdata2[,4]), las=2);

plot(rdata[,1]/288.,rdata[,5],yaxt="n",xlab="days",ylab="",main="OM2_A, k20",ylim=ylim_k(rdata[,5],rdata2[,5]));
lines(rdata2[,1]/288.,rdata2[,5],col="red");
axis(2, at=get_lab(rdata[,5],rdata2[,5]), labels=get_lab(rdata[,5],rdata2[,5]), las=2);

#--------------------------
par(mfrow=c(4,4));              #page with 4x4 plots

rdata  <- read.table("./Dir1/OM1_fp.txt");
rdata2 <- read.table("./Dir2/OM1_fp.txt");

plot(rdata[,1]/288.,rdata[,2],yaxt="n",xlab="days",ylab="",main="OM1_fp, k1",ylim=ylim_k(rdata[,2],rdata2[,2]));
lines(rdata2[,1]/288.,rdata2[,2],col="red");
axis(2,at=get_lab(rdata[,2],rdata2[,2]), labels=get_lab(rdata[,2],rdata2[,2]), las=2);

plot(rdata[,1]/288.,rdata[,3],yaxt="n",xlab="days",ylab="",main="OM1_fp, k6",ylim=ylim_k(rdata[,3],rdata2[,3]));
lines(rdata2[,1]/288.,rdata2[,3],col="red");
axis(2, at=get_lab(rdata[,3],rdata2[,3]), labels=get_lab(rdata[,3],rdata2[,3]), las=2);

plot(rdata[,1]/288.,rdata[,4],yaxt="n",xlab="days",ylab="",main="OM1_fp, k12",ylim=ylim_k(rdata[,4],rdata2[,4]));
lines(rdata2[,1]/288.,rdata2[,4],col="red");
axis(2, at=get_lab(rdata[,4],rdata2[,4]), labels=get_lab(rdata[,4],rdata2[,4]), las=2);

plot(rdata[,1]/288.,rdata[,5],yaxt="n",xlab="days",ylab="",main="OM1_fp, k20",ylim=ylim_k(rdata[,5],rdata2[,5]));
lines(rdata2[,1]/288.,rdata2[,5],col="red");
axis(2, at=get_lab(rdata[,5],rdata2[,5]), labels=get_lab(rdata[,5],rdata2[,5]), las=2);

#-----------------------------------
rdata  <- read.table("./Dir1/OM2_fp.txt");
rdata2 <- read.table("./Dir2/OM2_fp.txt");

plot(rdata[,1]/288.,rdata[,2],yaxt="n",xlab="days",ylab="",main="OM2_fp, k1",ylim=ylim_k(rdata[,2],rdata2[,2]));
lines(rdata2[,1]/288.,rdata2[,2],col="red");
axis(2,at=get_lab(rdata[,2],rdata2[,2]), labels=get_lab(rdata[,2],rdata2[,2]), las=2);

plot(rdata[,1]/288.,rdata[,3],yaxt="n",xlab="days",ylab="",main="OM2_fp, k6",ylim=ylim_k(rdata[,3],rdata2[,3]));
lines(rdata2[,1]/288.,rdata2[,3],col="red");
axis(2, at=get_lab(rdata[,3],rdata2[,3]), labels=get_lab(rdata[,3],rdata2[,3]), las=2);

plot(rdata[,1]/288.,rdata[,4],yaxt="n",xlab="days",ylab="",main="OM2_fp, k12",ylim=ylim_k(rdata[,4],rdata2[,4]));
lines(rdata2[,1]/288.,rdata2[,4],col="red");
axis(2, at=get_lab(rdata[,4],rdata2[,4]), labels=get_lab(rdata[,4],rdata2[,4]), las=2);

plot(rdata[,1]/288.,rdata[,5],yaxt="n",xlab="days",ylab="",main="OM2_fp, k20",ylim=ylim_k(rdata[,5],rdata2[,5]));
lines(rdata2[,1]/288.,rdata2[,5],col="red");
axis(2, at=get_lab(rdata[,5],rdata2[,5]), labels=get_lab(rdata[,5],rdata2[,5]), las=2);

#--------------------------
rdata  <- read.table("./Dir1/OM1_rp.txt");
rdata2 <- read.table("./Dir2/OM1_rp.txt");

plot(rdata[,1]/288.,rdata[,2],yaxt="n",xlab="days",ylab="",main="OM1_rp, k1",ylim=ylim_k(rdata[,2],rdata2[,2]));
lines(rdata2[,1]/288.,rdata2[,2],col="red");
axis(2,at=get_lab(rdata[,2],rdata2[,2]), labels=get_lab(rdata[,2],rdata2[,2]), las=2);

plot(rdata[,1]/288.,rdata[,3],yaxt="n",xlab="days",ylab="",main="OM1_rp, k6",ylim=ylim_k(rdata[,3],rdata2[,3]));
lines(rdata2[,1]/288.,rdata2[,3],col="red");
axis(2, at=get_lab(rdata[,3],rdata2[,3]), labels=get_lab(rdata[,3],rdata2[,3]), las=2);

plot(rdata[,1]/288.,rdata[,4],yaxt="n",xlab="days",ylab="",main="OM1_rp, k12",ylim=ylim_k(rdata[,4],rdata2[,4]));
lines(rdata2[,1]/288.,rdata2[,4],col="red");
axis(2, at=get_lab(rdata[,4],rdata2[,4]), labels=get_lab(rdata[,4],rdata2[,4]), las=2);

plot(rdata[,1]/288.,rdata[,5],yaxt="n",xlab="days",ylab="",main="OM1_rp, k20",ylim=ylim_k(rdata[,5],rdata2[,5]));
lines(rdata2[,1]/288.,rdata2[,5],col="red");
axis(2, at=get_lab(rdata[,5],rdata2[,5]), labels=get_lab(rdata[,5],rdata2[,5]), las=2);

#----------------------------------
rdata  <- read.table("./Dir1/OM2_rp.txt");
rdata2 <- read.table("./Dir2/OM2_rp.txt");

plot(rdata[,1]/288.,rdata[,2],yaxt="n",xlab="days",ylab="",main="OM2_rp, k1",ylim=ylim_k(rdata[,2],rdata2[,2]));
lines(rdata2[,1]/288.,rdata2[,2],col="red");
axis(2,at=get_lab(rdata[,2],rdata2[,2]), labels=get_lab(rdata[,2],rdata2[,2]), las=2);

plot(rdata[,1]/288.,rdata[,3],yaxt="n",xlab="days",ylab="",main="OM2_rp, k6",ylim=ylim_k(rdata[,3],rdata2[,3]));
lines(rdata2[,1]/288.,rdata2[,3],col="red");
axis(2, at=get_lab(rdata[,3],rdata2[,3]), labels=get_lab(rdata[,3],rdata2[,3]), las=2);

plot(rdata[,1]/288.,rdata[,4],yaxt="n",xlab="days",ylab="",main="OM2_rp, k12",ylim=ylim_k(rdata[,4],rdata2[,4]));
lines(rdata2[,1]/288.,rdata2[,4],col="red");
axis(2, at=get_lab(rdata[,4],rdata2[,4]), labels=get_lab(rdata[,4],rdata2[,4]), las=2);

plot(rdata[,1]/288.,rdata[,5],yaxt="n",xlab="days",ylab="",main="OM2_rp, k20",ylim=ylim_k(rdata[,5],rdata2[,5]));
lines(rdata2[,1]/288.,rdata2[,5],col="red");
axis(2, at=get_lab(rdata[,5],rdata2[,5]), labels=get_lab(rdata[,5],rdata2[,5]), las=2);

#----------------------------------
par(mfrow=c(4,4));              #page with 4x4 plots

rdata  <- read.table("./Dir1/CDOM.txt");
rdata2 <- read.table("./Dir2/CDOM.txt");

plot(rdata[,1]/288.,rdata[,2],yaxt="n",xlab="days",ylab="",main="CDOM, k1",ylim=ylim_k(rdata[,2],rdata2[,2]));
lines(rdata2[,1]/288.,rdata2[,2],col="red");
axis(2,at=get_lab(rdata[,2],rdata2[,2]), labels=get_lab(rdata[,2],rdata2[,2]), las=2);

plot(rdata[,1]/288.,rdata[,3],yaxt="n",xlab="days",ylab="",main="CDOM, k6",ylim=ylim_k(rdata[,3],rdata2[,3]));
lines(rdata2[,1]/288.,rdata2[,3],col="red");
axis(2, at=get_lab(rdata[,3],rdata2[,3]), labels=get_lab(rdata[,3],rdata2[,3]), las=2);

plot(rdata[,1]/288.,rdata[,4],yaxt="n",xlab="days",ylab="",main="CDOM, k12",ylim=ylim_k(rdata[,4],rdata2[,4]));
lines(rdata2[,1]/288.,rdata2[,4],col="red");
axis(2, at=get_lab(rdata[,4],rdata2[,4]), labels=get_lab(rdata[,4],rdata2[,4]), las=2);

plot(rdata[,1]/288.,rdata[,5],yaxt="n",xlab="days",ylab="",main="CDOM, k20",ylim=ylim_k(rdata[,5],rdata2[,5]));
lines(rdata2[,1]/288.,rdata2[,5],col="red");
axis(2, at=get_lab(rdata[,5],rdata2[,5]), labels=get_lab(rdata[,5],rdata2[,5]), las=2);

#----------------------------------
rdata  <- read.table("./Dir1/Si.txt");
rdata2 <- read.table("./Dir2/Si.txt");

plot(rdata[,1]/288.,rdata[,2],yaxt="n",xlab="days",ylab="",main="Si, k1",ylim=ylim_k(rdata[,2],rdata2[,2]));
lines(rdata2[,1]/288.,rdata2[,2],col="red");
axis(2,at=get_lab(rdata[,2],rdata2[,2]), labels=get_lab(rdata[,2],rdata2[,2]), las=2);

plot(rdata[,1]/288.,rdata[,3],yaxt="n",xlab="days",ylab="",main="Si, k6",ylim=ylim_k(rdata[,3],rdata2[,3]));
lines(rdata2[,1]/288.,rdata2[,3],col="red");
axis(2, at=get_lab(rdata[,3],rdata2[,3]), labels=get_lab(rdata[,3],rdata2[,3]), las=2);

plot(rdata[,1]/288.,rdata[,4],yaxt="n",xlab="days",ylab="",main="Si, k12",ylim=ylim_k(rdata[,4],rdata2[,4]));
lines(rdata2[,1]/288.,rdata2[,4],col="red");
axis(2, at=get_lab(rdata[,4],rdata2[,4]), labels=get_lab(rdata[,4],rdata2[,4]), las=2);

plot(rdata[,1]/288.,rdata[,5],yaxt="n",xlab="days",ylab="",main="Si, k20",ylim=ylim_k(rdata[,5],rdata2[,5]));
lines(rdata2[,1]/288.,rdata2[,5],col="red");
axis(2, at=get_lab(rdata[,5],rdata2[,5]), labels=get_lab(rdata[,5],rdata2[,5]), las=2);

#----------------------------------
rdata  <- read.table("./Dir1/OM1_bc.txt");
rdata2 <- read.table("./Dir2/OM1_bc.txt");

plot(rdata[,1]/288.,rdata[,2],yaxt="n",xlab="days",ylab="",main="OM1_bc, k1",ylim=ylim_k(rdata[,2],rdata2[,2]));
lines(rdata2[,1]/288.,rdata2[,2],col="red");
axis(2,at=get_lab(rdata[,2],rdata2[,2]), labels=get_lab(rdata[,2],rdata2[,2]), las=2);

plot(rdata[,1]/288.,rdata[,3],yaxt="n",xlab="days",ylab="",main="OM1_bc, k6",ylim=ylim_k(rdata[,3],rdata2[,3]));
lines(rdata2[,1]/288.,rdata2[,3],col="red");
axis(2, at=get_lab(rdata[,3],rdata2[,3]), labels=get_lab(rdata[,3],rdata2[,3]), las=2);

plot(rdata[,1]/288.,rdata[,4],yaxt="n",xlab="days",ylab="",main="OM1_bc, k12",ylim=ylim_k(rdata[,4],rdata2[,4]));
lines(rdata2[,1]/288.,rdata2[,4],col="red");
axis(2, at=get_lab(rdata[,4],rdata2[,4]), labels=get_lab(rdata[,4],rdata2[,4]), las=2);

plot(rdata[,1]/288.,rdata[,5],yaxt="n",xlab="days",ylab="",main="OM1_bc, k20",ylim=ylim_k(rdata[,5],rdata2[,5]));
lines(rdata2[,1]/288.,rdata2[,5],col="red");
axis(2, at=get_lab(rdata[,5],rdata2[,5]), labels=get_lab(rdata[,5],rdata2[,5]), las=2);

#-----------------------------------
rdata  <- read.table("./Dir1/OM2_bc.txt");
rdata2 <- read.table("./Dir2/OM2_bc.txt");

plot(rdata[,1]/288.,rdata[,2],yaxt="n",xlab="days",ylab="",main="OM2_bc, k1",ylim=ylim_k(rdata[,2],rdata2[,2]));
lines(rdata2[,1]/288.,rdata2[,2],col="red");
axis(2,at=get_lab(rdata[,2],rdata2[,2]), labels=get_lab(rdata[,2],rdata2[,2]), las=2);

plot(rdata[,1]/288.,rdata[,3],yaxt="n",xlab="days",ylab="",main="OM2_bc, k6",ylim=ylim_k(rdata[,3],rdata2[,3]));
lines(rdata2[,1]/288.,rdata2[,3],col="red");
axis(2, at=get_lab(rdata[,3],rdata2[,3]), labels=get_lab(rdata[,3],rdata2[,3]), las=2);

plot(rdata[,1]/288.,rdata[,4],yaxt="n",xlab="days",ylab="",main="OM2_bc, k12",ylim=ylim_k(rdata[,4],rdata2[,4]));
lines(rdata2[,1]/288.,rdata2[,4],col="red");
axis(2, at=get_lab(rdata[,4],rdata2[,4]), labels=get_lab(rdata[,4],rdata2[,4]), las=2);

plot(rdata[,1]/288.,rdata[,5],yaxt="n",xlab="days",ylab="",main="OM2_bc, k20",ylim=ylim_k(rdata[,5],rdata2[,5]));
lines(rdata2[,1]/288.,rdata2[,5],col="red");
axis(2, at=get_lab(rdata[,5],rdata2[,5]), labels=get_lab(rdata[,5],rdata2[,5]), las=2);



dev.off();
