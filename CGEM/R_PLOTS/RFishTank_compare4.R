## To use:
## source ("RFishTank_short_compare2.R")

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




pdf("FishTank_compare4.pdf");

par(pch=".",cex=3,oma=c(0,0,3,0));#,cex.axis=.75);


par(mfrow=c(4,4));              #page with 4x4 plots

rdata  <- read.table("./Dir1/A1.txt");
rdata2 <- read.table("./Dir2/A1.txt");
rdata3 <- read.table("./Dir3/A1.txt");
rdata4 <- read.table("./Dir4/A1.txt");

plot(rdata[,1]/288.,log10(rdata[,2]),yaxt="n",xlab="days",ylab="",main="Log10 A1, k1",ylim=log_ylim_k(rdata[,2],rdata2[,2]));
lines(rdata2[,1]/576.,log10(rdata2[,2]),col="red");
lines(rdata3[,1]/1152.,log10(rdata3[,2]),col="green");
lines(rdata4[,1]/2304.,log10(rdata4[,2]),col="blue");
axis(2, at=log_get_lab(rdata[,2],rdata2[,2]), labels=log_get_lab(rdata[,2],rdata2[,2]), las=2);

#-------------
rdata  <- read.table("./Dir1/Qn1.txt");
rdata2 <- read.table("./Dir2/Qn1.txt");
rdata3 <- read.table("./Dir3/Qn1.txt");
rdata4 <- read.table("./Dir4/Qn1.txt");

plot(rdata[,1]/288.,(rdata[,2]),yaxt="n",xlab="days",ylab="",main="Qn1, k1",ylim=ylim_k(rdata[,2],rdata2[,2]));
lines(rdata2[,1]/576.,rdata2[,2],col="red");
lines(rdata3[,1]/1152.,rdata3[,2],col="green");
lines(rdata4[,1]/2304.,rdata4[,2],col="blue");
axis(2,at=get_lab(rdata[,2],rdata2[,2]), labels=get_lab(rdata[,2],rdata2[,2]), las=2);

#---------------------------------------------
rdata  <- read.table("./Dir1/Qp1.txt");
rdata2 <- read.table("./Dir2/Qp1.txt");
rdata3 <- read.table("./Dir3/Qp1.txt");
rdata4 <- read.table("./Dir4/Qp1.txt");

plot(rdata[,1]/288.,(rdata[,2]),yaxt="n",xlab="days",ylab="",main="Qp1, k1",ylim=ylim_k(rdata[,2],rdata2[,2]));
lines(rdata2[,1]/576.,rdata2[,2],col="red");
lines(rdata3[,1]/1152.,rdata3[,2],col="green");
lines(rdata4[,1]/2304.,rdata4[,2],col="blue");
axis(2,at=get_lab(rdata[,2],rdata2[,2]), labels=get_lab(rdata[,2],rdata2[,2]), las=2);

#----------------------------------------
rdata  <- read.table("./Dir1/G1.txt");
rdata2 <- read.table("./Dir2/G1.txt");
rdata3 <- read.table("./Dir3/G1.txt");
rdata4 <- read.table("./Dir4/G1.txt");

plot(rdata[,1]/288.,log10(rdata[,2]),yaxt="n",xlab="days",ylab="",main="Log10 G1, k1",ylim=log_ylim_k(rdata[,2],rdata2[,2]));
lines(rdata2[,1]/576.,log10(rdata2[,2]),col="red");
lines(rdata3[,1]/1152.,log10(rdata3[,2]),col="green");
lines(rdata4[,1]/2304.,log10(rdata4[,2]),col="blue");
axis(2,at=log_get_lab(rdata[,2],rdata2[,2]), labels=log_get_lab(rdata[,2],rdata2[,2]), las=2);

#---------------------------------------
rdata  <- read.table("./Dir1/G2.txt");
rdata2 <- read.table("./Dir2/G2.txt");
rdata3 <- read.table("./Dir3/G2.txt");
rdata4 <- read.table("./Dir4/G2.txt");

plot(rdata[,1]/288.,log10(rdata[,2]),yaxt="n",xlab="days",ylab="",main="Log10 G2, k1",ylim=log_ylim_k(rdata[,2],rdata2[,2]));
lines(rdata2[,1]/576.,log10(rdata2[,2]),col="red");
lines(rdata3[,1]/1152.,log10(rdata3[,2]),col="green");
lines(rdata4[,1]/2304.,log10(rdata4[,2]),col="blue");
axis(2,at=log_get_lab(rdata[,2],rdata2[,2]), labels=log_get_lab(rdata[,2],rdata2[,2]), las=2);

#-----------------------------------
rdata  <- read.table("./Dir1/NO3.txt");
rdata2 <- read.table("./Dir2/NO3.txt");
rdata3 <- read.table("./Dir3/NO3.txt");
rdata4 <- read.table("./Dir4/NO3.txt");

plot(rdata[,1]/288.,rdata[,2],yaxt="n",xlab="days",ylab="",main="NO3, k1",ylim=ylim_k(rdata[,2],rdata2[,2]));
lines(rdata2[,1]/576.,rdata2[,2],col="red");
lines(rdata3[,1]/1152.,rdata3[,2],col="green");
lines(rdata4[,1]/2304.,rdata4[,2],col="blue");
axis(2,at=get_lab(rdata[,2],rdata2[,2]), labels=get_lab(rdata[,2],rdata2[,2]), las=2);

#-------------------------------
rdata  <- read.table("./Dir1/NH4.txt");
rdata2 <- read.table("./Dir2/NH4.txt");
rdata3 <- read.table("./Dir3/NH4.txt");
rdata4 <- read.table("./Dir4/NH4.txt");

plot(rdata[,1]/288.,rdata[,2],yaxt="n",xlab="days",ylab="",main="NH4, k1",ylim=ylim_k(rdata[,2],rdata2[,2]));
lines(rdata2[,1]/576.,rdata2[,2],col="red");
lines(rdata3[,1]/1152.,rdata3[,2],col="green");
lines(rdata4[,1]/2304.,rdata4[,2],col="blue");
axis(2,at=get_lab(rdata[,2],rdata2[,2]), labels=get_lab(rdata[,2],rdata2[,2]), las=2);

#------------------------------------------
rdata  <- read.table("./Dir1/PO4.txt");
rdata2 <- read.table("./Dir2/PO4.txt");
rdata3 <- read.table("./Dir3/PO4.txt");
rdata4 <- read.table("./Dir4/PO4.txt");

plot(rdata[,1]/288.,rdata[,2],yaxt="n",xlab="days",ylab="",main="PO4, k1",ylim=ylim_k(rdata[,2],rdata2[,2]));
lines(rdata2[,1]/576.,rdata2[,2],col="red");
lines(rdata3[,1]/1152.,rdata3[,2],col="green");
lines(rdata4[,1]/2304.,rdata4[,2],col="blue");
axis(2,at=get_lab(rdata[,2],rdata2[,2]), labels=get_lab(rdata[,2],rdata2[,2]), las=2);

#----------------------------
rdata  <- read.table("./Dir1/DIC.txt");
rdata2 <- read.table("./Dir2/DIC.txt");
rdata3 <- read.table("./Dir3/DIC.txt");
rdata4 <- read.table("./Dir4/DIC.txt");

plot(rdata[,1]/288.,rdata[,2],yaxt="n",xlab="days",ylab="",main="DIC, k1",ylim=ylim_k(rdata[,2],rdata2[,2]));
lines(rdata2[,1]/576.,rdata2[,2],col="red");
lines(rdata3[,1]/1152.,rdata3[,2],col="green");
lines(rdata4[,1]/2304.,rdata4[,2],col="blue");
axis(2,at=get_lab_4(rdata[,2],rdata2[,2]), labels=get_lab_4(rdata[,2],rdata2[,2]), las=2);

#--------------------------------
rdata  <- read.table("./Dir1/O2.txt");
rdata2 <- read.table("./Dir2/O2.txt");
rdata3 <- read.table("./Dir3/O2.txt");
rdata4 <- read.table("./Dir4/O2.txt");

plot(rdata[,1]/288.,rdata[,2],yaxt="n",xlab="days",ylab="",main="O2, k1",ylim=ylim_k(rdata[,2],rdata2[,2]));
lines(rdata2[,1]/576.,rdata2[,2],col="red");
lines(rdata3[,1]/1152.,rdata3[,2],col="green");
lines(rdata4[,1]/2304.,rdata4[,2],col="blue");
axis(2,at=get_lab(rdata[,2],rdata2[,2]), labels=get_lab(rdata[,2],rdata2[,2]), las=2);

#--------------------
rdata  <- read.table("./Dir1/OM1_A.txt");
rdata2 <- read.table("./Dir2/OM1_A.txt");
rdata3 <- read.table("./Dir3/OM1_A.txt");
rdata4 <- read.table("./Dir4/OM1_A.txt");

plot(rdata[,1]/288.,rdata[,2],yaxt="n",xlab="days",ylab="",main="OM1_A, k1",ylim=ylim_k(rdata[,2],rdata2[,2]));
lines(rdata2[,1]/576.,rdata2[,2],col="red");
lines(rdata3[,1]/1152.,rdata3[,2],col="green");
lines(rdata4[,1]/2304.,rdata4[,2],col="blue");
axis(2,at=get_lab(rdata[,2],rdata2[,2]), labels=get_lab(rdata[,2],rdata2[,2]), las=2);

#------------------------
rdata  <- read.table("./Dir1/OM2_A.txt");
rdata2 <- read.table("./Dir2/OM2_A.txt");
rdata3 <- read.table("./Dir3/OM2_A.txt");
rdata4 <- read.table("./Dir4/OM2_A.txt");

plot(rdata[,1]/288.,rdata[,2],yaxt="n",xlab="days",ylab="",main="OM2_A, k1",ylim=ylim_k(rdata[,2],rdata2[,2]));
lines(rdata2[,1]/576.,rdata2[,2],col="red");
lines(rdata3[,1]/1152.,rdata3[,2],col="green");
lines(rdata4[,1]/2304.,rdata4[,2],col="blue");
axis(2,at=get_lab(rdata[,2],rdata2[,2]), labels=get_lab(rdata[,2],rdata2[,2]), las=2);

#--------------------------
rdata  <- read.table("./Dir1/OM1_fp.txt");
rdata2 <- read.table("./Dir2/OM1_fp.txt");
rdata3 <- read.table("./Dir3/OM1_fp.txt");
rdata4 <- read.table("./Dir4/OM1_fp.txt");

plot(rdata[,1]/288.,rdata[,2],yaxt="n",xlab="days",ylab="",main="OM1_fp, k1",ylim=ylim_k(rdata[,2],rdata2[,2]));
lines(rdata2[,1]/576.,rdata2[,2],col="red");
lines(rdata3[,1]/1152.,rdata3[,2],col="green");
lines(rdata4[,1]/2304.,rdata4[,2],col="blue");
axis(2,at=get_lab(rdata[,2],rdata2[,2]), labels=get_lab(rdata[,2],rdata2[,2]), las=2);

#-----------------------------------
rdata  <- read.table("./Dir1/OM2_fp.txt");
rdata2 <- read.table("./Dir2/OM2_fp.txt");
rdata3 <- read.table("./Dir3/OM2_fp.txt");
rdata4 <- read.table("./Dir4/OM2_fp.txt");

plot(rdata[,1]/288.,rdata[,2],yaxt="n",xlab="days",ylab="",main="OM2_fp, k1",ylim=ylim_k(rdata[,2],rdata2[,2]));
lines(rdata2[,1]/576.,rdata2[,2],col="red");
lines(rdata3[,1]/1152.,rdata3[,2],col="green");
lines(rdata4[,1]/2304.,rdata4[,2],col="blue");
axis(2,at=get_lab(rdata[,2],rdata2[,2]), labels=get_lab(rdata[,2],rdata2[,2]), las=2);

#----------------------------------
rdata  <- read.table("./Dir1/CDOM.txt");
rdata2 <- read.table("./Dir2/CDOM.txt");
rdata3 <- read.table("./Dir3/CDOM.txt");
rdata4 <- read.table("./Dir4/CDOM.txt");

plot(rdata[,1]/288.,rdata[,2],yaxt="n",xlab="days",ylab="",main="CDOM, k1",ylim=ylim_k(rdata[,2],rdata2[,2]));
lines(rdata2[,1]/576.,rdata2[,2],col="red");
lines(rdata3[,1]/1152.,rdata3[,2],col="green");
lines(rdata4[,1]/2304.,rdata4[,2],col="blue");
axis(2,at=get_lab(rdata[,2],rdata2[,2]), labels=get_lab(rdata[,2],rdata2[,2]), las=2);

#----------------------------------
rdata  <- read.table("./Dir1/Si.txt");
rdata2 <- read.table("./Dir2/Si.txt");
rdata3 <- read.table("./Dir3/Si.txt");
rdata4 <- read.table("./Dir4/Si.txt");

plot(rdata[,1]/288.,rdata[,2],yaxt="n",xlab="days",ylab="",main="Si, k1",ylim=ylim_k(rdata[,2],rdata2[,2]));
lines(rdata2[,1]/576.,rdata2[,2],col="red");
lines(rdata3[,1]/1152.,rdata3[,2],col="green");
lines(rdata4[,1]/2304.,rdata4[,2],col="blue");
axis(2,at=get_lab(rdata[,2],rdata2[,2]), labels=get_lab(rdata[,2],rdata2[,2]), las=2);

label <- scan("label.dat",what=character());
mtext(label,outer=TRUE);

dev.off();
