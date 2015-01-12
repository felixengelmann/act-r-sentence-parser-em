###################################################
### chunk number 1: 
###################################################
#cwd <- "/Users/shravanvasishth/Desktop/"
#cwd <- "/Users/bruessow/Desktop/"
cwd <- "/Users/bruessow/NPI/model/latest6/traces/"



###################################################
### chunk number 2: 
###################################################
#setwd(paste(cwd,"/Mar212006/ans030-100runs/",sep=""))
setwd(paste(cwd,"/Mar212006/ans030-100runs/",sep=""))

a <- read.table("a.completed")
b <- read.table("b.completed")
c <- read.table("c.completed")
d <- read.table("d.completed")
e <- read.table("e.completed")
f <- read.table("f.completed")




###################################################
### chunk number 3: 
###################################################

setwd(paste(cwd,"/Mar192006",sep=""))
a <- read.table("a.500")
b <- read.table("b.500")
c <- read.table("c.500")
d <- read.table("d.500")
e <- read.table("e.500")
f <- read.table("f.500")

#if(0){
#ans .15, lf < 0.46
setwd(paste(cwd,"/Mar182006/german-npi-unix/traces/old/",sep=""))

apre <- read.table("a.completed")
bpre <- read.table("b.completed")
cpre <- read.table("c.completed")
dpre <- read.table("d.completed")
epre <- read.table("e.completed")
fpre <- read.table("f.completed")


#ans .15, sven
setwd(paste(cwd,"/Mar202006/ans015",sep=""))
a15 <- read.table("a.completed")
b15 <- read.table("b.completed")
c15 <- read.table("c.completed")
d15 <- read.table("d.completed")
e15 <- read.table("e.completed")
f15 <- read.table("f.completed")





colnames(a) <- c("trial","word","position","condition","RT")
colnames(b) <- c("trial","word","position","condition","RT")
colnames(c) <- c("trial","word","position","condition","RT")
colnames(d) <- c("trial","word","position","condition","RT")
colnames(e) <- c("trial","word","position","condition","RT")
colnames(f) <- c("trial","word","position","condition","RT")

colnames(a15) <- c("trial","word","position","condition","RT")
colnames(b15) <- c("trial","word","position","condition","RT")
colnames(c15) <- c("trial","word","position","condition","RT")
colnames(d15) <- c("trial","word","position","condition","RT")
colnames(e15) <- c("trial","word","position","condition","RT")
colnames(f15) <- c("trial","word","position","condition","RT")

colnames(apre) <- c("trial","word","position","condition","RT")
colnames(bpre) <- c("trial","word","position","condition","RT")
colnames(cpre) <- c("trial","word","position","condition","RT")
colnames(dpre) <- c("trial","word","position","condition","RT")
colnames(epre) <- c("trial","word","position","condition","RT")
colnames(fpre) <- c("trial","word","position","condition","RT")

setwd(paste(cwd,"/",sep=""))


###################################################
### chunk number 4: 
###################################################

data <- rbind(a,b,c,d,e,f)
pos8data <- subset(data,position==8)
data$trial <- as.factor(data$trial)


means <- with(pos8data, tapply(RT,IND=list(condition),mean))
stderr <- with(pos8data,tapply(RT,IND=list(condition),se))
tn <- with(pos8data,tapply(RT,IND=list(condition),length))
tu <- with(pos8data,means + qt(.975,df=tn-1) * stderr)
tl <- with(pos8data,means - qt(.975,df=tn-1) * stderr)




###################################################
### chunk number 5: 
###################################################

means <- means*1000
tu <- tu*1000
tl <- tl*1000



###################################################
### chunk number 6: 
###################################################

data15 <- rbind(a15,b15,c15,d15,e15,f15)
pos8data15 <- subset(data15,position==8)
data15$trial <- as.factor(data15$trial)


means15 <- with(pos8data15, tapply(RT,IND=list(condition),mean))
stderr15 <- with(pos8data15,tapply(RT,IND=list(condition),se))
tn15 <- with(pos8data15,tapply(RT,IND=list(condition),length))
tu15 <- with(pos8data15,means15 + qt(.975,df=tn15-1) * stderr15)
tl15 <- with(pos8data15,means15 - qt(.975,df=tn15-1) * stderr15)




###################################################
### chunk number 7: 
###################################################

means15 <- means15*1000
tu15 <- tu15*1000
tl15 <- tl15*1000



###################################################
### chunk number 8: 
###################################################

datapre <- rbind(apre,bpre,cpre,dpre,epre,fpre)
pos8datapre <- subset(datapre,position==8)
datapre$trial <- as.factor(datapre$trial)


meanspre <- with(pos8datapre, tapply(RT,IND=list(condition),mean))
stderrpre <- with(pos8datapre,tapply(RT,IND=list(condition),se))
tnpre <- with(pos8datapre,tapply(RT,IND=list(condition),length))
tupre <- with(pos8datapre,meanspre + qt(.975,df=tnpre-1) * stderrpre)
tlpre <- with(pos8datapre,meanspre - qt(.975,df=tnpre-1) * stderrpre)




###################################################
### chunk number 9: 
###################################################

meanspre <- meanspre*1000
tupre <- tupre*1000
tlpre <- tlpre*1000



###################################################
### chunk number 10: 
###################################################

cols <-  gray(0:5 / 5)

#createPS("NPIPredictions.ps")

 barplot(t(means),
          beside=TRUE, 
          ylim=range(0,max(means)+100),
          col=cols,
         main="Attachment times (msecs) at the polarity item (model)",
#         axisnames=FALSE,
         xlab="Condition",
         cex.main=1.7,
#         ylab="Attachment time (msecs)",
         names.arg=letters[1:6],
cex.names=3,cex.lab=3,cex.axis=3,cex.main=3)



initialx0 <- 1.5
# for(col in c(1:6)){
   for(row in c(1:6)){
      arrows(initialx0,
          t(tu)[row],
          initialx0,
          t(tl)[row],
          angle=90,
          length=.025,
          code=3)
   initialx0 <- initialx0+2
 }#end-for
#   initialx0 <- initialx0+1
#}#end-for

#dev.off()



###################################################
### chunk number 11: 
###################################################
datameansTRTpos9 <- c(564.4274, 701.4310, 705.9175, 571.8799, 477.3900, 424.8824)
datatuTRTpos9 <- c(616.4060, 783.5852, 781.8033, 632.7439, 521.1400, 460.9614)
datatlTRTpos9 <- c(512.4489, 619.2768, 630.0317, 511.0159, 433.6401, 388.8034)

# parafoveal view position:
datameansTRTpos8 <- c(322.0851, 338.9567, 388.6175, 306.0881, 279.2141, 275.4075) 
datatuTRTpos8 <- c(352.9892, 375.5709, 425.5560, 337.1555, 306.5616, 300.6031) 
datatlTRTpos8 <- c(291.1811, 302.3425, 351.6789, 275.0207, 251.8667, 250.2119) 

if(0){
modeldatameanspos9 <- rbind(means15,means,datameansTRTpos9)
modeldatatupos9 <- rbind(tu15,tu,datatuTRTpos9)
modeldatatlpos9 <- rbind(tl15,tl,datatlTRTpos9)

modeldatameanspos8 <- rbind(means15,means,datameansTRTpos8)
modeldatatupos8 <- rbind(tu15,tu,datatuTRTpos8)
modeldatatlpos8 <- rbind(tl15,tl,datatlTRTpos8)
}

modeldatameanspos9 <- rbind(means,datameansTRTpos9)
modeldatatupos9 <- rbind(tu,datatuTRTpos9)
modeldatatlpos9 <- rbind(tl,datatlTRTpos9)

modeldatameanspos8 <- rbind(means,datameansTRTpos8)
modeldatatupos8 <- rbind(tu,datatuTRTpos8)
modeldatatlpos8 <- rbind(tl,datatlTRTpos8)

#multiplot(2,1)

#createPS("preNPI100runsMar189PM.ps")

cols <-  gray(0:2 / 3)

 barplot(modeldatameanspos8,
          beside=TRUE, 
          ylim=range(0,800),
          col=cols,
         ylab="Processing/Reading Time (msecs)",
         main="Total reading time at region preceding polarity item",
#         axisnames=FALSE,
         names.arg=letters[1:6],
         xlab=c("Conditions"),
         cex.names=1.8,
         cex.main=2,
        cex.lab=1.8)

row <- 3 #data CIs only
if(0){
#initialx0 <- 1.5
initialx0 <- 3.5
 for(col in c(1:6)){  # for each condition
#   for(row in c(1:2)){ # for model then for data
      arrows(initialx0,
          modeldatatupos8[row,col],
          initialx0,
          modeldatatlpos8[row,col],
          angle=90,
          length=.025,
          code=3)
#   initialx0 <- initialx0+1
# }#end-for
   initialx0 <- initialx0+4  #was +1
}#end-for


initialx0 <- 1.5
 for(col in c(1:6)){  # for each condition
   for(row in c(1:2)){ # for model then for data
      arrows(initialx0,
          modeldatatupos8[row,col],
          initialx0,
          modeldatatlpos8[row,col],
          angle=90,
          length=.025,
          code=3)
   initialx0 <- initialx0+1
 }#end-for
   initialx0 <- initialx0+1
}#end-for
}

legend(list(x=24,y=800),
        legend = c("Model 1, noise=.15","Model 2, noise=.35","Data (95% CIs)"),
        fill = cols,
        cex=2,     #magnification of legend
        xjust=1,
        yjust=1,
        merge=TRUE,horiz=FALSE)#, trace=TRUE)


###################################################
### chunk number 12: 
###################################################
#createPS("twomodelsMar2010AM.ps")

cols <-  gray(0:2 / 3)

 barplot(modeldatameanspos9,
          beside=TRUE, 
          ylim=range(0,800),
          col=cols,
         ylab="Processing/Reading Time (msecs)",
         main="Comparison with total reading time at polarity item",
#         axisnames=FALSE,
         names.arg=letters[1:6],
         xlab=c("Conditions"),
         cex.names=1.8,
         cex.main=2,
        cex.lab=1.8)

row <- 3 #data CIs only

if(0){
#initialx0 <- 1.5
initialx0 <- 3.5
 for(col in c(1:6)){  # for each condition
#   for(row in c(1:2)){ # for model then for data
      arrows(initialx0,
          modeldatatupos9[row,col],
          initialx0,
          modeldatatlpos9[row,col],
          angle=90,
          length=.025,
          code=3)
#   initialx0 <- initialx0+1
# }#end-for
   initialx0 <- initialx0+4  #was +1
}#end-for


initialx0 <- 1.5
 for(col in c(1:6)){  # for each condition
   for(row in c(1:2)){ # for model then for data
      arrows(initialx0,
          modeldatatupos8[row,col],
          initialx0,
          modeldatatlpos8[row,col],
          angle=90,
          length=.025,
          code=3)
   initialx0 <- initialx0+1
 }#end-for
   initialx0 <- initialx0+1
}#end-for
}

legend(list(x=24,y=800),
        legend = c("Model 1, noise=.15","Model 2, noise=.35","Data (95% CIs)"),
        fill = cols,
        cex=2,     #magnification of legend
        xjust=1,
        yjust=1,
        merge=TRUE,horiz=FALSE)#, trace=TRUE)


#dev.off()


###################################################
### chunk number 13: 
###################################################

model <- modeldatameanspos8[1,]
data <- modeldatameanspos8[2,]

fm <- lm(data-model~1) # the diff between data and model RTs is some constant, using LSE it's the intercept
beta0 <- coefficients(fm)
fitted <- beta0 + model



###################################################
### chunk number 14: 
###################################################
#createPS("preNPIMar20.ps")
#createPS("preNPIMar21.ps")

cols <-  gray(0:1 / 2)

#modeldatameanspos8 <- rbind(meanspre,datameansTRTpos8)
#modeldatatupos8 <- rbind(tupre,datatuTRTpos8)
#modeldatatlpos8 <- rbind(tlpre,datatlTRTpos8)

 barplot(rbind(fitted,data),#modeldatameanspos8,
          beside=TRUE, 
          ylim=range(0,500),
          col=cols,
         ylab="Processing/Reading Time (msecs)",
         main="Comparison with TRT, region preceding polarity item",
#         axisnames=FALSE,
         names.arg=letters[1:6],
         xlab=c("Conditions"),
         cex.names=2,cex.lab=1.5,cex.main=2)

row <- 2 #data CIs only

#initialx0 <- 1.5
initialx0 <- 2.5
 for(col in c(1:6)){  # for each condition
#   for(row in c(1:2)){ # for model then for data
      arrows(initialx0,
          modeldatatupos8[row,col],
          initialx0,
          modeldatatlpos8[row,col],
          angle=90,
          length=.025,
          code=3)
#   initialx0 <- initialx0+1
# }#end-for
   initialx0 <- initialx0+3  #was +1
}#end-for

legend(list(x=17,y=500),
        legend = c("Model","Data"),
        fill = cols,
        cex=1.8,     #magnification of legend
        xjust=1,
        yjust=1,
        merge=TRUE,horiz=FALSE)#, trace=TRUE)

#dev.off()



###################################################
### chunk number 15: 
###################################################

model <- modeldatameanspos9[1,]
data <- modeldatameanspos9[2,]

fm <- lm(data-model~1) # the diff between data and model RTs is some constant, using LSE it's the intercept
beta0 <- coefficients(fm)
fitted <- beta0 + model



###################################################
### chunk number 16: 
###################################################
#createPS("NPI500runsMar1920060727hrs.ps")
#createPS("NPI100runsMar2120060600hrs.ps")
#createPS("NPIMar212006.ps")

cols <-  gray(0:1 / 2)

 barplot(rbind(fitted,data),#modeldatameanspos9,
          beside=TRUE, 
          ylim=range(0,800),
          col=cols,
         ylab="Processing/Reading Time (msecs)",
         main="Total reading time at polarity item",
#         axisnames=FALSE,
         names.arg=letters[1:6],
         xlab=c("Conditions"),
         cex.names=2,cex.lab=2,cex.main=2)

row <- 2 #data CIs only
#initialx0 <- 1.5
initialx0 <- 2.5
 for(col in c(1:6)){  # for each condition
#   for(row in c(1:2)){ # for model then for data
      arrows(initialx0,
          modeldatatupos9[row,col],
          initialx0,
          modeldatatlpos9[row,col],
          angle=90,
          length=.025,
          code=3)
#   initialx0 <- initialx0+1
# }#end-for
   initialx0 <- initialx0+3  #was +1
}#end-for

legend(list(x=17,y=800),
        legend = c("Model","Data"),
        fill = cols,
        cex=1.3,     #magnification of legend
        xjust=1,
        yjust=1,
        merge=TRUE,horiz=FALSE)#, trace=TRUE)

#dev.off()





###################################################
### chunk number 17: 
###################################################
datameansFPRTpos9 <- c(320.3565, 365.0736, 420.4009, 291.9220, 297.3808, 285.6657) 
datatuFPRTpos9 <- c(345.1104, 402.2283, 465.3879, 315.8310, 320.4303, 305.6427)
datatlFPRTpos9 <- c(295.6025, 327.9188, 375.4139, 268.0131, 274.3312, 265.6886)

modeldatameanspos9 <- rbind(means,datameansFPRTpos9)
modeldatatupos9 <- rbind(tu,datatuFPRTpos9)
modeldatatlpos9 <- rbind(tl,datatlFPRTpos9)


 barplot(modeldatameanspos9,
          beside=TRUE, 
          ylim=range(0,max(tu)*2),
#          col=cols,
         main="First pass reading time at polarity item",
#         axisnames=FALSE,
         names.arg=letters[1:6],
         xlab=c("Conditions"),
         cex.names=1.2,cex.lab=1.2)


initialx0 <- 1.5
 for(col in c(1:6)){  # for each condition
   for(row in c(1:2)){ # for model then for data
      arrows(initialx0,
          modeldatatupos8[row,col],
          initialx0,
          modeldatatlpos8[row,col],
          angle=90,
          length=.025,
          code=3)
   initialx0 <- initialx0+1
 }#end-for
   initialx0 <- initialx0+1
}#end-for



###################################################
### chunk number 18: 
###################################################
#library(car)
library("car", lib.loc="~/Library/R/library/")
#props <- read.table("props2.txt")


#props <- read.table("/Users/shravanvasishth/Desktop/Mar212006/ans030-100runs/ans030props.txt")
props <- read.table("/Users/bruessow/NPI/model/latest6/traces/Mar212006/ans030-100runs/ans030props.txt")
colnames(props) <- c("run","cond","DP")

a.prop <- subset(props,cond=="a")
a.prop.DPs <- recode(a.prop$DP, "'nom'=1;else=0",as.factor.result=FALSE)
a.prop <- data.frame(a.prop,correct=a.prop.DPs)
a.prop.correct <- sum(a.prop$correct)/dim(a.prop)[1] #.96

b.prop <- subset(props,cond=="b")
b.prop.DPs <- recode(b.prop$DP, "'nom'=1;else=0",as.factor.result=FALSE)
b.prop <- data.frame(b.prop,correct=b.prop.DPs)
b.prop.correct <- sum(b.prop$correct)/dim(b.prop)[1] #.61

c.prop <- subset(props,cond=="c")
c.prop.DPs <- recode(c.prop$DP, "'nom'=1;else=0",as.factor.result=FALSE)
c.prop <- data.frame(c.prop,correct=c.prop.DPs)
c.prop.correct <- sum(c.prop$correct)/dim(c.prop)[1] #.86

d.prop <- subset(props,cond=="d")
d.prop.DPs <- recode(d.prop$DP, "'nom'=1;else=0",as.factor.result=FALSE)
d.prop <- data.frame(d.prop,correct=d.prop.DPs)
d.prop.correct <- sum(d.prop$correct)/dim(d.prop)[1] #.62

e.prop <- subset(props,cond=="e")
e.prop.DPs <- recode(e.prop$DP, "'nom'=1;else=0",as.factor.result=FALSE)
e.prop <- data.frame(e.prop,correct=e.prop.DPs)
e.prop.correct <- sum(e.prop$correct)/dim(e.prop)[1] #.96

f.prop <- subset(props,cond=="f")
f.prop.DPs <- recode(f.prop$DP, "'nom'=1;else=0",as.factor.result=FALSE)
f.prop <- data.frame(f.prop,correct=f.prop.DPs)
f.prop.correct <- sum(f.prop$correct)/dim(f.prop)[1] #.85

barplot(c(a.prop.correct,b.prop.correct,c.prop.correct,d.prop.correct,e.prop.correct,f.prop.correct)*100)







       


###################################################
### chunk number 19: 
###################################################

prevdata <- c(85,70,83,NA,NA,NA)
newdata <- c(80.90278, 72.69504, 84.72222, 76.87500, 85.46099, 93.75000) 

prevmodel <- c(96,68,96,NA,NA,NA)
newmodel <- c(a.prop.correct,b.prop.correct,c.prop.correct)*100

#prevdm <- rbind(prevmodel,prevdata)
newdm <- rbind(newmodel,prevdata)

newprevmodeldata <- rbind(prevmodel,prevdata,newmodel,newdata)

cols <-  gray(0:3 / 4)

#createPS("prevdatamodelNPI.ps")
#createPS("newprevmodeldataprops.ps")

 barplot(newprevmodeldata[,1:3],
          beside=TRUE, 
          ylim=range(0,100),
          col=cols,
         ylab="",
         main="% correct responses: expt. and modeling results",
#         axisnames=FALSE,
         names.arg=letters[1:3],
         xlab=c("Conditions"),
         cex.names=3,cex.lab=3,cex.axis=3,cex.main=3)

# mtext("%",line=1,cex=3,
#        side=2,at=105)


legend(list(x=9.5,y=100),
        legend = c("2005 Model","2005 Data","2006 Model","2006 Data"),
        fill = cols,
        cex=2,     #magnification of legend
        xjust=1,
        yjust=1,
        merge=TRUE,horiz=FALSE)#, trace=TRUE)

#dev.off()




###################################################
### chunk number 20: 
###################################################
#createPS("datamodelNPIpropcorrect.ps")
if(0){
cols <-  gray(0:1 / 2)
 barplot(newdm,
          beside=TRUE, 
          ylim=range(0,100),
          col=cols,
         ylab="",
         main="%age correct responses: expt. and modeling results",
#         axisnames=FALSE,
         names.arg=letters[1:3],
         xlab=c("Conditions"),
         cex.names=3,cex.lab=3,cex.axis=3,cex.main=3)

# mtext("%",line=1,cex=3,
#        side=2,at=105)


legend(list(x=5.5,y=100),
        legend = c("Model","Data"),
        fill = cols,
        cex=3,     #magnification of legend
        xjust=1,
        yjust=1,
        merge=TRUE,horiz=FALSE)#, trace=TRUE)
}
#dev.off()




