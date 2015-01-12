###################################################
### chunk number 1: 
###################################################

#a <- read.table("a.txt")
#b <- read.table("b.txt")
#c <- read.table("c.txt")
#d <- read.table("d.txt")
#e <- read.table("e.txt")
#f <- read.table("f.txt")
a <- read.table("a.completed")
b <- read.table("b.completed")
c <- read.table("c.completed")
d <- read.table("d.completed")
e <- read.table("e.completed")
f <- read.table("f.completed")


colnames(a) <- c("trial","word","position","condition","RT")
colnames(b) <- c("trial","word","position","condition","RT")
colnames(c) <- c("trial","word","position","condition","RT")
colnames(d) <- c("trial","word","position","condition","RT")
colnames(e) <- c("trial","word","position","condition","RT")
colnames(f) <- c("trial","word","position","condition","RT")

data <- rbind(a,b,c,d,e,f)

dim(a)
dim(b)
dim(c)
dim(d)
dim(e)
dim(f)

###################################################
### chunk number 2: 
###################################################

pos9data <- subset(data,position==9)

data.abc <- rbind(a,b,c)

data$trial <- as.factor(data$trial)

means <- with(pos9data, tapply(RT,IND=list(condition),mean))
stderr <- with(pos9data,tapply(RT,IND=list(condition),se))
tn <- with(pos9data,tapply(RT,IND=list(condition),length))
tu <- with(pos9data,means + qt(.975,df=tn-1) * stderr)
tl <- with(pos9data,means - qt(.975,df=tn-1) * stderr)


###################################################
### chunk number 3: 
###################################################

means <- means*1000
tu <- tu*1000
tl <- tl*1000

#hack for now
#tu[3] <- tu[2]
#tl[3] <- tl[2]
#means[3] <- means[2]



###################################################
### chunk number 4: 
###################################################

cols <-  gray(0:5 / 5)

# barplot(t(means),
#          beside=TRUE, 
#          ylim=range(0,600),
#          col=cols,
#         main="Total reading time model",
#         axisnames=FALSE,
##         names.arg=paste(c("pos"),rep(1:10,1),sep=""),
#         cex.names=1.2,cex.lab=1.2)
#
#
#initialx0 <- 1.5
## for(col in c(1:6)){
#   for(row in c(1:6)){
#      arrows(initialx0,
#          t(tu)[row],
#          initialx0,
#          t(tl)[row],
#          angle=90,
#          length=.025,
#          code=3)
#   initialx0 <- initialx0+2
# }#end-for
#   initialx0 <- initialx0+1
#}#end-for



###################################################
### chunk number 5: 
###################################################
datameans <- c(564.4274, 701.4310, 705.9175, 571.8799, 477.3900, 424.8824)
datatu <- c(616.4060, 783.5852, 781.8033, 632.7439, 521.1400, 460.9614)
datatl <- c(512.4489, 619.2768, 630.0317, 511.0159, 433.6401, 388.8034)

modeldatameans <- rbind(means,datameans)
modeldatatu <- rbind(tu,datatu)
modeldatatl <- rbind(tl,datatl)

 barplot(modeldatameans,
          beside=TRUE, 
          ylim=range(0,800),
#          col=cols,
         main="TRT at PI: lf .26, noise .05, 800 runs",
#         axisnames=FALSE,
         names.arg=letters[1:6],
         xlab=c("Conditions"),
         cex.names=1.2,cex.lab=1.2)


initialx0 <- 1.5
 for(col in c(1:6)){  # for each condition
   #for(row in c(1:2)){ # for model then for data
   for(row in c(2:2)){ # for model then for data
      arrows(
          #initialx0,
          initialx0+1,
          modeldatatu[row,col],
          #initialx0,
          initialx0+1,
          modeldatatl[row,col],
          angle=90+1,
          length=.025,
          code=3)
   #initialx0 <- initialx0+1
   initialx0 <- initialx0+2
 }#end-for
   initialx0 <- initialx0+1
}#end-for


initialx0 <- 1.5
 for(col in c(1:6)){  # for each condition
   for(row in c(1:2)){ # for model then for data
   #for(row in c(2:2)){ # for model then for data
      arrows(
          initialx0,
          #initialx0+1,
          modeldatatu[row,col],
          initialx0,
          #initialx0+1,
          modeldatatl[row,col],
          angle=90+1,
          length=.025,
          code=3)
   initialx0 <- initialx0+1
   #initialx0 <- initialx0+2
 }#end-for
   initialx0 <- initialx0+1
}#end-for




 legend(18,750,
               legend=c("model data", "human data"),
               fill=c(1,0),
               xjust=1)


###########
F=0.26
noise=0.05
MD=-0.6
MP=1
CC="on"
runs=800
emma="off"

values <- data.frame(F,noise,MD,MP,CC,runs,rbind(means),emma,row.names=NULL)
write.table(values,"../v6pred.data",sep="\t",row.names=FALSE,append=TRUE,col.names=FALSE)
#v6pred <- exp1

###################
t <- read.table("../v6pred.data",header=T)
t$noise <- as.numeric(t$noise)
t2 <- subset(t,emma=="off" & runs==800)[order(noise), ]
nd <- t2[,c(1,2,7:12)]

###################
#nd <- read.table("../noisecomp.data",header=T)
compl.a <- c(790,616,403,129,63,33,17)
succ.att.a <- c(800,759,617,248,133,91,47)
corr.att.a <- c(800,759,617,236,126,79,43)
mean.att.a <- c(0,0.00395257, 0.00972447,0.0241935,0.037594,0.0549451,0)

nd$compl.a <- compl.a
nd$succ.att.a <- succ.att.a
nd$corr.att.a <- corr.att.a
nd$mean.att.jemals.a <- mean.att.a

write.table(nd,"../noisecomp.data",row.names=FALSE,sep="\t")