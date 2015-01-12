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


###################################################
### chunk number 2: 
###################################################

pos8data <- subset(data,position==8)

data.abc <- rbind(a,b,c)

data$trial <- as.factor(data$trial)

means <- with(pos8data, tapply(RT,IND=list(condition),mean))
stderr <- with(pos8data,tapply(RT,IND=list(condition),se))
tn <- with(pos8data,tapply(RT,IND=list(condition),length))
tu <- with(pos8data,means + qt(.975,df=tn-1) * stderr)
tl <- with(pos8data,means - qt(.975,df=tn-1) * stderr)


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
         main="Total reading time at polarity item: model versus data",
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





