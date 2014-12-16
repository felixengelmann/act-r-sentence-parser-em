# rm(list=ls())
library(em2)
library(reshape)
library(xtable)
library(gdata)
library(ggplot2)


#+ Read simulation data
f <- read.table("fixations.txt", header=T)
colnames(f) <- c("exp","iteration","cond","pos","word","dur")
# head(f) #str(f) # head(f,50)




##-------------------------------------------------------------------------------
## EM
##-------------------------------------------------------------------------------
f$iteration <- as.factor(as.character(f$iteration))
etm <- with(f, em2(pos, dur, data.frame(exp, iteration, cond)))
# head(etm) # summary(etm)
# save(etm, file="sim-etm.RData")

##-------------------------------------------------------------------------------
## OTHER INFO
##-------------------------------------------------------------------------------
m <- etm

#' attachment times ##
retr <- read.table("attachments.txt", header=TRUE)
colnames(retr) <- c("exp","iteration","cond","roi","word","retr")
# head(retr[-5]) # str(retr); head(retr,25)
m <- merge(etm, retr[-5], by=c("exp","iteration","cond","roi"), all.x=TRUE)

## encoding times ##
enc <- read.table("enctimes.txt", header=TRUE)
colnames(enc) <- c("exp","iteration","cond","roi","word","enc","ecc","freq1")
#head(enc) # str(enc); head(enc,25)
m <- merge(m, enc, by=c("exp","iteration","cond","roi"), all.x=TRUE)
#head(m)



##-------------------------------------------------------------------------------
## ADDITIONAL MEASURES
##-------------------------------------------------------------------------------
## first-pass regression prob.
m$fp_reg <- ifelse(m$RBRC>0,1,0) 
## skipping prob:
m$skip<-ifelse(m$FPRT==0,1,0)
## re-reading prob:
m$reread<-ifelse(m$RRT>0,1,0)
## re-fixation prob:
m$refix <- ifelse(m$FPRT>m$FFD,1,0)
#m$onefix <- ifelse((m$SFD>0 & m$FFP==1),1,0)
head(m)



##-------------------------------------------------------------------------------
## ROIs
##-------------------------------------------------------------------------------
colnames(m)[4] <- "pos"
m$pos <- as.vector(as.numeric(m$pos))
m$roi <- m$pos

m$roi[m$pos == 3] <- "REL"
m$roi[m$pos == 7] <- "V"
m$roi[!m$pos %in% 3:7] <- "other"
m$roi[m$cond == "SRC" & m$pos == 4] <- "RC-V"
m$roi[m$cond == "SRC" & m$pos == 5] <- "DET"
m$roi[m$cond == "SRC" & m$pos == 6] <- "N"
m$roi[m$cond == "ORC" & m$pos == 4] <- "DET"
m$roi[m$cond == "ORC" & m$pos == 5] <- "N"
m$roi[m$cond == "ORC" & m$pos == 6] <- "RC-V"


m$roi2 <- m$roi
m$roi2[m$roi %in% c("DET","N")] <- "DET/N"

#levels(m$roi)
m$roi <- factor(m$roi, levels=c("REL", "DET", "N", "RC-V", "V", "other"))
#levels(m$roi2)
m$roi2 <- factor(m$roi2, levels=c("REL", "DET/N", "RC-V", "V", "other"))

# #head(d,20)
# d$roi2 <- as.vector(d$roi)
# d$roi2[d$roi2%in%c("DET","N")] <- "DET/N"


##-------------------------------------------------------------------------------
## MEANS
##-------------------------------------------------------------------------------
# head(m)
m$roi <- as.factor(m$roi)

## Reading times ##
# mlt <- melt(m, id=c("roi","cond","word"), measure=c("FFD","FPRT","TFT","RRT","RPD","retr","enc"), na.rm = T)
mlt <- melt(m, id=c("roi","cond"), measure=c("FFD","FPRT","TFT","RPD","retr","enc"), na.rm = T)
cst <- cast(subset(mlt,value>0), variable+roi+cond ~ ., function(x) c(M=mean(x), SE=sd(x)/sqrt(length(x)), N=length(x), CI=ci(x)))
# head(cst); summary(cst)
means.t <- cst

# mlt <- melt(m, id=c("roi2","cond"), measure=c("FFD","FPRT","TFT","RRT","RPD","retr","enc"), na.rm = T)
mlt <- melt(m, id=c("roi2","cond"), measure=c("FFD","FPRT","TFT","RPD","retr","enc"), na.rm = T)
cst <- cast(subset(mlt,value>0), variable+roi2+cond ~ ., function(x) c(M=mean(x), SE=sd(x)/sqrt(length(x)), N=length(x), CI=ci(x)))
# head(cst); summary(cst)
means.t2 <- cst

# mlt <- melt(m, id=c("pos","cond","word"), measure=c("FFD","FPRT","TFT","RRT","RPD","retr","enc"), na.rm = T)
mlt <- melt(m, id=c("pos","cond"), measure=c("FFD","FPRT","TFT","RPD","retr","enc"), na.rm = T)
cst <- cast(subset(mlt,value>0), variable+pos+cond ~ ., function(x) c(M=mean(x), SE=sd(x)/sqrt(length(x)), N=length(x), CI=ci(x)))
# head(cst); summary(cst)
means.t.all <- cst


## Probabilities ##
mlt <- melt(m, id=c("roi","cond"), measure=c("refix","reread","fp_reg","skip"), na.rm=T)
cst <- cast(mlt, variable+roi+cond ~ ., function(x) c(M=mean(x), N=length(x)))
cst$SE <- sqrt(cst$M*(1-cst$M))/sqrt(cst$N)
cst$CI.lower <- cst$M + qt(.025, df=cst$N-1) * cst$SE
cst$CI.upper <- cst$M + qt(.975, df=cst$N-1) * cst$SE
# head(cst); summary(cst)
means.p <- cst

mlt <- melt(m, id=c("roi2","cond"), measure=c("refix","reread","fp_reg","skip"), na.rm=T)
cst <- cast(mlt, variable+roi2+cond ~ ., function(x) c(M=mean(x), N=length(x)))
cst$SE <- sqrt(cst$M*(1-cst$M))/sqrt(cst$N)
cst$CI.lower <- cst$M + qt(.025, df=cst$N-1) * cst$SE
cst$CI.upper <- cst$M + qt(.975, df=cst$N-1) * cst$SE
# head(cst); summary(cst)
means.p2 <- cst

mlt <- melt(m, id=c("pos","cond"), measure=c("refix","reread","fp_reg","skip"), na.rm=T)
cst <- cast(mlt, variable+pos+cond ~ ., function(x) c(M=mean(x), N=length(x)))
cst$SE <- sqrt(cst$M*(1-cst$M))/sqrt(cst$N)
cst$CI.lower <- cst$M + qt(.025, df=cst$N-1) * cst$SE
cst$CI.upper <- cst$M + qt(.975, df=cst$N-1) * cst$SE
# head(cst); summary(cst)
means.p.all <- cst

m.means <- rbind(means.t,means.p)
head(m.means)
m.means2 <- rbind(means.t2,means.p2)
#head(m.means2)


##-------------------------------------------------------------------------------
##' DATA
##-------------------------------------------------------------------------------
d <- read.table("../staub10-data.txt", header=T)
colnames(d)[1] <- "variable"
head(d) # str(d) # str(m.means)
#m.means$type <- "model"
d2 <- d
d2$roi2 <- as.vector(d2$roi)
d2$roi2[d2$roi2%in%c("DET","N")] <- "DET/N"
d2 <- cast(d2, variable+roi2+cond ~ ., value="data", mean)
colnames(d2)[4] <- "data"
#head(d2)

dim(m.means)
means <- merge(m.means, d, by=c("variable","cond","roi"), all.x=T)
dim(means)
dim(m.means2)
means2 <- merge(m.means2, d2, by=c("variable","cond","roi2"), all.x=T)
# head(means2)
dim(means2)



##-------------------------------------------------------------------------------
## PLOT
##-------------------------------------------------------------------------------
roilabels <- c("REL", "DET", "N", "RC-V", "V")
levels(means$variable)[2] <- "Gaze"
levels(means$variable)[4] <- "Go-past"
levels(means$variable)[9] <- "reg"

roilabels2 <- c("REL", "DET/N", "RC-V", "V")
levels(means2$variable)[2] <- "Gaze"
levels(means2$variable)[4] <- "Go-past"
levels(means2$variable)[9] <- "reg"

## Overview ##
# (p.all.t <- qplot(pos, M, colour=cond, linetype=cond, group=cond, data=droplevels(subset(means.t.all, (M!=0 & pos>1 & variable%in%c("FFD","FPRT","RPD")))), geom=c("line", "point"), main="Reading times", facets=variable~.)  + geom_errorbar(aes(max=CI.upper, min=CI.lower, width=0)) + theme_bw() + scale_color_brewer(palette="Set1"))

# (p.all.p <- qplot(pos, M, colour=cond, linetype=cond, group=cond, data=subset(means.p.all, (variable%in%c("reread","fp_reg","refix"))), geom=c("line", "point"), main="Probabilities", facets=variable~.)  + geom_errorbar(aes(max=CI.upper, min=CI.lower, width=0)) + theme_bw()+ scale_color_brewer(palette="Set1"))

(p.rois.t <- qplot(roi, M, colour=cond, linetype=cond, group=cond, data=droplevels(subset(means, (M!=0 & variable%in%c("FFD","Gaze","Go-past") & roi%in%roilabels))), geom=c("line", "point"), main="Reading times", facets=variable~.)  + geom_errorbar(aes(max=CI.upper, min=CI.lower, width=0)) + theme_bw() + scale_color_brewer(palette="Set1"))

(p.rois.p <- qplot(roi, M, colour=cond, linetype=cond, group=cond, data=subset(means, (variable%in%c("reread","reg","refix") & roi%in%roilabels)), geom=c("line", "point"), main="Probabilities", facets=variable~.)  + geom_errorbar(aes(max=CI.upper, min=CI.lower, width=0)) + theme_bw()+ scale_color_brewer(palette="Set1"))

(p.info <- qplot(roi, M, colour=cond, linetype=cond, group=cond, data=droplevels(subset(m.means, (variable%in%c("retr","enc") & roi%in%roilabels))), geom=c("line", "point"), main="Word information", facets=variable~.)  + geom_errorbar(aes(max=CI.upper, min=CI.lower, width=0)) + theme_bw()+ scale_color_brewer(palette="Set1"))




dodge1 <- position_dodge(width=0.6)
dodge2 <- position_dodge(width=0.07)
(p.d1 <- ggplot(aes(roi2, M, col=cond, linetype=cond, group=cond),
	data=droplevels(subset(means2, (M!=0 & roi2%in%c("DET/N","RC-V","V")  & variable%in%c("FFD")))))
	+ ylab("Mean first fixation duration (ms)") 
	+ xlab("Region")
	+ ggtitle("First fixation duration Model (color) vs. Data (gray)")
	+ theme_bw()
	+ theme(legend.position="right"
			,text=element_text(family="Helvetica")
			,legend.key=element_blank()
			,legend.title=element_blank(),
			strip.background=element_rect(fill="gray90", color=NA))
	+ scale_colour_manual("cond", values=UPpalette(firstcolor="humfak",secondcolor="darkblue"))
	+ scale_x_discrete(expand=c(0.1,0))
	+ scale_y_continuous(expand=c(0.1,0), limits=c(200,360))
	+ geom_line(colour="gray80", aes(y=data))
	+ geom_point(colour="gray80", size=2, aes(y=data))
	# + geom_bar(position=dodge1, stat="identity")
	+ geom_line(position=dodge2)
	+ geom_point(position=dodge2)
	# + geom_linerange(aes(max=CI.upper, min=CI.lower),position=dodge2)
	+ geom_errorbar(aes(max=CI.upper, min=CI.lower, width=0),position=dodge2)
	# + geom_smooth(aes(ymin = CI.lower, ymax = CI.upper), stat="identity")
	# + facet_grid(variable~.)
)
ggsave("staub10-model+data-rt.pdf", p.d1, width=5, height=4)


dodge1 <- position_dodge(width=.9)
dodge2 <- position_dodge(width=0.07)
dodge3 <- position_dodge(width=0.5)
(p.d1 <- ggplot(aes(roi2, M, fill=cond),
	data=droplevels(subset(means2, (roi2%in%c("DET/N","RC-V","V")  & variable%in%c("reg")))))
	+ ylab("Mean first pass regression rate") 
	+ xlab("Region")
	+ ggtitle("First pass regressions Model (bars) vs. Data (points)")
	+ theme_bw()
	+ theme(legend.position="right"
			,text=element_text(family="Helvetica")
			,legend.key=element_blank()
			,legend.title=element_blank(),
			strip.background=element_rect(fill="gray90", color=NA))
	+ scale_fill_manual("cond", values=UPpalette(firstcolor="humfak",secondcolor="darkblue"))
	+ scale_color_manual("cond", values=UPpalette(firstcolor="humfak",secondcolor="darkblue"))
	# + scale_x_discrete(expand=c(0.1,0))
	+ scale_y_continuous(expand=c(0.1,0), limits=c(-.01,0.6))
	# + geom_line(colour="gray90", linetype=2, aes(y=data))
	+ geom_bar(position=dodge1, stat="identity")
	# + geom_bar(aes(y=data), width=0.1, position=dodge1, stat="identity")
	# + geom_point(position=dodge2)
	+ geom_linerange(aes(max=CI.upper, min=CI.lower),position=dodge1)
	+ geom_point(size=3, aes(y=data), color=I("gray"), position=dodge1)
	# + geom_errorbar(aes(max=CI.upper, min=CI.lower, width=0),position=dodge1)
	# + geom_smooth(aes(ymin = CI.lower, ymax = CI.upper), stat="identity")
)
ggsave("staub10-model+data-prob.pdf", p.d1, width=5, height=4)



