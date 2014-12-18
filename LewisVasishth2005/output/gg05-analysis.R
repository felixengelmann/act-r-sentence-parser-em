rm(list=ls())
library(em2)
library(reshape)
library(ggplot2)

## Function for computing confidence intervals
ci <- function (x) 
{
    m <- mean(x, na.rm = TRUE)
    n <- length(x[!is.na(x)])
    s <- sd(x, na.rm = TRUE)
    upper <- m + qt(0.975, df = n - 1) * (s/sqrt(n))
    lower <- m + qt(0.025, df = n - 1) * (s/sqrt(n))
    return(data.frame(lower = lower, upper = upper))
}



##------------------------------------------------------------
## READ SIMULATION DATA
##------------------------------------------------------------
f <- read.table("fixations.txt", header=T)
colnames(f) <- c("exp","iteration","cond","pos","word","dur")


##------------------------------------------------------------
## COMPUTE EYE MOVEMENT MEASURES
##------------------------------------------------------------
f$iteration <- as.factor(as.character(f$iteration))
etm <- with(f, em2(pos, dur, data.frame(exp, iteration, cond)))
etm$pos <- etm$roi

## ADDITIONAL MEASURES

## first-pass regression probability:
etm$fp_reg <- ifelse(etm$RBRC>0,1,0) 
## skipping prob.:
etm$skip<-ifelse(etm$FPRT==0,1,0)
## re-reading prob:
etm$reread<-ifelse(etm$RRT>0,1,0)
## re-fixation prob.:
etm$refix <- ifelse(etm$FPRT>etm$FFD,1,0)
#m$onefix <- ifelse((m$SFD>0 & m$FFP==1),1,0)

# head(etm)
# summary(etm)



##------------------------------------------------------------
## READ ADDITIONAL INFO
##------------------------------------------------------------
m <- etm

## TRIAL MESSAGES:
msg <- read.table("trialmessages.txt", header=F)
colnames(msg) <- c("exp","iteration","cond","pos","word","variable","value")

trialinfo <- subset(msg[,-c(4,5)], variable!="timeout")
trialinfo <- reshape(trialinfo, idvar = c("exp","iteration","cond"), timevar="variable", direction="wide")
colnames(trialinfo) <- gsub("value.","", colnames(trialinfo))
trialinfo <- droplevels(trialinfo)
# head(trialinfo)
# summary(trialinfo)

## Merge with EM results
dim(m)
m <- merge(m, trialinfo, by=c("exp","iteration","cond"), all.x=TRUE)
m$fail <- ifelse(is.na(m$fail), 0, 1)
dim(m)

## TIMEOUTS:
tmo <- droplevels(subset(msg, variable=="timeout"))
colnames(tmo)[6:7] <- c("timeout", "tmo-eyloc")
tmo$timeout <- 1
dim(m)
m <- merge(m, tmo[,-5], by=c("exp","iteration","cond","pos"), all.x=TRUE)
dim(m)
m$timeout[is.na(m$timeout)] <- 0

## ATTACHMENT TIMES:
att <- read.table("attachments.txt", header=TRUE)
colnames(att) <- c("exp","iteration","cond","pos","word","AT")
dim(m)
m <- merge(m, att[-5], by=c("exp","iteration","cond","pos"), all.x=TRUE)
dim(m)

## ENCODING TIMES:
enc <- read.table("enctimes.txt", header=TRUE)
colnames(enc) <- c("exp","iteration","cond","pos","word","ET","ecc","freq")
enc$iteration <- factor(enc$iteration)
dim(m)
m <- merge(m, enc, by=c("exp","iteration","cond","pos"), all.x=TRUE)
dim(m)
# head(m); summary(m)


##------------------------------------------------------------
## DEFINE REGIONS OF INTEREST
##------------------------------------------------------------
levels(m$cond) <- c("OR","SR")

m$roi[m$pos == 3] <- "REL"
m$roi[m$pos == 7] <- "V"
m$roi[!m$pos %in% 3:7] <- "other"
m$roi[m$cond == "SR" & m$pos == 4] <- "RC-V"
m$roi[m$cond == "SR" & m$pos == 5] <- "DET"
m$roi[m$cond == "SR" & m$pos == 6] <- "N"
m$roi[m$cond == "OR" & m$pos == 4] <- "DET"
m$roi[m$cond == "OR" & m$pos == 5] <- "N"
m$roi[m$cond == "OR" & m$pos == 6] <- "RC-V"

m$roi <- factor(m$roi, levels=c("REL", "DET", "N", "RC-V", "V", "other"))



##------------------------------------------------------------
## MEANS
##------------------------------------------------------------

## Exclude failed trials
m.all <- m
m <- subset(m, fail==0)

## TIMES:
mlt <- melt(m, id=c("roi","pos","cond"), measure=c("FFD","FPRT","TFT","RPD","AT","ET"), na.rm = TRUE)

## by roi
cst <- cast(subset(mlt,value>0), variable+roi+cond ~ ., function(x) c(M=mean(x), SE=sd(x)/sqrt(length(x)), N=length(x), CI=ci(x)))
means.t <- cst
## by pos
cst <- cast(subset(mlt,value>0), variable+pos+cond ~ ., function(x) c(M=mean(x), SE=sd(x)/sqrt(length(x)), N=length(x), CI=ci(x)))
means.t.all <- cst

## PROBABILITIES:
mlt <- melt(m, id=c("roi","pos","cond","iteration"), measure=c("refix","reread","fp_reg","skip","fail","timeout"), na.rm=T)

## by roi
cst <- cast(mlt, variable+roi+cond ~ ., function(x) c(M=mean(x), N=length(x)))
cst$SE <- sqrt(cst$M*(1-cst$M))/sqrt(cst$N)
cst$CI.lower <- cst$M + qt(.025, df=cst$N-1) * cst$SE
cst$CI.upper <- cst$M + qt(.975, df=cst$N-1) * cst$SE
means.p <- cst
## by pos
cst <- cast(mlt, variable+pos+cond ~ ., function(x) c(M=mean(x), N=length(x)))
cst$SE <- sqrt(cst$M*(1-cst$M))/sqrt(cst$N)
cst$CI.lower <- cst$M + qt(.025, df=cst$N-1) * cst$SE
cst$CI.upper <- cst$M + qt(.975, df=cst$N-1) * cst$SE
means.p.all <- cst

means <- rbind(means.t,means.p)
head(means)



##------------------------------------------------------------
## PLOTS
##------------------------------------------------------------
roilabels <- c("REL", "DET", "N", "RC-V", "V")
dodge <- position_dodge(width=.9)

## TRIAL INFO
fails <- cast(m.all, cond~., mean, value="fail")
colnames(fails)[2] <- "fail"
(p1 <- ggplot(fails, 
	aes(cond, fail, fill=cond))
+ geom_bar(stat="identity", position=dodge, show_guide=FALSE)
+ ylim(0,1)
+ xlab("")
+ ylab("Probability")
+ ggtitle("Model failure")
+ theme_minimal()
)

## RT
(p1 <- ggplot(
	droplevels(subset(means, (M!=0 & variable%in%c("FFD","FPRT","RBRT") & roi%in%roilabels))), 
	aes(roi, M, col=cond, linetype=cond, group=cond))
+ facet_grid(variable~.)
+ geom_line() + geom_point()
+ geom_errorbar(aes(max=CI.upper, min=CI.lower, width=0))
+ ggtitle("Model reading times")
+ theme_bw()
)

## PROB
(p1 <- ggplot(
	droplevels(subset(means, (variable%in%c("reread","fp_reg","refix", "skip","timeout") & roi%in%roilabels))), 
	aes(roi, M, col=cond, linetype=cond, group=cond))
+ facet_grid(variable~., scales="free")
# + facet_wrap(~ variable, ncol=2)
+ geom_line() + geom_point() 
+ geom_errorbar(aes(max=CI.upper, min=CI.lower, width=0))
# + geom_bar(stat="identity", position=dodge)
# + geom_linerange(aes(max=CI.upper, min=CI.lower, width=0), position=dodge)
+ ggtitle("Model probabilities")
+ theme_bw()
)

## WORD INFO
(p1 <- ggplot(
	droplevels(subset(means, (variable%in%c("AT","ET") & roi%in%roilabels))), 
	aes(roi, M, col=cond, linetype=cond, group=cond))
+ facet_grid(variable~.)
+ geom_line() + geom_point()
+ geom_errorbar(aes(max=CI.upper, min=CI.lower, width=0))
+ ggtitle("Attachment and encoding times")
+ theme_bw()
)


## ATTACHMENT TIME
(p.att <- ggplot(
	droplevels(subset(means, (variable%in%c("AT") & roi%in%c("RC-V","V")))), aes(factor(roi,levels=c("V","RC-V")), M, fill=factor(cond, levels=c("SR","OR"))))
+ geom_bar(stat="identity", position=dodge)
+ geom_errorbar(aes(max=CI.upper, min=CI.lower, width=0), position=dodge)
+ guides(fill=guide_legend(title=""))
+ xlab("")
+ ylab("Mean duration in ms")
+ coord_cartesian(ylim=c(10,200))
+ ggtitle("Model attachment time")
+ theme_classic()
)
ggsave("gg05-model-attachment.pdf", p.att, width=5, height=4)

## READING TIME
dodge <- position_dodge(width=.9)
(p.rt <- ggplot(
	droplevels(subset(means, (M!=0 & variable%in%c("FPRT") & roi%in%c("RC-V","V")))), aes(factor(roi,levels=c("V","RC-V")), M, fill=factor(cond, levels=c("SR","OR"))))
+ geom_bar(stat="identity", position=dodge)
+ geom_errorbar(aes(max=CI.upper, min=CI.lower, width=0), position=dodge)
+ guides(fill=guide_legend(title=""))
+ xlab("")
+ ylab("Mean duration in ms")
+ coord_cartesian(ylim=c(200,600))
+ ggtitle("Model gaze duration")
+ theme_classic()
)
ggsave("gg05-model.pdf", p.rt, width=5, height=4)


