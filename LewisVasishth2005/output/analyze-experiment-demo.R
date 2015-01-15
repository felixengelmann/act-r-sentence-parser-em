## 
## Demo script for analyzing experiments. 
## 
## Example data from Grodner & Gibson Exp1 (relative clauses).
##

##
## To adjust for other experiments, change data and regions of interest in the following section.
##


##------------------------------------------------------------
## CHANGE EXPERIMENT SPECIFICS HERE
##------------------------------------------------------------

##
## DATA
##
data <- data.frame(cond=c("OBJECT-REL","SUBJECT-REL","OBJECT-REL","SUBJECT-REL"),
									 roi=c("embV","embV","mainV","mainV"),
									 rt=c(422,355,401,404)
									 )

##
## REGIONS OF INTEREST
## Define ROI for each condition

## Example: gg-exp1
	Reg1 <- list(2:5, 2:3)
	embV <- list(6,4)
	Reg2 <- list(7:9,5:9)
	mainV <- list(10,10)
	Reg3 <- list(11:12,11:12)
	spillover <- list()
	rois <- list(Reg1,embV,Reg2,mainV,Reg3)
	roinames <- c("Reg1","embV","Reg2","mainV","Reg3")
###



##------------------------------------------------------------
## SOME HELPER FUNCTIONS
##------------------------------------------------------------
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

rmsd <- function (obs, pred){
	sqrt(mean((obs - pred)^2, na.rm = TRUE))
}

## DATA CLEAN-UP
data.cleanup <- function(m){
  m <- subset(m, pos!=1 & word!="*")
  m$trial <- paste(m$exp, m$iteration, m$cond)
  (n.trials <- length(unique(m$trial)))
  m$trialpos <- paste(m$exp, m$iteration, m$cond, m$pos)
  (length(unique(m$trialpos)))
  # head(m)
  m$dupl <- duplicated(m$trialpos)
  dim(subset(m,dupl))[1]
  m <- subset(m, !dupl)
  return(m)
}


#
prefix <- ""
#
# analyze <- function(prefix=""){
##------------------------------------------------------------
## READ SIMULATION DATA
##------------------------------------------------------------
	f <- read.table(paste(prefix,"fixations.txt",sep=""), header=T)
	colnames(f) <- c("exp","iteration","cond","pos","word","dur")
	f$cond <- factor(f$cond)
	head(f)
	summary(f)



##------------------------------------------------------------
## COMPUTE EYE MOVEMENT MEASURES
##------------------------------------------------------------
	f$iteration <- as.factor(as.character(f$iteration))
	etm <- with(f, em2(pos, dur, data.frame(exp, iteration, cond)))
	etm$pos <- etm$roi

##
## ADDITIONAL MEASURES
##
## first-pass regression probability:
	etm$fp_reg <- ifelse(etm$RBRC>0,1,0) 
## skipping prob.:
	etm$skip<-ifelse(etm$FPRT==0,1,0)
## re-reading prob:
	etm$reread<-ifelse(etm$RRT>0,1,0)
## re-fixation prob.:
	etm$refix <- ifelse(etm$FPRT>etm$FFD,1,0)
# head(etm)

## re-add words:
	dim(etm)
	etm <- merge(etm, unique(f[,3:5]), by=c("cond","pos"))
	dim(etm)



##------------------------------------------------------------
## READ ADDITIONAL INFO
##------------------------------------------------------------
	m <- etm

## TRIAL MESSAGES
	msg <- read.table(paste(prefix,"trialmessages.txt",sep=""), header=F)
	colnames(msg) <- c("exp","iteration","cond","pos","word","variable","value")

	trialinfo <- subset(msg[,-c(4,5)], variable!="timeout" & variable!="regression")
	trialinfo <- reshape(trialinfo, idvar = c("exp","iteration","cond"), timevar="variable", direction="wide")
	colnames(trialinfo) <- gsub("value.","", colnames(trialinfo))
	trialinfo <- droplevels(trialinfo)

## Merge with EM results
	if(dim(trialinfo)[1]>0){
		dim(m)
		m <- merge(m, trialinfo, by=c("exp","iteration","cond"), all.x=TRUE)
		m$fail <- ifelse(is.na(m$fail), 0, 1)
		dim(m)
	}else{m$fail<-0}

## TIMEOUTS:
	tmo <- droplevels(subset(msg, variable=="timeout"))
	colnames(tmo)[6:7] <- c("timeout", "tmo-eyloc")
	tmo$timeout <- 1
	dim(m)
	m <- merge(m, tmo[,-5], by=c("exp","iteration","cond","pos"), all.x=TRUE)
	dim(m)
	m$timeout[is.na(m$timeout)] <- 0



##------------------------------------------------------------
## REGIONS OF INTEREST
##------------------------------------------------------------
	summary(factor(m$cond))


	m$roi <- NA
	n.cond <- 0
	for(c in levels(factor(m$cond))){
		n.cond <- n.cond+1
		# print(n.cond)
		# print(c)
		n.roi <- 0
		for(r in rois){
			n.roi <- n.roi+1
			p <- unlist(r[n.cond])
			# print(p)
			rname <- roinames[n.roi]
			# print(rname)
			m$roi[m$cond==c & m$pos%in%p] <- rname
		}
	}
	m$roi <- factor(m$roi, levels=roinames)
	# summary(m)
	# head(subset(m,!is.na(roi)))



##------------------------------------------------------------
## MEANS
##------------------------------------------------------------
## FAILURES
	fail <- cast(m, cond~., function(x) c(M=mean(x), N=length(x)), value="fail")
	fail$SE <- sqrt(fail$M*(1-fail$M))/sqrt(fail$N)
	fail$CI.lower <- fail$M + qt(.025, df=fail$N-1) * fail$SE
	fail$CI.upper <- fail$M + qt(.975, df=fail$N-1) * fail$SE
	colnames(fail)[2] <- "fail"


## Exclude failed trials
	m.all <- m
	m <- subset(m, fail==0)


## TIMES:
	mlt <- melt(subset(m), id=c("roi","pos","cond"), measure=c("SFD","FFD","FPRT","TFT","RPD"), na.rm = TRUE)
## by roi
	cst <- cast(subset(mlt, !is.na(roi) & value>0), variable+roi+cond ~ ., function(x) c(M=mean(x), SE=sd(x)/sqrt(length(x)), N=length(x), CI=ci(x)))
	means.t <- cst
## by pos
	cst <- cast(subset(mlt, value>0 & variable%in%c("FPRT","TFT","RPD")), variable+pos+cond ~ ., function(x) c(M=mean(x), SE=sd(x)/sqrt(length(x)), N=length(x), CI=ci(x)))
	means.pos.t <- cst

## PROBABILITIES:
	mlt <- melt(subset(m), id=c("roi","pos","cond"), measure=c("refix","reread","fp_reg","skip","timeout"), na.rm=T)
## by roi
	cst <- cast(subset(mlt, !is.na(roi)), variable+roi+cond ~ ., function(x) c(M=mean(x), N=length(x)))
	cst$SE <- sqrt(cst$M*(1-cst$M))/sqrt(cst$N)
	cst$CI.lower <- cst$M + qt(.025, df=cst$N-1) * cst$SE
	cst$CI.upper <- cst$M + qt(.975, df=cst$N-1) * cst$SE
	means.p <- cst
## by pos
	cst <- cast(subset(mlt), variable+pos+cond ~ ., function(x) c(M=mean(x), N=length(x)))
	cst$SE <- sqrt(cst$M*(1-cst$M))/sqrt(cst$N)
	cst$CI.lower <- cst$M + qt(.025, df=cst$N-1) * cst$SE
	cst$CI.upper <- cst$M + qt(.975, df=cst$N-1) * cst$SE
	means.pos.p <- cst


	means <- rbind(means.t,means.p)
	head(means)
	means.pos <- rbind(means.pos.t,means.pos.p)



##------------------------------------------------------------
## PLOTS
##------------------------------------------------------------
	dodge <- position_dodge(width=.9)
	dodge2 <- position_dodge(width=.6)


## FAILURES
	(pf <- ggplot(fail, 
		aes(cond, fail, fill=cond))
	+ geom_bar(stat="identity", position=dodge, show_guide=FALSE)
	+ geom_errorbar(aes(max=CI.upper, min=CI.lower, width=0), position=dodge)
	+ ylim(0,1)
	+ xlab("")
	+ ylab("Probability")
	+ ggtitle("Model failure")
	)
	ggsave(pf, file=paste(prefix,"plot-failures.pdf",sep=""))


## READING TIME
	(pr <- ggplot(subset(means, (M!=0 & variable%in%c("FFD","FPRT","RPD","TFT"))), aes(roi, M, col=cond, shape=cond))
		+ geom_point(position=dodge2)
		+ geom_errorbar(aes(max=CI.upper, min=CI.lower, width=0), position=dodge2)
		+ xlab("")
		+ ylab("Mean duration in ms")
		+ coord_cartesian()
		+ ggtitle("Model reading times")
		+ theme_bw()
# + facet_grid(variable~.)
		+ facet_wrap(~variable, ncol=2, scales="free")
		+ geom_point(data=data, aes(roi,rt),color=c("gray30"))
		)

	ggsave(pr, file=paste(prefix,"plot-reading-times.pdf",sep=""), height=7, width=9)

## PROBABILITIES
	(pp <- ggplot(subset(means, variable%in%c("reread","fp_reg","refix", "skip","timeout")), 
		aes(roi, M, fill=cond))
	+ geom_bar(stat="identity", position=dodge)
	+ geom_linerange(aes(max=CI.upper, min=CI.lower, width=0), position=dodge)
	+ ggtitle("Model probabilities")
	+ theme_bw()
	+ facet_wrap(~ variable, ncol=2, scales="free")
	)
	ggsave(pp, file=paste(prefix,"plot-probabilities.pdf",sep=""), height=7, width=9)


## OVERVIEW RT
(por <- ggplot(subset(means.pos, (M!=0 & variable%in%c("AT","ET","FPRT","TFT"))), aes(pos, M, col=cond, linetype=cond))
+ geom_point() + geom_line()
+ geom_errorbar(aes(max=CI.upper, min=CI.lower, width=0))
+ xlab("")
+ ylab("Mean duration in ms")
+ coord_cartesian()
+ ggtitle("Model overview RT")
+ theme_bw()
# + facet_grid(variable~.)
+ facet_wrap(~variable, ncol=1, scales="free")
+ theme(legend.position="bottom")
)
ggsave(por, file=paste(prefix,"plot-overview1.pdf",sep=""), height=9, width=9)

## OVERVIEW PROB
(pop <- ggplot(subset(means.pos, variable%in%c("reread","fp_reg","refix", "skip","timeout")), 
	aes(pos, M, col=cond, linetype=cond, group=cond))
+ geom_line() + geom_point()
+ geom_linerange(aes(max=CI.upper, min=CI.lower, width=0))
+ ggtitle("Model overview Prob")
+ theme_bw()
+ facet_wrap(~variable, ncol=1, scales="free")
+ theme(legend.position="bottom")
)
ggsave(pop, file=paste(prefix,"plot-overview2.pdf",sep=""), height=9, width=9)



##------------------------------------------------------------
## SCANPATHS
##------------------------------------------------------------

## PLOT SCANPATH OF RANDOM TRIAL ##
i <- sample(unique(f$iteration),1)
t1 <- subset(f, iteration%in%i)
t1$index <- NA
for(i in unique(t1$iteration)){
	for(c in t1$cond){
		s <- (t1$iteration==i & t1$cond==c)
		t1$index[s] <- 1:length(t1$pos[s])
	}
}
(psc <- ggplot(t1, aes(index, pos, group=cond, col=cond)) + geom_point(aes(size=dur)) + geom_line() 
	+ scale_y_discrete(labels=min(t1$pos):max(t1$pos)) 
	+ ggtitle(i))
ggsave(psc, file="plot-scanpath.pdf")


## PLOT SCANPATH OF 7 RANDOM TRIALS ##
i <- sample(unique(f$iteration),7)
t1 <- subset(f, iteration%in%i)
t1$index <- NA
for(i in unique(t1$iteration)){
	for(c in t1$cond){
		s <- (t1$iteration==i & t1$cond==c)
		t1$index[s] <- 1:length(t1$pos[s])
	}
}
(psc7 <- ggplot(t1, aes(index, pos, group=cond, col=cond)) + geom_point(aes(size=dur)) + geom_line() + facet_grid(.~iteration) + scale_y_discrete(labels=min(t1$pos):max(t1$pos)) + theme(legend.position="bottom"))
ggsave(psc7, file="plot-scanpaths7.pdf", width=13, height=6)



##------------------------------------------------------------
## FIT WITH THE DATA
##------------------------------------------------------------
	model <- subset(means,variable=="TFT" & roi%in%c("embV","mainV"))
	data
	model
	(error <- rmsd(data$rt,model$M))
	(r <- cor(data$rt,model$M))
	(score <- error-100*r)

	# return(list(m=m,error=error,r=r,score=score))
# }
