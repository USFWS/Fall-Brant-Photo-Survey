rm(list=ls(all=TRUE))
options(stringsAsFactors=FALSE)
options(scipen=10)
setwd("C:/Users/eweiser/Documents/02 Brant photo survey")
library(gplots)
library(viridis)
cols <- viridis(5)
cols <- c(cols[-length(cols)], 'gold')
# purple, dark blue, mid blue, green, bright green, gold

# progress bar for 'for' loops:
pbar <- function(index, N, per=0.1){
	if(index %in% round(seq(0,N,N*per),0)) print(c(paste(round(index/N*100,0), "% completed at ", Sys.time(), sep="")))
	flush.console()
}


## Read in the CountThings counts (may not use) and the manual counts for 2017-2019 surveys:
f <- dir('Data/Counts_combined_files', '.csv')
survs <- numeric(0)
for(i in 1:length(f)) survs <- c(survs, unlist(strsplit(f[i], "_"))[3])
survs <- unique(survs)
# don't use 2017-10-02 (partial survey):
survs <- survs[which(survs!='2017-10-02')]
dat <- read.csv(paste0('Data/Counts_combined_files/', f[1]),header=TRUE)
dat$Survey <- survs[1]
for(i in 1:length(survs)){
	f1 <- f[grep(survs[i],f)]
	for(j in 1:length(f1)){
		dat1 <- read.csv(paste0('Data/Counts_combined_files/', f1[j]),header=TRUE)
		dat1$Survey <- survs[i]
		dat <- rbind(dat, dat1)
	}
}
dat <- unique(dat)
# Leave the NAs as NA. These were not manually checked and thus can't be used to validate YOLO.
# dat$Manual_Brant[which(is.na(dat$Manual_Brant))] <- 0
# dat$Manual_Canada[which(is.na(dat$Manual_Canada))] <- 0
sort(unique(dat$Survey))



## Read in the YOLO counts:
yolo <- read.csv('00 YOLO validation/eval_set_YOLO_counts.csv',header=TRUE)
# remove Out_lagoon (they will mess up strsplit below, and we didn't check them):
yolo <- yolo[-grep('Out_lagoon', yolo$image_path_original),]
# Create columns to cross-reference with dat:
yolo$FileName <- yolo$Camera <- yolo$Survey <- NA
for(i in 1:nrow(yolo)){
	yolo$Survey[i] <- unlist(strsplit(
		unlist(strsplit(yolo$image_path_original[i],'Replicate_'))[2],
		'/', fixed=TRUE))[1]
	yolo$Camera[i] <- unlist(strsplit(yolo$image_path_original[i],'/', fixed=TRUE))[5]
	yolo$FileName[i] <- unlist(strsplit(
		unlist(strsplit(yolo$image_path_original[i],'/', fixed=TRUE))[6],
		'.', fixed=TRUE))[1]
}
head(yolo)
yolo <- yolo[,-which(colnames(yolo)=='image_path_local')]

# note 2017-10-03, Alison's survey, ended up with only one with-brant photo in the eval set (maybe because it had no annotations--so Dan couldn't subsample those with geese). I'm going to drop it from the eval set, but then add in the full run from that survey to compare with the others (ONLY when we're comparing surveys; not with surveys pooled):
yolo <- yolo[which(yolo$Survey!='2017-10-03'),]
ayolo <- read.csv('00 YOLO validation/eval_2017-10-03_YOLO_counts.csv',header=TRUE)
# remove Out_lagoon (they will mess up strsplit below, and we didn't check them):
ayolo <- ayolo[-grep('Out_lagoon', ayolo$image_path_drive_relative),]
colnames(ayolo)[1] <- colnames(yolo)[1]
ayolo$Survey <- '2017-10-03'
ayolo$FileName <- ayolo$Camera <- NA
for(i in 1:nrow(ayolo)){
	ayolo$Camera[i] <- unlist(strsplit(ayolo$image_path_original[i],'/', fixed=TRUE))[5]
	ayolo$FileName[i] <- unlist(strsplit(
		unlist(strsplit(ayolo$image_path_original[i],'/', fixed=TRUE))[6],
		'.', fixed=TRUE))[1]
}	# Several sec

head(ayolo)
yolo <- rbind(yolo, ayolo)


# must all be false:
any(is.na(yolo$FileName))
any(is.na(yolo$Camera))
any(is.na(yolo$Survey))
any(!(yolo$FileName %in% dat$FileName))	# this won't necessarily catch everything because it's only by file name, not file nested within survey. BUT should not be TRUE, so check if it is:

yolo[which(!(yolo$FileName %in% dat$FileName)),]
# these must have been a few surplus, like maybe pre/post survey, that we didn't run through CountThings. Remove so they don't trip anything up:
yolo <- yolo[which(yolo$FileName %in% dat$FileName),]


# filter out dat that don't have matching yolo to speed processes below:
nrow(dat)
dat <- dat[which(dat$FileName %in% yolo$FileName),]
nrow(dat)
# this does not filter out everything--file names can be duplicated across surveys, and wouldn't necessarily be in yolo for more than one of those surveys, so those extras will be kept here. But this still drastically reduces the size of dat and thus runtime. Will filter the rest below.


## compile yolo with dat:
thresh <- sort(unique(yolo$confidence_threshold))
sps <- c('Brant','Canada','Brant+Canada')	# Alison didn't count gulls, and we don't care about Other or Emperor here

dat[,apply(expand.grid('YOLO', sps, thresh), 1, paste, collapse="_")] <- NA


i <- 1 # to test (is in yolo)
for(i in 1:nrow(dat)){
	myyo <- yolo[which(yolo$FileName==dat$FileName[i] & yolo$Survey==dat$Survey[i] & yolo$Camera==dat$Camera[i]),]
	if(nrow(myyo)>0){
		for(j in 1:length(thresh)){
			for(k in 1:(length(sps)-1)){
				dat[i, which(colnames(dat)==paste0('YOLO_',sps[k],'_',thresh[j]))] <- 
					myyo[which(myyo$confidence_threshold==thresh[j]), 
						which(colnames(myyo)==paste0('count_',tolower(sps[k])))]
			}
		}
	}
	pbar(i, nrow(dat), 0.1)
} # few min (with full 2017-10-03)

# remove where yolo is NA (was not run through yolo):
nrow(dat)
dat <- dat[which(!is.na(dat$YOLO_Brant_0.4)),]
nrow(dat)
# Dan ran 2000 but this is slightly fewer (1774) because I've dropped Out_lagoon and a few that we did not manually count

## BUT ALSO we need to drop anything without a manual count, because we don't know what truth is there (it is "probably" zero):
length(which(dat$YOLO_Brant_0.7>0 & is.na(dat$Manual_Brant)))
# those 23 would be worth checking; YOLO thinks there are brant that CountThings missed.
length(which(is.na(dat$Manual_Brant)))	# nearly half of the YOLO-checked images do not have manual counts...
# But we really can't use those NAs when evaluating YOLO:
dat <- dat[which(!is.na(dat$Manual_Brant)),]
nrow(dat)



length(which(dat$YOLO_Brant_0.8>0))
length(which(dat$YOLO_Brant_0.8==0 & dat$Manual_Brant>0))	# yolo's false negatives
length(which(dat$YOLO_Brant_0.8>0 & dat$Manual_Brant==0))	# yolo's false positives
	# note the above does not address over/under-counting (will explore below)
	# note this is only at one threshold... see plot below for others
# higher threshold = more false negatives, fewer false positives. The threshold is reflecting confidence, so high confidence = must be very certain there are geese in the image, i.e. it'll fail to indicate those it isn't sure about. It only counts those it's sure about.

##########################

## Set up columns for the two brown geese combined, for thresholds 0.6 and 0.7:
dat$'Auto_Brant+Canada' <- dat$Auto_Brant + dat$Auto_Canada
dat$'Manual_Brant+Canada' <- dat$Manual_Brant + dat$Manual_Canada
dat[,apply(expand.grid('YOLO',sps[3], thresh),1,paste, collapse='_')] <- NA
for(j in 1:length(thresh)){
	dat[,which(colnames(dat)==paste0('YOLO_Brant+Canada_', thresh[j]))] <- dat[,which(colnames(dat)==paste0('YOLO_Brant_', thresh[j]))] + dat[,which(colnames(dat)==paste0('YOLO_Canada_', thresh[j]))]
}
head(dat)


dat[,paste0('propYOLO_Brant_', thresh)] <- NA
dat[,paste0('propYOLO_Canada_', thresh)] <- NA
dat[,paste0('propYOLO_Brant+Canada_', thresh)] <- NA

for(j in 1:length(thresh)){

	dat[, which(colnames(dat)==paste0('propYOLO_Brant_', thresh[j]))] <- 
		dat[, which(colnames(dat)==paste0('YOLO_Brant_', thresh[j]))] /
		dat$Manual_Brant
	
	dat[, which(colnames(dat)==paste0('propYOLO_Canada_', thresh[j]))] <- 
		dat[, which(colnames(dat)==paste0('YOLO_Canada_', thresh[j]))] /
		dat$Manual_Canada

	dat[, which(colnames(dat)==paste0('propYOLO_Brant+Canada_', thresh[j]))] <- 
		dat[, which(colnames(dat)==paste0('YOLO_Brant+Canada_', thresh[j]))] /
		dat$'Manual_Brant+Canada'
}	

head(dat)

# Note anything with manual = 0 will be NaN (if yolo=0) or Inf (if yolo>0) for the proportion. Make NA instead so we can drop them from consideration below:
for(i in 1:nrow(dat)){ for(j in 1:ncol(dat)){
	if(is.nan(dat[i,j])) dat[i,j] <- NA
	if(!is.na(dat[i,j])) if(dat[i,j]==Inf) dat[i,j] <- NA
}
pbar(i, nrow(dat))
}
head(dat,20)
	
range(dat$propYOLO_Brant_0.6, na.rm=TRUE)
range(dat$propYOLO_Canada_0.6, na.rm=TRUE)
range(dat$'propYOLO_Brant+Canada_0.6', na.rm=TRUE)

range(dat$propYOLO_Brant_0.7, na.rm=TRUE)
range(dat$propYOLO_Canada_0.7, na.rm=TRUE)
range(dat$'propYOLO_Brant+Canada_0.7', na.rm=TRUE)



#####################
## For those with manual counts, check manual vs each threshold of yolo:
	# EXCLUDE ALISON'S SURVEY HERE because we already evaluated that, and it has a zillion more photos (that yolo evaluated) than any other survey.
datnoa <- dat[which(dat$Survey!='2017-10-03'),]
# False positives/negatives for each taxon:
falsepn <- data.frame(Taxon=sps)
falsepn[,c(paste0('Falsepos_', thresh), paste0('Falseneg_',thresh))] <- NA
for(i in 1:(length(sps)-1)){
	for(j in 1:length(thresh)){
		# false pos:
		falsepn[i,j+1] <- length(
			which(datnoa[,which(colnames(datnoa)==paste0('Manual_', sps[i]))]==0 & 
				datnoa[,which(colnames(datnoa)==paste0('YOLO_', sps[i], '_', thresh[j]))] > 0)) /
				length(which(datnoa[,which(colnames(datnoa)==paste0('Manual_', sps[i]))]==0))
		# false neg:
		falsepn[i,j+6] <- length(
			which(datnoa[,which(colnames(datnoa)==paste0('Manual_', sps[i]))]>0 & 
				datnoa[,which(colnames(datnoa)==paste0('YOLO_', sps[i], '_', thresh[j]))]==0)) /
				length(which(datnoa[,which(colnames(datnoa)==paste0('Manual_', sps[i]))]>0))
}}

	
#### BUT Instead of the above by species, what's false in the context of *any* brown geese? We can correct misIDs more easily than missed counts.
i <- nrow(falsepn)
for(j in 1:length(thresh)){
	# false pos: how many have none manually detected, but YOLO detected either species, out of all photos with none manually detected:
	falsepn[i,j+1] <- length(which(
			(datnoa$Manual_Brant + datnoa$Manual_Canada)==0 & 
			# YOLO detected brant or canada:
			(datnoa[,which(colnames(datnoa)==paste0('YOLO_Brant_', thresh[j]))] + datnoa[,which(colnames(datnoa)==paste0('YOLO_Canada_', thresh[j]))]) > 0)) /
			length(which((datnoa$Manual_Brant + datnoa$Manual_Canada)==0))
	# false neg:
	falsepn[i,j+6] <- length(which(
			(datnoa$Manual_Brant + datnoa$Manual_Canada)>0 & 
			# YOLO detected brant or canada:
			(datnoa[,which(colnames(datnoa)==paste0('YOLO_Brant_', thresh[j]))] + datnoa[,which(colnames(datnoa)==paste0('YOLO_Canada_', thresh[j]))]) == 0)) /
			length(which((datnoa$Manual_Brant + datnoa$Manual_Canada)>0))
}
falsepn

dev.new(width=8, height=4)
par(mfrow=c(1,3), oma=c(3,3,0.1,0.1), mai=rep(0.1,4))
for(i in 1:nrow(falsepn)){
	plot(as.numeric(falsepn[i,2:6]), type='l', col='dark green', lwd=2, ylim=c(0,1), xaxt='n', yaxt='n')
	lines(as.numeric(falsepn[i, 7:11]), col='dark orange', lwd=2)
	axis(1, at=1:5, labels=thresh)
	axis(2, at=seq(0,1,0.2), labels=(i %in% c(1,4)))
	mtext(sps[i], side=3, line=-1.5)
	if(i==1) legend(x='topleft', legend=c('False positive', 'False negative'), col=c('dark green', 'dark orange'), lwd=2, bty='n')
}
## Note these are all relative to only the photos we manually checked, i.e. CountThings didn't detect any geese. Thus we can't tell the YOLO error rate for photos where we're pretty sure there's nothing. Over all those photos, the false negative (and false positive?) rate would likely be lower than shown here.
	# And indeed, both are higher here than for Alison's survey where we can do a full comparison. But Alison's survey was also good quality. So the true error rates may be somewhere between hers and this plot.
	# but we similarly see that huge jump around threshold 0.7.
	



####################################################
## Proportion counted (or missed) for each taxon and combined:



### show the prop counted by YOLO by threshold:

pc <- data.frame(Threshold=thresh, Prop_Brant=NA, Prop_Canada=NA)

for(i in 1:nrow(pc)){
	pc$Prop_Brant[i] <- sum(datnoa[,which(colnames(datnoa)==paste0('YOLO_Brant_', thresh[i]))])/sum(datnoa$Manual_Brant)
	pc$Prop_Canada[i] <- sum(datnoa[,which(colnames(datnoa)==paste0('YOLO_Canada_', thresh[i]))])/sum(datnoa$Manual_Canada)
	pc$'Prop_Brant+Canada'[i] <- sum(datnoa[,which(colnames(datnoa)==paste0('YOLO_Brant+Canada_', thresh[i]))])/sum(datnoa$'Manual_Brant+Canada')
	
	# calculate SE for photos where possible, using conservative sample size:
	b <- datnoa[,which(colnames(datnoa)==paste0('YOLO_Brant_', thresh[i]))]/datnoa$Manual_Brant
	b <- b[which(!is.na(b) & b!=Inf)]
	pc$SE_Brant[i] <- sd(b, na.rm=TRUE)/sqrt(length(b))
	
	cc <- datnoa[,which(colnames(datnoa)==paste0('YOLO_Canada_', thresh[i]))]/datnoa$Manual_Canada
	cc <- cc[which(!is.na(cc) & cc!=Inf)]
	pc$SE_Canada[i] <- sd(cc, na.rm=TRUE)/sqrt(length(cc))
	
	x <- datnoa[,which(colnames(datnoa)==paste0('YOLO_Brant+Canada_', thresh[i]))]/(datnoa$'Manual_Brant+Canada')
	x <- x[which(!is.na(x) & x!=Inf)]
	pc$'SE_Brant+Canada'[i] <- sd(x, na.rm=TRUE)/sqrt(length(x))
}
pc

## add a column for CountThings, with blank row between:
pc <- rbind(pc, c('',rep(0, ncol(pc)-1)))
pc <- rbind(pc, c('CT',rep(0, ncol(pc)-1)))
i <- nrow(pc)
pc$Prop_Brant[i] <- sum(datnoa$Auto_Brant)/sum(datnoa$Manual_Brant)
pc$Prop_Canada[i] <- sum(datnoa$Auto_Canada)/sum(datnoa$Manual_Canada)
pc$'Prop_Brant+Canada'[i] <- sum(datnoa$Auto_Canada + datnoa$Auto_Brant)/sum(datnoa$Manual_Canada + datnoa$Manual_Brant)
# calculate SD for photos where possible, and use conservative sample size:
b <- datnoa$Auto_Brant/datnoa$Manual_Brant
b <- b[which(!is.na(b) & b!=Inf)]
pc$SE_Brant[i] <- sd(b, na.rm=TRUE)/sqrt(length(b))
cc <- datnoa$Auto_Canada/datnoa$Manual_Canada
cc <- cc[which(!is.na(cc) & cc!=Inf)]
pc$SE_Canada[i] <- sd(cc, na.rm=TRUE)/sqrt(length(cc))
x <- (datnoa$Auto_Canada + datnoa$Auto_Brant)/(datnoa$Manual_Canada + datnoa$Manual_Brant)
x <- x[which(!is.na(x) & x!=Inf)]
pc$'SE_Brant+Canada'[i] <- sd(x, na.rm=TRUE)/sqrt(length(x))
pc
for(j in 2:ncol(pc)) pc[,j] <- as.numeric(pc[,j])


## PLOT:

dev.new(width=4, height=8); ylims=c(0,1.8)
par(mfrow=c(3,1), oma=c(3,3,0.1,0.1), mai=c(0.1,0.1,0.1,0.1), cex=1, mgp=c(3,0.7,0))
barplot(pc$Prop_Brant, space=0, las=1, xaxt="n", yaxs="i", ylim=ylims); box()
plotCI(x=1:nrow(pc)-0.5, y=pc$Prop_Brant, liw=NA, uiw=1.96*pc$SE_Brant, gap=0, add=TRUE, sfrac=0, pch=NA)
axis(side=1, at=1:nrow(pc)-0.5, labels=F, las=2)
mtext(" a) Pacific brant", side=3, line=-1.1, adj=0)
lines(c(-10,10), c(1,1), lty=2, col=cols[4])

barplot(pc$Prop_Canada, names=pc$Threshold, space=0, las=1, xaxt="n", yaxs="i", ylim=ylims); box()
plotCI(x=1:nrow(pc)-0.5, y=pc$Prop_Canada, liw=NA, uiw=1.96*pc$SE_Canada, gap=0, add=TRUE, sfrac=0, pch=NA)
axis(side=1, at=1:nrow(pc)-0.5, labels=F, las=2)
mtext(" b) Cackling goose", side=3, line=-1.1, adj=0)
lines(c(-10,10), c(1,1), lty=2, col=cols[4])
mtext("Proportion of true geese automatically counted (95% CI)", side=2, line=2.5)

barplot(pc$'Prop_Brant+Canada', names=pc$Threshold, space=0, las=1, xaxt="n", yaxs="i", ylim=ylims); box()
plotCI(x=1:nrow(pc)-0.5, y=pc$'Prop_Brant+Canada', liw=NA, uiw=1.96*pc$'SE_Brant+Canada', gap=0, add=TRUE, sfrac=0, pch=NA)
axis(side=1, at=1:nrow(pc)-0.5, labels=pc$Threshold)
mtext('YOLO confidence threshold', side=1, line=2, at=2.5)
mtext(" c) Both species pooled", side=3, line=-1.1, adj=0)
lines(c(-10,10), c(1,1), lty=2, col=cols[4])


## Compared to Alison's survey, YOLO underestimates more across all surveys pooled. This could EITHER be a per-survey situation (Alison's was good quality), OR a result of here not evaluating the photos that CT said had no geese (perhaps YOLO is picking up false positives, especially CACG, in those images and that's why the overall counts were higher in Alison's survey... BUT the overall false positive rate was considerably lower in Alison's survey... so the discrepancy is probably due to miscounting geese within the photos that have geese).


############################
## What about by survey? Will reveal whether that's a per-survey quality thing, or perhaps Alison's survey was unusually accurate. DO show Alison's survey here.

dev.new(width=13, height=6)
par(mfrow=c(length(sps),length(survs)), oma=c(3,3,1.2,1.2), mai=c(0.1,0.1,0.1,0.1), cex=1, mgp=c(3,0.7,0))
survs
k <- 1
for(m in 1:length(sps)){
for(k in 1:length(survs)){

	survdat <- dat[which(dat$Survey==survs[k]),]
	pc <- data.frame(Threshold=thresh, Prop=NA)

	for(i in 1:nrow(pc)){
		pc$Prop[i] <- sum(survdat[,which(colnames(survdat)==paste0('YOLO_',sps[m],'_', thresh[i]))])/sum(survdat[,which(colnames(survdat)==paste0('Manual_', sps[m]))])
		# calculate SE for photos where possible, using conservative sample size:
		b <- survdat[,which(colnames(survdat)==paste0('YOLO_', sps[m],'_', thresh[i]))]/survdat[,which(colnames(survdat)==paste0('Manual_', sps[m]))]
		b <- b[which(!is.na(b) & b!=Inf)]
		pc$SE[i] <- sd(b, na.rm=TRUE)/sqrt(length(b))
	}
	pc

	## add a column for CountThings, with blank row between:
	pc <- rbind(pc, c('',rep(0, ncol(pc)-1)))
	pc <- rbind(pc, c('CT',rep(0, ncol(pc)-1)))
	i <- nrow(pc)
	pc$Prop[i] <- sum(survdat[,which(colnames(survdat)==paste0('Auto_', sps[m]))])/sum(survdat[,which(colnames(survdat)==paste0('Manual_', sps[m]))])
	# calculate SD for photos where possible, and use conservative sample size:
	b <- survdat[,which(colnames(survdat)==paste0('Auto_',sps[m]))]/survdat[,which(colnames(survdat)==paste0('Manual_',sps[m]))]
	b <- b[which(!is.na(b) & b!=Inf)]
	pc$SE[i] <- sd(b, na.rm=TRUE)/sqrt(length(b))
	pc
	for(j in 2:ncol(pc)) pc[,j] <- as.numeric(pc[,j])


	## PLOT:

	barplot(pc$Prop, space=0, las=1, xaxt="n", yaxt='n', yaxs="i", ylim=c(0,1.8)); box()
	plotCI(x=1:nrow(pc)-0.5, y=pc$Prop, liw=NA, uiw=1.96*pc$SE, gap=0, add=TRUE, sfrac=0, pch=NA)
	axis(side=1, at=1:nrow(pc)-0.5, labels=F)
	if(m==length(sps)) axis(side=1, at=1:nrow(pc)-0.5, labels=c(thresh, '', 'CT'),cex.axis=0.8)
	axis(side=2, at=seq(0,2,0.5), labels=(k==1), las=1,cex=0.8)
	lines(c(-10,10), c(1,1), lty=2, col=cols[4])
	
	if(m==1) mtext(survs[k], side=3, line=0.1)
	if(k==length(survs)) mtext(sps[m], side=4, line=0.1, las=3)
	
#	mtext(paste0(" a) ", sps[m], '  ', survs[k]), side=3, line=-1.1, adj=0)
	if(k==1 & m==2) mtext("Proportion of true geese automatically counted (95% CI)", side=2, line=2.5)
	if(m==3 & k==5) mtext('YOLO confidence threshold or CountThings', side=1, line=2)
}}


#####################
#########################################################################
## Histograms of image-level errors:
	# Above we saw there aren't huge differences across surveys for brant at most thresholds (or at least 0.4-0.6). We also don't have large sample sizes for the number of images with both manual counts and YOLO. So here we'll pool surveys. EXCLUDE Alison's because it has a huge number of photos. We'll do this for only that survey separately below.

# show only a subset for clarity:
thresh2 <- thresh[c(2:4)]
cols2 <- cols[c(1,3,5)]

dev.new(width=8, height=8)
par(mfrow=c(length(sps),1), oma=c(3,3,1.2,1.2), mai=c(0.1,0.1,0.1,0.1), cex=1, mgp=c(3,0.7,0), xpd=FALSE)
survs
m <- i <- 1
for(m in 1:length(sps)){

	# set up empty plot:
	plot(x=0, y=0, type='n', xaxt="n", yaxt='n', yaxs="i", xaxs="i", ylim=c(0,1), xlim=c(0,5), col=cols2[i])
	axis(side=1, at=seq(0,27,1), labels=(m==length(sps)))
	axis(side=2, at=seq(0,1,0.2), labels=T, las=1,cex=0.8)
	lines(c(1,1), c(0,1), lty=1, col='dark orange')
	if(m==1) legend(x='topright', legend=thresh2, lwd=2, col=cols2, bty='n', x.intersp=0.5)
	for(i in 1:length(thresh2)){
	
		sptdat <- datnoa[,which(colnames(datnoa)==paste0('propYOLO_',sps[m],'_', thresh2[i]))]
		print(c(sps[m], thresh2[i], max(sptdat, na.rm=TRUE)))

		h <- hist(sptdat, breaks=seq(0,27,0.1), plot=FALSE)
		
		## cut off the histo tail when there are no large counts so we can see the color that corresponds to large counts:
		lines(x=h$breaks[1:max(which(h$counts!=0))], y=(h$counts/sum(h$counts, na.rm=TRUE))[1:max(which(h$counts!=0))], lwd=2, col=cols2[i])
	
	}
	mtext(sps[m], side=3, line=-1.1)
	
	if(m==2) mtext("Proportion of photos", side=2, line=2.5)
	if(m==3) mtext('Proportion of true geese automatically counted', side=1, line=2)
}
# note values go up to 26 but I've truncated the plots for legibility
# note the above can include only photos that truly have geese
# the below includes photos without geese


## Same as above, except here show numerical difference rather than proportion (hopefully we're rarely miscounting by a large magnitude):

dev.new(width=8, height=8)
par(mfrow=c(length(sps),1), oma=c(3,3,1.2,1.2), mai=c(0.1,0.1,0.1,0.1), cex=1, mgp=c(3,0.7,0), xpd=FALSE)
survs
m <- i <- 1
for(m in 1:length(sps)){

	# set up empty plot:
	plot(x=0, y=0, type='n', xaxt="n", yaxt='n', yaxs="i", xaxs="i", ylim=c(0,1), xlim=c(-300,300), col=cols2[i])
	axis(side=1, at=seq(-300,300,100), labels=(m==length(sps)))
	axis(side=2, at=seq(0,1,0.2), labels=T, las=1,cex=0.8)
	lines(c(1,1), c(0,1), lty=1, col='dark orange')
	if(m==1) legend(x='topright', legend=thresh2, lwd=2, col=cols2, bty='n', x.intersp=0.5)
	for(i in 1:length(thresh2)){
	
		sptdat <- datnoa[,which(colnames(datnoa)==paste0('YOLO_',sps[m],'_', thresh2[i]))] - datnoa[,which(colnames(datnoa)==paste0('Manual_',sps[m]))]
		print(c(sps[m], thresh2[i], range(sptdat, na.rm=TRUE)))

		h <- hist(sptdat, breaks=seq(-3000,300,10), plot=FALSE)
		
		lines(x=h$breaks[-length(h$breaks)], y=(h$counts/sum(h$counts, na.rm=TRUE)), lwd=2, col=cols2[i])
	
	}
	mtext(sps[m], side=3, line=-1.1)
	
	if(m==2) mtext("Proportion of photos", side=2, line=2.5)
	if(m==3) mtext('Difference in geese counted (auto - manual)', side=1, line=2)
}
# note the difference spans -2211 to 205 but I've cut off the bottom for legibility. 
# Brant and combined are centered near-ish 0, which is good. Canadas are consistently undercounted regardless of the threshold.
# There are cases where YOLO misses hundreds or thousands at a time. Will want to look into those:
datnoa[which((datnoa$YOLO_Brant_0.6 - datnoa$Manual_Brant) < -300),]
datnoa[which((datnoa$YOLO_Canada_0.6 - datnoa$Manual_Canada) < -100),]
#datnoa[which((datnoa$'YOLO_Brant+Canada_0.6' - datnoa$'Manual_Brant+Canada') < -300),]	# same as the brant list above
# note none of these are false negatives, and the missing geese are sometimes a small fraction of the (very large) number present. So these could still be accounted for by a correction factor.

# overall I would say 0.6 for brant and 0.5 for Canada or brant+canada. Next script will evaluate correction factor given proportion manually corrected.
