##### Signatures function to create stream signatures from data #####
	## Last modified 2020-02-12 by J.D. Muehlbauer
	
	
## Function computes 50 and 10% stream signatures from model data
	## dat argument is the dataframe. 
		## The remaining arguments are column names found in this dataframe.
	## effVar argument is the response variable.
	## distVar argument is the distance variable.
	## siteVar argument is the site random effect variable.
	## otherVars are any other variables to be fit by the model
	
signatures <- function(dat, effVar, distVar, siteVar, otherVars){
	## Load required libraries
	require('MuMIn')
	require('lme4')
	## Make variables
	perc <- dat[, effVar]
	dstn <- dat[, distVar]
	strm <- dat[, siteVar]
	## Add predictors to intercept and slope & intercept
	out0 <- lmer(perc ~ dstn + (1 | strm), REML = FALSE)
	slist <- list()
	for(i in 1 : length(otherVars)){
		variable <- dat[, otherVars[i]]
		slist[[i]] <- lmer(perc ~ dstn + variable + (1 | strm), REML = FALSE)
		slist[[i + length(otherVars)]] <- lmer(perc ~ dstn * variable + (1 | strm), REML = FALSE)
	}
		names(slist) <- c(paste0('p', otherVars), paste0('x', otherVars))
	alist <- c(out0, slist)
	var0 <- VarCorr(out0)[[1]][[1]]
	out1 <- t(sapply(alist, function(x){
		pR <- (var0 - VarCorr(x)[[1]][[1]]) / var0
		a1 <- anova(out0, x)[2, c(6, 8, 1)]
		a2 <- round(AICc(x), 2)
		round(cbind(pR, a1, a2), 4)
	}))
		rownames(out1)[1] <- 'Distance'
		colnames(out1) <- c('PseudoR2', 'LikRatio', 'p', 'DF', 'AICc')
			## Anova p is from likelihood ratio test vs. the distance only model.
		out1$delta <- c(out1$AICc - min(out1$AICc))
		out1$weight <- round(exp(-0.5 * out1$delta) / sum(exp(-0.5 * out1$delta)), 4)
		out1 <- out1[order(-out1$AICc),]


##### Get 50% and 10% distances and confidence intervals from out1 results #####

## Subset only the variables of interest
lme.AICcs<-as.data.frame(1,nrow=1,ncol=1)
	for(i in 1:length(slist)){
		lme.AICcs[i,1]<-AICc(slist[[i]])}
	colnames(lme.AICcs)<-c('AICc')
	rownames(lme.AICcs)<-names(slist)
	lme.AICcs$Keepers<-ifelse(lme.AICcs$AIC<AICc(out0),'Keep','Toss')
sub.vars<-rownames(subset(lme.AICcs,Keepers=='Keep'))
sub.singles.list1<-list()
	sub.singles.list1$Overall<-out0
sub.singles.list<-c(sub.singles.list1,slist[sub.vars])

## Get confidence intervals
sub.singles.CI<-list()
for(i in 1:length(sub.singles.list)){
sub.singles.CI[[i]]<-intervals(sub.singles.list[[i]],level=.5)
	}
names(sub.singles.CI)<-names(sub.singles.list)

## Compute signatures for these variable conditions
percents<-c(.5,.1)
addon<-c(0,3)
sigs<-list()
for(i in 1:length(sub.singles.CI)){
	sigs[[i]]<-as.data.frame(list())
		for(k in 1:2){for (l in 1:3){
			sigs[[i]][1,addon[k]+l]<-exp((log(percents[k])-sub.singles.CI[[i]]$fixed[1,'est.'])/(sub.singles.CI[[i]]$fixed[2,l]))
			}}
		if(substr(names(sub.singles.CI)[i],1,1)=='x'){
			if(is.numeric(dat[,substr(names(sub.singles.CI)[[i]],2,100)])){
				for(k in 1:2){for (l in 1:3){
					sigs[[i]][1,addon[k]+l]<-exp((log(percents[k])-sub.singles.CI[[i]]$fixed[1,'est.']-sub.singles.CI[[i]]$fixed[3,'est.'])/(sub.singles.CI[[i]]$fixed[2,'est.']+sub.singles.CI[[i]]$fixed[4,l]))
					}}}			
			else{
				for(j in 1:(dim(sub.singles.CI[[i]]$fixed)[1]/2-1)){for(k in 1:2){for (l in 1:3){
					sigs[[i]][j+1,addon[k]+l]<-exp((log(percents[k])-sub.singles.CI[[i]]$fixed[1,'est.']-sub.singles.CI[[i]]$fixed[j+2,'est.'])/(sub.singles.CI[[i]]$fixed[2,'est.']+sub.singles.CI[[i]]$fixed[j+2+(length(sub.singles.CI[[i]]$fixed[,l])-2)/2,l]))
					}}}}}
		if(substr(names(sub.singles.CI)[i],1,1)=='p'){
			if(is.numeric(dat[,substr(names(sub.singles.CI)[[i]],2,100)])){
				for(k in 1:2){for (l in 1:3){
					sigs[[i]][1,addon[k]+l]<-exp((log(percents[k])-sub.singles.CI[[i]]$fixed[1,'est.']-sub.singles.CI[[i]]$fixed[3,'est.'])/sub.singles.CI[[i]]$fixed[2,l])
					}}}
			else{
				for(j in 1:(dim(sub.singles.CI[[i]]$fixed)[1]-2)){for(k in 1:2){for (l in 1:3){			
					sigs[[i]][j+1,addon[k]+l]<-exp((log(percents[k])-sub.singles.CI[[i]]$fixed[1,'est.']-sub.singles.CI[[i]]$fixed[j+2,'est.'])/sub.singles.CI[[i]]$fixed[2,l])
					}}}}}
		colnames(sigs[[i]])<-c('Lower CI 50%','50%','Upper CI 50%','Lower CI 10%','10%','Upper CI 10%')
		if(names(sub.singles.CI)[i]=='Overall'||names(sub.singles.CI)[i]=='pWidth'||names(sub.singles.CI)[i]=='xWidth'){rownames(sigs[[i]])<-''}
		else{rownames(sigs[[i]])<-levels(droplevels(dat[,substr(names(sub.singles.CI),2,100)[i]]))}
	sigs[[i]]<-signif(round(sigs[[i]],2))
		}
	names(sigs)<-c('Overall',names(sub.singles.CI[-1]))
final<-list()
	final$fits<-out1
	final$sigs<-sigs
final
}