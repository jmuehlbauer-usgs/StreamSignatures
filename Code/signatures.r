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
	packload('MuMIn', 'lme4')
	## Make variables
	perc <- dat[, effVar]
	dstn <- dat[, distVar]
	strm <- dat[, siteVar]
	## Add predictors to intercept and slope & intercept
	out0 <- lmer(perc ~ dstn + (1 | strm), REML = FALSE)
	l1 <- list()
	for(i in 1 : length(otherVars)){
		variable <- dat[, otherVars[i]]
		l1[[i]] <- lmer(perc ~ dstn + variable + (1 | strm), REML = FALSE)
		l1[[i + length(otherVars)]] <- lmer(perc ~ dstn * variable + (1 | strm), REML = FALSE)
	}
		names(l1) <- c(paste0('p', otherVars), paste0('x', otherVars))
	l2 <- c(out0, l1)
		names(l2)[1] <- 'Overall'
	var0 <- VarCorr(out0)[[1]][[1]]
	out1 <- data.frame(t(sapply(l2, function(x){
		pR <- (var0 - VarCorr(x)[[1]][[1]]) / var0
		a1 <- anova(out0, x)[2, c(6, 8, 1)]
		a2 <- as.numeric(round(AICc(x), 2))
		unlist(round(cbind(pR, a1, a2), 4))
	})))
		rownames(out1)[1] <- 'Distance'
		colnames(out1) <- c('PseudoR2', 'LikRatio', 'p', 'DF', 'AICc')
			## Anova p is from likelihood ratio test vs. the distance only model.
		out1$delta <- out1$AICc - min(out1$AICc)
		out1$weight <- round(exp(-0.5 * out1$delta) / sum(exp(-0.5 * out1$delta)), 4)
		out1 <- out1[order(-out1$AICc),]


##### Get 50% and 10% distances and confidence intervals from out1 results #####

	## Subset only the variables that improve over the base (Distance-only) model
	l3 <- l2[names(l2) %in% c('Overall', rownames(out1)[which(rownames(out1) == 'Distance') : dim(out1)[1]])]
	## Get confidence intervals
	l4<-list()
	for(i in 1:length(l3)){
	l4[[i]]<-intervals(l3[[i]],level=.5)
	}
	names(l4)<-names(l3)
	l4 <- lapply(l3, confint, level = 0.5)

## Compute signatures for these variable conditions
percents<-c(.5,.1)
addon<-c(0,3)
sigs<-list()
for(i in 1:length(l4)){
	sigs[[i]]<-as.data.frame(list())
		for(k in 1:2){for (l in 1:3){
			sigs[[i]][1,addon[k]+l]<-exp((log(percents[k])-l4[[i]]$fixed[1,'est.'])/(l4[[i]]$fixed[2,l]))
			}}
		if(substr(names(l4)[i],1,1)=='x'){
			if(is.numeric(dat[,substr(names(l4)[[i]],2,100)])){
				for(k in 1:2){for (l in 1:3){
					sigs[[i]][1,addon[k]+l]<-exp((log(percents[k])-l4[[i]]$fixed[1,'est.']-l4[[i]]$fixed[3,'est.'])/(l4[[i]]$fixed[2,'est.']+l4[[i]]$fixed[4,l]))
					}}}			
			else{
				for(j in 1:(dim(l4[[i]]$fixed)[1]/2-1)){for(k in 1:2){for (l in 1:3){
					sigs[[i]][j+1,addon[k]+l]<-exp((log(percents[k])-l4[[i]]$fixed[1,'est.']-l4[[i]]$fixed[j+2,'est.'])/(l4[[i]]$fixed[2,'est.']+l4[[i]]$fixed[j+2+(length(l4[[i]]$fixed[,l])-2)/2,l]))
					}}}}}
		if(substr(names(l4)[i],1,1)=='p'){
			if(is.numeric(dat[,substr(names(l4)[[i]],2,100)])){
				for(k in 1:2){for (l in 1:3){
					sigs[[i]][1,addon[k]+l]<-exp((log(percents[k])-l4[[i]]$fixed[1,'est.']-l4[[i]]$fixed[3,'est.'])/l4[[i]]$fixed[2,l])
					}}}
			else{
				for(j in 1:(dim(l4[[i]]$fixed)[1]-2)){for(k in 1:2){for (l in 1:3){			
					sigs[[i]][j+1,addon[k]+l]<-exp((log(percents[k])-l4[[i]]$fixed[1,'est.']-l4[[i]]$fixed[j+2,'est.'])/l4[[i]]$fixed[2,l])
					}}}}}
		colnames(sigs[[i]])<-c('Lower CI 50%','50%','Upper CI 50%','Lower CI 10%','10%','Upper CI 10%')
		if(names(l4)[i]=='Overall'||names(l4)[i]=='pWidth'||names(l4)[i]=='xWidth'){rownames(sigs[[i]])<-''}
		else{rownames(sigs[[i]])<-levels(droplevels(dat[,substr(names(l4),2,100)[i]]))}
	sigs[[i]]<-signif(round(sigs[[i]],2))
		}
	names(sigs)<-c('Overall',names(l4[-1]))
final<-list()
	final$fits<-out1
	final$sigs<-sigs
final
}