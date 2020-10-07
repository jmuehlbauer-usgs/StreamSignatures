##### Signatures function to create stream signatures from data #####
	## Last modified 2020-10-01 by J.D. Muehlbauer
	
	
## Function computes 50 and 10% stream signatures from model data
	## Assumes both response (effect size) and distance are log-transformed
		## (i.e., a power curve). Will not work properly otherwise.
	## dat argument is the dataframe. 
		## The remaining arguments are column names found in this dataframe.
	## effVar argument is the response variable.
	## distVar argument is the distance variable.
	## siteVar argument is the site random effect variable.
	## otherVars are any other variables to be fit by the model.
		## These must be factors (not continuous variables).
	
signatures <- function(dat, effVar, distVar, siteVar, otherVars){
	## Load required libraries
	if(!'packload' %in% lsf.str()){
		source('https://github.com/jmuehlbauer-usgs/R-packages/blob/master/packload.r?raw=TRUE')
	}
	packload(c('MuMIn', 'lme4'))
	## Make variables
	perc <- dat[, effVar]
	dstn <- dat[, distVar]
	strm <- dat[, siteVar]
	## Add predictors to intercept and slope & intercept
	out0 <- lmer(perc ~ dstn + (1 | strm), REML = FALSE)
	l1 <- list()
	for(i in 1 : length(otherVars)){
		if(class(dat[, otherVars[i]]) != 'factor'){
			warning(paste0('Coercing variable "', otherVars[i], '" to a factor.\n  If the function crashes or errors out, consider removing this variable.'))
			dat[, otherVars[i]] <- as.factor(dat[, otherVars[i]])
		}
		vble <- dat[, otherVars[i]]
		l1[[i]] <- lmer(perc ~ dstn + vble + (1 | strm), REML = FALSE)
		l1[[i + length(otherVars)]] <- lmer(perc ~ dstn * vble + (1 | strm), REML = FALSE)
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
		out1[1, c('PseudoR2', 'LikRatio', 'p')] <- NA
			## Anova p is from likelihood ratio test vs. the distance only model.
		out1$delta <- out1$AICc - min(out1$AICc)
		out1$weight <- round(exp(-0.5 * out1$delta) / sum(exp(-0.5 * out1$delta)), 4)
		out1 <- out1[order(-out1$AICc),]
	## Subset only the variables that improve over the base (Distance-only) model
	l3 <- l2[names(l2) %in% c('Overall', rownames(out1)[which(rownames(out1) == 'Distance') : dim(out1)[1]])]
	## Get confidence intervals
	l4 <- lapply(l3, function(x){
		c1 <- suppressMessages(confint(x, level = 0.5)[c(-1, -2),])
		e1 <- fixef(x)
		c2 <- cbind(c1[, 1], e1, c1[, 2])
			colnames(c2) <- c('Lower', 'Estimate', 'Upper')
		return(c2)
	})
	## Compute signatures for these variable conditions
	ss <- c(0.5, 0.1)
	ao <- c(0, 3)
	l5 <- list()
	for(i in 1 : length(l4)){
		t1 <- l4[[i]]
		l5[[i]] <- as.data.frame(list())
		if(i == 1){
			t2 <- 'Overall'
		} else {
			t2 <- levels(droplevels(dat[,substr(names(l4),2,100)[i]]))
		}
		for(j in 1:2){
			for (k in 1:3){
				for(l in 1 : length(t2)){
					if(l == 1){
						t3 <- exp((log(ss[j]) - t1[1, k]) / t1[2, k])
					} else {
						if(substr(names(l4)[i], 1, 1) == 'p'){
							t3 <- exp((log(ss[j]) - (t1[1, k] + t1[l + 1, k])) / t1[2, k])
						} else if(substr(names(l4)[i], 1, 1) == 'x'){
							t3 <- exp((log(ss[j]) - (t1[1, k] + t1[l + 1, k])) / 
								(t1[2, k] + t1[l + length(t2), k]))
						}
					}
					l5[[i]][l, ao[j] + k] <- t3
				}
				rownames(l5[[i]]) <- t2
			}
		}
		colnames(l5[[i]]) <- paste0(rep(c('Low', 'Est', 'Up'), 2), rep(ss * 100, c(3, 3)))
		l5[[i]] <- round(l5[[i]], 1)
	}
	names(l5) <- names(l4)
	list(fits = out1, sigs = l5)
}