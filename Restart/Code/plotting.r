##### Plotting function to plot multi-panel, multi-page stream signatures graphs #####
	## Last modified 2020-10-07 by J.D. Muehlbauer
	
	
## Function plots stream signatures data for each specified site type
	## Plots a 6-panel figure per page (2 columns, 3 rows).
	## Number of pages is dictated by number of sites.
	## Method is the type of data being plotted. Defaults to "Predator".
	## dat argument is the dataframe. 
	## effVar argument is the response variable (a column name in dat).
	## distVar argument is the distance variable (a column name in dat).
	## siteVar argument is the site random effect variable (a column name in dat).
	## yLab is the label placed on the y-axis. Defaults to the name of effVar.
	## nameAppend is any optional text to append to the filename.
	## filetype allows saving figures as pdf or png, or both. Defaults to both.
	
plotting<-function(Method, dat, distVar, effVar, siteVar, yLab = 'effVar', 
	nameAppend = '', filetype = c('pdf', 'png')){
	## Set parameters depending on Method
	if(yLab == 'effVar'){yLab <- effVar}
	if(Method == 'Non-Predator'){
		shortname <- as.factor(c('HeHemi','HeLepi','AeLepi','TeMisc'))
		fullname <- c('Hemipteran herbivores','Lepidoptera herbivores',
			'Other lepidoptera','Miscellaneous other insects')
	} else{
		shortname <- as.factor(c('PrBeet','PrSpHu','PrSpWe'))
		fullname <- c('Beetles','Hunting spiders','Webbed spiders')
	}
	pages1 <- seq(1, length(unique(dat[, siteVar])), 6)
	pages2 <- c(pages1[-1] - 1, length(unique(dat[, siteVar])))
		names(pages1) <- LETTERS[1:length(pages1)]
	## Conditions for png plotting
	if('png' %in% filetype){
		for(j in 1:length(pages1)){
			png(paste('FiguresTables/Raw/', Method, effVar, siteVar, nameAppend, 
				'_', names(pages1)[j], '.png', sep = ''), width = 6.5, 
				height = 9.75, res = 300, bg = 0, units = 'in')
			oldpar <- par(mfrow = c(3, 2), mar = c(2.5, 1.5, 0, 0),
				oma = c(2, 2.5, 0.1, 0.1), cex = 1)
			for(i in pages1[j]:pages2[j]){
				preds <- droplevels(dat[dat[, siteVar] == unique(dat[, siteVar])[i],])
				plot(c(0, max(preds[, distVar])), c(0, max(preds[, effVar])), 
					col = 'white', xlab = '', ylab = '', yaxt = 'n')
					if(i%%2 == 1){axis(2, las = 2, lwd = 0, lwd.ticks = 1)
					} else{axis(2, labels = FALSE, lwd = 0, lwd.ticks = 1)}
					points(preds[, distVar], preds[, effVar], 
						pch = match(preds$Group, shortname))
					legend('topright', 
						legend = paste(substr(unique(dat[, siteVar])[i], 1, 5), '-', 
						substr(unique(dat[, siteVar])[i], 6, 7), sep = ''), bty = 'n')
					if(i %in% seq(2, 200, 6)){					
						legend('top', bty = 'n', legend = fullname,
							pch = 1:length(fullname))
					}
					if(pages1[j] == max(pages1) && max(pages1) == max(pages2)){
						legend('top', bty = 'n', legend = fullname,
							pch = 1:length(fullname))
					}					
				if(pages2[j]%%6 == 0){
					mtext("Distance from water's edge (m)", side = 1,
						outer = TRUE, line = 0.5)
					mtext(yLab, side = 2, outer = TRUE, line = 1.5)	
				} else{
					if(pages1[j] == max(pages1) && pages2[j] - pages1[j] == 4){
						mtext("Distance from water's edge (m)", side = 1,
						outer = TRUE, line = -0.5)
						mtext(yLab, side = 2, outer = TRUE, line = 1.5)	
					} else{
						if(pages1[j] == max(pages1) && pages2[j] - pages1[j] == 3){
							mtext("Distance from water's edge (m)", side = 1,
								outer = TRUE, line = -15)
							mtext(yLab, side = 2, outer = TRUE, at = 0.69, 
								line = 1.5)	
						} else{
							if(pages1[j] == max(pages1) && 
								pages2[j] - pages1[j] == 2){
								mtext("Distance from water's edge (m)", side = 1,
									outer = TRUE, line = -15)
								mtext(yLab, side = 2, outer = TRUE, at = 0.69,
									line = 1.5)	
							} else{
								if(pages1[j] == max(pages1) && pages2[j] -
									pages1[j] == 1){
									mtext("Distance from water's edge (m)",
										side = 1, outer = TRUE, line = -31)
									mtext(yLab, side = 2, outer = TRUE, at = 0.86,
										line = 1.5)	
								} else{
									mtext("Distance from water's edge (m)", 
										side = 1, outer = TRUE, at = 0.28, line = -31)
									mtext(yLab, side = 2, outer = TRUE, at = 0.86,
										line = 1.5)								
			}}}}}}				
		par(oldpar)
		invisible(capture.output(dev.off()))
	}}
	## Conditions for pdf plotting.
	if('pdf' %in% filetype){
		pdf(paste('FiguresTables/Raw/', Method, effVar, siteVar, nameAppend, '.pdf',
			sep = ''), width = 6.5, height = 9.75)
		oldpar <- par(mfrow = c(3, 2), mar = c(2.5, 1.5, 0, 0),
			oma = c(2, 2.5, 0.1, 0.1), cex = 1)
		for(i in 1:length(unique(dat[, siteVar]))){
			preds <- droplevels(dat[dat[, siteVar] == unique(dat[, siteVar])[i],])
			plot(c(0, max(preds[, distVar])), c(0, max(preds[, effVar])),
				col = 'white', xlab ='', ylab = '', yaxt = 'n')
				if(i%%2 == 1){axis(2, las = 2, lwd = 0, lwd.ticks = 1)
				} else{axis(2, labels = FALSE, lwd = 0, lwd.ticks = 1)}
				points(preds[, distVar], preds[, effVar], 
					pch = match(preds$Group, shortname))
				legend('topright', legend = paste(substr(unique(dat[, siteVar])[i], 
					1, 5), '-', substr(unique(dat[, siteVar])[i], 6, 7), sep = ''), 
					bty = 'n')
				if(i %in% seq(2, 200, 6)){					
					legend('top', bty = 'n', legend = fullname, 
						pch = 1:length(fullname))
				}
				if(i == max(pages1) && max(pages1) == max(pages2)){
					legend('top', bty = 'n', legend = fullname,
						pch = 1:length(fullname))
				}	
				if(i %in% pages1 && i!=max(pages1)){
					mtext("Distance from water's edge (m)", side = 1, outer = TRUE,
						line=.5)
					mtext(yLab, side = 2, outer = TRUE, line = 1.5)	
				}
				if(i %in% pages1 && i == max(pages1)){
					if(max(pages2) - max(pages1) == 5 || 
						max(pages2) - max(pages1) == 4){
						mtext("Distance from water's edge (m)", side = 1,
							outer = TRUE, line = -0.5)
						mtext(yLab, side = 2, outer = TRUE, line = 1.5)
					} else{
						if(max(pages2) - max(pages1) == 3 || 
							max(pages2) - max(pages1) == 2){
							mtext("Distance from water's edge (m)", side = 1,
								outer = TRUE, line = -15)
							mtext(yLab, side = 2, outer = TRUE, at = 0.69,
								line = 1.5)						
						} else{
							if(max(pages2) - max(pages1) == 1){
								mtext("Distance from water's edge (m)", side = 1,
									outer = TRUE, line = -31)
								mtext(yLab, side = 2, outer = TRUE, at = 0.86,
									line = 1.5)								
							} else{
								mtext("Distance from water's edge (m)", side = 1,
									outer = TRUE, at = 0.28, line = -31)
								mtext(yLab, side = 2, outer = TRUE, at = 0.86,
									line = 1.5)								
		}}}}}				
		par(oldpar)
		invisible(capture.output(dev.off()))
}}