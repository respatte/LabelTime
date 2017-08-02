# LIBRARY IMPORTS
library(ggplot2)
library(Hmisc)

## Summarizes data.
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

# DATA HANDLING
# Import data, from both Category and SingleObject
h_rep.LTM <- read.csv("../Results/Category_hidden_LTM.csv", head=TRUE)
h_rep.STM <- read.csv("../Results/Category_hidden_STM.csv", head=TRUE)
# Add empty pca columns on the left
h_rep.LTM <- cbind(pca1=0, pca2=0, h_rep.LTM)
h_rep.STM <- cbind(pca1=0, pca2=0, h_rep.STM)
# Initialise d to maximum possible number of cases
# (subject(80)*step(24)*memory_type(LTM;STM)*dist_type(labelled;unlabelled;between) = 11520)
d <- data.frame(subject=numeric(11520), theory=factor(c("CR", "LaF")), step=0, memory_type=factor(c("LTM", "STM")),
				dist_type=NA, mu=0, sigma=0)

# Loop over subject and step
i <- 1
for (subject in 0:79){
	for (step in levels(factor(h_rep.LTM$step[h_rep.LTM$subject==subject]))){
		print(c(subject, step))
		# PRINCIPAL COMPONENT ANALYSIS
		# Extract first two dimensions after PCA
		# First LTM
		index <- h_rep.LTM$subject==subject & h_rep.LTM$step==step
		pca.LTM <- prcomp(h_rep.LTM[index, -(1:7)], retx=T)$x[, 1:2]
		h_rep.LTM$pca1[index] <- pca.LTM[, 1]
		h_rep.LTM$pca2[index] <- pca.LTM[, 2]
		# Then STM
		index <- h_rep.STM$subject==subject & h_rep.STM$step==step
		pca.STM <- prcomp(h_rep.STM[index, -(1:7)], retx=T)$x[, 1:2]
		h_rep.STM$pca1[index] <- pca.STM[, 1]
		h_rep.STM$pca2[index] <- pca.STM[, 2]
		# WITHIN CATEGORY MEAN DISTANCE
		dl.LTM <- c()  # Labelled category (coded 0 in df)
		dl.STM <- c()  # Labelled category (coded 0 in df)
		dnl.LTM <- c() # Unlabelled category (coded 1 in df)
		dnl.STM <- c() # Unlabelled category (coded 1 in df)
		# Create index for category
		index.l <- index & h_rep.LTM$labelled=="label"
		index.nl <- index & h_rep.LTM$labelled=="no_label"
		for (e1 in 0:2){
			# Create index for first exemplar
			index1.l <- index.l & h_rep.LTM$exemplar==e1
			index1.nl <- index.nl & h_rep.LTM$exemplar==e1+4
			for (e2 in (e1+1):3){
				# Create index for second exemplar
				index2.l <- index.l & h_rep.LTM$exemplar==e2
				index2.nl <- index.nl & h_rep.LTM$exemplar==e2+4
				# Append distances
				dl.LTM <- c(dl.LTM,
							dist(rbind(c(h_rep.LTM$pca1[index1.l],
										 h_rep.LTM$pca1[index2.l]),
									   c(h_rep.LTM$pca2[index1.l],
										 h_rep.LTM$pca2[index2.l])))[1])
				dl.STM <- c(dl.STM,
							dist(rbind(c(h_rep.STM$pca1[index1.l],
										 h_rep.STM$pca1[index2.l]),
									   c(h_rep.STM$pca2[index1.l],
										 h_rep.STM$pca2[index2.l])))[1])
				dnl.LTM <- c(dl.LTM,
							 dist(rbind(c(h_rep.LTM$pca1[index1.nl],
										  h_rep.LTM$pca1[index2.nl]),
										c(h_rep.LTM$pca2[index1.nl],
										  h_rep.LTM$pca2[index2.nl])))[1])
				dnl.STM <- c(dl.STM,
							 dist(rbind(c(h_rep.STM$pca1[index1.nl],
										  h_rep.STM$pca1[index2.nl]),
										c(h_rep.STM$pca2[index1.nl],
										  h_rep.STM$pca2[index2.nl])))[1])
			}
		}
		# Save mean and sd of distance for labelled category with all info
		d[i,] <- c(h_rep.LTM[index1.l, 3:5], "LTM", "labelled",
						   mean(dl.LTM), sd(dl.LTM))
		d[i + 1,] <- c(h_rep.LTM[index1.l, 3:5], "STM", "labelled",
						       mean(dl.STM), sd(dl.STM))
		# Save mean and sd of distance for unlabelled category with all info
		d[i + 2,] <- c(h_rep.LTM[index1.nl, 3:5], "LTM", "unlabelled",
							     mean(dnl.LTM), sd(dnl.LTM))
		d[i + 3,] <- c(h_rep.LTM[index1.nl, 3:5], "STM", "unlabelled",
							   mean(dnl.STM), sd(dnl.STM))
		# Compute centre ("prototype") of each category
		proto_l.LTM <- c(mean(h_rep.LTM$pca1[index.l]),
						 mean(h_rep.LTM$pca2[index.l]))
		proto_l.STM <- c(mean(h_rep.STM$pca1[index.l]),
						 mean(h_rep.STM$pca2[index.l]))
		proto_nl.LTM <- c(mean(h_rep.LTM$pca1[index.nl]),
						  mean(h_rep.LTM$pca2[index.nl]))
		proto_nl.STM <- c(mean(h_rep.STM$pca1[index.nl]),
						  mean(h_rep.STM$pca2[index.nl]))
		# Save distances between categories (sd=0 here)
		d[i + 4,] <- c(h_rep.LTM[index1.nl, 3:5], "LTM", "between",
							   dist(rbind(proto_l.LTM, proto_nl.LTM))[1], 0)
		d[i + 5,] <- c(h_rep.LTM[index1.nl, 3:5], "STM", "between",
							   dist(rbind(proto_l.STM, proto_nl.STM))[1], 0)
		i <- i + 6
	}
}
# Code dist_type as a factor, get rid of NAs
d$dist_type <- factor(d$dist_type)
d <- na.omit(d)
# Write file if generating PCA, read file otherwise
#write.csv(d, file="../Results/Category_hidden_distances.csv", row.names=F)
d <- read.csv(file="../Results/Category_hidden_distances.csv", header=T)

# GRAPH
# Get summary of the data
d.sum <- summarySE(d, measurevar="mu", groupvars=c("theory", "step", "memory_type", "dist_type"), conf.interval=.89)
# Select observations for plot, dropping unusued factors
d.sum.plot <- droplevels(d.sum[d.sum$step>0 & d.sum$dist_type!="between",])
# Create plot
d.plot <- ggplot(d.sum.plot,
                 aes(x = step,
                     y = mu,
                     colour = dist_type,
                     shape = dist_type)) +
  facet_grid(memory_type~theory, scales="free_y") +
  xlab("Step") + ylab("Mean distance") + theme_bw(base_size=18) +
  theme(panel.grid.minor.x=element_blank()) +
  scale_fill_brewer(palette = "Dark2") +
  scale_shape_manual(name = "Category",
                     breaks = c("labelled", "unlabelled"),
                     labels = c("Labelled", "Unlabelled"),
                     values = c(21,24)) +
  scale_colour_discrete(name = "Category",
                        breaks = c("labelled", "unlabelled"),
                        labels = c("Labelled", "Unlabelled")) +
  geom_line(position=position_dodge(50)) +
  geom_errorbar(aes(ymin=mu-ci, ymax=mu+ci),
                colour="black", width=50,
                position=position_dodge(50)) +
  geom_point(position=position_dodge(50),
             size=1.5, fill="white")
# Save plot
ggsave("../Results/Distance.pdf", plot = d.plot, height = 8, width = 10)
