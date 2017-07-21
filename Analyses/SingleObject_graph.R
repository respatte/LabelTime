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
# Import data
# Choose the data to import (BPN/DMN, 40 subjects or 5000 subjects)
#single_obj.data <- read.csv("../Results/SingleObject_BPN_LT.csv", head=TRUE)
#single_obj.data <- read.csv("../Results/SingleObject_DMN_LT.csv", head=TRUE)
#single_obj.data <- read.csv("../Results/SingleObject_BPN_big_LT.csv", head=TRUE)
#single_obj.data <- read.csv("../Results/SingleObject_DMN_big_LT.csv", head=TRUE)
#single_obj.data <- read.csv("../Results/SingleObject_BPN_ImpairUniform_LT.csv", head=TRUE)
single_obj.data <- read.csv("../Results/SingleObject_DMN_ImpairUniform_LT.csv", head=TRUE)
# Set all factor variables to factors
single_obj.data$subject <- as.factor(single_obj.data$subject)
single_obj.data$explo_overlap <- as.factor(single_obj.data$explo_overlap)
# Transform trial number to start at 1
single_obj.data$trial <- single_obj.data$trial + 1
# Summarising data for mean+CI graph
single_obj.data.sum <- summarySE(single_obj.data, measurevar="looking_time",
								 groupvars=c("labelled","model","trial"),
								 conf.interval=.89)

# GENERATING GRAPHS
# Graph from data (not models), mean and error bars (CI)
single_obj.data.plot <- ggplot(single_obj.data.sum, aes(x = trial,
														y = looking_time,
														colour = labelled,
														group = labelled,
														shape = labelled)) +
						facet_grid(.~model) +
						scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8)) +
						xlab("Trial") + ylab("Looking time") + theme_bw() +
						scale_fill_brewer(palette = "Dark2") +
						scale_shape_discrete(name = "Condition",
											 labels = c("label","no label"),
											 solid=FALSE) +
						geom_errorbar(aes(ymin=looking_time-ci,
										  ymax=looking_time+ci),
									  colour="black", width=.1,
									  position=position_dodge(0.1)) +
						geom_line(position=position_dodge(0.1)) +
						geom_point(position=position_dodge(0.1),
								   size=2, fill="white")
ggsave("../Results/MeanCI.pdf", plot = single_obj.data.plot,
	   height = 4, width = 9)
