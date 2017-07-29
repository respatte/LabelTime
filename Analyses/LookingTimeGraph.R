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
SObj <- "SingleObject"
Cate <- "Category"
LT.SingObj <- read.csv(paste0("../Results/",SObj,"_lsize5_LT.csv"), head=TRUE)
LT.Cat <- read.csv(paste0("../Results/",Cate,"_lsize5_LT.csv"), head=TRUE)
# Drop Simple auto-encoder (shit results)
LT.SingObj <- LT.SingObj[LT.SingObj$model == "DMN",]
LT.Cat <- LT.Cat[LT.Cat$model == "DMN",]
# Create experiment variable for each dataset
LT.SingObj$experiment <- "SingleObject"
LT.Cat$experiment <- "Category"
# Increment subject from Cat to not overlap with SingObj
LT.Cat$subject <- LT.Cat$subject + (tail(LT.SingObj$subject, n=1) + 1)
# Merge both datasets
LT.data <- rbind(LT.SingObj, LT.Cat)
# Set all factor variables to factors, with labels if meaningful
LT.data$subject <- factor(LT.data$subject)
LT.data$explo_overlap <- factor(LT.data$explo_overlap)
LT.data$theory <- factor(LT.data$theory, labels = c("Compound Representations",
													"Labels as Features"))
LT.data$experiment <- factor(LT.data$experiment, labels = c("Category",
															"Single Object"))
# Transform trial number to start at 1
LT.data$trial <- LT.data$trial + 1
# Summarising data for mean+CI graph
LT.data.sum <- summarySE(LT.data, measurevar="looking_time",
								groupvars=c("labelled","model",
											"theory","trial","experiment"),
								conf.interval=.89)

# GENERATING GRAPHS
# Graph from data (not models), mean and error bars (CI)
LT.data.plot <- ggplot(LT.data.sum, aes(x = trial,
										y = looking_time,
										colour = labelled,
										shape = labelled)) +
				facet_grid(theory~experiment) +
				scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8)) +
				xlab("Trial") + ylab("Looking time") + theme_bw(base_size=18) +
				theme(panel.grid.minor.x=element_blank()) +
				scale_fill_brewer(palette = "Dark2") +
				scale_shape_manual(name = "Condition",
								   breaks = c("label","no_label"),
								   labels = c("label","no label"),
								   values = c(21,24)) +
				scale_colour_discrete(name = "Condition",
									  breaks = c("label","no_label"),
									  labels = c("label","no label")) +
				geom_line(position=position_dodge(0.3)) +
				geom_errorbar(aes(ymin=looking_time-ci,
								  ymax=looking_time+ci),
							  colour="black", width=.2,
							  position=position_dodge(0.3)) +
				geom_point(position=position_dodge(0.3),
						   size=1.5, fill="white")

ggsave("../Results/Mean+CI_lsize5.pdf", plot = LT.data.plot, height = 8, width = 9)
