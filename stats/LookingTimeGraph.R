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
  datac <- plyr::rename(datac, replace = c("mean" = measurevar))

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
LT.SingObj <- read.csv("../results/SingleObject_LT.csv", head=TRUE)
LT.Cat <- read.csv("../results/Category_LT.csv", head=TRUE)
# Create experiment variable for each dataset
LT.SingObj$experiment <- factor("Single Object")
LT.Cat$experiment <- factor("Category")
# Merge both datasets
LT.data <- rbind(LT.SingObj, LT.Cat)
# Set all factor variables to factors, with labels if meaningful
LT.data$explo_overlap <- factor(LT.data$explo_overlap)
LT.data$theory <- factor(LT.data$theory, labels = c("Compound Representations",
                                                    "Labels as Features"))
# Transform trial number to start at 1
LT.data$trial <- LT.data$trial + 1
# Summarising data for mean+CI graph
LT.data.sum <- summarySE(LT.data, measurevar="looking_time",
                         groupvars=c("labelled", "theory", "trial",
                                     "experiment"),
                         conf.interval=.95)
LT.SingObj.sum <- summarySE(LT.data[LT.data$experiment=="Single Object",],
                            measurevar="looking_time",
                            groupvars=c("labelled", "theory", "trial"),
                            conf.interval=.95)
LT.Cat.sum <- summarySE(LT.data[LT.data$experiment=="Category",],
                        measurevar="looking_time",
                        groupvars=c("labelled", "theory", "trial"),
                        conf.interval=.95)

# GENERATING GRAPHS
# Graph from data (not models), mean and error bars (CI)
LT.all.plot <- ggplot(LT.data.sum, aes(x = trial,
                                        y = looking_time,
                                        colour = labelled,
                                        shape = labelled)) +
  facet_grid(theory~experiment) +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8)) +
  xlab("Trial") + ylab("Looking time") + theme_bw(base_size=8) +
  theme(panel.grid.minor.x=element_blank()) +
  scale_shape_manual(name = "Condition",
                     breaks = c("label","no_label"),
                     labels = c("label","no label"),
                     values = c(21,24)) +
  scale_colour_brewer(palette = "Dark2",
                      name = "Condition",
                      breaks = c("label","no_label"),
                      labels = c("label","no label")) +
  geom_line(position=position_dodge(0.3), size=.4) +
  geom_errorbar(aes(ymin=looking_time-ci,
                    ymax=looking_time+ci),
                colour="black", width=.2, size=.35,
                position=position_dodge(0.3)) +
  geom_point(position=position_dodge(0.3),
             size=.8, fill="white")

ggsave("../results/LT_all.pdf", plot = LT.all.plot, height = 5.8, width = 6.5)

# Graph for Single Object only
LT.SOb.plot <- ggplot(LT.SingObj.sum, aes(x = trial,
                                           y = looking_time,
                                           colour = labelled,
                                           shape = labelled)) +
  facet_grid(theory~.) +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8)) +
  xlab("Trial") + ylab("Looking time") + theme_bw(base_size=8) +
  theme(panel.grid.minor.x=element_blank(),
        legend.position="top") +
  scale_shape_manual(name = "Condition",
                     breaks = c("label","no_label"),
                     labels = c("label","no label"),
                     values = c(21,24)) +
  scale_colour_brewer(palette = "Dark2",
                      name = "Condition",
                      breaks = c("label","no_label"),
                      labels = c("label","no label")) +
  geom_line(position=position_dodge(0.3), size=.4) +
  geom_errorbar(aes(ymin=looking_time-ci,
                    ymax=looking_time+ci),
                colour="black", width=.2, size=.35,
                position=position_dodge(0.3)) +
  geom_point(position=position_dodge(0.3),
             size=.8, fill="white")

ggsave("../results/LT_SingleObject.pdf", plot = LT.SOb.plot,
       height = 5.8, width = 3.1)

# Graph from data (not models), mean and error bars (CI)
LT.Cat.plot <- ggplot(LT.Cat.sum, aes(x = trial,
                                       y = looking_time,
                                       colour = labelled,
                                       shape = labelled)) +
  facet_grid(theory~.) +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8)) +
  xlab("Trial") + ylab("Looking time") + theme_bw(base_size=8) +
  theme(panel.grid.minor.x=element_blank(),
        legend.position="top") +
  scale_shape_manual(name = "Condition",
                     breaks = c("label","no_label"),
                     labels = c("label","no label"),
                     values = c(21,24)) +
  scale_colour_brewer(palette = "Dark2",
                      name = "Condition",
                      breaks = c("label","no_label"),
                      labels = c("label","no label")) +
  geom_line(position=position_dodge(0.3), size=.4) +
  geom_errorbar(aes(ymin=looking_time-ci,
                    ymax=looking_time+ci),
                colour="black", width=.2, size=.35,
                position=position_dodge(0.3)) +
  geom_point(position=position_dodge(0.3),
             size=.8, fill="white")

ggsave("../results/LT_Category.pdf", plot = LT.Cat.plot,
       height = 5.8, width = 3.1)
