# LIBRARY IMPORTS
library(ggplot2)
library(Hmisc)
library(plyr)

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

# TASK SELECTION
compute_distances <- F #Whether or not to compute the distances

# DATA HANDLING
# Import data, from both Category and SingleObject
h_rep.LTM <- read.csv("../Results/Category_hidden_LTM.csv", head=TRUE)
h_rep.STM <- read.csv("../Results/Category_hidden_STM.csv", head=TRUE)
# Add memory_type column on the left
h_rep.LTM <- cbind(memory_type="LTM", h_rep.LTM)
h_rep.STM <- cbind(memory_type="STM", h_rep.STM)
# Binding dataframes together
h_rep <- rbind.fill(h_rep.LTM,h_rep.STM)

if (!compute_distances) {
  d <- read.csv(file="../Results/Category_hidden_distances.csv", header=T)
} else {
  # Initialise d to maximum possible number of cases
  # (subject(80)*step(24)*memory_type(LTM;STM)*dist_type(labelled;unlabelled;between;r_labelled;r_unlabelled) = 19200)
  d <- data.frame(memory_type=factor(c("LTM", "STM")), subject=numeric(19200), theory=factor(c("CR", "LaF")), step=0,
                  dist_type=NA, mu=0)
  # Loop over subject, memory_type, and step
  i <- 1
  for (subject in 0:79) {
    print(subject)
    for (memory_type in levels(h_rep$memory_type)){
      for (step in levels(factor(h_rep$step[h_rep$subject==subject & h_rep$memory_type==memory_type]))){
        index <- h_rep$subject==subject & h_rep$step==step & h_rep$memory_type==memory_type
        # WITHIN CATEGORY MEAN DISTANCE
        dist.l <- c()  # Labelled category (coded 0 in df)
        dist.nl <- c() # Unlabelled category (coded 1 in df)
        # Create index for category
        index.l <- index & h_rep$labelled=="label"
        index.nl <- index & h_rep$labelled=="no_label"
        for (e1 in 0:2){
          # Create index for first exemplar
          index1.l <- index.l & h_rep$exemplar==e1
          index1.nl <- index.nl & h_rep$exemplar==e1+4
          for (e2 in (e1+1):3){
            # Create index for second exemplar
            index2.l <- index.l & h_rep$exemplar==e2
            index2.nl <- index.nl & h_rep$exemplar==e2+4
            # Append distances
            dist.l <- c(dist.l,
                        dist(rbind(h_rep[index1.l, -(1:6)],
                                   h_rep[index2.l, -(1:6)]))[1])
            #print(c("Added distance between",e1,"and",e2))
            dist.nl <- c(dist.nl,
                         dist(rbind(h_rep[index1.nl, -(1:6)],
                                    h_rep[index2.nl, -(1:6)]))[1])
            #print(c("Added distance between",e1+4,"and",e2+4))
          }
        }
        # Save mean and sd of distance for labelled category with all info
        d[i,] <- c(h_rep[index1.l, 1:4], "labelled",  mean(dist.l))
        #print("Added a row for dist.l")
        # Save mean and sd of distance for unlabelled category with all info
        d[i + 1,] <- c(h_rep[index1.nl, 1:4], "unlabelled", mean(dist.nl))
        #print("Added a row for dist.nl")
        # PROTOTYPE: NOT WORKING FOR NOW (mean(...) returns warning because of non-numerical values leading to NA)
        # Compute centre ("prototype") of each category
        proto.l <- colMeans(h_rep[index.l, -(1:6)])
        #print("Computed labelled prototype")
        proto.nl <- colMeans(h_rep[index.nl, -(1:6)])
        #print("Computed unlabelled prototype")
        # Save distances between categories (sd=0 here)
        d[i + 2,] <- c(h_rep[index1.nl, 1:4], "between",
                       dist(rbind(proto.l, proto.nl))[1])
        #print("Added a row for LTM between- distance")
        # Relative distances
        d[i + 3,] <- c(h_rep[index1.l, 1:4], "r_labelled",  d[i,6]/d[i+2,6])
        d[i + 4,] <- c(h_rep[index1.nl, 1:4], "r_unlabelled",  d[i+1,6]/d[i+2,6])
        i <- i + 5
      }
    }
  }
  # Code dist_type as a factor, get rid of NAs
  d$dist_type <- factor(d$dist_type)
  d <- na.omit(d)
  # Write file if generating distances
  write.csv(d, file="../Results/Category_hidden_distances.csv", row.names=F)
}


# GRAPH
# Absolute distance
# Get summary of the data
d.sum <- summarySE(d, measurevar="mu", groupvars=c("theory", "step", "memory_type", "dist_type"), conf.interval=.89)
# Select observations for plot, dropping unusued factors
d.sum.abs <- droplevels(d.sum[d.sum$step>0 & d.sum$dist_type %in% c("labelled","unlabelled"),])
# Create plot
d.abs.plot <- ggplot(d.sum.abs,
                     aes(x = step,
                         y = mu,
                         colour = dist_type,
                         shape = dist_type)) +
  facet_grid(memory_type~theory, scales="free_y") +
  xlab("Step") + ylab("Mean distance (absolute)") + theme_bw(base_size=30) +
  theme(panel.grid.minor.x=element_blank(),
        legend.position = "top") +
  scale_fill_brewer(name = "Category", palette="Dark2",
                    breaks = c("labelled", "unlabelled"),
                    labels = c("Labelled", "Unlabelled")) +
  scale_colour_brewer(name = "Category", palette="Dark2",
                      breaks = c("labelled", "unlabelled"),
                      labels = c("Labelled", "Unlabelled")) +
  geom_line() +
  geom_ribbon(aes(ymin=mu-ci, ymax=mu+ci, fill=dist_type), alpha=0.1, size=0)
# Save plot
ggsave("../Results/DistancesAbsolute.png", plot = d.abs.plot, height = 20, width = 20)
# Relative distance
# Select observations for plot, dropping unusued factors
d.sum.rel <- droplevels(d.sum[d.sum$step>0 & d.sum$dist_type %in% c("r_labelled","r_unlabelled"),])
# Create plot
d.rel.plot <- ggplot(d.sum.rel,
                 aes(x = step,
                     y = mu,
                     colour = dist_type,
                     shape = dist_type)) +
  facet_grid(memory_type~theory, scales="free_y") +
  xlab("Step") + ylab("Mean distance (relative)") + theme_bw(base_size=30) +
  theme(panel.grid.minor.x=element_blank(),
        legend.position = "top") +
  scale_fill_brewer(name = "Category", palette="Dark2",
                    breaks = c("r_labelled", "r_unlabelled"),
                    labels = c("Labelled", "Unlabelled")) +
  scale_colour_brewer(name = "Category", palette="Dark2",
                      breaks = c("r_labelled", "r_unlabelled"),
                      labels = c("Labelled", "Unlabelled")) +
  geom_line() +
  geom_ribbon(aes(ymin=mu-ci, ymax=mu+ci, fill=dist_type), alpha=0.1, size=0)
# Save plot
ggsave("../Results/DistancesRelative.png", plot = d.rel.plot, height = 20, width = 20)
# Relative distance
# Select observations for plot, dropping unusued factors
d.sum.btw <- droplevels(d.sum[d.sum$step>0 & d.sum$dist_type %in% c("between"),])
# Create plot
d.btw.plot <- ggplot(d.sum.btw,
                     aes(x = step,
                         y = mu)) +
  facet_grid(memory_type~theory, scales="free_y") +
  xlab("Step") + ylab("Mean distance") + theme_bw(base_size=30) +
  theme(panel.grid.minor.x=element_blank(),
        legend.position = "top") +
  guides(fill="none") +
  geom_line(colour="#7570b3") +
  geom_ribbon(aes(ymin=mu-ci, ymax=mu+ci, fill="#7570b3"), alpha=0.1, size=0)
# Save plot
ggsave("../Results/DistancesBetween.png", plot = d.btw.plot, height = 20, width = 20)
