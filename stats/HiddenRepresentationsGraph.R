# LIBRARY IMPORTS
library(plyr)
library(tidyverse)
library(Hmisc)
library(car)

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
  datac <- plyr::rename(datac, c("mean" = measurevar))

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

if (!compute_distances) {
  d <- read.csv(file="../results/Category_hidden_distances.csv", header=T)
} else {
  # DATA HANDLING
  # Import data, from both Category and SingleObject
  h_rep <- read.csv("../results/Category_hidden_LTM.csv", head=TRUE)
  # Initialise d to maximum possible number of cases
  # (subject(80)*step(20)*theory(CR,LaF)*dist_type(labelled;unlabelled;between;r_labelled;r_unlabelled) = 16000)
  d <- data.frame(subject=numeric(20000), theory=factor(NA), step=0,
                  dist_type=NA, mu=0)
  levels(d$theory) <- c("CR", "LaF")
  # Loop over subject, memory_type, and step
  i <- 1
  for (subject in 0:max(h_rep$subject)) {
    print(subject)
    for (step in levels(factor(h_rep$step[h_rep$subject==subject]))){
      index <- h_rep$subject==subject & h_rep$step==step
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
                      dist(rbind(h_rep[index1.l, -(1:5)],
                                 h_rep[index2.l, -(1:5)]))[1])
          #print(c("Added distance between",e1,"and",e2))
          dist.nl <- c(dist.nl,
                       dist(rbind(h_rep[index1.nl, -(1:5)],
                                  h_rep[index2.nl, -(1:5)]))[1])
          #print(c("Added distance between",e1+4,"and",e2+4))
        }
      }
      # Save mean of distance for labelled category with all info
      d[i,] <- c(h_rep[index1.l, 1:3], "labelled",  mean(dist.l))
      #print("Added a row for dist.l")
      # Save mean of distance for unlabelled category with all info
      d[i + 1,] <- c(h_rep[index1.nl, 1:3], "unlabelled", mean(dist.nl))
      #print("Added a row for dist.nl")
      # Compute centre ("prototype") of each category
      proto.l <- colMeans(h_rep[index.l, -(1:5)])
      #print("Computed labelled prototype")
      proto.nl <- colMeans(h_rep[index.nl, -(1:5)])
      #print("Computed unlabelled prototype")
      # Save distances between categories
      d[i + 2,] <- c(h_rep[index1.nl, 1:3], "between",
                     dist(rbind(proto.l, proto.nl))[1])
      #print("Added a row for LTM between- distance")
      # Relative distances
      d[i + 3,] <- c(h_rep[index1.l, 1:3], "r_labelled",  d[i,5]/d[i+2,5])
      d[i + 4,] <- c(h_rep[index1.nl, 1:3], "r_unlabelled",  d[i+1,5]/d[i+2,5])
      i <- i + 5
    }
  }
  # Code dist_type as a factor, get rid of NAs
  d$dist_type <- factor(d$dist_type)
  d <- na.omit(d)
  # Write file if generating distances
  write.csv(d, file="../results/Category_hidden_distances.csv", row.names=F)
}

# DATA HANDLING
# Create two distance type columns (labeled/unlabeled/between, and absolute/relative/between)
d <- d %>%
  mutate(abs_rel = case_when(grepl("r_", dist_type) ~ "Relative - Within",
                             grepl("labelled", dist_type) ~ "Absolute - Within",
                             dist_type == "between" ~ "Between"),
         dist_type = case_when(grepl("unlabelled", dist_type) ~ "no label",
                               grepl("labelled", dist_type) ~ "label",
                               dist_type == "between" ~ "between"),
         theory = factor(theory, labels = c("Compound-Representations",
                                            "Labels-as-Features")))

# GRAPH
# Select observations for plot, dropping unusued factors
d <- d %>%
  subset(step>0 & step<=2000 & abs_rel == "Absolute - Within" & theory == "Labels-as-Features") %>%
  droplevels()
# Get summary of the data
d.sum <- summarySE(d, measurevar="mu", groupvars=c("theory", "step", "dist_type", "abs_rel"), conf.interval=.95)
# Create plot
d.plot <- ggplot(d.sum,
                 aes(x = step,
                     y = mu,
                     colour = dist_type)) +
  facet_grid(theory~.) +
  xlab("Step") + ylab("Mean distance") + theme_bw(base_size=10, base_family = "serif") +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        legend.position = "top",
        legend.margin = margin(b=2,t=0,unit="mm"),
        legend.box.spacing = unit(.01,"mm")) +
  scale_fill_brewer(name = "Distance type", palette="Dark2") +
  scale_colour_brewer(name = "Distance type", palette="Dark2") +
  geom_line(size=.1) +
  geom_ribbon(aes(ymin=mu-ci, ymax=mu+ci, fill=dist_type), alpha=0.1, size=0)
# Save plot
ggsave("../results/Distances.png", plot = d.plot,
       height = 2.5, width = 3.5, dpi = 600)
