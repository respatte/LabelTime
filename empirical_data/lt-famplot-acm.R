require("ggplot2")
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

datafamlooks = read.csv("labeltime-data-fam-screenlooks.csv")
datafammeanspnum = aggregate(look ~  Label + trial + pnum, datafamlooks, sum)#looking times for participants by trial and cond
datafammeanspnum$Time = datafammeanspnum$look*17
head(datafammeanspnum)
datafammeanserrorbar = aggregate(look ~  Label + trial + pnum, datafamlooks, sum)

plotdata=summarySE(datafammeanspnum, measurevar = "Time", groupvars=c("Label", "trial"))
plotdata$Label=as.factor(plotdata$Label)
plotdata$Label2=ifelse(plotdata$Label == "Labelled", "Labeled", "Unlabeled")
plotdata$Label=plotdata$Label2
plotdata$Label2=NULL

empirical.plot <- ggplot(plotdata,
                         aes(x = trial,
                             y = Time,
                             colour = Label,
                             shape = Label)) +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8)) +
  xlab("Trial") + ylab("Looking time (ms)") + theme_bw(base_size=10, base_family = "serif") +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        legend.position="top",
        legend.margin = margin(b=0,t=0,unit="mm"),
        legend.box.spacing = unit(.01,"mm")) +
  scale_shape_manual(name = "Condition",
                     breaks = c("Labeled","Unlabeled"),
                     labels = c("label","no label"),
                     values = c(21,24)) +
  scale_colour_brewer(palette = "Dark2",
                      name = "Condition",
                      breaks = c("Labeled","Unlabeled"),
                      labels = c("label","no label")) +
  geom_line(position=position_dodge(0.3), size=.4) +
  geom_errorbar(aes(ymin=Time-ci,
                    ymax=Time+ci),
                colour="black", width=.2, size=.35,
                position=position_dodge(0.3)) +
  geom_point(position=position_dodge(0.3),
             size=1.25, fill="white")

ggsave("empirical.png", plot = empirical.plot,
       height = 2.5, width = 3.5, dpi = 600)
