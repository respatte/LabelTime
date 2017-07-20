# LIBRARY IMPORTS
library(ggplot2)
library(dplyr)
library(lme4)

# DATA HANDLING
# Import data
single_obj.data <- read.csv("../Results/SingleObject_BPN_LT.csv", head=TRUE)
# Set all factor variables to factors
single_obj.data$subject <- as.factor(single_obj.data$subject)
single_obj.data$explo_overlap <- as.factor(single_obj.data$explo_overlap)
single_obj.LaF.data <- filter(single_obj.data, model == "LaF")
single_obj.CR.data <- filter(single_obj.data, model == "CR")

# MODELS
# Global model for both LaF and CR together
single_obj.lmer.all <- lmer(looking_time ~ trial*model*labelled +
							(1 + trial + labelled | subject) +
							(1 + trial + labelled + model | explo_overlap),
							data = single_obj.data
							)
# Models for LaF only -- There's no point running models without main effect of trial
# Including only main effect for trial
single_obj.LaF.lmer.trial_only <- lmer(looking_time ~ trial +
									   (1 + trial + labelled | subject) +
									   (1 + trial + labelled | explo_overlap),
									   data = single_obj.LaF.data
									   )
# Including main effects for trial and labelled
single_obj.LaF.lmer.labelled <- lmer(looking_time ~ trial + labelled +
									 (1 + trial + labelled | subject) +
									 (1 + trial + labelled | explo_overlap),
									 data = single_obj.LaF.data
									 )
# Including all interactions and main effects
single_obj.LaF.lmer.all <- lmer(looking_time ~ trial*labelled +
								(1 + trial + labelled | subject) +
								(1 + trial + labelled | explo_overlap),
								data = single_obj.LaF.data
								)
# Comparing models with different fixed effects
single_obj.LaF.comparison <- anova(single_obj.LaF.lmer.trial_only,
								   single_obj.LaF.lmer.labelled,
								   single_obj.LaF.lmer.all)
# Results: adding all interactions helps the model, not label only
# Best model: single_obj.LaF.lmer.all
# Models for CR only
# Including only main effect for trial
single_obj.CR.lmer.trial_only <- lmer(looking_time ~ trial +
									  (1 + trial + labelled | subject) +
									  (1 + trial + labelled | explo_overlap),
									  data = single_obj.CR.data
									  )
# Including main effects for trial and labelled
single_obj.CR.lmer.labelled <- lmer(looking_time ~ trial + labelled +
									(1 + trial + labelled | subject) +
									(1 + trial + labelled | explo_overlap),
									data = single_obj.CR.data
									)
# Including all interactions and main effects
single_obj.CR.lmer.all <- lmer(looking_time ~ trial*labelled +
							   (1 + trial + labelled | subject) +
							   (1 + trial + labelled | explo_overlap),
							   data = single_obj.CR.data
							   )
# Comparing models with different fixed effects
single_obj.CR.comparison <- anova(single_obj.CR.lmer.trial_only,
								  single_obj.CR.lmer.labelled,
								  single_obj.CR.lmer.all)
# Results: adding more parameters than trial alone didn't help the model
# Best model: single_obj.CR.lmer.trial_only

# GENERATING GRAPHS
# Transform trial number to start at 1
single_obj.data$trial <- single_obj.data$trial + 1
# Graph from data (not models)
#pdf("../Results/General.pdf", w=9, h=4)
single_obj.data.plot <- ggplot(single_obj.data) +
						geom_smooth(size = .2,
									mapping = aes(trial, looking_time,
												  colour = labelled),
									method = "loess") +
						facet_grid(.~model) +
						scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8)) +
						xlab("Trial") + ylab("Looking time") + theme_bw() +
						scale_fill_brewer(palette = "Dark2") +
						scale_colour_discrete(name = "Condition",
											  labels = c("label","no label"))
ggsave("../Results/General.pdf", plot = single_obj.data.plot,
	   height = 4, width = 9)
#print(single_obj.data.plot)
#dev.off()
# Model plot for LaF
#pdf("../Results/LaF_results_LT.pdf", w=8, h=5)
# TODO
#dev.off()
# Model plot for CR
#pdf("../Results/CR_results_LT.pdf", w=8, h=5)
# TODO
#dev.off()
