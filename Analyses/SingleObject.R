# LIBRARY IMPORTS
library(ggplot2)
library(dplyr)
library(lme4)

# DATA HANDLING
# Import data
label_time.data <- read.csv("../Results/SingleObject_LT.csv", head=TRUE)
# Set all factor variables to factors
label_time.data$subject <- as.factor(label_time.data$subject)
label_time.data$explo_overlap <- as.factor(label_time.data$explo_overlap)
label_time.LaF.data <- filter(label_time.data, model == "LaF")
label_time.CR.data <- filter(label_time.data, model == "CR")

# MODELS
# Global model for both LaF and CR together
label_time.lmer.all <- lmer(looking_time ~ trial*model*labelled +
							(1 + trial + labelled | subject) +
							(1 + trial + labelled + model | explo_overlap),
							data = label_time.data
							)
# Models for LaF only -- There's no point running models without main effect of trial
# Including only main effect for trial
label_time.LaF.lmer.trial_only <- lmer(looking_time ~ trial +
									   (1 + trial + labelled | subject) +
									   (1 + trial + labelled | explo_overlap),
									   data = label_time.LaF.data
									   )
# Including main effects for trial and labelled
label_time.LaF.lmer.labelled <- lmer(looking_time ~ trial + labelled +
									 (1 + trial + labelled | subject) +
									 (1 + trial + labelled | explo_overlap),
									 data = label_time.LaF.data
									 )
# Including all interactions and main effects
label_time.LaF.lmer.all <- lmer(looking_time ~ trial*labelled +
								(1 + trial + labelled | subject) +
								(1 + trial + labelled | explo_overlap),
								data = label_time.LaF.data
								)
# Comparing models with different fixed effects
label_time.LaF.comparison <- anova(label_time.LaF.lmer.trial_only,
								   label_time.LaF.lmer.labelled,
								   label_time.LaF.lmer.all)
# Results: adding all interactions helps the model, not label only
# Best model: label_time.LaF.lmer.all
# Models for CR only
# Including only main effect for trial
label_time.CR.lmer.trial_only <- lmer(looking_time ~ trial +
									  (1 + trial + labelled | subject) +
									  (1 + trial + labelled | explo_overlap),
									  data = label_time.CR.data
									  )
# Including main effects for trial and labelled
label_time.CR.lmer.labelled <- lmer(looking_time ~ trial + labelled +
									(1 + trial + labelled | subject) +
									(1 + trial + labelled | explo_overlap),
									data = label_time.CR.data
									)
# Including all interactions and main effects
label_time.CR.lmer.all <- lmer(looking_time ~ trial*labelled +
							   (1 + trial + labelled | subject) +
							   (1 + trial + labelled | explo_overlap),
							   data = label_time.CR.data
							   )
# Comparing models with different fixed effects
label_time.CR.comparison <- anova(label_time.CR.lmer.trial_only,
								  label_time.CR.lmer.labelled,
								  label_time.CR.lmer.all)
# Results: adding more parameters than trial alone didn't help the model
# Best model: label_time.CR.lmer.trial_only

# GENERATING GRAPHS
# Transform trial number to start at 1
label_time.data$trial <- label_time.data$trial + 1
# Graph from data (not models)
pdf("../Results/General.pdf", w=9, h=4)
label_time.data.plot <- ggplot(label_time.data) +
						geom_smooth(size = .2,
									mapping = aes(trial, looking_time,
												  colour = labelled)) +
						facet_grid(.~model) +
						scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8)) +
						xlab("Trial") + ylab("Looking time") + theme_bw() +
						scale_fill_brewer(palette = "Dark2") +
						scale_colour_discrete(name = "Condition",
											  labels = c("label","no label"))
print(label_time.data.plot)
dev.off()
# Model plot for LaF
#pdf("../Results/LaF_results_LT.pdf", w=8, h=5)
# TODO
#dev.off()
# Model plot for CR
#pdf("../Results/CR_results_LT.pdf", w=8, h=5)
# TODO
#dev.off()
