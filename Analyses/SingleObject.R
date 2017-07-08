# LIBRARY IMPORTS
library(ggplot2)
library(dplyr)
library(lme4)

# DATA HANDLING
# Import data
label_time.data <- read.csv("../Results/SingleObject_LT.csv", head=TRUE)
# Set all factor variables to factors
# label_time.data$trial <- as.factor(label_time.data$trial)
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
label_time.LaF.trial_only <- lmer(looking_time ~ trial +
								  (1 + trial + labelled | subject) +
								  (1 + trial + labelled | explo_overlap),
								  data = label_time.LaF.data
								  )
# Including main effects for trial and labelled
label_time.LaF.labelled <- lmer(looking_time ~ trial + labelled +
								(1 + trial + labelled | subject) +
								(1 + trial + labelled | explo_overlap),
								data = label_time.LaF.data
								)
# Including all interactions and main effects
label_time.LaF.all <- lmer(looking_time ~ trial*labelled +
						   (1 + trial + labelled | subject) +
						   (1 + trial + labelled | explo_overlap),
						   data = label_time.LaF.data
						   )
# Comparing models with different fixed effects
label_time.LaF.comparison <- anova(label_time.LaF.trial_only,
								   label_time.LaF.labelled,
								   label_time.LaF.all)
# Results: adding all interactions helps the model, not label only
# Best model: label_time.LaF.all
# Models for CR only
# Including only main effect for trial
label_time.CR.trial_only <- lmer(looking_time ~ trial +
								 (1 + trial + labelled | subject) +
								 (1 + trial + labelled | explo_overlap),
								 data = label_time.CR.data
								 )
# Including main effects for trial and labelled
label_time.CR.labelled <- lmer(looking_time ~ trial + labelled +
							   (1 + trial + labelled | subject) +
							   (1 + trial + labelled | explo_overlap),
							   data = label_time.CR.data
							   )
# Including all interactions and main effects
label_time.CR.all <- lmer(looking_time ~ trial*labelled +
						  (1 + trial + labelled | subject) +
						  (1 + trial + labelled | explo_overlap),
						  data = label_time.CR.data
						  )
# Comparing models with different fixed effects
label_time.CR.comparison <- anova(label_time.CR.trial_only,
								  label_time.CR.labelled,
								  label_time.CR.all)
# Results: adding more parameters than trial alone didn't help the model
# Best model: label_time.CR.trial_only

# GENERATING GRAPHS
# Model plot for LaF
pdf("../Results/LaF_results_LT.pdf", w=8, h=5)
# TODO
dev.off()
# Model plot for CR
pdf("../Results/CR_results_LT.pdf", w=8, h=5)
# TODO
dev.off()
