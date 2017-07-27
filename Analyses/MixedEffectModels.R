# LIBRARY IMPORTS
library(ggplot2)
library(dplyr)
library(lme4)

# DATA HANDLING
# Import data, from both Category and SingleObject
LT.SingObj <- read.csv("../Results/SingleObject_lsize1_LT.csv", head=TRUE)
LT.Cat <- read.csv("../Results/Category_lsize1_LT.csv", head=TRUE)
# Drop out simple BPN
LT.SingObj <- LT.SingObj[LT.SingObj$model == "DMN",]
LT.Cat <- LT.Cat[LT.Cat$model == "DMN",]
# Create experiment variable for each dataset
LT.SingObj$experiment <- "SingleObject"
LT.Cat$experiment <- "Category"
# Select training sample
LT.SingObj.training <- LT.SingObj[LT.SingObj$subject < 160,]
LT.Cat.training <- LT.Cat[LT.Cat$subject < 160,]
# Increment subject from Cat to not overlap with SingObj
n_subjects <- tail(LT.SingObj$subject, n=1) + 1
LT.Cat.training$subject <- LT.Cat.training$subject + n_subjects
# Merge both datasets
LT.training <- rbind(LT.SingObj.training, LT.Cat.training)
# Set all factor variables to factors, with labels if meaningful
LT.training$subject <- factor(LT.training$subject)
LT.training$explo_overlap <- factor(LT.training$explo_overlap)
LT.training$theory <- factor(LT.training$theory,
							 labels = c("Compound Representations",
										"Labels as Features"))
LT.training$experiment <- factor(LT.training$experiment,
								 labels = c("Category",
											"Single Object"))

# MODELS
# Global model for both LaF and CR together
#LT.all.lmer.15 <- lmer(looking_time ~ trial*labelled*theory +
#					(1 + trial + labelled | subject) +
#					(1 + trial + labelled + theory | experiment),
#					data = LT.training
#					)
#print("Model 15")
#print(summary(LT.all.lmer.15))
#LT.all.lmer.14 <- lmer(looking_time ~ trial*labelled + trial*theory +
#					labelled*theory +
#					(1 + trial + labelled | subject) +
#					(1 + trial + labelled + theory | experiment),
#					data = LT.training
#					)
#print("Model 14")
#print(summary(LT.all.lmer.14))
#LT.all.lmer.13 <- lmer(looking_time ~ trial*labelled + trial*theory +
#					(1 + trial + labelled | subject) +
#					(1 + trial + labelled + theory | experiment),
#					data = LT.training
#					)
#print("Model 13")
#print(summary(LT.all.lmer.13))
#LT.all.lmer.12 <- lmer(looking_time ~ trial*labelled + labelled*theory +
#					(1 + trial + labelled | subject) +
#					(1 + trial + labelled + theory | experiment),
#					data = LT.training
#					)
#print("Model 12")
#print(summary(LT.all.lmer.12))
#LT.all.lmer.11 <- lmer(looking_time ~ trial*theory + labelled*theory +
#					(1 + trial + labelled | subject) +
#					(1 + trial + labelled + theory | experiment),
#					data = LT.training
#					)
#print("Model 11")
#print(summary(LT.all.lmer.11))
#LT.all.lmer.10 <- lmer(looking_time ~ trial*labelled + theory +
#					(1 + trial + labelled | subject) +
#					(1 + trial + labelled + theory | experiment),
#					data = LT.training
#					)
#print("Model 10")
#print(summary(LT.all.lmer.10))
#LT.all.lmer.9 <- lmer(looking_time ~ trial*theory + labelled +
#					(1 + trial + labelled | subject) +
#					(1 + trial + labelled + theory | experiment),
#					data = LT.training
#					)
#print("Model 9")
#print(summary(LT.all.lmer.9))
#LT.all.lmer.8 <- lmer(looking_time ~ labelled*theory + trial +
#					(1 + trial + labelled | subject) +
#					(1 + trial + labelled + theory | experiment),
#					data = LT.training
#					)
#print("Model 8")
#print(summary(LT.all.lmer.8))
#LT.all.lmer.7 <- lmer(looking_time ~ trial + labelled + theory +
#					(1 + trial + labelled | subject) +
#					(1 + trial + labelled + theory | experiment),
#					data = LT.training
#					)
#print("Model 7")
#print(summary(LT.all.lmer.7))
#LT.all.lmer.6 <- lmer(looking_time ~ trial + labelled +
#					(1 + trial + labelled | subject) +
#					(1 + trial + labelled + theory | experiment),
#					data = LT.training
#					)
#print("Model 6")
#print(summary(LT.all.lmer.6))
#LT.all.lmer.5 <- lmer(looking_time ~ trial + theory +
#					(1 + trial + labelled | subject) +
#					(1 + trial + labelled + theory | experiment),
#					data = LT.training
#					)
#print("Model 5")
#print(summary(LT.all.lmer.5))
#LT.all.lmer.4 <- lmer(looking_time ~ labelled + theory +
#					(1 + trial + labelled | subject) +
#					(1 + trial + labelled + theory | experiment),
#					data = LT.training
#					)
#print("Model 4")
#print(summary(LT.all.lmer.4))
#LT.all.lmer.3 <- lmer(looking_time ~ theory +
#					(1 + trial + labelled | subject) +
#					(1 + trial + labelled + theory | experiment),
#					data = LT.training
#					)
#print("Model 3")
#print(summary(LT.all.lmer.3))
#LT.all.lmer.2 <- lmer(looking_time ~ labelled +
#					(1 + trial + labelled | subject) +
#					(1 + trial + labelled + theory | experiment),
#					data = LT.training
#					)
#print("Model 2")
#print(summary(LT.all.lmer.2))
#LT.all.lmer.1 <- lmer(looking_time ~ trial +
#					(1 + trial + labelled | subject) +
#					(1 + trial + labelled + theory | experiment),
#					data = LT.training
#					)
#print("Model 1")
#print(summary(LT.all.lmer.1))
#LT.all.lmer.0 <- lmer(looking_time ~ 1 +
#					(1 + trial + labelled | subject) +
#					(1 + trial + labelled + theory | experiment),
#					data = LT.training
#					)
#print("Model 00")
#print(summary(LT.all.lmer.0))
#LT.all.comparison <- anova(LT.all.lmer.0,
#						   LT.all.lmer.1,
#						   LT.all.lmer.2,
#						   LT.all.lmer.3,
#						   LT.all.lmer.4,
#						   LT.all.lmer.5,
#						   LT.all.lmer.6,
#						   LT.all.lmer.7,
#						   LT.all.lmer.8,
#						   LT.all.lmer.9,
#						   LT.all.lmer.10,
#						   LT.all.lmer.11,
#						   LT.all.lmer.12,
#						   LT.all.lmer.13,
#						   LT.all.lmer.14,
#						   LT.all.lmer.15)
#print("Model comparison")
#print(LT.all.comparison)

## Models for LaF only -- There's no point running models without main effect of trial
## Including only main effect for trial
#single_obj.LaF.lmer.trial_only <- lmer(looking_time ~ trial +
#									   (1 + trial + labelled | subject) +
#									   (1 + trial + labelled | explo_overlap),
#									   data = single_obj.LaF.data
#									   )
## Including main effects for trial and labelled
#single_obj.LaF.lmer.labelled <- lmer(looking_time ~ trial + labelled +
#									 (1 + trial + labelled | subject) +
#									 (1 + trial + labelled | explo_overlap),
#									 data = single_obj.LaF.data
#									 )
## Including all interactions and main effects
#single_obj.LaF.all.lmer <- lmer(looking_time ~ trial*labelled +
#								(1 + trial + labelled | subject) +
#								(1 + trial + labelled | explo_overlap),
#								data = single_obj.LaF.data
#								)
## Comparing models with different fixed effects
#single_obj.LaF.comparison <- anova(single_obj.LaF.lmer.trial_only,
#								   single_obj.LaF.lmer.labelled,
#								   single_obj.LaF.all.lmer)
## Results: adding all interactions helps the model, not label only
## Best model: single_obj.LaF.all.lmer
## Models for CR only
## Including only main effect for trial
#single_obj.CR.lmer.trial_only <- lmer(looking_time ~ trial +
#									  (1 + trial + labelled | subject) +
#									  (1 + trial + labelled | explo_overlap),
#									  data = single_obj.CR.data
#									  )
## Including main effects for trial and labelled
#single_obj.CR.lmer.labelled <- lmer(looking_time ~ trial + labelled +
#									(1 + trial + labelled | subject) +
#									(1 + trial + labelled | explo_overlap),
#									data = single_obj.CR.data
#									)
## Including all interactions and main effects
#single_obj.CR.all.lmer <- lmer(looking_time ~ trial*labelled +
#							   (1 + trial + labelled | subject) +
#							   (1 + trial + labelled | explo_overlap),
#							   data = single_obj.CR.data
#							   )
## Comparing models with different fixed effects
#single_obj.CR.comparison <- anova(single_obj.CR.lmer.trial_only,
#								  single_obj.CR.lmer.labelled,
#								  single_obj.CR.all.lmer)
## Results: adding more parameters than trial alone didn't help the model
## Best model: single_obj.CR.lmer.trial_only
