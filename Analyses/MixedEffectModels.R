# LIBRARY IMPORTS
library(ggplot2)
library(dplyr)
library(lme4)

# DIVERSION
# Create a diversion of standard output to a file (to save results)
# sink(file="lmer_results", append=TRUE, type="output")


# DATA HANDLING
# Import data, from both Category and SingleObject
LT.SingObj <- read.csv("../Results/SingleObject_lsize5_LT.csv", head=TRUE)
LT.Cat <- read.csv("../Results/Category_lsize5_LT.csv", head=TRUE)
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

# MODELS -- FIXED EFFECTS
# Test main effects against intercept only
LT.intercept <- lmer(looking_time ~ 1 +
					 (1 + trial + labelled | subject),
					 data = LT.training
					 )
#LT.main_effect.1 <- update(LT.intercept, . ~ . + trial)
#LT.main_effect.2 <- update(LT.intercept, . ~ . + labelled)
#LT.main_effect.3 <- update(LT.intercept, . ~ . + theory)
#LT.main_effect.4 <- update(LT.intercept, . ~ . + experiment)
#LT.main_effect.comparison <- anova(LT.intercept,
#								   LT.main_effect.1,
#								   LT.main_effect.2,
#								   LT.main_effect.3,
#								   LT.main_effect.4)
#print(LT.main_effect.comparison)
LT.main_effect <- update(LT.intercept, . ~ . + trial + theory + experiment)
# Test 2-way interactions against intercept + main effects
#LT.2_way.1 <- update(LT.main_effect, . ~ . + trial:labelled)
#LT.2_way.2 <- update(LT.main_effect, . ~ . + trial:theory)
#LT.2_way.3 <- update(LT.main_effect, . ~ . + trial:experiment)
#LT.2_way.4 <- update(LT.main_effect, . ~ . + labelled:theory)
#LT.2_way.5 <- update(LT.main_effect, . ~ . + labelled:experiment)
#LT.2_way.6 <- update(LT.main_effect, . ~ . + theory:experiment)
#LT.2_way.comparison <- anova(LT.main_effect,
#							 LT.2_way.1,
#							 LT.2_way.2,
#							 LT.2_way.3,
#							 LT.2_way.4,
#							 LT.2_way.5,
#							 LT.2_way.6)
#print(LT.2_way.comparison)
LT.2_way <- update(LT.main_effect, . ~ . + trial:labelled + labelled:theory)
# Test 3-way interactions agains intercept + main effects + 2-way interaction
#LT.3_way.1 <- update(LT.2_way, . ~ . + trial:labelled:theory)
LT.3_way.2 <- update(LT.2_way, . ~ . + trial:labelled:experiment)
#LT.3_way.3 <- update(LT.2_way, . ~ . + trial:theory:experiment)
#LT.3_way.4 <- update(LT.2_way, . ~ . + labelled:theory:experiment)
#LT.3_way.comparison <- anova(LT.2_way,
#							 LT.3_way.1,
#							 LT.3_way.2,
#							 LT.3_way.3,
#							 LT.3_way.4)
#print(LT.3_way.comparison)
LT.3_way <- LT.3_way.2
# Test 4-way interaction against intercept + main effects + 2/3-way interactions
#LT.4_way <- update(LT.3_way, . ~ . + trial:labelled:theory:experiment)
#LT.4_way.comparison <- anova(LT.3_way, LT.4_way)
#print(LT.4_way.comparison)
LT.fixed <- LT.3_way

# MODELS -- RANDOM EFFECTS
LT.random.0 <- update(LT.fixed, . ~ . - (1 + trial + labelled | subject) + (1 | subject))
LT.random.2 <- update(LT.fixed, . ~ . - (1 + trial + labelled | subject) + (0 + trial | subject))
LT.random.3 <- update(LT.fixed, . ~ . - (1 + trial + labelled | subject) + (0 + labelled | subject))
LT.random.4 <- update(LT.fixed, . ~ . - (1 + trial + labelled | subject) + (1 + trial | subject))
LT.random.5 <- update(LT.fixed, . ~ . - (1 + trial + labelled | subject) + (1 + labelled | subject))
LT.random.6 <- update(LT.fixed, . ~ . - (1 + trial + labelled | subject) + (1 + trial + labelled | subject))
LT.random.comparison <- anova(LT.random.0,
							  LT.random.2,
							  LT.random.3,
							  LT.random.4,
							  LT.random.5,
							  LT.random.6)
print(LT.random.comparison)
LT.final <- LT.random.6

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
