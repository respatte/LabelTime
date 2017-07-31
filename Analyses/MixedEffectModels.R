# LIBRARY IMPORTS
library(ggplot2)
library(dplyr)
library(lme4)

# DIVERSION
# Create a diversion of standard output to a file (to save results)
# sink(file="lmer_results", append=TRUE, type="output")


# DATA HANDLING
# Import data, from both Category and SingleObject
LT.SingObj <- read.csv("../Results/SingleObject_LT.csv", head=TRUE)
LT.Cat <- read.csv("../Results/Category_LT.csv", head=TRUE)
# Drop out simple BPN
LT.SingObj <- LT.SingObj[LT.SingObj$model == "DMN",]
LT.Cat <- LT.Cat[LT.Cat$model == "DMN",]
# Create experiment variable for each dataset
LT.SingObj$experiment <- "SingleObject"
LT.Cat$experiment <- "Category"
# Increment subject from Cat to not overlap with SingObj
n_subjects <- tail(LT.SingObj$subject, n=1) + 1
LT.Cat$subject <- LT.Cat$subject + n_subjects
# Set all factor variables to factors, with labels if meaningful
LT.SingObj$explo_overlap <- factor(LT.SingObj$explo_overlap)
LT.SingObj$theory <- factor(LT.SingObj$theory,
							labels = c("Compound Representations",
									   "Labels as Features"))
LT.Cat$explo_overlap <- factor(LT.Cat$explo_overlap)
LT.Cat$theory <- factor(LT.Cat$theory, labels = c("Compound Representations",
												  "Labels as Features"))
# Select training sample
LT.SingObj.training <- LT.SingObj[LT.SingObj$subject < 160,]
LT.Cat.training <- LT.Cat[LT.Cat$subject < 160 + n_subjects,]
# Merge both datasets
LT.training <- rbind(LT.SingObj.training, LT.Cat.training)
LT.training$experiment <- factor(LT.training$experiment,
								 labels = c("Category",
											"Single Object"))

# GLOBAL MODEL -- FIXED EFFECTS
# Test main effects against intercept only
LT.intercept <- lmer(looking_time ~ 1 +
					 (1 + trial + labelled | subject),
					 data = LT.training
					 )
LT.main_effect.1 <- update(LT.intercept, . ~ . + trial)
LT.main_effect.2 <- update(LT.intercept, . ~ . + labelled)
LT.main_effect.3 <- update(LT.intercept, . ~ . + theory)
LT.main_effect.4 <- update(LT.intercept, . ~ . + experiment)
LT.main_effect.comparison <- anova(LT.intercept,
								   LT.main_effect.1,
								   LT.main_effect.2,
								   LT.main_effect.3,
								   LT.main_effect.4)
print(LT.main_effect.comparison)
LT.main_effect <- update(LT.intercept, . ~ . + trial + theory + experiment)
# Test 2-way interactions against intercept + main effects
LT.2_way.1 <- update(LT.main_effect, . ~ . + trial:labelled)
LT.2_way.2 <- update(LT.main_effect, . ~ . + trial:theory)
LT.2_way.3 <- update(LT.main_effect, . ~ . + trial:experiment)
LT.2_way.4 <- update(LT.main_effect, . ~ . + labelled:theory)
LT.2_way.5 <- update(LT.main_effect, . ~ . + labelled:experiment)
LT.2_way.6 <- update(LT.main_effect, . ~ . + theory:experiment)
LT.2_way.comparison <- anova(LT.main_effect,
							 LT.2_way.1,
							 LT.2_way.2,
							 LT.2_way.3,
							 LT.2_way.4,
							 LT.2_way.5,
							 LT.2_way.6)
print(LT.2_way.comparison)
LT.2_way <- update(LT.main_effect, . ~ . + trial:labelled + labelled:theory)
# Test 3-way interactions agains intercept + main effects + 2-way interaction
LT.3_way.1 <- update(LT.2_way, . ~ . + trial:labelled:theory)
LT.3_way.2 <- update(LT.2_way, . ~ . + trial:labelled:experiment)
LT.3_way.3 <- update(LT.2_way, . ~ . + trial:theory:experiment)
LT.3_way.4 <- update(LT.2_way, . ~ . + labelled:theory:experiment)
LT.3_way.comparison <- anova(LT.2_way,
							 LT.3_way.1,
							 LT.3_way.2,
							 LT.3_way.3,
							 LT.3_way.4)
print(LT.3_way.comparison)
LT.3_way <- LT.3_way.2
# Test 4-way interaction against intercept + main effects + 2/3-way interactions
LT.4_way <- update(LT.3_way, . ~ . + trial:labelled:theory:experiment)
LT.4_way.comparison <- anova(LT.3_way, LT.4_way)
print(LT.4_way.comparison)
LT.fixed <- LT.3_way

# GLOBAL MODEL -- RANDOM EFFECTS
LT.random.0 <- update(LT.fixed, . ~ . - (1 + trial + labelled | subject) + (1 | subject))
LT.random.1 <- update(LT.fixed, . ~ . - (1 + trial + labelled | subject) + (0 + trial | subject))
LT.random.2 <- update(LT.fixed, . ~ . - (1 + trial + labelled | subject) + (0 + labelled | subject))
LT.random.3 <- update(LT.fixed, . ~ . - (1 + trial + labelled | subject) + (1 + trial | subject))
LT.random.4 <- update(LT.fixed, . ~ . - (1 + trial + labelled | subject) + (1 + labelled | subject))
LT.random.5 <- update(LT.fixed, . ~ . - (1 + trial + labelled | subject) + (1 + trial + labelled | subject))
LT.random.6 <- update(LT.fixed, . ~ . - (1 + trial + labelled | subject) + (trial*labelled | subject))
LT.random.comparison <- anova(LT.random.0,
							  LT.random.1,
							  LT.random.2,
							  LT.random.3,
							  LT.random.4,
							  LT.random.5,
							  LT.random.6)
print(LT.random.comparison)
LT.final <- LT.random.6

# SINGLE OBJECT MODEL -- FIXED EFFECTS
# Test main effects against intercept only
LT.SingObj.intercept <- lmer(looking_time ~ 1 +
							 (1 + trial + labelled | subject),
							 data = LT.SingObj.training
							 )
LT.SingObj.main_effect.1 <- update(LT.SingObj.intercept, . ~ . + trial)
LT.SingObj.main_effect.2 <- update(LT.SingObj.intercept, . ~ . + labelled)
LT.SingObj.main_effect.3 <- update(LT.SingObj.intercept, . ~ . + theory)
LT.SingObj.main_effect.comparison <- anova(LT.SingObj.intercept,
								   LT.SingObj.main_effect.1,
								   LT.SingObj.main_effect.2,
								   LT.SingObj.main_effect.3)
print(LT.SingObj.main_effect.comparison)
LT.SingObj.main_effect <- update(LT.SingObj.intercept, . ~ . + trial + theory)
# Test 2-way interactions against intercept + main effects
LT.SingObj.2_way.1 <- update(LT.SingObj.main_effect, . ~ . + trial:labelled)
LT.SingObj.2_way.2 <- update(LT.SingObj.main_effect, . ~ . + trial:theory)
LT.SingObj.2_way.3 <- update(LT.SingObj.main_effect, . ~ . + labelled:theory)
LT.SingObj.2_way.comparison <- anova(LT.SingObj.main_effect,
							 LT.SingObj.2_way.1,
							 LT.SingObj.2_way.2,
							 LT.SingObj.2_way.3)
print(LT.SingObj.2_way.comparison)
LT.SingObj.2_way <- update(LT.SingObj.main_effect, . ~ . + trial:labelled + labelled:theory)
# Test 3-way interaction agains intercept + main effects + 2-way interaction
LT.SingObj.3_way.1 <- update(LT.SingObj.2_way, . ~ . + trial:labelled:theory)
LT.SingObj.3_way.comparison <- anova(LT.SingObj.2_way,
							 LT.SingObj.3_way.1)
print(LT.SingObj.3_way.comparison)
LT.SingObj.fixed <- LT.SingObj.2_way

# SINGLE OBJECT MODEL -- RANDOM EFFECTS
LT.SingObj.random.0 <- update(LT.SingObj.fixed, . ~ . - (1 + trial + labelled | subject) + (1 | subject))
LT.SingObj.random.1 <- update(LT.SingObj.fixed, . ~ . - (1 + trial + labelled | subject) + (0 + trial | subject))
LT.SingObj.random.2 <- update(LT.SingObj.fixed, . ~ . - (1 + trial + labelled | subject) + (0 + labelled | subject))
LT.SingObj.random.3 <- update(LT.SingObj.fixed, . ~ . - (1 + trial + labelled | subject) + (1 + trial | subject))
LT.SingObj.random.4 <- update(LT.SingObj.fixed, . ~ . - (1 + trial + labelled | subject) + (1 + labelled | subject))
LT.SingObj.random.5 <- update(LT.SingObj.fixed, . ~ . - (1 + trial + labelled | subject) + (1 + trial + labelled | subject))
LT.SingObj.random.6 <- update(LT.SingObj.fixed, . ~ . - (1 + trial + labelled | subject) + (trial*labelled | subject))
LT.SingObj.random.comparison <- anova(LT.SingObj.random.0,
									  LT.SingObj.random.1,
									  LT.SingObj.random.2,
									  LT.SingObj.random.3,
									  LT.SingObj.random.4,
									  LT.SingObj.random.5,
									  LT.SingObj.random.6)
print(LT.SingObj.random.comparison)
LT.SingObj.final <- LT.SingObj.random.6

# SINGLE OBJECT MODEL -- MAKE PREDICTIONS
LT.SingObj$fit <- predict(LT.SingObj.final, newdata=LT.SingObj, re.form=NA)
write.csv(LT.SingObj, file="../Results/SingleObject_LT_fitted.csv", row.names=F)

# SINGLE OBJECT PER THEORY -- MAIN EFFECTS
# Create sub-datasets
LT.SingObj.LaF <- LT.SingObj.training[LT.SingObj.training$theory == "Labels as Features",]
LT.SingObj.CR <- LT.SingObj.training[LT.SingObj.training$theory == "Compound Representations",]
# All effects seen in SingObj model, for LaF
LT.SingObj.LaF.0 <- lmer(looking_time ~ 1 +
						 (trial*labelled | subject),
						 data = LT.SingObj.LaF
						 )
LT.SingObj.LaF.1 <- update(LT.SingObj.LaF.0, . ~ . + trial)
LT.SingObj.LaF.2 <- update(LT.SingObj.LaF.0, . ~ . + trial:labelled)
LT.SingObj.LaF.3 <- update(LT.SingObj.LaF.0, . ~ . + trial*labelled)
LT.SingObj.LaF.comparison <- anova(LT.SingObj.LaF.0,
								   LT.SingObj.LaF.1,
								   LT.SingObj.LaF.2,
								   LT.SingObj.LaF.3)
print(LT.SingObj.LaF.comparison)
LT.SingObj.LaF.final <- LT.SingObj.LaF.3
# All effects seen in SingObj model, for CR
LT.SingObj.CR.0 <- lmer(looking_time ~ 1 +
						(trial*labelled | subject),
						data = LT.SingObj.CR
						)
LT.SingObj.CR.1 <- update(LT.SingObj.CR.0, . ~ . + trial)
LT.SingObj.CR.2 <- update(LT.SingObj.CR.0, . ~ . + trial:labelled)
LT.SingObj.CR.3 <- update(LT.SingObj.CR.0, . ~ . + trial*labelled)
LT.SingObj.CR.comparison <- anova(LT.SingObj.CR.0,
								  LT.SingObj.CR.1,
								  LT.SingObj.CR.2,
								  LT.SingObj.CR.3)
print(LT.SingObj.CR.comparison)
LT.SingObj.CR.final <- LT.SingObj.CR.1

# CATEGORY MODEL -- FIXED EFFECTS
# Test main effects against intercept only
LT.Cat.intercept <- lmer(looking_time ~ 1 +
					 (1 + trial + labelled | subject),
					 data = LT.Cat.training
					 )
LT.Cat.main_effect.1 <- update(LT.Cat.intercept, . ~ . + trial)
LT.Cat.main_effect.2 <- update(LT.Cat.intercept, . ~ . + labelled)
LT.Cat.main_effect.3 <- update(LT.Cat.intercept, . ~ . + theory)
LT.Cat.main_effect.comparison <- anova(LT.Cat.intercept,
								   LT.Cat.main_effect.1,
								   LT.Cat.main_effect.2,
								   LT.Cat.main_effect.3)
print(LT.Cat.main_effect.comparison)
LT.Cat.main_effect <- update(LT.Cat.intercept, . ~ . + trial)
# Test 2-way interactions against intercept + main effects
LT.Cat.2_way.1 <- update(LT.Cat.main_effect, . ~ . + trial:labelled)
LT.Cat.2_way.2 <- update(LT.Cat.main_effect, . ~ . + trial:theory)
LT.Cat.2_way.3 <- update(LT.Cat.main_effect, . ~ . + labelled:theory)
LT.Cat.2_way.comparison <- anova(LT.Cat.main_effect,
							 LT.Cat.2_way.1,
							 LT.Cat.2_way.2,
							 LT.Cat.2_way.3)
print(LT.Cat.2_way.comparison)
LT.Cat.2_way <- update(LT.Cat.main_effect, . ~ . + trial:labelled + labelled:theory)
# Test 3-way interaction agains intercept + main effects + 2-way interaction
LT.Cat.3_way.1 <- update(LT.Cat.2_way, . ~ . + trial:labelled:theory)
LT.Cat.3_way.comparison <- anova(LT.Cat.2_way,
							 LT.Cat.3_way.1)
print(LT.Cat.3_way.comparison)
LT.Cat.fixed <- LT.Cat.2_way

# CATEGORY MODEL -- RANDOM EFFECTS
LT.Cat.random.0 <- update(LT.Cat.fixed, . ~ . - (1 + trial + labelled | subject) + (1 | subject))
LT.Cat.random.1 <- update(LT.Cat.fixed, . ~ . - (1 + trial + labelled | subject) + (0 + trial | subject))
LT.Cat.random.2 <- update(LT.Cat.fixed, . ~ . - (1 + trial + labelled | subject) + (0 + labelled | subject))
LT.Cat.random.3 <- update(LT.Cat.fixed, . ~ . - (1 + trial + labelled | subject) + (1 + trial | subject))
LT.Cat.random.4 <- update(LT.Cat.fixed, . ~ . - (1 + trial + labelled | subject) + (1 + labelled | subject))
LT.Cat.random.5 <- update(LT.Cat.fixed, . ~ . - (1 + trial + labelled | subject) + (1 + trial + labelled | subject))
LT.Cat.random.6 <- update(LT.Cat.fixed, . ~ . - (1 + trial + labelled | subject) + (trial*labelled | subject))
LT.Cat.random.comparison <- anova(LT.Cat.random.0,
								  LT.Cat.random.1,
								  LT.Cat.random.2,
								  LT.Cat.random.3,
								  LT.Cat.random.4,
								  LT.Cat.random.5,
								  LT.Cat.random.6)
print(LT.Cat.random.comparison)
LT.Cat.final <- LT.Cat.random.6

# CATEGORY MODEL -- MAKE PREDICTIONS
LT.Cat$fit <- predict(LT.Cat.final, newdata=LT.Cat, re.form=NA)
write.csv(LT.Cat, file="../Results/Category_LT_fitted.csv", row.names=F)

# SINGLE OBJECT PER THEORY -- MAIN EFFECTS
# Create sub-datasets
LT.Cat.LaF <- LT.Cat.training[LT.Cat.training$theory == "Labels as Features",]
LT.Cat.CR <- LT.Cat.training[LT.Cat.training$theory == "Compound Representations",]
# All effects seen in SingObj model, for LaF
LT.Cat.LaF.0 <- lmer(looking_time ~ 1 +
					 (trial*labelled | subject),
					 data = LT.Cat.LaF
					 )
LT.Cat.LaF.1 <- update(LT.Cat.LaF.0, . ~ . + trial)
LT.Cat.LaF.2 <- update(LT.Cat.LaF.0, . ~ . + trial:labelled)
LT.Cat.LaF.3 <- update(LT.Cat.LaF.0, . ~ . + trial*labelled)
LT.Cat.LaF.comparison <- anova(LT.Cat.LaF.0,
							   LT.Cat.LaF.1,
							   LT.Cat.LaF.2,
							   LT.Cat.LaF.3)
print(LT.Cat.LaF.comparison)
LT.Cat.LaF.final <- LT.Cat.LaF.1
 All effects seen in SingObj model, for CR
LT.Cat.CR.0 <- lmer(looking_time ~ 1 +
					(trial*labelled | subject),
					data = LT.Cat.CR
					)
LT.Cat.CR.1 <- update(LT.Cat.CR.0, . ~ . + trial)
LT.Cat.CR.2 <- update(LT.Cat.CR.0, . ~ . + trial:labelled)
LT.Cat.CR.3 <- update(LT.Cat.CR.0, . ~ . + trial*labelled)
LT.Cat.CR.comparison <- anova(LT.Cat.CR.0,
							  LT.Cat.CR.1,
							  LT.Cat.CR.2,
							  LT.Cat.CR.3)
print(LT.Cat.CR.comparison)
LT.Cat.CR.final <- LT.Cat.CR.1
