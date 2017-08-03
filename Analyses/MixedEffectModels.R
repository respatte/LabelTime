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
# Merge both datasets
LT <- rbind(LT.SingObj, LT.Cat)
LT$experiment <- factor(LT$experiment,
								 labels = c("Category",
											"Single Object"))

# GLOBAL MODEL


# SINGLE OBJECT MODEL -- FIXED EFFECTS
# All models taken as previous one minus last effect
LT.SingObj.lmer.0 <- lmer(looking_time ~ 1 + trial + labelled +
                            labelled:theory + trial:theory +
                            (1 + trial + labelled | subject),
                          data = LT.SingObj) # 4. Adding trial:labelled:theory didn't improve the model
#LT.SingObj.lmer.1 <- update(LT.SingObj.lmer.0, . ~ . - trial:labelled:theory)
LT.SingObj.lmer.2 <- update(LT.SingObj.lmer.1, . ~ . - trial:theory) # 3. Adding labelled:theory only marginally improved the model (>.1)
LT.SingObj.lmer.3 <- update(LT.SingObj.lmer.2, . ~ . - labelled:theory) # 2. Adding trial:labelled didn't improve the model
#LT.SingObj.lmer.4 <- update(LT.SingObj.lmer.3, . ~ . - trial:labelled) # 1. Adding theory didn't improve the model, removing from global model
#LT.SingObj.lmer.5 <- update(LT.SingObj.lmer.4, . ~ . - theory)
LT.SingObj.lmer.6 <- update(LT.SingObj.lmer.5, . ~ . - labelled)
LT.SingObj.lmer.7 <- update(LT.SingObj.lmer.6, . ~ . - trial)
# Model comparison against each other hierarchically
LT.SingObj.comparison <- anova(LT.SingObj.lmer.7,
                               LT.SingObj.lmer.6,
                               LT.SingObj.lmer.2,
                               LT.SingObj.lmer.0)
print(LT.SingObj.comparison)
# Select final fixed effects model
LT.SingObj.fixed <- LT.SingObj.lmer.0

# SINGLE OBJECT MODEL -- RANDOM EFFECTS
LT.SingObj.random.0 <- update(LT.SingObj.fixed, . ~ . - (1 + trial + labelled | subject) + (1 | subject))
LT.SingObj.random.1 <- update(LT.SingObj.fixed, . ~ . - (1 + trial + labelled | subject) + (0 + trial | subject))
LT.SingObj.random.2 <- update(LT.SingObj.fixed, . ~ . - (1 + trial + labelled | subject) + (0 + labelled | subject))
#LT.SingObj.random.3 <- update(LT.SingObj.fixed, . ~ . - (1 + trial + labelled | subject) + (1 + trial | subject))
# Didn't converge
LT.SingObj.random.4 <- update(LT.SingObj.fixed, . ~ . - (1 + trial + labelled | subject) + (1 + labelled | subject))
LT.SingObj.random.5 <- update(LT.SingObj.fixed, . ~ . - (1 + trial + labelled | subject) + (1 + trial + labelled | subject))
LT.SingObj.random.6 <- update(LT.SingObj.fixed, . ~ . - (1 + trial + labelled | subject) + (trial*labelled | subject))
LT.SingObj.random.comparison <- anova(LT.SingObj.random.0,
									  LT.SingObj.random.1,
									  LT.SingObj.random.2,
									  LT.SingObj.random.4,
									  LT.SingObj.random.5,
									  LT.SingObj.random.6)
print(LT.SingObj.random.comparison)
LT.SingObj.final <- LT.SingObj.random.6

# SINGLE OBJECT MODEL -- MAKE PREDICTIONS
#LT.SingObj$fit <- predict(LT.SingObj.final, newdata=LT.SingObj, re.form=NA)
#write.csv(LT.SingObj, file="../Results/SingleObject_LT_fitted.csv", row.names=F)

# SINGLE OBJECT PER THEORY -- MAIN EFFECTS
# Create sub-datasets
LT.SingObj.LaF <- LT.SingObj[LT.SingObj$theory == "Labels as Features",]
LT.SingObj.CR <- LT.SingObj[LT.SingObj$theory == "Compound Representations",]
# LABELS AS FEATURES
# All models taken as previous one minus last effect
LT.SingObj.LaF.lmer.0 <- lmer(looking_time ~ 1 + trial + labelled + trial:labelled +
                                (1 + trial + labelled | subject),
                              data = LT.SingObj.LaF) # Adding the interaction only marginally improved the model
LT.SingObj.LaF.lmer.1 <- update(LT.SingObj.LaF.lmer.0, . ~ . - trial:labelled)
LT.SingObj.LaF.lmer.2 <- update(LT.SingObj.LaF.lmer.1, . ~ . - labelled)
LT.SingObj.LaF.lmer.3 <- update(LT.SingObj.LaF.lmer.2, . ~ . - trial)
# Model comparison against each other hierarchically
LT.SingObj.LaF.comparison <- anova(LT.SingObj.LaF.lmer.3,
                                   LT.SingObj.LaF.lmer.2,
                                   LT.SingObj.LaF.lmer.1,
                                   LT.SingObj.LaF.lmer.0)
print(LT.SingObj.LaF.comparison)
# Select final fixed effects model
LT.SingObj.LaF.final <- LT.SingObj.LaF.lmer.0
# COMPOUND REPRESENTATIONS
# All models taken as previous one minus last effect
LT.SingObj.CR.lmer.0 <- lmer(looking_time ~ 1 + trial + labelled + trial:labelled +
                                (1 + trial + labelled | subject),
                              data = LT.SingObj.CR)
LT.SingObj.CR.lmer.1 <- update(LT.SingObj.CR.lmer.0, . ~ . - trial:labelled) # No effect other than trial
LT.SingObj.CR.lmer.2 <- update(LT.SingObj.CR.lmer.1, . ~ . - labelled)
LT.SingObj.CR.lmer.3 <- update(LT.SingObj.CR.lmer.2, . ~ . - trial)
# Model comparison against each other hierarchically
LT.SingObj.CR.comparison <- anova(LT.SingObj.CR.lmer.3,
                                   LT.SingObj.CR.lmer.2,
                                   LT.SingObj.CR.lmer.1,
                                   LT.SingObj.CR.lmer.0)
print(LT.SingObj.CR.comparison)
# Select final fixed effects model
LT.SingObj.CR.final <- LT.SingObj.CR.lmer.2

# CATEGORY MODEL -- FIXED EFFECTS
# All models taken as previous one minus last effect
LT.Cat.lmer.0 <- lmer(looking_time ~ 1 + trial + labelled +
                            trial:labelled + trial:theory +
                            trial:labelled:theory +
                            (1 + trial + labelled | subject),
                          data = LT.Cat)
LT.Cat.lmer.1 <- update(LT.Cat.lmer.0, . ~ . - trial:labelled:theory)
LT.Cat.lmer.2 <- update(LT.Cat.lmer.1, . ~ . - trial:theory) # 2. Adding labelled:theory didn't improve the model
#LT.Cat.lmer.3 <- update(LT.Cat.lmer.2, . ~ . - labelled:theory)
LT.Cat.lmer.4 <- update(LT.Cat.lmer.3, . ~ . - trial:labelled) # 1. Adding theory didn't improve the model
#LT.Cat.lmer.5 <- update(LT.Cat.lmer.4, . ~ . - theory)
LT.Cat.lmer.6 <- update(LT.Cat.lmer.5, . ~ . - labelled)
LT.Cat.lmer.7 <- update(LT.Cat.lmer.6, . ~ . - trial)
# Model comparison against each other hierarchically
LT.Cat.comparison <- anova(LT.Cat.lmer.7,
                           LT.Cat.lmer.6,
                           LT.Cat.lmer.4,
                           LT.Cat.lmer.2,
                           LT.Cat.lmer.1,
                           LT.Cat.lmer.0)
print(LT.Cat.comparison)
# Select final fixed effects model
LT.Cat.fixed <- LT.Cat.lmer.0

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

# CATEGORY PER THEORY -- MAIN EFFECTS
# Create sub-datasets
LT.Cat.LaF <- LT.Cat[LT.Cat$theory == "Labels as Features",]
LT.Cat.CR <- LT.Cat[LT.Cat$theory == "Compound Representations",]
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
