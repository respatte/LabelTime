# LIBRARY IMPORTS
library(ggplot2)
library(dplyr)
library(lme4)

# DATA HANDLING
# Import data
label_time.data <- read.csv("SingleObject_LT.csv", head=TRUE)
# Set all factor variables to factors
label_time.data$trial <- as.factor(looking_times$trial)
label_time.data$subject <- as.factor(looking_times$subject)
label_time.data$explo_overlap <- as.factor(looking_times$explo_overlap)
label_time.LaF.data <- filter(label_time.data, model == "LaF")
label_time.CR.data <- filter(label_time.data, model == "CR")

# MODELS
label_time.lmer1 <- lmer(label_time.data ~ trial + model + labelled +
						 trial*model + trial*labelled + model*labelled +
						 trial*model*labelled +
						 (1 + trial + labelled | subject) +
						 (1 + trial + labelled + model | explo_ratio)
						 )
label_time.LaF.lmer1 <- lmer(label_time.LaF.data ~ trial + labelled +
							 trial*labelled +
							 (1 + trial + labelled | subject) +
							 (1 + trial + labelled | explo_ratio)
							 )
label_time.CR.lmer1 <- lmer(label_time.CR.data ~ trial + labelled +
							trial*labelled +
							(1 + trial + labelled | subject) +
							(1 + trial + labelled | explo_ratio)
							)
