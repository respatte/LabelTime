# LIBRARY IMPORTS
library(reshape2)
library(plyr)
library(tidyverse)
library(lme4)

##' @importFrom reshape2 melt
##' @importFrom plyr ldply name_rows
augment.ranef.mer <- function(x,
                              ci.level=0.9,
                              reorder=TRUE,
                              order.var=1) {
  tmpf <- function(z) {
    if (is.character(order.var) && !order.var %in% names(z)) {
      order.var <- 1
      warning("order.var not found, resetting to 1")
    }
    ## would use plyr::name_rows, but want levels first
    zz <- data.frame(level=rownames(z),z,check.names=FALSE)
    if (reorder) {
      ## if numeric order var, add 1 to account for level column
      ov <- if (is.numeric(order.var)) order.var+1 else order.var
      zz$level <- reorder(zz$level, zz[,order.var+1], FUN=identity)
    }
    ## Q-Q values, for each column separately
    qq <- c(apply(z,2,function(y) {
      qnorm(ppoints(nrow(z)))[order(order(y))]
    }))
    rownames(zz) <- NULL
    pv   <- attr(z, "postVar")
    cols <- 1:(dim(pv)[1])
    se   <- unlist(lapply(cols, function(i) sqrt(pv[i, i, ])))
    ## n.b.: depends on explicit column-major ordering of se/melt
    zzz <- cbind(melt(zz,id.vars="level",value.name="estimate"),
                 qq=qq,std.error=se)
    ## reorder columns:
    subset(zzz,select=c(variable, level, estimate, qq, std.error))
  }
  dd <- ldply(x,tmpf,.id="grp")
  ci.val <- -qnorm((1-ci.level)/2)
  transform(dd,
            p=2*pnorm(-abs(estimate/std.error)), ## 2-tailed p-val
            lb=estimate-ci.val*std.error,
            ub=estimate+ci.val*std.error)
}

# DATA HANDLING
# Import data, from both Category and SingleObject
LT.SingObj <- read.csv("../results/SingleObject_LT.csv", head=TRUE)
LT.Cat <- read.csv("../results/Category_LT.csv", head=TRUE)
# Set all factor variables to factors, with labels if meaningful
LT.SingObj$explo_overlap <- factor(LT.SingObj$explo_overlap)
LT.SingObj$theory <- factor(LT.SingObj$theory,
                            labels = c("Compound Representations",
                                       "Labels as Features"))
LT.Cat$explo_overlap <- factor(LT.Cat$explo_overlap)
LT.Cat$theory <- factor(LT.Cat$theory, labels = c("Compound Representations",
                                                  "Labels as Features"))

# SINGLE OBJECT MODEL -- FIXED EFFECTS
# All models taken as previous one minus last effect
LT.SingObj.lmer.0 <- lmer(looking_time ~ 1 + trial + labelled + theory +
                            trial:labelled + labelled:theory + trial:theory +
                            trial:labelled:theory +
                            (1 + trial + labelled | subject),
                          data = LT.SingObj)
LT.SingObj.lmer.1 <- update(LT.SingObj.lmer.0, . ~ . - trial:labelled:theory)
LT.SingObj.lmer.2 <- update(LT.SingObj.lmer.1, . ~ . - trial:theory)
LT.SingObj.lmer.3 <- update(LT.SingObj.lmer.2, . ~ . - labelled:theory)
LT.SingObj.lmer.4 <- update(LT.SingObj.lmer.3, . ~ . - trial:labelled)
LT.SingObj.lmer.5 <- update(LT.SingObj.lmer.4, . ~ . - theory)
LT.SingObj.lmer.6 <- update(LT.SingObj.lmer.5, . ~ . - labelled) # labelled marginally sig.
LT.SingObj.lmer.7 <- update(LT.SingObj.lmer.6, . ~ . - trial)
# Model comparison against each other hierarchically
LT.SingObj.comparison <- anova(LT.SingObj.lmer.7,
                               LT.SingObj.lmer.6,
                               LT.SingObj.lmer.5,
                               LT.SingObj.lmer.4,
                               LT.SingObj.lmer.3,
                               LT.SingObj.lmer.2,
                               LT.SingObj.lmer.1,
                               LT.SingObj.lmer.0)
#print(LT.SingObj.comparison)
# Select final fixed effects model
LT.SingObj.lmer.final <- update(LT.SingObj.lmer.0, . ~ . - labelled)

# Computing confidence intervals for parameter estimates, storing as a dataframe
#LT.SingObj.CI <- cbind(as.data.frame(confint(LT.SingObj.lmer.final, parm="beta_", level=0.89))[1:3,],
#                       as.data.frame(confint(LT.SingObj.lmer.final, parm="beta_", level=0.97))[1:3,]) # Only keep effects without theory
#LT.SingObj.CI$estimate <- fixef(LT.SingObj.lmer.final)[1:3]
#LT.SingObj.CI$parameter <- c("Intercept","Trial","Condition (no label)") # Add meaningful names
#LT.SingObj.CI$intercept <- c("Intercept","Coefficients", "Coefficients")
#LT.SingObj.CI$theory <- "Both theories"

# SINGLE OBJECT MODEL -- MAKE PREDICTIONS
#LT.SingObj$fit <- predict(LT.SingObj.lmer.final, newdata=LT.SingObj, re.form=NA)
#write.csv(LT.SingObj, file="../Results/SingleObject_LT_fitted.csv", row.names=F)

# SINGLE OBJECT PER THEORY -- MAIN EFFECTS
# Create sub-datasets
LT.SingObj.LaF <- LT.SingObj[LT.SingObj$theory == "Labels as Features",]
LT.SingObj.CR <- LT.SingObj[LT.SingObj$theory == "Compound Representations",]
# LABELS AS FEATURES
# All models taken as previous one minus last effect
LT.SingObj.LaF.lmer.0 <- lmer(looking_time ~ 1 + trial + labelled +
                                trial:labelled +
                                (1 + trial + labelled | subject),
                              data = LT.SingObj.LaF)
LT.SingObj.LaF.lmer.1 <- update(LT.SingObj.LaF.lmer.0, . ~ . - trial:labelled)
LT.SingObj.LaF.lmer.2 <- update(LT.SingObj.LaF.lmer.1, . ~ . - labelled)
LT.SingObj.LaF.lmer.3 <- update(LT.SingObj.LaF.lmer.2, . ~ . - trial)
# Model comparison against each other hierarchically
LT.SingObj.LaF.comparison <- anova(LT.SingObj.LaF.lmer.3,
                                   LT.SingObj.LaF.lmer.2,
                                   LT.SingObj.LaF.lmer.1,
                                   LT.SingObj.LaF.lmer.0)
#print(LT.SingObj.LaF.comparison)
# Select final fixed effects model
LT.SingObj.LaF.lmer.final <- LT.SingObj.LaF.lmer.0
# Compute confidence intervals
# LT.SingObj.LaF.CI <- cbind(as.data.frame(confint(LT.SingObj.LaF.final, parm="beta_", level=0.89)),
#                            as.data.frame(confint(LT.SingObj.LaF.final, parm="beta_", level=0.97)))
# LT.SingObj.LaF.CI$estimate <- fixef(LT.SingObj.LaF.final)
# LT.SingObj.LaF.CI$parameter <- c("Intercept","Trial","Condition (no label)", "Trial * Condition (no label)") # Add meaningful names
# LT.SingObj.LaF.CI$intercept <- c("Intercept","Coefficients","Coefficients","Coefficients")
# LT.SingObj.LaF.CI$theory <- "Labels as Features"
# # Preparing random effects dataframe for caterpillar plots
# rf <- ranef(LT.SingObj.LaF.final, condVar=T)
# LT.SingObj.LaF.ranef <- augment(rf)
# LT.SingObj.LaF.ranef$theory <- "Labels as Features"
# levels(LT.SingObj.LaF.ranef$variable) <- c("Intercept","Trial","Condition (no label)")
# COMPOUND REPRESENTATIONS
# All models taken as previous one minus last effect
LT.SingObj.CR.lmer.0 <- lmer(looking_time ~ 1 + trial + labelled +
                               trial:labelled +
                               (1 + trial + labelled | subject),
                             data = LT.SingObj.CR)
LT.SingObj.CR.lmer.1 <- update(LT.SingObj.CR.lmer.0, . ~ . - trial:labelled)
LT.SingObj.CR.lmer.2 <- update(LT.SingObj.CR.lmer.1, . ~ . - labelled)  # No effect of labelled
LT.SingObj.CR.lmer.3 <- update(LT.SingObj.CR.lmer.2, . ~ . - trial)
# Model comparison against each other hierarchically
LT.SingObj.CR.comparison <- anova(LT.SingObj.CR.lmer.3,
                                  LT.SingObj.CR.lmer.2,
                                  LT.SingObj.CR.lmer.1,
                                  LT.SingObj.CR.lmer.0)
#print(LT.SingObj.CR.comparison)
# Select final fixed effects model
LT.SingObj.CR.lmer.final <- update(LT.SingObj.CR.lmer.0, . ~ . - labelled)
# Compute confidence intervals
# LT.SingObj.CR.CI <- cbind(as.data.frame(confint(LT.SingObj.CR.final, parm="beta_", level=0.89)),
#                           as.data.frame(confint(LT.SingObj.CR.final, parm="beta_", level=0.97)))
# LT.SingObj.CR.CI$estimate <- fixef(LT.SingObj.CR.final)
# LT.SingObj.CR.CI$parameter <- c("Intercept","Trial") # Add meaningful names
# LT.SingObj.CR.CI$intercept <- c("Intercept","Coefficients")
# LT.SingObj.CR.CI$theory <- "Compound Representations"
# # Preparing random effects dataframe for caterpillar plots
# rf <- ranef(LT.SingObj.CR.final, condVar=T)
# LT.SingObj.CR.ranef <- augment(rf)
# LT.SingObj.CR.ranef$theory <- "Compound Representations"
# levels(LT.SingObj.CR.ranef$variable) <- c("Intercept","Trial","Condition (no label)")

# CATEGORY MODEL -- FIXED EFFECTS
# All models taken as previous one minus last effect
LT.Cat.lmer.0 <- lmer(looking_time ~ 1 + trial + labelled + theory +
                        labelled:theory + trial:theory +
                        (1 + trial + labelled | subject),
                      data = LT.Cat)
LT.Cat.lmer.1 <- update(LT.Cat.lmer.0, . ~ . - trial:theory)    # No effect of trial:theory
LT.Cat.lmer.2 <- update(LT.Cat.lmer.1, . ~ . - labelled:theory)
LT.Cat.lmer.3 <- update(LT.Cat.lmer.2, . ~ . - theory)
LT.Cat.lmer.4 <- update(LT.Cat.lmer.3, . ~ . - labelled)
LT.Cat.lmer.5 <- update(LT.Cat.lmer.4, . ~ . - trial)
# Model comparison against each other hierarchically
LT.Cat.comparison <- anova(LT.Cat.lmer.5,
                           LT.Cat.lmer.4,
                           LT.Cat.lmer.3,
                           LT.Cat.lmer.2,
                           LT.Cat.lmer.1,
                           LT.Cat.lmer.0)
#print(LT.Cat.comparison)
# Select final fixed effects model
LT.Cat.lmer.final <- update(LT.Cat.lmer.0, . ~ . - trial:theory)

# Compute CI
# LT.Cat.CI <- cbind(as.data.frame(confint(LT.Cat.final, parm="beta_", level=0.89)),
#                    as.data.frame(confint(LT.Cat.final, parm="beta_", level=0.97)))[1:4,]# Only keep effects without theory
# LT.Cat.CI$estimate <- fixef(LT.Cat.final)[1:4]
# LT.Cat.CI$parameter <- c("Intercept","Trial","Condition (no label)", "Trial * Condition (no label)") # Add meaningful names
# LT.Cat.CI$intercept <- c("Intercept","Coefficients","Coefficients","Coefficients")
# LT.Cat.CI$theory <- "Both theories"

# CATEGORY MODEL -- MAKE PREDICTIONS
#LT.Cat$fit <- predict(LT.Cat.final, newdata=LT.Cat, re.form=NA)
#write.csv(LT.Cat, file="../Results/Category_LT_fitted.csv", row.names=F)

# CATEGORY PER THEORY -- MAIN EFFECTS
# Create sub-datasets
LT.Cat.LaF <- LT.Cat[LT.Cat$theory == "Labels as Features",]
LT.Cat.CR <- LT.Cat[LT.Cat$theory == "Compound Representations",]
# LABELS AS FEATURES
# All models taken as previous one minus last effect
LT.Cat.LaF.lmer.0 <- lmer(looking_time ~ 1 + trial + labelled +
                            trial:labelled +
                            (1 + trial + labelled | subject),
                          data = LT.Cat.LaF)
LT.Cat.LaF.lmer.1 <- update(LT.Cat.LaF.lmer.0, . ~ . - trial:labelled)
LT.Cat.LaF.lmer.2 <- update(LT.Cat.LaF.lmer.1, . ~ . - labelled)
LT.Cat.LaF.lmer.3 <- update(LT.Cat.LaF.lmer.2, . ~ . - trial)
# Model comparison against each other hierarchically
LT.Cat.LaF.comparison <- anova(LT.Cat.LaF.lmer.3,
                               LT.Cat.LaF.lmer.2,
                               LT.Cat.LaF.lmer.1,
                               LT.Cat.LaF.lmer.0)
#print(LT.Cat.LaF.comparison)
# Select final fixed effects model
LT.Cat.LaF.lmer.final <- LT.Cat.LaF.lmer.0
# Compute confidence intervals
# LT.Cat.LaF.CI <- cbind(as.data.frame(confint(LT.Cat.LaF.final, parm="beta_", level=0.89)),
#                        as.data.frame(confint(LT.Cat.LaF.final, parm="beta_", level=0.97)))
# LT.Cat.LaF.CI$estimate <- fixef(LT.Cat.LaF.final)
# LT.Cat.LaF.CI$parameter <- c("Intercept","Trial","Condition (no label)", "Trial * Condition (no label)") # Add meaningful names
# LT.Cat.LaF.CI$intercept <- c("Intercept","Coefficients","Coefficients","Coefficients")
# LT.Cat.LaF.CI$theory <- "Labels as Features"
# # Preparing random effects dataframe for caterpillar plots
# rf <- ranef(LT.Cat.LaF.final, condVar=T)
# LT.Cat.LaF.ranef <- augment(rf)
# LT.Cat.LaF.ranef$theory <- "Labels as Features"
# levels(LT.Cat.LaF.ranef$variable) <- c("Intercept","Trial","Condition (no label)")
# COMPOUND REPRESENTATIONS
# All models taken as previous one minus last effect
LT.Cat.CR.lmer.0 <- lmer(looking_time ~ 1 + trial + labelled +
                           (1 + trial + labelled | subject),
                         data = LT.Cat.CR)
LT.Cat.CR.lmer.1 <- update(LT.Cat.CR.lmer.0, . ~ . - labelled)  # No effect of labelled
LT.Cat.CR.lmer.2 <- update(LT.Cat.CR.lmer.1, . ~ . - trial)
# Model comparison against each other hierarchically
LT.Cat.CR.comparison <- anova(LT.Cat.CR.lmer.2,
                              LT.Cat.CR.lmer.1,
                              LT.Cat.CR.lmer.0)
#print(LT.Cat.CR.comparison)
# Select final fixed effects model
LT.Cat.CR.lmer.final <- update(LT.Cat.CR.lmer.0, . ~ . - labelled)
# Compute confidence intervals
# LT.Cat.CR.CI <- cbind(as.data.frame(confint(LT.Cat.CR.final, parm="beta_", level=0.89)),
#                       as.data.frame(confint(LT.Cat.CR.final, parm="beta_", level=0.97)))
# LT.Cat.CR.CI$estimate <- fixef(LT.Cat.CR.final)
# LT.Cat.CR.CI$parameter <- c("Intercept","Trial") # Add meaningful names
# LT.Cat.CR.CI$intercept <- c("Intercept","Coefficients")
# LT.Cat.CR.CI$theory <- "Compound Representations"
# # Preparing random effects dataframe for caterpillar plots
# rf <- ranef(LT.Cat.CR.final, condVar=T)
# LT.Cat.CR.ranef <- augment(rf)
# LT.Cat.CR.ranef$theory <- "Compound Representations"
# levels(LT.Cat.CR.ranef$variable) <- c("Intercept","Trial","Condition (no label)")


# # MIXED EFFECT MODELS GRAPHS
#
# # SINGLE OBJECT -- FIXED EFFECT
# # Merge confindence intervals dataframes for global, LaF, and CR
# LT.SingObj.all.CI <- rbind(LT.SingObj.CI, LT.SingObj.LaF.CI, LT.SingObj.CR.CI)
# LT.SingObj.plot.CI <- ggplot(data=LT.SingObj.all.CI,
#                              aes(x=parameter, y=estimate,
#                                  shape=theory, colour=theory)) +
#   ylab("Estimate") + xlab("") + theme_bw() +
#   scale_shape_manual(name = "Theory",
#                      breaks = c("Labels as Features","Compound Representations","Both theories"),
#                      values = c(21,23,24)) +
#   scale_colour_brewer(name = "Theory",
#                       breaks = c("Labels as Features","Compound Representations","Both theories"),
#                       palette="Dark2") +
#   coord_flip() + facet_grid(~intercept, scales="free_x", space="free_x") +
#   scale_y_continuous(breaks = seq(-3,43,0.5)) +
#   geom_errorbar(aes(ymin=`5.5 %`, ymax=`94.5 %`),
#                 width=0,
#                 position=position_dodge(0.5)) +
#   geom_errorbar(aes(ymin=`1.5 %`, ymax=`98.5 %`), alpha=0.6,
#                 width=0,
#                 position=position_dodge(0.5)) +
#   geom_point(position=position_dodge(0.5),
#              size=1, fill="white")
# ggsave("../Results/FixedEffects_SingleObject.pdf", plot = LT.SingObj.plot.CI,
#        height = 2, width = 10)
#
# # CATEGORY -- FIXED EFFECT
# # Merge confindence intervals dataframes for global, LaF, and CR
# LT.Cat.all.CI <- rbind(LT.Cat.CI, LT.Cat.LaF.CI, LT.Cat.CR.CI)
# LT.Cat.plot.CI <- ggplot(data=LT.Cat.all.CI,
#                              aes(x=parameter, y=estimate,
#                                  shape=theory, colour=theory)) +
#   ylab("Estimate") + xlab("") + theme_bw() +
#   scale_shape_manual(name = "Theory",
#                      breaks = c("Labels as Features","Compound Representations","Both theories"),
#                      values = c(21,23,24)) +
#   scale_colour_brewer(name = "Theory",
#                       breaks = c("Labels as Features","Compound Representations","Both theories"),
#                       palette="Dark2") +
#   coord_flip() + facet_grid(~intercept, scales="free_x", space="free_x") +
#   scale_y_continuous(breaks = seq(-3,43,0.5)) +
#   geom_errorbar(aes(ymin=`5.5 %`, ymax=`94.5 %`),
#                 width=0,
#                 position=position_dodge(0.5)) +
#   geom_errorbar(aes(ymin=`1.5 %`, ymax=`98.5 %`), alpha=0.6,
#                 width=0,
#                 position=position_dodge(0.5)) +
#   geom_point(position=position_dodge(0.5),
#              size=1, fill="white")
# ggsave("../Results/FixedEffects_Category.pdf", plot = LT.Cat.plot.CI,
#        height = 2, width = 10)
#
# # SINGLE OBJECT -- RANDOM EFFECTS
# LT.SingObj.ranef <- rbind(LT.SingObj.LaF.ranef, LT.SingObj.CR.ranef)
# LT.SingObj.plot.ranef <- ggplot(LT.SingObj.ranef, aes(estimate, level, xmin=lb, xmax=ub)) +
#   theme_bw() + xlab("Estimate") +
#   theme(axis.text.y = element_blank(),
#         axis.title.y = element_blank(),
#         axis.ticks.y = element_blank(),
#         axis.text.x = element_text(size=6)) +
#   scale_x_continuous(breaks = seq(-20,10,2)) +
#   geom_errorbarh(height=0) + geom_vline(xintercept=0, lty=2) +
#   geom_point(size=1, fill="white", shape=21) +
#   facet_grid(theory~variable, scale="free", space="free_x")
# ggsave("../Results/RandomEffects_SingleObject.pdf", plot = LT.SingObj.plot.ranef,
#        height = 4, width = 6)
#
# # CATEGORY -- RANDOM EFFECTS
# LT.Cat.ranef <- rbind(LT.Cat.LaF.ranef, LT.Cat.CR.ranef)
# LT.Cat.plot.ranef <- ggplot(LT.Cat.ranef, aes(estimate, level, xmin=lb, xmax=ub)) +
#   theme_bw() + xlab("Estimate") +
#   theme(axis.text.y = element_blank(),
#         axis.title.y = element_blank(),
#         axis.ticks.y = element_blank(),
#         axis.text.x = element_text(size=6)) +
#   scale_x_continuous(breaks = seq(-20,10,2)) +
#   geom_errorbarh(height=0) + geom_vline(xintercept=0, lty=2) +
#   geom_point(size=1, fill="white", shape=21) +
#   facet_grid(theory~variable, scale="free", space="free_x")
# ggsave("../Results/RandomEffects_Category.pdf", plot = LT.Cat.plot.ranef,
#        height = 4, width = 7)
