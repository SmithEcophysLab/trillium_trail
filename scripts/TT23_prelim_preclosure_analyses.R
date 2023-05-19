## Load libraries
library(tidyverse)
library(lme4)
library(car)
library(multcomp)
library(MuMIn)
library(glmmTMB)

## Read in compiled curve fit file
df <- read.csv("../data/TT23_pre_closure_phys_data.csv") %>%
  mutate(gm.trt = factor(gm.trt, levels = c("weeded", "invaded")))
head(df)

##############################################################################
## Vcmax regressed against garlic mustard total density (rosettes + adults)
##############################################################################
vcmax <- lmer(log(vcmax) ~ gm.trt * spp + (1 | plot), data = df)

# Check model assumptions
plot(vcmax)
qqnorm(residuals(vcmax))
qqline(residuals(vcmax))
densityPlot(residuals(vcmax))
shapiro.test(residuals(vcmax))
outlierTest(vcmax)

# Model output
summary(vcmax)
Anova(test.vcmax)
r.squaredGLMM(vcmax)

# Post-hoc comparisons
emmeans(vcmax, pairwise~spp, type = "response")

##############################################################################
## Jmax
##############################################################################
df$jmax[20] <- NA

jmax <- lmer(jmax ~ gm.trt * spp + (1 | plot), data = df)

# Check model assumptions
plot(jmax)
qqnorm(residuals(jmax))
qqline(residuals(jmax))
densityPlot(residuals(jmax))
shapiro.test(residuals(jmax))
outlierTest(jmax)

# Model output
summary(jmax)
Anova(jmax)
r.squaredGLMM(jmax)

# Post-hoc comparisons
emmeans(jmax, pairwise~spp)


##############################################################################
## Jmax
##############################################################################
df$jmax.vcmax[91] <- NA

jvmax <- lmer(log(jmax.vcmax) ~ gm.trt * spp + (1 | plot), data = df)

# Check model assumptions
plot(jvmax)
qqnorm(residuals(jvmax))
qqline(residuals(jvmax))
densityPlot(residuals(jvmax))
shapiro.test(residuals(jvmax))
outlierTest(jvmax)

# Model output
summary(jvmax)
Anova(jvmax)
r.squaredGLMM(jvmax)

# Post-hoc comparisons
emmeans(jvmax, pairwise~gm.trt, type = "response") ## Lower ratio in weeded plots


##############################################################################
## Ci:Ca
##############################################################################
df$ci.ca[c(47, 83)] <- NA

ci.ca <- lmer(ci.ca ~ gm.trt * spp + (1 | plot), data = df)

# Check model assumptions
plot(ci.ca)
qqnorm(residuals(ci.ca))
qqline(residuals(ci.ca))
densityPlot(residuals(ci.ca))
shapiro.test(residuals(ci.ca))
outlierTest(ci.ca)

# Model output
summary(ci.ca)
Anova(ci.ca)
r.squaredGLMM(ci.ca)

# Post-hoc comparisons
cld(emmeans(ci.ca, pairwise~spp*gm.trt), Letters = LETTERS)
## GM invasion insignificantly increases Ci:Ca in Trillium; decreases
## Ci:Ca in Maianthemum

##############################################################################
## gsw
##############################################################################
df$gsw[c(47,91,111,117,128)] <- NA

gsw <- lmer(log(gsw) ~ gm.trt * spp + (1 | plot), data = df)

# Check model assumptions
plot(gsw)
qqnorm(residuals(gsw))
qqline(residuals(gsw))
densityPlot(residuals(gsw))
shapiro.test(residuals(gsw))
outlierTest(gsw)

# Model output
summary(gsw)
Anova(gsw)
r.squaredGLMM(gsw)

# Post-hoc comparisons
cld(emmeans(gsw, pairwise~spp*gm.trt, type = "response"), Letters = LETTERS)
## GM invasion decreases gsw, but only in Maianthemum

##############################################################################
## iwue
##############################################################################
df$iwue[c(39, 78, 83, 94, 101)] <- NA

iwue <- lmer(log(iwue) ~ gm.trt * spp + (1 | plot), data = df)

# Check model assumptions
plot(iwue)
qqnorm(residuals(iwue))
qqline(residuals(iwue))
densityPlot(residuals(iwue))
shapiro.test(residuals(iwue))
outlierTest(iwue)

# Model output
summary(iwue)
Anova(iwue)
r.squaredGLMM(iwue)

# Post-hoc comparisons
cld(emmeans(iwue, pairwise~spp*gm.trt, type = "response"), Letters = LETTERS)
## GM invasion increases water use efficiency, but only in Maianthemum

##############################################################################
## Stomatal limitation
##############################################################################
df$stom.lim[df$stom.lim > 1 | df$stom.lim < 0] <- NA

stom.lim <- lmer(log(stom.lim) ~ gm.trt * spp + (1 | plot), data = df)

# Check model assumptions
plot(stom.lim)
qqnorm(residuals(stom.lim))
qqline(residuals(stom.lim))
densityPlot(residuals(stom.lim))
shapiro.test(residuals(stom.lim))
outlierTest(stom.lim)

# Model output
summary(stom.lim)
Anova(stom.lim)
r.squaredGLMM(stom.lim)

# Post-hoc comparisons
emmeans(stom.lim, pairwise~spp, type = "response")
## Greater stomatal limitation in Trillium

##############################################################################
## Energetic cost of maintaining carboxylation
##############################################################################
rd.vcmax <- lmer(rd.vcmax ~ gm.trt * spp + (1 | plot), data = df)

# Check model assumptions
plot(rd.vcmax)
qqnorm(residuals(rd.vcmax))
qqline(residuals(rd.vcmax))
densityPlot(residuals(rd.vcmax))
shapiro.test(residuals(rd.vcmax))
outlierTest(rd.vcmax)

# Model output
summary(rd.vcmax)
Anova(rd.vcmax)
r.squaredGLMM(rd.vcmax)

# Post-hoc comparisons
emmeans(rd.vcmax, pairwise~spp, type = "response")
## Greater costs of maintaining carboxylation in Trillium (likely just driven
## by greater Vcmax)

##############################################################################
## Categorical treatment plots
##############################################################################
## Vcmax categorical
vcmax.cat <- ggplot(data = df, 
                    aes(x = gm.trt, y = vcmax, fill = spp)) +
  geom_boxplot(alpha = 0.75) +
  geom_point(shape = 21, size = 3, alpha = 0.75, 
             position = position_jitterdodge(jitter.width = 0.1, 
                                             dodge.width = 0.75)) +
  labs(x = "Garlic mustard treatment", 
       y = expression(bold("V"["cmax"]*" ("*mu*"mol"*" m"^"-2"*" s"^"-1"*")")),
       fill = "Species", color = "Species") +
  theme_bw(base_size = 18) +
  theme(axis.title = element_text(face = "bold"),
        legend.title = element_text(face = "bold"))

jmax.cat <- ggplot(data = df, 
                   aes(x = gm.trt, y = jmax, fill = spp)) +
  geom_boxplot(alpha = 0.75) +
  geom_point(shape = 21, size = 3, alpha = 0.75, 
             position = position_jitterdodge(jitter.width = 0.1, 
                                             dodge.width = 0.75)) +
  labs(x = "Garlic mustard treatment", 
       y = expression(bold("J"["max"]*" ("*mu*"mol"*" m"^"-2"*" s"^"-1"*")")),
       fill = "Species", color = "Species") +
  theme_bw(base_size = 18) +
  theme(axis.title = element_text(face = "bold"),
        legend.title = element_text(face = "bold"))

cica.cat <- ggplot(data = df, 
                   aes(x = gm.trt, y = ci.ca, fill = spp)) +
  geom_boxplot(alpha = 0.75) +
  geom_point(shape = 21, size = 3, alpha = 0.75, 
             position = position_jitterdodge(jitter.width = 0.1, 
                                             dodge.width = 0.75)) +
  scale_y_continuous(limits = c(0.45, 0.9), breaks = seq(0.5, 0.9, 0.1)) +
  labs(x = "Garlic mustard treatment", 
       y = expression(bold("C"["i"]*": C"["a"]*" (unitless)")),
       fill = "Species", color = "Species") +
  theme_bw(base_size = 18) +
  theme(axis.title = element_text(face = "bold"),
        legend.title = element_text(face = "bold"))


##############################################################################
##############################################################################
## Summary of categorical treatments
##############################################################################
##############################################################################
# Garlic mustard treatments have limited effects on leaf physiology, namely
# Rubisco carboxylation and RuBP regeneration rates. However, treatments seem
# to have a strong impact on water usage, which are particularly apparent in 
# Maianthemum. GM presence decreased Maianthemum stomatal conductance and water
# use efficiency.

# However, I am curious if the lack of garlic mustard presence in plot 7 might
# be skewing these results. To address this, the following code substitutes
# garlic mustard density for garlic mustard treatment. Note that this approach
# creates large zero inflation in models, so a zero-inflated, generalized linear
# mixed effect model is used.




##############################################################################
## Zero-inflated GLMM for Vcmax
##############################################################################
df$vcmax[c(20, 34)] <- NA

vcmax.gmdens <- lmer(log(vcmax) ~ gm.total.dens * spp + (1 | plot), data = df)

# Check model assumptions
plot(vcmax.gmdens)
qqnorm(residuals(vcmax.gmdens))
qqline(residuals(vcmax.gmdens))
densityPlot(residuals(vcmax.gmdens))
shapiro.test(residuals(vcmax.gmdens))
outlierTest(vcmax.gmdens)

# Model output
summary(vcmax.gmdens)
Anova(vcmax.gmdens)
r.squaredGLMM(vcmax.gmdens)

# Post-hoc comparisons
emmeans(vcmax.gmdens, pairwise~spp, type = "response")
test(emtrends(vcmax.gmdens, ~spp, "gm.total.dens", type = "response"))

##############################################################################
## Zero-inflated GLMM for Jmax
##############################################################################
jmax.gmdens <- lmer(jmax ~ gm.total.dens * spp + (1 | plot), data = df)


# Check model assumptions
plot(jmax.gmdens)
qqnorm(residuals(jmax.gmdens))
qqline(residuals(jmax.gmdens))
densityPlot(residuals(jmax.gmdens))
shapiro.test(residuals(jmax.gmdens))
outlierTest(jmax.gmdens)

# Model output
summary(jmax.gmdens)
Anova(jmax.gmdens)
r.squaredGLMM(jmax.gmdens)

# Post-hoc comparisons
test(emtrends(jmax.gmdens, ~spp, "gm.total.dens"))

##############################################################################
## Zero-inflated GLMM for Ci:Ca
##############################################################################
cica.gmdens <- lmer(ci.ca ~ gm.total.dens * spp + (1 | plot), data = df)

# Check model assumptions
plot(cica.gmdens)
qqnorm(residuals(cica.gmdens))
qqline(residuals(cica.gmdens))
densityPlot(residuals(cica.gmdens))
shapiro.test(residuals(cica.gmdens))
outlierTest(cica.gmdens)

# Model output
summary(cica.gmdens)
Anova(cica.gmdens)
r.squaredGLMM(cica.gmdens)

# Post-hoc comparisons
test(emtrends(cica.gmdens, ~spp, "gm.total.dens"))

##############################################################################
## Zero-inflated GLMM for iWUE
##############################################################################
iwue.gmdens <- lmer(iwue ~ gm.total.dens * spp + (1 | plot), data = df)

# Check model assumptions
plot(iwue.gmdens)
qqnorm(residuals(iwue.gmdens))
qqline(residuals(iwue.gmdens))
densityPlot(residuals(iwue.gmdens))
shapiro.test(residuals(iwue.gmdens))
outlierTest(iwue.gmdens)

# Model output
summary(iwue.gmdens)
Anova(iwue.gmdens)
r.squaredGLMM(iwue.gmdens)

# Post-hoc comparisons
test(emtrends(iwue.gmdens, ~spp, "gm.total.dens"))

##############################################################################
## Rd:Vcmax regressed against GM total density
##############################################################################
rd.vcmax.gmdens <- lmer(rd.vcmax ~ gm.total.dens * spp + (1 | plot), data = df)

# Check model assumptions
plot(rd.vcmax.gmdens)
qqnorm(residuals(rd.vcmax.gmdens))
qqline(residuals(rd.vcmax.gmdens))
densityPlot(residuals(rd.vcmax.gmdens))
shapiro.test(residuals(rd.vcmax.gmdens))
outlierTest(rd.vcmax.gmdens)

# Model output
summary(rd.vcmax.gmdens)
Anova(rd.vcmax.gmdens)
r.squaredGLMM(rd.vcmax.gmdens)

# Post-hoc comparisons
emmeans(rd.vcmax.gmdens, pairwise~spp)

##############################################################################
## Stomatal limitation regressed against GM total density
##############################################################################
stom.lim.gmdens <- lmer(log(stom.lim) ~ gm.total.dens * spp + (1 | plot), data = df)

# Check model assumptions
plot(stom.lim.gmdens)
qqnorm(residuals(stom.lim.gmdens))
qqline(residuals(stom.lim.gmdens))
densityPlot(residuals(stom.lim.gmdens))
shapiro.test(residuals(stom.lim.gmdens))
outlierTest(stom.lim.gmdens)

# Model output
summary(stom.lim.gmdens)
Anova(stom.lim.gmdens)
r.squaredGLMM(stom.lim.gmdens)

# Post-hoc comparisons
emmeans(stom.lim.gmdens, pairwise~spp)
test(emtrends(stom.lim.gmdens, ~spp, "gm.total.dens"))

##############################################################################
## Visualize broad patterns due to garlic mustard treatment
##############################################################################

## Vcmax categorical
vcmax.cat <- ggplot(data = df, 
       aes(x = gm.trt, y = vcmax, fill = spp)) +
  geom_boxplot(alpha = 0.75) +
  geom_point(shape = 21, size = 3, alpha = 0.75, 
             position = position_jitterdodge(jitter.width = 0.1, 
                                             dodge.width = 0.75)) +
  labs(x = "Garlic mustard treatment", 
       y = expression(bold("V"["cmax"]*" ("*mu*"mol"*" m"^"-2"*" s"^"-1"*")")),
       fill = "Species", color = "Species") +
  theme_bw(base_size = 18) +
  theme(axis.title = element_text(face = "bold"),
        legend.title = element_text(face = "bold"))

jmax.cat <- ggplot(data = df, 
                    aes(x = gm.trt, y = jmax, fill = spp)) +
  geom_boxplot(alpha = 0.75) +
  geom_point(shape = 21, size = 3, alpha = 0.75, 
             position = position_jitterdodge(jitter.width = 0.1, 
                                             dodge.width = 0.75)) +
  labs(x = "Garlic mustard treatment", 
       y = expression(bold("J"["max"]*" ("*mu*"mol"*" m"^"-2"*" s"^"-1"*")")),
       fill = "Species", color = "Species") +
  theme_bw(base_size = 18) +
  theme(axis.title = element_text(face = "bold"),
        legend.title = element_text(face = "bold"))

cica.cat <- ggplot(data = df, 
                   aes(x = gm.trt, y = ci.ca, fill = spp)) +
  geom_boxplot(alpha = 0.75) +
  geom_point(shape = 21, size = 3, alpha = 0.75, 
             position = position_jitterdodge(jitter.width = 0.1, 
                                             dodge.width = 0.75)) +
  scale_y_continuous(limits = c(0.45, 0.9), breaks = seq(0.5, 0.9, 0.1)) +
  labs(x = "Garlic mustard treatment", 
       y = expression(bold("C"["i"]*": C"["a"]*" (unitless)")),
       fill = "Species", color = "Species") +
  theme_bw(base_size = 18) +
  theme(axis.title = element_text(face = "bold"),
        legend.title = element_text(face = "bold"))



## Vcmax
ggplot(data = subset(df, gm.trt == "invaded"), aes(x = gm.total.dens, y = vcmax, fill = spp)) +
  geom_point(shape = 21, size = 4) +
  geom_smooth(method = 'lm') +
  labs(x = "Garlic mustard density (rosette + adults)", 
       y = expression(bold("V"["cmax"]*" ("*mu*"mol"*" m"^"-2"*" s"^"-1"*")")),
       fill = "Species", color = "Species") +
  theme_bw(base_size = 18) +
  theme(axis.title = element_text(face = "bold"),
        legend.title = element_text(face = "bold"))




## Jmax
ggplot(data = df, aes(x = spp, y = jmax, fill = gm.trt)) +
  geom_jitter(shape = 21, position = position_jitterdodge(jitter.width = 0.1, 
                                                          dodge.width = 0.75)) +
  geom_boxplot(alpha = 0.5) +
  labs(x = "Garlic mustard treatment", 
       y = expression(bold("J"["max"]*" ("*mu*"mol"*" m"^"-2"*" s"^"-1"*")")),
       fill = "Species", color = "Species") +
  theme_bw(base_size = 18) +
  theme(axis.title = element_text(face = "bold"),
        legend.title = element_text(face = "bold"))

## Jmax:Vcmax
ggplot(data = df, aes(x = gm.trt, y = jmax.vcmax)) +
  geom_boxplot(aes(fill = spp))

## Ci:Ca
ggplot(data = df, aes(x = gm.trt, y = ci.ca)) +
  geom_boxplot(aes(fill = spp))

## iWUE
ggplot(data = df, aes(x = gm.trt, y = iwue)) +
  geom_boxplot(aes(fill = spp))

## Rd:Vcmax
ggplot(data = df, aes(x = gm.trt, y = rd.vcmax)) +
  geom_boxplot(aes(fill = spp))