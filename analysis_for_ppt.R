# Example analyses and plots for the survival analysis class 2/23/26
# Jillian Whitton (jillian.whitton@northwestern.edu)
# February 2026

# my "standard" library-install set
library(readr)
library(tidyverse)
library(readxl)
library(emmeans)
library(glmertree)
library(RColorBrewer)
library(rlang)
library(ggplot2)
#install.packages("crosstable")
library(crosstable)

# install survival analysis packages
# note that the "lung" and "aml" data are included in the survival package
library(survival)
#install.packages("ggsurvfit")
library(ggsurvfit)
#install.packages("survminer")
library(survminer)
#install.packages("cmprsk")
library(cmprsk)


# Our tiny example from the powerpoint, plotted

# create the "tiny_km" tibble containing the data

time <- c(0, 34, 57, 64, 78, 138, 186, 246, 293, 365, 365)
status <- c(NA, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0)
person <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
atrisk <- c(10, 9, 8, 7, 6, 5, 4, 3, 2, 2, 2)

tiny_km <- tibble(time, status, person, atrisk)


# create a graphics object using survfit2
# - which has imilar laguage and options to ggplot2

gg_tinykm <-
  survfit2(Surv(time, status) ~ person, data = tiny_km) %>%
  ggsurvfit(color="purple", linewidth=2) +
  scale_x_continuous(limits=c(0, 365)) +
  scale_y_continuous(limits=c(0, 1)) +
  add_censor_mark(size = 4, alpha = 2, color="purple")# +
#  labs(title = "Tiny example")

# generate the graph
gg_tinykm


# A real-world example using the "lung" data in the R "survival" package
# (adapted from https://thriv.github.io/biodatasci2018/r-survival.html
# - translated to ggsurvfit, and with risk table dropped for clarity)

lung <- as_tibble(lung)

# Create the plot, including 95% confidence intervals, a p-value,
# and marks for censored data
gg_lung <-
  survfit2(Surv(time, status) ~ sex, data = lung) %>%
  ggsurvfit() +
  add_confidence_interval() +
  add_pvalue() +
  scale_ggsurvfit() +
  add_censor_mark() +
  scale_color_manual(values = c('dodgerblue2', 'orchid2'),
                     labels = c("Male", "Female")) +
  scale_fill_manual(values = c('dodgerblue2', 'orchid2'),
                    labels = c("Male", "Female")) +
  labs(title = "Kaplan-Meier Curve for Lung Cancer Survival")

gg_lung  

# Univariate Cox regression using the lung data

# fit cox model and save results
lung_cox_sex <- coxph(Surv(time, status) ~ sex, data=lung)
# summary of results
summary(lung_cox_sex)

# test for proportionality
cox.zph(lung_cox_sex)

# Multivariable Cox regression, including age and weight loss in addition to sex

# fit cox model and save results
lung_cox_mv <- coxph(Surv(time, status) ~ age + sex + wt.loss, data=lung)
# summary of results
summary(lung_cox_mv)

# test for proportionality
cox.zph(lung_cox_mv)

# competing risks with the cmprisk package (adapted from https://stackoverflow.com/questions/64186565/customizing-a-competing-risks-plot-in-r-with-package-cmprsk)

# simulated data to get started (reduced from the original for groups to two, for clarity)
comp.risk.data2 <- data.frame("tfs.days" = rweibull(n = 100, shape = 1, scale = 1)*100,
                             "status.tfs" = c(sample(c(0,1,1,1,1,2), size=50, replace=T)),
                             "Typing" = sample(c("M","F"), size=100, replace=T))

# fitting a competing risks model
CR2 <- cuminc(ftime = comp.risk.data2$tfs.days, 
             fstatus = comp.risk.data2$status.tfs, 
             cencode = 0,
             group = comp.risk.data2$Typing)

p <- ggcompetingrisks(fit = CR2, 
                      multiple_panels = F, 
                      xlab = "Days", 
                      ylab = "Cumulative incidence of event",
                      title = "Competing Risks Analysis: simulated data for two groups, two events") 

p$mapping <- aes(x = time, y = est, colour = group, linetype = event)

p + labs(linetype = "event", colour = "group")























































































