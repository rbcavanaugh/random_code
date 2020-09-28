library(tidyverse) #ggplot
library(lme4) #fit linear and generalized linear mixed-effect models
library(lmerTest) #tests linear mixed-effect models
library(performance) #calculate ICC\
library(bayesplot) # ppc dens overlay package
library(see)
library(insight)

WC.glmer.2 <- glmer(WordCount ~ GroupNumeric*Barrier + (1 + Barrier|SubjectID) + (1|Object), data = dat, family = poisson) 

yrep_poisson<-t(as.matrix(performance::pp_check(WC.glmer.obs)))
y<-dat$WordCount
color_scheme_set("brightblue")
ppc_dens_overlay(y, yrep_poisson) 

